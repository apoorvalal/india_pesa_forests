# %% ####################################################
rm(list=ls())
set.seed(42)
library(LalRUtils)

libreq(tidyverse, data.table, zoo, tictoc, fixest,
  PanelMatch, patchwork, MCPanel, gsynth
  )

theme_set(lal_plot_theme())
# %% ####################################################
dbox_root = '/home/alal/res/india_pesa_forests'
sher_root = "/home/users/apoorval/aa_scratch/forests_analysis"
root = dbox_root
tmp  = file.path(root, "tmp")
# %%
tic()
load(file.path(tmp, "regdata.rds"))
toc()
# %% VCF prep
vcf_data = vcf_data[year>=1995]
vcf_data[, t := year - 1995]
ex_ante_med = quantile(vcf_data$cover_1990, 0.5)
vcf_data = vcf_data[cover_1990 > ex_ante_med]
vcf_data[, never_treated := max(D) == 0, cellid]
vcf_data |> str()

mindata = vcf_data[, .(forest_index, D, cellid, year, styear)]

#### ######## ########
 ##  ##       ##
 ##  ##       ##
 ##  ######   ######
 ##  ##       ##
 ##  ##       ##
#### ##       ########

# %%
ifeOut = interFE(forest_index ~ D, data = mindata, index=c("cellid", "year"),
               r = 2, force = "two-way", nboots = 50)
ifeOut
# %%
ifeOut2 = interFE(forest_index ~ D, data = mindata, index=c("cellid", "styear"),
               r = 2, force = "two-way", nboots = 50)
ifeOut2
# %%
##     ##  ######  ########     ###    ##    ## ######## ##
###   ### ##    ## ##     ##   ## ##   ###   ## ##       ##
#### #### ##       ##     ##  ##   ##  ####  ## ##       ##
## ### ## ##       ########  ##     ## ## ## ## ######   ##
##     ## ##       ##        ######### ##  #### ##       ##
##     ## ##    ## ##        ##     ## ##   ### ##       ##
##     ##  ######  ##        ##     ## ##    ## ######## ########
# %% reshape to wide for matrix completion
wideY = dcast(mindata, cellid  ~ year, value.var = "forest_index") %>% .[, -1] %>% as.matrix()
wideD = dcast(mindata, cellid  ~ year, value.var = "D") %>% .[, -1] %>% as.matrix()
# indices of Y0 observed
mask = (1 - wideD)
tic()
mcmod = mcnnm_cv(wideY, mask, to_estimate_u = 1, to_estimate_v = 1)
toc()
# 231.599 sec elapsed
# %%
predict_Y0 = function(MCObj){
  # number of units, number of time periods
  NN = length(MCObj$u); TT = length(MCObj$v)
  # imputed counterfactual outcome matrix
  Y0imp = ( # factor
    MCObj$L +
    # state FEs
    replicate(TT, MCObj$u) +
    # time FEs
    t(replicate(NN, MCObj$v))
    )
  return(Y0imp)
}
# %%
wideY0 = predict_Y0(mcmod)
treat_effects = wideY - wideY0
treat_effects[wideD] %>% mean()
# %%
