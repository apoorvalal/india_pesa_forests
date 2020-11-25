#%%
rm(list = ls())
library(LalRUtils)
libreq(ggplot2, dplyr, tidyr, magrittr, rio, lfe, data.table, texreg, knitr, stargazer, broom,
      fastDummies, janitor, patchwork, tictoc, panelView, PanelMatch)
theme_set(lal_plot_theme())
####################################################
dbox_root = '~/Dropbox/1_Research/India_Forests'
sherlock_root = '/home/groups/gulzar/1_Research/India_Forests'
root = dbox_root
setwd(root)
out = '/home/alal/Dropbox/Apps/Overleaf/forests_paper/Output'
loc_out = file.path(root, "Output")
options(repr.plot.width=12, repr.plot.height=9)
#%%

######## ##     ## ######## ##    ## ########
##       ##     ## ##       ###   ##    ##
##       ##     ## ##       ####  ##    ##
######   ##     ## ######   ## ## ##    ##
##        ##   ##  ##       ##  ####    ##
##         ## ##   ##       ##   ###    ##
########    ###    ######## ##    ##    ##

#%% time series plot
tic()
df = import(file.path(root, "Data/Intermediate/est_clean2.rds")) %>% setDT
toc()

# %%
n = colnames(df)
n %>% grep('dist_to', .) %>% n[.] -> dropcols
df[, (dropcols) := NULL]
# %%
df[,
  treat_year :=  case_when(
     state_ut == 'Andhra Pradesh'   ~ 2001,
     state_ut == 'Chhattisgarh'     ~ 2005,
     state_ut == 'Gujarat'          ~ 2001,
     state_ut == 'Himachal Pradesh' ~ 2000,
     state_ut == 'Jharkhand'        ~ 2010,
     state_ut == 'Madhya Pradesh'   ~ 2000,
     state_ut == 'Maharashtra'      ~ 2007,
     state_ut == 'Odisha'           ~ 2002,
     state_ut == 'Rajasthan'        ~ 2000,
     TRUE ~ 0)
]
df[, `:=`(
  vilf = as.factor(code_2011),
  yf   = as.factor(year),
  stf  = as.factor(state_ut)
)]
df_pref = df[pref == 1]

matchdat = df_pref[, .(year, stf, pref_mean, pref_bin, D, vilf, def_ha)] %>% as.data.frame
matchdat$vilf = as.integer(matchdat$vilf)
matchdat$year = as.integer(matchdat$year)
matchdat$pref_bin = as.integer(matchdat$pref_bin)


# %% basic
tic()
match_basic <- PanelMatch(lag = 4, time.id = "year", unit.id = "vilf",
                         treatment = "D",
                         refinement.method = "none",
                         data = matchdat,
                         exact.match.variables = c("stf", "pref_bin"),
                         size.match = 5, qoi = "att" , outcome.var = "def_ha",
                         lead = 0:4, forbid.treatment.reversal = FALSE,
                         use.diagonal.variance.matrix = TRUE)
basic_results <- PanelEstimate(sets = match_basic, data = matchdat)
toc()

######################################################################
tic()
match_ps <- PanelMatch(lag = 4, time.id = "year", unit.id = "vilf",
                     treatment = "D",
                     refinement.method = "ps.weight",
                     data = matchdat,
                     covs.formula = ~ I(lag(def_ha, 1:4)),
                     exact.match.variables = c("stf", "pref_bin"),
                     size.match = 10, qoi = "att" , outcome.var = "def_ha",
                     lead = 0:4, forbid.treatment.reversal = FALSE,
                     use.diagonal.variance.matrix = TRUE)
ps_results <- PanelEstimate(sets = match_ps, data = matchdat)
toc()

save.image(file = file.path(out, "matching_ws.RData"))
print("Matching Done")
######## ####  ######
##        ##  ##    ##
##        ##  ##
######    ##  ##   ####
##        ##  ##    ##
##        ##  ##    ##
##       ####  ######

# %% run locally
load(file.path(loc_out, "matching_ws.RData"))

panelmatch_res = cbind(t = 0:4, est = basic_results$estimates,
      se = basic_results$standard.error) %>% as.data.frame

(f = panelmatch_res %>% ggplot(., aes(t, y = est)) +
  geom_point(colour = 'red') +
  geom_pointrange(aes(ymin = est - 1.96*se, ymax = est + 1.96*se), colour = 'red', alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = 'dotted') +
  NULL
  # labs(title = '', y = 'estimate',
  #   subtitle = "exact matching on State and ex-ante forest decile \n coarce matching on deforestation for 4 periods prior to PESA",
  #   caption  = 'Matching performed using PanelMatch')
)

# ggsave(file.path(out, 'Dyn_treat_eff_big.pdf'), twfe_event, device = cairo_pdf)
ggsave(file.path(out, 'panelmatch_fig.pdf'), f, device = cairo_pdf)
