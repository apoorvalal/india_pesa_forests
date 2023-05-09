# Run on cluster
# takes around 9 hours on slurm job with specs

#SBATCH --begin=now
#SBATCH --time=12:00:00
#SBATCH --partition=hns
#SBATCH --mem=40G
#SBATCH --ntasks-per-node=12
#SBATCH --mail-type=ALL

# %% ####################################################
rm(list=ls())
library(LalRUtils)
libreq(tidyverse, data.table, zoo, tictoc, fst, fixest, PanelMatch, patchwork,
  rio, magrittr, janitor, did, panelView, vtable, RPushbullet)
set.seed(42)
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

vcf_data$cellid %>% nunique
# %% VCF prep
if (exists("vcf_data")){
  vcf = vcf_data[year>=1995]
  rm(vcf_data)
}
vcf[, t := year - 1995]
ex_ante_med = quantile(vcf$cover_1990, 0.5)
above_med = vcf[cover_1990 > ex_ante_med]
above_med[, never_treated := max(D) == 0, cellid]

########     ###    ##    ## ######## ##       ##     ##    ###    ########  ######  ##     ##
##     ##   ## ##   ###   ## ##       ##       ###   ###   ## ##      ##    ##    ## ##     ##
##     ##  ##   ##  ####  ## ##       ##       #### ####  ##   ##     ##    ##       ##     ##
########  ##     ## ## ## ## ######   ##       ## ### ## ##     ##    ##    ##       #########
##        ######### ##  #### ##       ##       ##     ## #########    ##    ##       ##     ##
##        ##     ## ##   ### ##       ##       ##     ## ##     ##    ##    ##    ## ##     ##
##        ##     ## ##    ## ######## ######## ##     ## ##     ##    ##     ######  ##     ##


# %% VCF
above_med[, stf := as.factor(state)]
pm_data = as.data.frame(above_med)
# %%
tic()
match_ps_vcf <- PanelMatch(lag = 4, time.id = "year", unit.id = "cellid",
                     treatment = "D", refinement.method = "ps.weight",
                     data = pm_data,
                     covs.formula = ~ I(lag(forest_index, 1:4)),
                     exact.match.variables = c("stf"),
                     outcome.var = "forest_index",
                     size.match = 5, qoi = "att" ,
                     match.missing = FALSE, listwise.delete = TRUE,
                     lead = 0:4, forbid.treatment.reversal = F,
                     use.diagonal.variance.matrix = T)
toc()
notif("matching done")

# %%
ps_results_vcf <- PanelEstimate(sets = match_ps_vcf, data = pm_data, se.method = "conditional")

# %%
toc()
bal = get_covariate_balance(match_ps_vcf$att,
                      data = pm_data,
                      covariates = "forest_index", plot = F
                      )
toc()

# %%
save(match_ps_vcf, ps_results_vcf, bal, file = file.path(root, "tmp/panelmatch_vcf2.RData"))
# %%

 ######  ##     ## ##     ## ##     ##    ###    ########  ##    ##
##    ## ##     ## ###   ### ###   ###   ## ##   ##     ##  ##  ##
##       ##     ## #### #### #### ####  ##   ##  ##     ##   ####
 ######  ##     ## ## ### ## ## ### ## ##     ## ########     ##
      ## ##     ## ##     ## ##     ## ######### ##   ##      ##
##    ## ##     ## ##     ## ##     ## ##     ## ##    ##     ##
 ######   #######  ##     ## ##     ## ##     ## ##     ##    ##
# %%
tic()
load(file.path(root, "tmp/panelmatch_vcf2.RData"))
toc()

# %% summary stats on matched set for ATT
msets <- match_ps_vcf$att

summary(msets) %>% names
summary(msets)$set.size.summary
summary(msets)$number.of.treated.units
summary(msets)$num.units.empty.set


# %% balance fig
baltab = data.frame(t = -4:-1, forest_index = bal[-nrow(bal), ])

(
  balfig = ggplot(baltab, aes(t, forest_index)) + geom_point(size = 2) +
  geom_hline(yintercept = 0, alpha = 0.8) +
  ylim(c(-0.5, 3.2)) +
  geom_hline(yintercept = c(-.25, .25), color = 'red', size = 0.5, linetype = 'dashed') +
  labs(y = "Standardised Difference in Lagged Forest Index",
    x = "Years since PESA")
)

# %%
ggsave(file.path(root, "out/panelmatch_vcf_balfig.pdf"), balfig, device = cairo_pdf)

# %% event study fig
vcf_res = data.frame(
  t =ps_results_vcf[['lead']],
  est = ps_results_vcf[['estimates']],
  se = ps_results_vcf[['standard.error']]
)  |> as.data.frame()
(f1 = vcf_res %>% ggplot(., aes(t, y = est)) +
  geom_point() +
  geom_pointrange(aes(ymin = est - 1.96*se, ymax = est + 1.96*se), alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = 'dotted') +
  # scale_y_continuous(position = "right", limits = c(-.5, 3.2)) +
  labs(y = "Treatment Effect", x = "Years since PESA")
)
ggsave(file.path(root, "out/panelmatch_vcf.pdf"), f1, device = cairo_pdf)

# %%
(ff = balfig | f1)

# %%
ggsave(file.path(root, "out/panelmatch_wide.pdf"), ff, device = cairo_pdf,
  width = 12, height = 5)

# %%
