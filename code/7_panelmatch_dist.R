# Run on cluster
# takes around 9 hours on slurm job with specs

# SBATCH --begin=now
# SBATCH --time=12:00:00
# SBATCH --partition=hns
# SBATCH --mem=40G
# SBATCH --ntasks-per-node=12
# SBATCH --mail-type=ALL

# %% ####################################################
# rm(list = ls())
sessionInfo()
libreq(
  tidyverse, data.table, zoo, tictoc, fst, fixest, PanelMatch, patchwork,
  rio, magrittr, janitor, did, panelView, vtable, RPushbullet
)
set.seed(42)
theme_set(lal_plot_theme())

# %% ####################################################
dbox_root = '/home/alal/res/india_pesa_forests'
sher_root = "/home/users/apoorval/aa_scratch/forests_analysis"
root = dbox_root
tmp = file.path(root, "tmp")

# %%
tic()
load(file.path(tmp, "regdata.rds"))
toc()


# %% VCF prep
if (exists("vcf_data")) {
  vcf = vcf_data[year >= 1995]
  rm(vcf_data)
}
vcf[, t := year - 1995]
ex_ante_med = quantile(vcf$cover_1990, 0.5)
above_med = vcf[cover_1990 > ex_ante_med]
above_med[, never_treated := max(D) == 0, cellid]

above_med[, stf := as.factor(state)]
pm_data = as.data.frame(above_med)

########     ###    ##    ## ######## ##       ##     ##    ###    ########  ######  ##     ##
##     ##   ## ##   ###   ## ##       ##       ###   ###   ## ##      ##    ##    ## ##     ##
##     ##  ##   ##  ####  ## ##       ##       #### ####  ##   ##     ##    ##       ##     ##
########  ##     ## ## ## ## ######   ##       ## ### ## ##     ##    ##    ##       #########
##        ######### ##  #### ##       ##       ##     ## #########    ##    ##       ##     ##
##        ##     ## ##   ### ##       ##       ##     ## ##     ##    ##    ##    ## ##     ##
##        ##     ## ##    ## ######## ######## ##     ## ##     ##    ##     ######  ##     ##



# %%
tic()
match_ps_vcf = PanelMatch(
  lag = 4, time.id = "year", unit.id = "cellid",
  treatment = "D", refinement.method = "ps.weight",
  data = pm_data,
  covs.formula = ~ I(lag(forest_index, 1:4)),
  exact.match.variables = c("stf"),
  outcome.var = "forest_index",
  size.match = 5, qoi = "att",
  match.missing = FALSE, listwise.delete = TRUE,
  lead = 0:4, forbid.treatment.reversal = F,
  use.diagonal.variance.matrix = T
)
toc()
notif("matching done")

ps_results_vcf = PanelEstimate(sets = match_ps_vcf, data = pm_data,
  se.method = "conditional")

tic()
bal = get_covariate_balance(match_ps_vcf$att,
  data = pm_data,
  covariates = "forest_index", plot = F
)
toc()

# %%
save(match_ps_vcf, ps_results_vcf, bal,
  file = file.path(root, "tmp/panelmatch_vcf2.RData"))

# %% CBPS match including night lights
tic()
match_cbps_vcf = PanelMatch(
  lag = 4, time.id = "year", unit.id = "cellid",
  treatment = "D", refinement.method = "CBPS.weight",
  data = pm_data,
  covs.formula = ~ I(lag(forest_index, 1:4)) + I(lag(nl_p95, 1:4)),
  exact.match.variables = c("stf"),
  outcome.var = "forest_index",
  size.match = 5, qoi = "att",
  match.missing = FALSE, listwise.delete = TRUE,
  lead = 0:4, forbid.treatment.reversal = F,
  use.diagonal.variance.matrix = T
)
toc()
notif("cbps matching done")


# %%

cbps_results_vcf = PanelEstimate(sets = match_cbps_vcf,
  data = pm_data, se.method = "bootstrap")
notif("cbps estimated")


save(match_cbps_vcf, cbps_results_vcf,
  file = file.path(root, "tmp/panelmatch_cbps_vcf.RData"))

######################################################################
ps_bal1 = get_covariate_balance(match_ps_vcf$att,
  data = pm_data,
  covariates = "forest_index", plot = F
)

cbps_bal1 = get_covariate_balance(match_cbps_vcf$att,
  data = pm_data,
  covariates = "forest_index", plot = F
)
cbps_bal2 = get_covariate_balance(match_cbps_vcf$att,
  data = pm_data,
  covariates = "nl_p95", plot = F
)

######################################################################
# %% load fit models

load(file.path(root, "tmp/panelmatch_vcf2.RData"))
load(file.path(root, "tmp/panelmatch_cbps_vcf.RData"))

ls()

# %%
pdf(file.path(root, "out/before_after.pdf"))
balance_scatter(
    list(match_ps_vcf$att,
         match_cbps_vcf$att),
    data = pm_data,
    covariates = c("forest_index", "nl_p95"))
    # add legend
legend(x = 0, y = 0.8,
legend = c("propensity score",
           "covariate balancing propensity score"),
  y.intersp = 0.65,
  x.intersp = 0.3,
  xjust = 0,
  pch = c(1, 3), pt.cex = 1,
  bty = "n", ncol = 1, cex = 1, bg = "white")
dev.off()

# %% match set size etc
above_med[cellid == 8]
above_med[state == 'Himachal Pradesh', nunique(cellid)]


cbps_matched_sets = match_cbps_vcf$att
cbps_matched_sets %>% summary() %>% .$overview %>% head

# %% balance fig
baltab = \(x, v) data.frame(v = v, t = -4:-1, imbal = x[,1]) %>% set_rownames(NULL)

imbal = rbind(
  ps_bal1   %>% baltab('pscore; forest index'),
  cbps_bal1 %>% baltab(  'cbps; forest index'),
  cbps_bal2 %>% baltab(  'cbps; nightlights')
)

(
  balfig = ggplot(imbal, aes(t, imbal, colour = v)) +
    geom_point(size = 3) +
    geom_hline(yintercept = 0, alpha = 0.8) +
    ylim(c(-0.5, 0.5)) +
    geom_hline(yintercept = c(-.2, .2), color = 'red', size = 0.5, linetype = 'dashed') +
    scale_colour_brewer(palette = "Set1") +
    labs(
      y = "Standardised Difference in Pre-treatment variables",
      x = "Years since PESA", colour = ""
    )
)
ggsave(file.path(root, "out/panelmatch_vcf_balfig.pdf"), balfig, device = cairo_pdf)

# %% event study fig

matchrestable = \(x, v) data.frame(v, with(x, cbind(lead, estimates, standard.error)))
matchEst = rbind(
  matchrestable(ps_results_vcf, "Lagged Forest"),
  matchrestable(cbps_results_vcf, "Lagged Forest + Lagged Night Lights")
)

(f1 = ggplot(matchEst, aes(lead, y = estimates, colour = v)) +
  # geom_point(position = jitter) +
  geom_pointrange(position = position_dodge2(width = .2),
              aes(ymin = estimates - 1.96 * `standard.error`,
                  ymax = estimates + 1.96 * `standard.error`), alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = 'dotted') +
  scale_colour_brewer(palette = "Set1") +
  labs(y = "Treatment Effect", x = "Years since PESA", colour = "Match variables")
)

ggsave(file.path(root, "out/panelmatch_vcf.pdf"), f1, device = cairo_pdf)
# %%
# (ff = balfig | f1)
#
# # %%
# ggsave(file.path(root, "out/panelmatch_wide.pdf"), ff,
#   device = cairo_pdf,
#   width = 12, height = 5
# )
#
# %%
