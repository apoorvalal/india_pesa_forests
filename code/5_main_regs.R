# %% ####################################################
rm(list=ls())
library(LalRUtils)
libreq(tidyverse, data.table, zoo, tictoc, fst, fixest, PanelMatch, patchwork,
  rio, magrittr, janitor, did, panelView, ggiplot)
set.seed(42)
theme_set(lal_plot_theme())
# %% ####################################################
dbox_root = '/home/alal/res/india_pesa_forests'
root = dbox_root
data = file.path(root, 'inp')
tmp  = file.path(root, 'tmp')

# %%
tic()
load(file.path(tmp, "regdata.rds"))
toc()

# %% VCF prep
if (exists("vcf_data")){
  vcf = vcf_data[year>=1995]
  rm(vcf_data)
}
vcf[, t := year - 1995]
# %%
########  ########  ######   ########    ###    ########
##     ## ##       ##    ##     ##      ## ##   ##     ##
##     ## ##       ##           ##     ##   ##  ##     ##
########  ######   ##   ####    ##    ##     ## ########
##   ##   ##       ##    ##     ##    ######### ##     ##
##    ##  ##       ##    ##     ##    ##     ## ##     ##
##     ## ########  ######      ##    ##     ## ########

# %% GFC regressions
# 2wFE
m00 = feols(def_ha ~ D | village + year, cluster = "block", gfc[pref == 1])
# 2wFE + state year FEs
m01 = feols(def_ha ~ D | village + styear, cluster = "block",
  gfc[gfc3 == TRUE & pref == 1])
# with effective sample for cols 3, 4
m003 = feols(def_ha ~ D | village + village[t] + styear, cluster = "block",
  gfc[gfc3 == TRUE & pref == 1])

# %% control means for table
gfc[, ever_treated := max(D), .(village)]
ctrl_mean  = round(gfc[pref == 1 & ever_treated == 0 & pesa_exposure == 0, mean(def_ha)], 2)
ctrl_mean2 = round(gfc[pref == 1 & gfc3 == T & ever_treated == 0 & pesa_exposure == 0, mean(def_ha)], 2)
# VCF regressions
# %% above median sample - cutoff in 1990
(ex_ante_med = quantile(vcf$cover_1990, 0.5))
above_med = vcf[cover_1990 > ex_ante_med]
above_med[, never_treated := max(D) == 0, cellid]
controls_pre1 = above_med[never_treated == 1 & year < first_pesa_exposure, mean(forest_index)]
# %%
m0 = feols(forest_index ~ D | cellid + year,  	  data = above_med, cluster = "blk")
m1 = feols(forest_index ~ D | cellid + styear,    data = above_med, cluster = "blk")
m3 = feols(forest_index ~ D | cellid[t] + styear, data = above_med, cluster = "blk")
# %% export
treatmap =c(
        # GFC
        "def_ha"       = "Annual Deforestation in Hectares",
        "village"      = "Village",
        "village[t]"   = "Village + Village TT",
        # VCF
        "forest_index" = "Forest cover index",
        "green_index"  = "Non-forest green index",
        "built_index"  = "Non-forest index",
        "cellid"       = "Pixel",
        "cellid[t]"    = "pixel + pixel TT",
        # both
        "yr"           = "Year",
        "year"         = "Year",
        "styear"       = "State $\\times$ Year",
        "D"            = "PESA $\\times$ Scheduled",
        "block"        = "Block",
        "blk"          = "Block"
      )
# %%
desc = "Deforestation and Forest cover index"
fn = "regs_all_main"; lab = "tab:regs_all_main";
mods = list(m0, m1, m3, m00, m01, m003)
etable(mods, fixef_sizes = T, fixef_sizes.simplify = F,
  fitstat = ~ n, dict = treatmap)

# %%
etable(mods,
  style.tex = style.tex(main = "base", depvar.title = "", model.title = "",
    yesNo = c("$\\checkmark$", "")),
  signifCode = NA,
  fixef_sizes = T, fixef_sizes.simplify = F,
  fitstat = ~ n, tex = TRUE,
  dict = treatmap,
  label = lab,
  title = glue::glue("{desc} regression estimates (ex-ante median cutoff)"),
  notes = "First three columns report report estimates from the VCF dataset, where the dependent variable is is a forest cover index; hence, a positive (negative) coefficient implies an increase (decrease) in forest cover. The next three columns estimates from the GFC dataset, where the dependent variable is annual deforested area; hence, a negative (positive) coefficient implies an increase (decrease) in forest cover. Columns 5 and 6 restrict the sample to four states: Chhattisgarh, Jharkhand, Maharashtra, and Orissa, which switch to PESA status during the GFC panel (2001- 2017).",
    extraline=list(
      "Dep Var Mean"= c(rep(controls_pre1, 3), ctrl_mean, ctrl_mean2, ctrl_mean2),
      "Dataset"     = c(rep("VCF", 3), rep("GFC", 3)),
      "Timespan"    = c(rep("1995-2017", 3), rep("2001-2017", 3))
    ),
  file = file.path(root, glue::glue("out/{fn}_block.tex")), replace = TRUE)

# %% robustness to cutoffs - VCF
 ######  ##     ## ########  #######  ######## ########  ######
##    ## ##     ##    ##    ##     ## ##       ##       ##    ##
##       ##     ##    ##    ##     ## ##       ##       ##
##       ##     ##    ##    ##     ## ######   ######    ######
##       ##     ##    ##    ##     ## ##       ##             ##
##    ## ##     ##    ##    ##     ## ##       ##       ##    ##
 ######   #######     ##     #######  ##       ##        ######

# %% GFC robustness

fitter = function(cutoff){
  dat = gfc[pref_bin >= cutoff]
  m = feols(def_ha ~ D | village[t] + styear, cluster = ~village, dat)
  tidy(m)[, 2:3]
}
# %%
tic()
cutoff_res = map_dfr(1:10, fitter) %>% setDT
toc()
cutoff_res$n = 1:10
colnames(cutoff_res)[1:2] = c('beta', 'se')
# %%
(rob_fit_gfc = ggplot(cutoff_res, aes(n, beta)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymax = beta + 1.96 * se,
    ymin = beta - 1.96 * se), alpha = 1, width = 0) +
  geom_hline(yintercept = 0) +
  scale_x_continuous(breaks = 1:10) +
  labs(title = 'GFC', y = "Effect", x = "",
  caption = '') + theme(legend.position = 'none')
)
# %% VCF Robustness
vcf[, pref_bin := ntile(cover_1990, 10)]
fitter = function(cut) {
  m = feols(forest_index ~ D | cellid[t] + styear, data = vcf[pref_bin >= cut], cluster = ~cellid)
  tidy(m)[, 2:3]
}
cutmods = map_dfr(1:10, fitter) %>% setDT
cutmods$n = 1:10
colnames(cutmods)[1:2] = c('beta', 'se')
# %%
(rob_fit_vcf = ggplot(cutmods, aes(n, beta)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymax = beta + 1.96 * se,
    ymin = beta - 1.96 * se), alpha = 1, width = 0) +
  geom_hline(yintercept = 0) +
  scale_x_continuous(breaks = 1:10) +
  scale_colour_brewer(palette = 'Set1') +
  labs(title = 'VCF',
   y = "Effect", x = "Cutoff Decile",
  caption = '') + theme(legend.position = 'none')
)

# %%
frob = (rob_fit_gfc / rob_fit_vcf) + plot_annotation("Estimates by ex-ante forest cover inclusion threshold")
ggsave(file.path(root, "out/cutoffs_figure.pdf"), device = cairo_pdf, width = 8, height = 8)
# %%

######## ##     ## ######## ##    ## ########
##       ##     ## ##       ###   ##    ##
##       ##     ## ##       ####  ##    ##
######   ##     ## ######   ## ## ##    ##
##        ##   ##  ##       ##  ####    ##
##         ## ##   ##       ##   ###    ##
########    ###    ######## ##    ##    ##

# %% GFC
vcf[, time := year - first_pesa_exposure]
vcf[, pref_bin := ntile(cover_1990, 10)]
vcf_evsamp = vcf[time %between% c(-6, 6)]
# %%
estudy_plot = function(cutoff, title){
  es1 = feols(forest_index ~ i(time, sch,  ref=-1) | cellid + styear,
    cluster = ~ blk, vcf_evsamp[pref_bin >= cutoff])
  f = ggiplot(es1) + ylim(c(-3, 3)) + ggtitle(title) + lal_plot_theme()
  return(f)
}
# %%
ff = (estudy_plot(1, "full sample") + labs(y = "", x = "") |
      estudy_plot(6,  "5th decile and up") + labs(y = "", x = "")) /
     (estudy_plot(7,  "6th decile and up") + labs(y = "", x = "") |
      estudy_plot(8,  "7th decile and up") + labs(y = "", x = "") ) /
     (estudy_plot(9,  "8th decile and up") + labs(y = "")|
      estudy_plot(10, "9th decile and up") + labs(y = ""))
ggsave(file.path(root, "out/evstudy_vcf.pdf"), ff, width = 12, height = 15,
  device = cairo_pdf)
# %%

