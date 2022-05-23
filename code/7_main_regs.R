# %% ####################################################
rm(list=ls())
library(LalRUtils)
libreq(data.table, zoo, tictoc, fixest, PanelMatch, patchwork,
  rio, magrittr, janitor, did, panelView, ggiplot, RPushbullet)

set.seed(42)
theme_set(lal_plot_theme())

notif = function(x) pbPost("note", x)
# %% ####################################################
dbox_root = '/home/alal/res/india_pesa_forests'
root = dbox_root
data = file.path(root, 'inp')
tmp  = file.path(root, 'tmp')

tic()
load(file.path(tmp, "regdata.rds"))
toc()

# %%
vcf_data %>% str

# %%
 ######  ##     ## ##     ## ##     ##    ###    ########  ##    ##
##    ## ##     ## ###   ### ###   ###   ## ##   ##     ##  ##  ##
##       ##     ## #### #### #### ####  ##   ##  ##     ##   ####
 ######  ##     ## ## ### ## ## ### ## ##     ## ########     ##
      ## ##     ## ##     ## ##     ## ######### ##   ##      ##
##    ## ##     ## ##     ## ##     ## ##     ## ##    ##     ##
 ######   #######  ##     ## ##     ## ##     ## ##     ##    ##

# %% treatment timing figure
state_status = vcf_data[year>=1990][, .(out = 1, treat = max(D)), .(state, year)]

f0 = panelView(out ~ treat,
  data = as.data.frame(state_status),
  index = c("state","year"),
  xlab = "Year", ylab = "State",
  main = "",
  by.timing = TRUE, legendOff = TRUE,
  background = "white")
(f0 = f0 +
  lal_plot_theme(textangle = 90) + theme(legend.pos = "None") + labs(x = "", y = "") +
  geom_vline(xintercept = 11.5, color = 'red', size = 1.2) +
  annotate("text", x = 8, y = 1, label = "GFC Coverage Begins", color = 'red', size = 5.0)
)
# %%
ggsave(file.path(root, "out/panelview_vcf_ann.pdf"), height = 6, width = 12, device = cairo_pdf)


# %% summary table - VCF
sumvars = c('forest_index', 'green_index', 'built_index',  'sch', 'cover_1990')
summvars=c('notNA(x)', 'mean(x)', 'sd(x)',  'min(x)', 'pctile(x)[25]', 'median(x)', 'pctile(x)[75]', 'max(x)')
labs = c('Forest cover index (0-100)',
         'Non-forest green index (0-100)',
         'Non-green index (0-100)',
    'Scheduled Status',
    'Forest Cover in 1990 (Ex-Ante)')
# %%
st(vcf_data[, ..sumvars],
  factor.percent = FALSE, factor.counts = FALSE, summ = summvars,
  labels = labs, title = "Summary Statistics for primary analysis sample (VCF Data) - full sample",
  file = file.path(root, 'out/vcf_sumstats_all.tex'), out = 'latex')

st(vcf_data[cover_1990 > quantile(cover_1990, 0.5), ..sumvars],
  factor.percent = FALSE, factor.counts = FALSE, summ = summvars,
  labels = labs, title = "Summary Statistics for primary analysis sample (VCF Data) - above median forest cover in 1990",
  file = file.path(root, 'out/vcf_sumstats_regsamp.tex'), out = 'latex')
# %% summary table - GFC
sumvars = c('def_ha', 'sch', 'pref_mean')
labs = c('Deforested Area (Hectares)',
         'Scheduled Status', 'Ex-ante forest cover in 2000 (ex-ante)'
         )
st(gfc[, ..sumvars], summ = summvars,
  factor.percent = FALSE, factor.counts = FALSE,
  labels = labs, title = "Summary Statistics (GFC Data) - full sample",
  file = file.path(root, 'out/gfc_sumstats_all.tex'), out = 'latex')

st(gfc[pref == 1, ..sumvars], summ = summvars,
  factor.percent = FALSE, factor.counts = FALSE,
  labels = labs, title = "Summary Statistics (GFC Data) - above 2 percent forest cover in 2000",
  file = file.path(root, 'out/gfc_sumstats_regsamp.tex'), out = 'latex')


# %% aggregate figure
ts = vcf_data[, .(avg_tree_cover = mean(forest_index),
             deforested = mean(ifelse(forest_index < 1, 1, 0))), year]
gfc_ts = gfc[, .(deforested_area = sum(def_ha)), year]
f1 = rbind(data.frame(year = 1982:2000, deforested_area = NA), gfc_ts) %>%
  ggplot(aes(year, deforested_area)) + geom_point() +
  geom_smooth(se = F) + labs(y = "Total Deforestation (Hectares)", x = "Year",
    title = "GFC")
f2 = ggplot(ts, aes(year, avg_tree_cover)) + geom_point() +
  geom_smooth(se = F) + labs(y = "Average Tree Canopy across all pixels", x = "Year",
    title = "VCF")
(ff = f2 / f1)

# %%
ggsave(file.path(root, "out/agg_ts.pdf"), device = cairo_pdf, width = 6, height = 8)

# %% VCF prep
if (exists("vcf_data")){
  vcf = vcf_data[year>=1995]
  rm(vcf_data)
}
vcf[, t := year - 1995]
(ex_ante_med = quantile(vcf$cover_1990, 0.5))
above_med = vcf[cover_1990 > ex_ante_med]
above_med[, never_treated := max(D) == 0, cellid]
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
#

# %% control means for table
gfc[, ever_treated := max(D), .(village)]
ctrl_mean  = round(gfc[pref == 1 & ever_treated == 0 & pesa_exposure == 0, mean(def_ha)], 2)
ctrl_mean2 = round(gfc[pref == 1 & gfc3 == T & ever_treated == 0 & pesa_exposure == 0, mean(def_ha)], 2)
# VCF regressions
# %% above median sample - cutoff in 1990
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
  m = feols(def_ha ~ D | village[t] + styear, cluster = ~block, dat)
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
  labs(title = 'GFC', y = "Effect (deforested area)", x = "Inclusion Threshold decile of forest cover in 2000 ",
  caption = '') + theme(legend.position = 'none')
)
# %% VCF Robustness
vcf[, pref_bin := ntile(cover_1990, 10)]
fitter = function(cut) {
  m = feols(forest_index ~ D | cellid[t] + styear, data = vcf[pref_bin >= cut], cluster = ~blk)
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
   y = "Effect (forest index)", x = "Inclusion Threshold decile of forest cover in 1990",
  caption = '') + theme(legend.position = 'none')
)

# %%
frob = ( rob_fit_vcf / rob_fit_gfc )
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
