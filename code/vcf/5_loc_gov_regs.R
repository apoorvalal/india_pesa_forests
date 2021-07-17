# %% ####################################################
rm(list=ls())
library(LalRUtils)
LalRUtils::libreq(tidyverse, data.table, zoo, tictoc, fst,
  fixest, PanelMatch, rio, magrittr, janitor, did,
  panelView)
set.seed(42)
theme_set(lal_plot_theme())
# %% ####################################################
dbox_root = '/home/alal/res/india_pesa_forests'
root = dbox_root
data = file.path(root , 'inp')
tmp  = file.path(root, "tmp")
# %%
tic()
df = read_fst(file.path(tmp, 'vcf_cell_sample.fst')) %>% setDT # write file for future runs
toc()
# %% sort
setorder(df, cellid, year)
df[, blk:= .GRP, .(state, nameb)]

# %%
df[, sum_index := forest_index + green_index + built_index]
df$sum_index %>% summary
# %% # construct ex-ante cover using 5 year window preceding treatment
slice = df[, .(cellid, forest_index, green_index, built_index, D, state, year, first_pesa_exposure)]
slice[, five_yr_win := fifelse(year %between% c(1988, 1993), 1, 0)]
ex_ante = slice[five_yr_win == 1,
  .(ex_ante_forest = mean(forest_index),
    ex_ante_green  = mean(green_index),
    ex_ante_built  = mean(built_index)),
  cellid]
df = merge(df, ex_ante, by = 'cellid')
# %%
######## ########  ########    ###    ########
   ##    ##     ## ##         ## ##      ##
   ##    ##     ## ##        ##   ##     ##
   ##    ########  ######   ##     ##    ##
   ##    ##   ##   ##       #########    ##
   ##    ##    ##  ##       ##     ##    ##
   ##    ##     ## ######## ##     ##    ##

fifth_sched_pre_2k = c("Andhra Pradesh", "Chhattisgarh", "Gujarat", "Himachal Pradesh",
  "Orissa", "Rajasthan", "Madhya Pradesh")
regsamp = df[year<=1999 & state %in% fifth_sched_pre_2k]
regsamp[, first_panch_elec := fcase(
  state == "Andhra Pradesh", 1995,
  state == "Chhattisgarh", 1995,
  state == "Gujarat", 1995,
  state == "Madhya Pradesh", 1994,
  state == "Himachal Pradesh", 1995,
  state == "Orissa", 1997,
  state == "Rajasthan", 1995
)]
regsamp[, time := year - first_panch_elec]
regsamp[, D := (1 - sch) * (year >= first_panch_elec)]
regsamp[, nsch := (1 - sch)]
# %%
state_status = regsamp[, .(out = 1, treat = max(D)), .(state, year)]
f0 = panelView(out ~ treat,
  data = as.data.frame(state_status),
  index = c("state","year"),
  xlab = "Year", ylab = "State", main = "Scheduled Areas PESA Status \n VCF cell level data",
  by.timing = TRUE, legendOff = TRUE,
  background = "white")
ggsave(file.path(root, "out/panelview_panch.pdf"), height = 8, width = 10, device = cairo_pdf)
# %%
treatmap =c(
        "forest_index" = "Forest Cover",
        "green_index"  = "Non-forest green cover",
        "built_index"  = "Non-forest cover",
        "D" = "PESA $\\times$ Scheduled",
        "cellid"  = "Pixel",
        "year" = "Year",
        "styear" = "State $\\times$ Year",
        "cellid[i]" = "pixel + pixel TT"
      )
regsamp[, never_treated := max(D) == 0, cellid]
# %%
m0 = feols(forest_index ~ D | cellid + year,      cluster = ~ blk, data =regsamp)
m1 = feols(forest_index ~ D | cellid[t] + year,   cluster = ~ blk, data = regsamp)
m2 = feols(forest_index ~ D | cellid + styear,    cluster = ~ blk, data = regsamp)
m3 = feols(forest_index ~ D | cellid[t] + styear, cluster = ~ blk, data = regsamp)
# %%
controls_pre0 = regsamp[never_treated == 1 & year < first_pesa_exposure, mean(forest_index)]
fn = "panch_vcf_regs_all"; lab = "tab:panch_vcf_all";
clustlev = "blk"; desc = "Forest cover index"; FEsize = T
etable(list(m0, m1, m2, m3),
  style.tex = style.tex("aer"),
  fixef_sizes = T,
  fitstat = ~ n, tex = TRUE,
  dict = treatmap,
  label = lab, cluster = clustlev,
  title = glue::glue("{desc} regression estimates"),
  notes = glue::glue("robust SEs clustered by {clustlev} in parentheses"),
  extraline = list("Control mean" = rep(controls_pre0, 4)),
  file = file.path(root, glue::glue("out/{fn}_{clustlev}.tex")), replace = TRUE)

# %% above median sample
(ex_ante_med = quantile(df$ex_ante_forest, 0.75))
above_med = regsamp[ex_ante_forest > ex_ante_med]
above_med[, never_treated := max(D) == 0, cellid]
controls_pre1 = above_med[never_treated == 1 & year < first_pesa_exposure, mean(forest_index)]
# %%
m0 = feols(forest_index ~ D | cellid + year,  	  cluster = ~ blk, data = above_med)
m1 = feols(forest_index ~ D | cellid[t] + year,   cluster = ~ blk, data = above_med)
m2 = feols(forest_index ~ D | cellid + styear,    cluster = ~ blk, data = above_med)
m3 = feols(forest_index ~ D | cellid[t] + styear, cluster = ~ blk, data = above_med)
# %%
fn = "panch_vcf_regs_above_med"; lab = "tab:panch_vcf_above_med";
etable(list(m0, m1, m2, m3),
  style.tex = style.tex("aer"),
  fixef_sizes = T,
  fitstat = ~ n, tex = TRUE,
  dict = treatmap,
  label = lab, cluster = clustlev,
  title = glue::glue("{desc} regression estimates (ex-ante median cutoff)"),
  notes = glue::glue("robust SEs clustered by {clustlev} in parentheses"),
  extraline = list("Control mean" = rep(controls_pre1, 4)),
  file = file.path(root, glue::glue("out/{fn}_{clustlev}.tex")), replace = TRUE)
# %%

######## ##     ## ######## ##    ## ########
##       ##     ## ##       ###   ##    ##
##       ##     ## ##       ####  ##    ##
######   ##     ## ######   ## ## ##    ##
##        ##   ##  ##       ##  ####    ##
##         ## ##   ##       ##   ###    ##
########    ###    ######## ##    ##    ##


evsamp = regsamp[time %between% c(-6, 6)]
(deciles = quantile(regsamp[year == 1995, ex_ante_forest], seq(0.1, 0.9, 0.1)))
# %%
estudy_plot = function(cutoff, title, ...){
  es1 = feols(c(forest_index, green_index, built_index)  ~ i(time, nsch,  ref=-1) | cellid + styear,
    cluster = ~ blk,
    evsamp[ex_ante_forest >= cutoff])
  iplot(es1, pt.join = F, main = title, pch = 15, col = c(3, 2, 4), ...)
}

# %%
pdf(file.path(root, "out/evstudy_panch_vcf.pdf"), width = 12)
par(mfrow = c(2, 3))
estudy_plot(deciles[1], "full sample")
estudy_plot(deciles[2], "2nd decile and up")
estudy_plot(deciles[4], "4th decile and up")
estudy_plot(deciles[6], "6th decile and up")
estudy_plot(deciles[8], "8th decile and up")
plot(1, type = "n", axes=FALSE, xlab="", ylab="") # Create empty plot
legend("topleft", col= c(3, 2, 4), lwd = 2,
  legend = c("Forest Cover", "Non-forest  green", "Bare"), cex = 2)
dev.off()
# %%
