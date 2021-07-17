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
df[, blk   := .GRP, by =  .(state, year)]
# %% sanity check
df[, sum_index := forest_index + green_index + built_index]
df$sum_index %>% summary

# %% # construct ex-ante cover using 5 year window preceding treatment
slice = df[, .(cellid, forest_index, green_index, built_index, D, state, year, first_pesa_exposure)]
slice[, first_pesa_exposure := max(first_pesa_exposure, na.rm = T), state]
slice[, five_yr_win := fifelse(year >= (first_pesa_exposure - 5) & year < first_pesa_exposure,
  1, 0)]
ex_ante = slice[five_yr_win == 1,
  .(ex_ante_forest = mean(forest_index),
    ex_ante_green  = mean(green_index),
    ex_ante_built  = mean(built_index)),
  cellid]
# %% # merge it onto main
df = merge(df, ex_ante, by = 'cellid')
regsamp = df[year>=1995]
regsamp[, time := year - first_pesa_exposure]
# %%
state_status = regsamp[, .(out = 1, treat = max(D)), .(state, year)]
f0 = panelView(out ~ treat,
  data = as.data.frame(state_status),
  index = c("state","year"),
  xlab = "Year", ylab = "State", main = "Scheduled Areas PESA Status \n VCF cell level data",
  by.timing = TRUE, legendOff = TRUE,
  background = "white")
ggsave(file.path(root, "out/panelview_vcf.pdf"), height = 8, width = 10, device = cairo_pdf)
# %%

########  #######  ########  ########  ######  ########
##       ##     ## ##     ## ##       ##    ##    ##
##       ##     ## ##     ## ##       ##          ##
######   ##     ## ########  ######    ######     ##
##       ##     ## ##   ##   ##             ##    ##
##       ##     ## ##    ##  ##       ##    ##    ##
##        #######  ##     ## ########  ######     ##
# %% # Setting a dictionary
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
# %%
regsamp[, never_treated := max(D) == 0, cellid]
m0 = feols(forest_index ~ D | cellid + year,      data = regsamp, cluster = "blk")
m1 = feols(forest_index ~ D | cellid[t] + year,   data = regsamp, cluster = "blk")
m2 = feols(forest_index ~ D | cellid + styear,    data = regsamp, cluster = "blk")
m3 = feols(forest_index ~ D | cellid[t] + styear, data = regsamp, cluster = "blk")
# %%
controls_pre0 = regsamp[never_treated == 1 & year < first_pesa_exposure, mean(forest_index)]
treat_pre0    = regsamp[never_treated == 0 & year < first_pesa_exposure, mean(forest_index)]
fn = "vcf_regs_all"; lab = "tab:vcf_all";
clustlev = "blk"; desc = "Forest cover index"; FEsize = T
etable(list(m0, m1, m2, m3),
  style.tex = style.tex("aer"),
  fixef_sizes = T,
  fitstat = ~ n, tex = TRUE,
  dict = treatmap,
  label = lab, cluster = clustlev,
  title = glue::glue("{desc} regression estimates"),
  notes = glue::glue("robust SEs clustered by block in parentheses"),
  extraline = list("Control mean" = rep(controls_pre0, 4)),
  file = file.path(root, glue::glue("out/{fn}_{clustlev}.tex")), replace = TRUE)

# %% above median sample
(ex_ante_med = quantile(df$ex_ante_forest, 0.75))
above_med = regsamp[ex_ante_forest > ex_ante_med]
above_med[, never_treated := max(D) == 0, cellid]
controls_pre1 = above_med[never_treated == 1 & year < first_pesa_exposure, mean(forest_index)]
treat_pre1    = above_med[never_treated == 0 & year < first_pesa_exposure, mean(forest_index)]
# %%
m0 = feols(forest_index ~ D | cellid + year,  	  data = above_med, cluster = "blk")
m1 = feols(forest_index ~ D | cellid[t] + year,   data = above_med, cluster = "blk")
m2 = feols(forest_index ~ D | cellid + styear,    data = above_med, cluster = "blk")
m3 = feols(forest_index ~ D | cellid[t] + styear, data = above_med, cluster = "blk")
# %%
fn = "vcf_regs_above_med"; lab = "tab:vcf_above_med";
etable(list(m0, m1, m2, m3),
  style.tex = style.tex("aer"),
  fixef_sizes = T,
  fitstat = ~ n, tex = TRUE,
  dict = treatmap,
  label = lab, cluster = clustlev,
  title = glue::glue("{desc} regression estimates (ex-ante median cutoff)"),
  notes = glue::glue("robust SEs clustered by block in parentheses"),
  extraline = list("Control mean" = rep(controls_pre1, 4)),
  file = file.path(root, glue::glue("out/{fn}_{clustlev}.tex")), replace = TRUE)

# %%
(deciles = quantile(regsamp[year == 1995, ex_ante_forest], seq(0.1, 0.9, 0.1)))
fitter = function(cut) {
  m = feols(forest_index ~ D | cellid[t] + styear, data = regsamp[ex_ante_forest >= cut], cluster = "blk")
  tidy(m)
}
cutmods = map_dfr(deciles, fitter)

cutmods$dec = 1:9
cutmods


f = ggplot(cutmods, aes(x = dec, y = estimate)) +
  geom_pointrange(aes(ymin = estimate - 1.96 * `std.error`,
    ymax = estimate + 1.96 * `std.error`), colour = 'cornflowerblue') +
  geom_hline(yintercept=0) +
  scale_x_continuous(breaks = 1:10) +
  labs(y = "PESA Effect", x = "Decile cutoff")
ggsave(file.path(root, "out/pesa_by_decile.pdf"), device = cairo_pdf)

# %%
######   ########  ######## ######## ##    ##
##    ##  ##     ## ##       ##       ###   ##
##        ##     ## ##       ##       ####  ##
##   #### ########  ######   ######   ## ## ##
##    ##  ##   ##   ##       ##       ##  ####
##    ##  ##    ##  ##       ##       ##   ###
######   ##     ## ######## ######## ##    ##

m0 = feols(green_index ~ D | cellid + year,      cluster = ~blk, data = regsamp)
m1 = feols(green_index ~ D | cellid[t] + year,   cluster = ~blk, data = regsamp)
m2 = feols(green_index ~ D | cellid + styear,    cluster = ~blk, data = regsamp)
m3 = feols(green_index ~ D | cellid[t] + styear, cluster = ~blk, data = regsamp)
# %%
controls_pre0 = regsamp[never_treated == 1 & year < first_pesa_exposure, mean(green_index)]
treat_pre0    = regsamp[never_treated == 0 & year < first_pesa_exposure, mean(green_index)]
fn = "vcf_green_regs_all"; lab = "tab:vcf_green_all";
clustlev = "blk"; desc = "Non-forest green cover index"; FEsize = T
etable(list(m0, m1, m2, m3),
  style.tex = style.tex("aer"),
  fixef_sizes = T,
  fitstat = ~ n, tex = TRUE,
  dict = treatmap,
  label = lab, cluster = clustlev,
  title = glue::glue("{desc} regression estimates"),
  notes = glue::glue("robust SEs clustered by block in parentheses"),
  extraline = list("Control mean" = rep(controls_pre0, 4)),
  file = file.path(root, glue::glue("out/{fn}_{clustlev}.tex")), replace = TRUE)
# %% above median
above_med = regsamp[ex_ante_forest > ex_ante_med]
above_med[, never_treated := max(D) == 0, cellid]
controls_pre1 = above_med[never_treated == 1 & year < first_pesa_exposure, mean(green_index)]
treat_pre1    = above_med[never_treated == 0 & year < first_pesa_exposure, mean(green_index)]
# %%
m0 = feols(green_index ~ D | cellid + year,  	   data = above_med, cluster = ~blk)
m1 = feols(green_index ~ D | cellid[t] + year,   data = above_med, cluster = ~blk)
m2 = feols(green_index ~ D | cellid + styear,    data = above_med, cluster = ~blk)
m3 = feols(green_index ~ D | cellid[t] + styear, data = above_med, cluster = ~blk)
# %%
fn = "vcf_green_regs_above_med"; lab = "tab:vcf_green_above_med";
etable(list(m0, m1, m2, m3),
  style.tex = style.tex("aer"),
  fixef_sizes = T,
  fitstat = ~ n, tex = TRUE,
  dict = treatmap,
  label = lab, cluster = clustlev,
  title = glue::glue("{desc} regression estimates (ex-ante median cutoff)"),
  notes = glue::glue("robust SEs clustered by block in parentheses"),
  extraline = list("Control mean" = rep(controls_pre1, 4)),
  file = file.path(root, glue::glue("out/{fn}_{clustlev}.tex")), replace = TRUE)

# %%
########  ##     ## #### ##       ########
##     ## ##     ##  ##  ##          ##
##     ## ##     ##  ##  ##          ##
########  ##     ##  ##  ##          ##
##     ## ##     ##  ##  ##          ##
##     ## ##     ##  ##  ##          ##
########   #######  #### ########    ##

m0 = feols(built_index ~ D | cellid + year,      cluster = ~ blk, data = regsamp)
m1 = feols(built_index ~ D | cellid[t] + year,   cluster = ~ blk, data = regsamp)
m2 = feols(built_index ~ D | cellid + styear,    cluster = ~ blk, data = regsamp)
m3 = feols(built_index ~ D | cellid[t] + styear, cluster = ~ blk, data = regsamp)
controls_pre0 = regsamp[never_treated == 1 & year < first_pesa_exposure, mean(built_index)]
# %%
fn = "vcf_built_regs_all"; lab = "tab:vcf_built_all";
clustlev = "blk"; desc = "Bare ground index"; FEsize = T
etable(list(m0, m1, m2, m3),
  style.tex = style.tex("aer"),
  fixef_sizes = T,
  fitstat = ~ n, tex = TRUE,
  dict = treatmap,
  label = lab, cluster = clustlev,
  title = glue::glue("{desc} regression estimates"),
  notes = glue::glue("robust SEs clustered by block in parentheses"),
  extraline = list("Control mean" = rep(controls_pre0, 4)),
  file = file.path(root, glue::glue("out/{fn}_{clustlev}.tex")), replace = TRUE)

# %% above median
above_med = regsamp[ex_ante_forest > ex_ante_med]
above_med[, never_treated := max(D) == 0, cellid]
controls_pre1 = above_med[never_treated == 1 & year < first_pesa_exposure, mean(built_index)]
# %%
m0 = feols(built_index ~ D | cellid + year,  	   cluster = ~blk, data = above_med)
m1 = feols(built_index ~ D | cellid[t] + year,   cluster = ~blk, data = above_med)
m2 = feols(built_index ~ D | cellid + styear,    cluster = ~blk, data = above_med)
m3 = feols(built_index ~ D | cellid[t] + styear, cluster = ~blk, data = above_med)
# %%
fn = "vcf_built_regs_above_med"; lab = "tab:vcf_built_above_med";
etable(list(m0, m1, m2, m3),
  style.tex = style.tex("aer"),
  fixef_sizes = T,
  fitstat = ~ n, tex = TRUE,
  dict = treatmap,
  label = lab, cluster = clustlev,
  title = glue::glue("{desc} regression estimates (ex-ante median cutoff)"),
  notes = glue::glue("robust SEs clustered by block in parentheses"),
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
evsamp = evsamp[, non_forest_index := 100 - forest_index]
evsamp = evsamp[, index_gap := forest_index - non_forest_index]

# %%
state_status = evsamp[, .(out = 1, treat = max(D)), .(state, time)]
panelView(out ~ treat,
  data = as.data.frame(state_status),
  index = c("state","time"),
  xlab = "Year", ylab = "State", main = "Scheduled Areas PESA Status \n VCF cell level data",
  by.timing = TRUE, legendOff = TRUE,
  background = "white")
# %%
(deciles = quantile(regsamp[year == 1995, ex_ante_forest], seq(0.1, 0.9, 0.1)))
# %%
estudy_plot = function(cutoff, title){
  es1 = feols(c(forest_index, green_index, built_index)  ~ i(time, sch,  ref=-1) | cellid + styear,
    cluster = ~ blk,
    evsamp[ex_ante_forest >= cutoff])
  iplot(es1, pt.join = F, main = title, pch = 15, ylim = c(-3, 3), col = c(3, 2, 4))
}
estudy_plot2 = function(cutoff, title, ...){
  es1 = feols(index_gap ~ i(time, sch,  ref=-1) | cellid + styear,
    cluster = ~ blk,
    evsamp[ex_ante_forest >= cutoff])
  iplot(es1, pt.join = F, main = title, ...)
}
# %%
pdf(file.path(root, "out/evstudy_vcf.pdf"), width = 12)
par(mfrow = c(2, 3))
estudy_plot(deciles[1], "full sample")
estudy_plot(deciles[2], "2nd decile and up")
estudy_plot(deciles[4], "4th decile and up")
estudy_plot(deciles[6], "6th decile and up")
estudy_plot(deciles[8], "8th decile and up")
plot(1, type = "n", axes=FALSE, xlab="", ylab="") # Create empty plot
legend("topleft", col= c(3, 2, 4), lwd = 2,
  legend = c("Forest Cover", "Non-forest green", "Bare"), cex = 2)
dev.off()
# %%

pdf(file.path(root, "out/evstudy_vcf_gap.pdf"), width = 10)
par(mfrow = c(2, 3))
estudy_plot2(deciles[1], ylim = c(-5, 5), "full sample")
estudy_plot2(deciles[2], ylim = c(-5, 5), "2nd decile and up")
estudy_plot2(deciles[4], ylim = c(-5, 5), "4th decile and up")
estudy_plot2(deciles[6], ylim = c(-5, 5), "6th decile and up")
estudy_plot2(deciles[8], ylim = c(-5, 5), "8th decile and up")
dev.off()

# %%
########     ###    ##    ## ######## ##       ##     ##    ###    ########  ######  ##     ##
##     ##   ## ##   ###   ## ##       ##       ###   ###   ## ##      ##    ##    ## ##     ##
##     ##  ##   ##  ####  ## ##       ##       #### ####  ##   ##     ##    ##       ##     ##
########  ##     ## ## ## ## ######   ##       ## ### ## ##     ##    ##    ##       #########
##        ######### ##  #### ##       ##       ##     ## #########    ##    ##       ##     ##
##        ##     ## ##   ### ##       ##       ##     ## ##     ##    ##    ##    ## ##     ##
##        ##     ## ##    ## ######## ######## ##     ## ##     ##    ##     ######  ##     ##

# %%

tic()
match_ps <- PanelMatch(lag = 4, time.id = "year", unit.id = "cellid",
                     treatment = "D",
                     refinement.method = "ps.weight",
                     data = evsamp,
                     covs.formula = ~ I(lag(def_ha, 1:4)),
                     exact.match.variables = c("stf", "pref_bin"),
                     size.match = 10, qoi = "att" , outcome.var = "forest_index",
                     lead = 0:4, forbid.treatment.reversal = FALSE,
                     use.diagonal.variance.matrix = TRUE)
ps_results <- PanelEstimate(sets = match_ps, data = matchdat)
toc()

save.image(file = file.path(out, "matching_ws.RData"))
print("Matching Done")
