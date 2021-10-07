# %% ####################################################
rm(list=ls())
library(LalRUtils)
libreq(tidyverse, data.table, zoo, tictoc, fst, fixest, PanelMatch, patchwork,
  rio, magrittr, janitor, did, panelView, ggiplot, tictoc, binsreg, interflex)
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

# %%
if (exists("vcf_data")){
  vcf = vcf_data[year>= 1995]
}
vcf[, t := year - 1995]
vcf[, time := year - first_pesa_exposure]

(ex_ante_med = quantile(vcf$cover_1990, 0.5))
above_med = vcf[cover_1990 > ex_ante_med]
above_med[, never_treated := max(D) == 0, cellid]

# %%
      ## ##     ##    ###    ########  ##    ## ##     ##    ###    ##    ## ########
      ## ##     ##   ## ##   ##     ## ##   ##  ##     ##   ## ##   ###   ## ##     ##
      ## ##     ##  ##   ##  ##     ## ##  ##   ##     ##  ##   ##  ####  ## ##     ##
      ## ######### ##     ## ########  #####    ######### ##     ## ## ## ## ##     ##
##    ## ##     ## ######### ##   ##   ##  ##   ##     ## ######### ##  #### ##     ##
##    ## ##     ## ##     ## ##    ##  ##   ##  ##     ## ##     ## ##   ### ##     ##
 ######  ##     ## ##     ## ##     ## ##    ## ##     ## ##     ## ##    ## ########

# %% jharkhand - VCF
above_med[, jh := ifelse(state == "Jharkhand", "Jharkhand", "Others")]
bystate_vcf = feols(forest_index ~ D | cellid[t] + styear,
  data = above_med, cluster = "blk", fsplit = ~ jh)
# %%
gfc[, jh := ifelse(state == "Jharkhand", "Jharkhand", "Others")]
bystate_gfc = feols(def_ha ~ D | village + village[t] + styear, cluster = "block",
  gfc[gfc3 == TRUE & pref == 1], fsplit = ~ jh)
# %%
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
etable(bystate_vcf, bystate_gfc)


# %%
etable(bystate_vcf, bystate_gfc,
  style.tex = style.tex(main = "base", depvar.title = "", model.title = "",
    yesNo = c("$\\checkmark$", "")),
  signifCode = NA,
  fixef_sizes = T, fixef_sizes.simplify = F,
  fitstat = ~ n, tex = TRUE,
  dict = treatmap,
  label = "tab:regs_all_bystate",
  title = glue::glue("regression estimates decomposed by state (ex-ante median cutoff)"),
  file = file.path(root, glue::glue("out/mainres_by_state.tex")), replace = TRUE
)



# %%
   ###    ##       ########  #######  ##     ## ########
  ## ##   ##          ##    ##     ## ##     ##    ##
 ##   ##  ##          ##    ##     ## ##     ##    ##
##     ## ##          ##    ##     ## ##     ##    ##
######### ##          ##    ##     ## ##     ##    ##
##     ## ##          ##    ##     ## ##     ##    ##
##     ## ########    ##     #######   #######     ##

# %% above median sample - cutoff in 1990
evsamp = vcf[cover_1990 > quantile(vcf$cover_1990, 0.5)][time %between% c(-6, 10)]
es1 = feols(forest_index  ~ i(time, sch,  ref=-1) | cellid + styear,
    cluster = ~ blk, evsamp)
es2 = feols(green_index ~ i(time, sch,  ref=-1) | cellid + styear,
    cluster = ~ blk, evsamp)
es3 = feols(built_index ~ i(time, sch,  ref=-1) | cellid + styear,
    cluster = ~ blk, evsamp)
# %%
ggi = function(x) ggiplot(x, theme = lal_plot_theme()) + labs(y = "")
(ff = ((ggi(es1) + ggtitle("VCF Forest Index")) /
      (ggi(es2) + ggtitle("VCF Non-Forest Green Index"))  /
      (ggi(es3) + ggtitle("VCF Bare Ground Index")))
)
ggsave(file.path(root, "out/evstudy_allout_vcf.pdf"), ff, width = 7, height = 7,
  device = cairo_pdf)

# %% jharkhand


# %%
########  ########  ####
##     ## ##     ##  ##
##     ## ##     ##  ##
########  ########   ##
##        ##   ##    ##
##        ##    ##   ##
##        ##     ## ####
# %% create treatment of PRI
tic()
load(file.path(tmp, "regdata.rds"))
toc()

# %%
vcf = vcf_data[year>= 1990]
fifth_sched_pre_2k = c("Andhra Pradesh", "Chhattisgarh", "Gujarat", "Himachal Pradesh",
  "Orissa", "Rajasthan", "Madhya Pradesh")
pri = vcf[year<=1999 & state %in% fifth_sched_pre_2k]
pri[, first_panch_elec := fcase(
  state == "Andhra Pradesh", 1995,
  state == "Chhattisgarh", 1995,
  state == "Gujarat", 1995,
  state == "Madhya Pradesh", 1994,
  state == "Himachal Pradesh", 1995,
  state == "Orissa", 1997,
  state == "Rajasthan", 1995
)]

# %% flip treatment status since PRI treatment is PESA control and vice vers
pri[, time_pri := year - first_panch_elec]
pri[, D := (1 - sch) * (year >= first_panch_elec)]
pri[, nsch := (1 - sch)]

# %%
state_status = pri[, .(out = 1, treat = max(D)), .(state, year)]
f0 = panelView(out ~ treat,
  data = as.data.frame(state_status),
  index = c("state","year"),
  xlab = "Year", ylab = "State", main = "Scheduled Areas PESA Status \n VCF cell level data",
  by.timing = TRUE, legendOff = TRUE,
  background = "white")
ggsave(file.path(root, "out/panelview_panch.pdf"), height = 8, width = 10, device = cairo_pdf)

# %%
pri[, never_treated := max(D) == 0, cellid]
pri[, pref_bin := ntile(cover_1990, 10)]
fitter = function(cut) {
  m = feols(forest_index ~ D | cellid + styear, data = pri[pref_bin >= cut], cluster = ~blk)
  tidy(m)[, 2:3]
}

cutmods = map_dfr(1:10, fitter) %>% setDT
cutmods$n = 1:10
colnames(cutmods)[1:2] = c('beta', 'se')
# %%
(rob_fit_pri = ggplot(cutmods, aes(n, beta)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymax = beta + 1.96 * se,
    ymin = beta - 1.96 * se), alpha = 1, width = 0) +
  geom_hline(yintercept = 0) +
  scale_x_continuous(breaks = 1:10) +
  labs(y = "Effect (forest index)", x = "Inclusion Threshold decile of forest cover in 1990",
  caption = '') + theme(legend.position = 'none')
)
# %%
ggsave(file.path(root, "out/pri_estimate.pdf"), device = cairo_pdf)
# %%
######## ########     ###
##       ##     ##   ## ##
##       ##     ##  ##   ##
######   ########  ##     ##
##       ##   ##   #########
##       ##    ##  ##     ##
##       ##     ## ##     ##

# %% one-shot treatment in 2008
vcf[, pref_bin := ntile(cover_1990, 10)]
vcf[, time_fra := year - 2008]
vcf[, D_fra := sch * (year >= 2008)]
fitter = function(cut) {
  m = feols(forest_index ~ D_fra | cellid + styear, data = vcf[pref_bin >= cut], cluster = ~blk)
  tidy(m)[, 2:3]
}

cutmods = map_dfr(1:10, fitter) %>% setDT
cutmods$n = 1:10
colnames(cutmods)[1:2] = c('beta', 'se')
# %%
(FRA_fit_vcf = ggplot(cutmods, aes(n, beta)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymax = beta + 1.96 * se,
    ymin = beta - 1.96 * se), alpha = 1, width = 0) +
  geom_hline(yintercept = 0) +
  scale_x_continuous(breaks = 1:10) +
  labs(y = "Effect (forest index)", x= "Inclusion Threshold decile of forest cover in 1990"
  ) + theme(legend.position = 'none')
)
# %%
ggsave(file.path(root, "out/fra_estimate_vcf.pdf"), device = cairo_pdf)
# %% GFC estimate for PRI
gfc[, D_fra := sch * (year >= 2008)]
fitter = function(cutoff){
  dat = gfc[pref_bin >= cutoff & gfc3 == TRUE]
  m = feols(def_ha ~ D_fra | village[t] + styear, cluster = ~block, dat)
  tidy(m)[, 2:3]
}

tic()
cutoff_res = map_dfr(1:10, fitter) %>% setDT
toc()
cutoff_res$n = 1:10
colnames(cutoff_res)[1:2] = c('beta', 'se')
# %%
(FRA_fit_gfc = ggplot(cutoff_res, aes(n, beta)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymax = beta + 1.96 * se,
    ymin = beta - 1.96 * se), alpha = 1, width = 0) +
  geom_hline(yintercept = 0) +
  scale_x_continuous(breaks = 1:10) +
  labs(title = 'GFC', y = "Effect (deforested area)",
  x = "Inclusion Threshold decile of forest cover in 2000 ",
  caption = '') + theme(legend.position = 'none')
)
# %%
ggsave(file.path(root, "out/fra_estimate_gfc.pdf"), device = cairo_pdf)
# %%


# %%
##     ## #### ##    ## #### ##    ##  ######
###   ###  ##  ###   ##  ##  ###   ## ##    ##
#### ####  ##  ####  ##  ##  ####  ## ##
## ### ##  ##  ## ## ##  ##  ## ## ## ##   ####
##     ##  ##  ##  ####  ##  ##  #### ##    ##
##     ##  ##  ##   ###  ##  ##   ### ##    ##
##     ## #### ##    ## #### ##    ##  ######
mine_dist = file.path(root, "tmp/fishnet_w_mine_distances.csv") |> fread()
vcf_data = merge(vcf, mine_dist, by.x = c("row", "col"), by.y = c("row", "col"))
# %% ####################################################
xcreg = vcf_data[year == first_pesa_exposure - 1][min_dist_to_mine <= 1]
xcreg[, del_forest := cover_1990 - forest_index]
# %% mining summary figure - binned scatterplot - fig8 panel A ;
gen = binsreg(y = xcreg$del_forest, x = xcreg$min_dist_to_mine,
  nbins = 30, polyreg = 2)
f = gen$bins_plot + lal_plot_theme() +
  labs(y = 'Change in Forest cover relative to baseline', x = 'Distance')
# %%
ggsave(file.path(root, 'out/deforestation_v_mines_vcf.pdf'), f, device = cairo_pdf)
# %%
#### ##    ## ######## ######## ########  ######## ##       ######## ##     ##
 ##  ###   ##    ##    ##       ##     ## ##       ##       ##        ##   ##
 ##  ####  ##    ##    ##       ##     ## ##       ##       ##         ## ##
 ##  ## ## ##    ##    ######   ########  ######   ##       ######      ###
 ##  ##  ####    ##    ##       ##   ##   ##       ##       ##         ## ##
 ##  ##   ###    ##    ##       ##    ##  ##       ##       ##        ##   ##
#### ##    ##    ##    ######## ##     ## ##       ######## ######## ##     ##
vcf_data[, `:=`(id_f = as.factor(cellid),
              sty_f = as.factor(styear),
              year_f = as.factor(year),
              D_f    = as.factor(D)
              )]
vcf_data[, distance := min_dist_to_mine * 110]
intsamp = vcf_data[distance <= 100]
#%% binning estimator - figure 8 panel B
mining_all = interflex(Y = "forest_index", D = "D_f", X = "distance",
  FE = c("id_f", "sty_f"), cl = "id_f",
  data = intsamp, theme.bw = T,
  Xlabel = 'Distance', Dlabel = "PESA",
  Ylabel = 'Forest Index (vcf)',
  cutoffs = seq(0, 100, 10),
  estimator = 'binning', CI = FALSE
)
mining_all$graph
# %%
gc()
ggsave(file.path(root, "out/Interflex_main_vcf_data.pdf"), mining_all$graph,
       device = cairo_pdf)



# %%
##     ## #### ##    ## ######## ########  ########  ######    ######
###   ###  ##  ###   ## ##       ##     ## ##       ##    ##  ##    ##
#### ####  ##  ####  ## ##       ##     ## ##       ##        ##
## ### ##  ##  ## ## ## ######   ########  ######   ##   ####  ######
##     ##  ##  ##  #### ##       ##   ##   ##       ##    ##        ##
##     ##  ##  ##   ### ##       ##    ##  ##       ##    ##  ##    ##
##     ## #### ##    ## ######## ##     ## ########  ######    ######

# %% vcf prep
vcf_estim = vcf_data[cover_1990 >= quantile(cover_1990, 0.5)]
vcf_estim[, mine_dist_q := ntile(min_dist_to_mine, 3)]
setnames(vcf_estim, 'blk', 'block')

vcf_estim[, `:=`(
  D_mine_1 = D * (mine_dist_q == 1),
  D_mine_2 = D * (mine_dist_q == 2),
  D_mine_3 = D * (mine_dist_q == 3))]
# %%
m00_vcf = feols(forest_index ~ D_mine_1 + D_mine_2 + D_mine_3 |
  cellid + year, cluster = "block", vcf_estim)
# 2wFE + state year FEs
m01_vcf = feols(forest_index ~ D_mine_1 + D_mine_2 + D_mine_3 |
  cellid + styear, cluster = "block",
  vcf_estim)
m03_vcf = feols(forest_index ~ D_mine_1 + D_mine_2 + D_mine_3 |
  cellid + cellid[t] + styear, cluster = "block",
  vcf_estim)


# %% GFC
gfc_estim = gfc[pref == 1]
gfc_estim[, mine_dist_q := ntile(min_dist_to_mine, 3)]
gfc_estim[, mine_dist_50_b := ifelse(min_dist_to_mine>5, 1, 0)]
gfc_estim[, `:=`(
  D_mine_1 = D * (mine_dist_q == 1),
  D_mine_2 = D * (mine_dist_q == 2),
  D_mine_3 = D * (mine_dist_q == 3))]


# %%
m00_gfc = feols(def_ha ~ D_mine_1 + D_mine_2 + D_mine_3 |
  village + year, cluster = "block", gfc_estim)
# 2wFE + state year FEs
m01_gfc = feols(def_ha ~ D_mine_1 + D_mine_2 + D_mine_3 |
  village + styear, cluster = "block",
  gfc_estim)
m03_gfc = feols(def_ha ~ D_mine_1 + D_mine_2 + D_mine_3 |
  village + village[t] + styear, cluster = "block",
  gfc_estim)
# %%
mods = list(m00_vcf, m01_vcf, m03_vcf, m00_gfc, m01_gfc, m03_gfc)
# %%
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
        "D_mine_1"     = "Scheduled X PESA X 1st Tercile",
        "D_mine_2"     = "Scheduled X PESA X 2nd Tercile",
        "D_mine_3"     = "Scheduled X PESA X 3rd Tercile",
        "block"        = "Block",
        "blk"          = "Block"
      )

# %%
etable(mods,
  style.tex = style.tex(main = "base", depvar.title = "", model.title = "",
    yesNo = c("$\\checkmark$", "")),
  signifCode = NA,
  fixef_sizes = T, fixef_sizes.simplify = F,
  fitstat = ~ n, tex = TRUE,
  dict = treatmap,
  label = "tab:regs_mining",
  title = glue::glue("regression estimates decomposed by distance to mines (ex-ante median cutoff)"),
  file = file.path(root, glue::glue("out/mining_test_regs.tex")), replace = TRUE
)

# %%
