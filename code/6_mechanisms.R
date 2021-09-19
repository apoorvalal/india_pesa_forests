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
if (exists("vcf_data")){
  vcf = vcf_data[year>= 1995]
}
vcf[, t := year - 1995]
vcf[, time := year - first_pesa_exposure]
# %%
 ######   ########  ######## ######## ##    ##
##    ##  ##     ## ##       ##       ###   ##
##        ##     ## ##       ##       ####  ##
##   #### ########  ######   ######   ## ## ##
##    ##  ##   ##   ##       ##       ##  ####
##    ##  ##    ##  ##       ##       ##   ###
 ######   ##     ## ######## ######## ##    ##

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
# %%
########  ########  ####
##     ## ##     ##  ##
##     ## ##     ##  ##
########  ########   ##
##        ##   ##    ##
##        ##    ##   ##
##        ##     ## ####
# %% create treatment of PRI
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
pri[, time_pri := year - first_panch_elec]
pri[, D := (1 - sch) * (year >= first_panch_elec)]
pri[, nsch := (1 - sch)]
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
  labs(title = 'Effects of PRI on Tree Canopy Index by inclusion threshold',
   y = "Effect", x = "Cutoff Decile",
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
(rob_fit_FRA = ggplot(cutmods, aes(n, beta)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymax = beta + 1.96 * se,
    ymin = beta - 1.96 * se), alpha = 1, width = 0) +
  geom_hline(yintercept = 0) +
  scale_x_continuous(breaks = 1:10) +
  labs(title = 'Effects of FRA on Tree Canopy Index by inclusion threshold',
   y = "Effect", x = "Cutoff Decile",
  caption = '') + theme(legend.position = 'none')
)
# %%
ggsave(file.path(root, "out/fra_estimate.pdf"), device = cairo_pdf)
# %%

##     ## #### ##    ## #### ##    ##  ######
###   ###  ##  ###   ##  ##  ###   ## ##    ##
#### ####  ##  ####  ##  ##  ####  ## ##
## ### ##  ##  ## ## ##  ##  ## ## ## ##   ####
##     ##  ##  ##  ####  ##  ##  #### ##    ##
##     ##  ##  ##   ###  ##  ##   ### ##    ##
##     ## #### ##    ## #### ##    ##  ######
vcf = read_fst(file.path(tmp, 'vcf_reg_ready.fst')) %>% setDT
vcf = vcf[year>= 1995]
vcf[, t := year - 1995]
vcf[, time := year - first_pesa_exposure]
mine_dist = file.path(root, "tmp/fishnet_w_mine_distances.csv") |> fread()
vcf = merge(vcf, mine_dist, by.x = c("row", "col"), by.y = c("row", "col"))
# %% ####################################################
xcreg = vcf[year == first_pesa_exposure - 1][min_dist_to_mine <= 1]
xcreg[, del_forest := cover_1990 - forest_index]
# %% mining summary figure - binned scatterplot - fig8 panel A ;
gen = binsreg(y = xcreg$del_forest, x = xcreg$min_dist_to_mine,
  nbins = 30, polyreg = 2)
f = gen$bins_plot + lal_plot_theme() +
  labs(y = 'Change in Forest cover relative to baseline', x = 'Distance')
ggsave(file.path(root, 'out/deforestation_v_mines_vcf.pdf'), f, device = cairo_pdf)
# %%
#### ##    ## ######## ######## ########  ######## ##       ######## ##     ##
 ##  ###   ##    ##    ##       ##     ## ##       ##       ##        ##   ##
 ##  ####  ##    ##    ##       ##     ## ##       ##       ##         ## ##
 ##  ## ## ##    ##    ######   ########  ######   ##       ######      ###
 ##  ##  ####    ##    ##       ##   ##   ##       ##       ##         ## ##
 ##  ##   ###    ##    ##       ##    ##  ##       ##       ##        ##   ##
#### ##    ##    ##    ######## ##     ## ##       ######## ######## ##     ##
vcf[, `:=`(id_f = as.factor(cellid),
              sty_f = as.factor(styear),
              year_f = as.factor(year),
              D_f    = as.factor(D)
              )]
vcf[, distance := min_dist_to_mine * 110]
intsamp = vcf[distance <= 100]
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
ggsave(file.path(root, "out/Interflex_main_vcf.pdf"), mining_all$graph,
       device = cairo_pdf)
# %%
