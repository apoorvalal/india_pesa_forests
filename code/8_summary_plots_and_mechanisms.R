#%%
rm(list = ls())
library(LalRUtils)

load_or_install(c('tidyverse','magrittr', 'lfe','janitor',
                  'data.table', 'knitr', 'stargazer2',
                  'ggstatsplot', 'interflex', 'tictoc', 'rio', 'binsreg'
                  ))

####################################################
root = "../"
# root = yen_root
data = paste0(root, 'inp')
setwd(data)
theme_set(lal_plot_theme())

ch.row <- function(name, yesno, format = 'latex') {
  if (format == "latex"){
    return(c(name, ifelse(yesno, "$\\checkmark$", "")))
  } else {
    return(c(name, ifelse(yesno, "✓", "")))
  }
}


#%% sumstats on wide data
######  ##     ## ##     ## ##     ##    ###    ########  ##    ##
##    ## ##     ## ###   ### ###   ###   ## ##   ##     ##  ##  ##
##       ##     ## #### #### #### ####  ##   ##  ##     ##   ####
######  ##     ## ## ### ## ## ### ## ##     ## ########     ##
     ## ##     ## ##     ## ##     ## ######### ##   ##      ##
##    ## ##     ## ##     ## ##     ## ##     ## ##    ##     ##
######   #######  ##     ## ##     ## ##     ## ##     ##    ##
# pre-reshape wide data
widedat = fread(file.path(data, '../villages_points_all2.csv'))
widedat %>% glimpse
widedat = lower_varnames(widedat) %>% setDT
keepvars = c(
    'code_2011', 'state_ut', 'district', 'sub_dist', 'name',
    'tot_hh', 'tot_pop', 'tot_sc', 'tot_st', 'tot_w',
    'sch', 'pref_max', 'pref_mean', 'pref_count'
)
dat = widedat[, ..keepvars]
dat[, .SD[1],
  by = .(code_2011, state_ut, district, sub_dist, name)] -> dat_dedup
dat_dedup[, `:=`(st_share = tot_st / tot_pop,
                 sc_share = tot_sc / tot_pop)]
dat_trimmed = dat_dedup[tot_pop >= 10 &  !(tot_pop >= 1e6)]
dat_trimmed[, (c('tot_sc', 'tot_st', 'tot_w')) := NULL]

# %%
cov_labels = c('\\# households', 'Population', 'Scheduled', '2000 Forest Index (Max)',
  '200 Forest Index (Mean)', 'Number of Cells', 'ST Share' , 'SC Share' )

## ----st1----------------------------------------------------------------------
# %% table A1, A2
stargazer(dat_trimmed, covariate.labels = cov_labels, type = 'latex',
    out = file.path(out, 'vil_sumstats.tex'), float = F)
stargazer(dat_trimmed[pref_mean >= 2], covariate.labels = cov_labels, type = 'latex',
    out = file.path(out, 'vil_sumstats_forested.tex'), float = F)

# %%

#%% mine distance
##     ## #### ##    ## #### ##    ##  ######
###   ###  ##  ###   ##  ##  ###   ## ##    ##
#### ####  ##  ####  ##  ##  ####  ## ##
## ### ##  ##  ## ## ##  ##  ## ## ## ##   ####
##     ##  ##  ##  ####  ##  ##  #### ##    ##
##     ##  ##  ##   ###  ##  ##   ### ##    ##
##     ## #### ##    ## #### ##    ##  ######

# %% mining summary figure - binned scatterplot - fig8 panel A ;
dat = import(file.path(data, 'villages_estimation_sample.rds'))
late_states = dat[state_ut %in% c("Chhattisgarh", "Maharashtra", "Jharkhand") & year == 2001]

# %%
late_states = late_states[tot_pop > 0 & tot_pop < 1000000]
late_states$def_ha = late_states$def * 0.09

figsamp = late_states[min_dist_to_mine <= 1]
figsamp[, min_dist_to_mine  := min_dist_to_mine * 100]

gen = binsreg(y = figsamp$def_ha, x = figsamp$min_dist_to_mine,
  w = figsamp[, .(pref_count, pref_mean)],
  nbins = 20, polyreg = 2)

# %%
f = gen$bins_plot + lal_plot_theme() +
  labs(y = 'deforestated area in 2001 (residualised)', x = 'Distance')

ggsave(file.path(out, 'deforestation_v_mines.pdf'), f, device = cairo_pdf)

# %% estimate het-TE by distance to mines - table 2

# %%
tic()
estim_samp = import("../est_clean2.rds") %>% setDT
toc()
#%%
estim_samp2 = estim_samp[pref == 1]
estim_samp2[, mine_dist_q := ntile(min_dist_to_mine, 3)]

estim_samp2[, `:=`(
  D_mine_1 = D * (mine_dist_q == 1),
  D_mine_2 = D * (mine_dist_q == 2),
  D_mine_3 = D * (mine_dist_q == 3))]

# %% 2wFE
tic()
m00 = felm(def_ha ~ D_mine_1 + D_mine_2 + D_mine_3 |
  village + yr | 0 | village, estim_samp2)
toc()
# %% 2wFE + linear time trends
tic()
m01 = felm(def_ha ~ D_mine_1 + D_mine_2 + D_mine_3 |
  village + yr + village:t | 0 | village, estim_samp2)
toc()
# %% Villge + state X year FEs
tic()
m02 = felm(def_ha ~ D_mine_1 + D_mine_2 + D_mine_3 |
  village + state*yr | 0 | village, estim_samp2)
toc()
# %%
tic()
m03 = felm(def_ha ~ D_mine_1 + D_mine_2 + D_mine_3 |
  village + village:t + styear | 0 | village, estim_samp2)
toc()

# %%
dvmean = round(mean(estim_samp2$def_ha), 2)
nvill  = nunique(estim_samp2$code_2011)
# %% export
mods = list(m00, m01, m02, m03)

covlabs = c("Scheduled X PESA X 1st Tercile", "Scheduled X PESA X 2nd Tercile",
  "Scheduled X PESA X 3rd Tercile")


stargazer(mods, keep.stat = c("N"),
        covariate.labels = covlabs,
        dep.var.labels = c("Annual Deforestation in Hectares"),
        style = "apsr",
        column.sep.width = "0pt",
        title = "Treatment Effects on Annual Deforestation by Distance to Nearest Mine",
        model.names = F,
        label = "table:regresmine",
        notes = "Cluster-Robust Standard Errors (by village)",
        add.lines = list(
        ch.row('Village FE',      c(T,T,T,T), format = 'latex'),
        ch.row('Year FE',         c(T,T,F,F), format = 'latex'),
        ch.row('Village TT',      c(F,T,F,T), format = 'latex'),
        ch.row('State X Year FE', c(F,F,T,T), format = 'latex'),
        c("Dep. Var. Mean", rep(dvmean, 4)),
        c("N. Villages", rep(nvill, 4))
        ),
        type = 'latex', out = file.path(out, 'mining_test_regs.tex'))

# %%
#### ##    ## ######## ######## ########  ######## ##       ######## ##     ##
 ##  ###   ##    ##    ##       ##     ## ##       ##       ##        ##   ##
 ##  ####  ##    ##    ##       ##     ## ##       ##       ##         ## ##
 ##  ## ## ##    ##    ######   ########  ######   ##       ######      ###
 ##  ##  ####    ##    ##       ##   ##   ##       ##       ##         ## ##
 ##  ##   ###    ##    ##       ##    ##  ##       ##       ##        ##   ##
#### ##    ##    ##    ######## ##     ## ##       ######## ######## ##     ##

estim_samp = setDT(estim_samp)
estim_samp = estim_samp[pref == 1]
estim_samp[, min_dist_to_border := NULL]
# columns to min over
(min_distances = str_subset(colnames(estim_samp), "^min_dist_to_") %>%
    .[2:length(.)])
estim_samp[, min_dist_computed := do.call(pmin, c(.SD, list(na.rm = T))),
  .SDcols = min_distances]

# replace with distance in km

estim_samp[, min_dist_computed := min_dist_computed *110]
estim_samp[, (min_distances) := lapply(.SD, function(x) x * 110),
           .SDcols = min_distances ]

#%%
estim_samp[, `:=`(D_f = as.factor(D),
                  code_2011_f = as.factor(code_2011),
                  year_f = as.factor(year)) ]

#%% binning estimator - figure 8 panel B
tic()
mining_all = interflex(Y = "def_ha", D = "D_f", X = "min_dist_computed",
  FE = c("code_2011_f", "year"), # nbins = 10, Xunif = T,
  cutoffs = seq(0, 100, 10),
  xlab = "Distance", ylab = "Treatment Effect",
  main = "Treatment Effect is strongest in villages close to mines",
  data = estim_samp[min_dist_computed <= 100], theme.bw = T,
  estimator = 'binning', CI = FALSE)
toc()

mining_all$graph

# %%
ggsave(file.path(root, "Output/Interflex_main.pdf"), mining_all$graph,
       device = cairo_pdf)



# %% kernel estimator
tic()
mining_kernel = inter.kernel(
  Y = "def_ha", D = "D_f", X = "min_dist_computed",
  FE = c("code_2011_f", "year"), CI = F,
  xlab = "Distance", ylab = "Treatment Effect",
  data = estim_samp[min_dist_computed <= 100], theme.bw = T)
toc()

mining_all$graph

# %%

##################################################
# iterate through all mines
##################################################
# %% Fig A3
mining = list()
tic()
# iterate through list
for(m in 1:length(min_distances)){
    v = min_distances[m]
    print(v)
    cond = paste0(v, "<= 500")
    df = estim_samp[eval(parse(text = cond))]
    mod = interflex(Y = "def_ha", D = "D_f",
      X = min_distances[m],
      FE = c("code_2011_f", "year"), nbins = 5,
      data = df,
      ylab = "", xlab = "", theme_bw = T,
      estimator = 'binning', CI = FALSE)
    mining[[m]] = mod
}
toc()

saveRDS(mining, file = file.path(root, "tmp/mining_distances.rds"))

# uniform X
mining = list()
tic()
# iterate through list
for(m in 1:length(min_distances)){
    v = min_distances[m]
    print(v)
    cond = paste0(v, "<= 500")
    df = estim_samp[eval(parse(text = cond))]
    mod = interflex(Y = "def_ha", D = "D_f",
      X = min_distances[m],
      FE = c("code_2011_f", "year"), nbins = 5,
      data = df,
      ylab = "", xlab = "", Xunif = T, theme.bw = T,
      estimator = 'binning', CI = FALSE)
    mining[[m]] = mod
}
toc()


saveRDS(mining, file = file.path(data, "tmp/mining_distances_XUnif.rds"))

# %% for each mine type
mining_figs = readRDS(file = file.path(data, "tmp/mining_distances.rds"))

all_figs = list(
  mining_figs[[1]],
  mining_figs[[2]],
  mining_figs[[3]],
  mining_figs[[4]],
  # mining_figs[[5]],
  mining_figs[[6]],
  mining_figs[[7]],
  mining_figs[[8]],
  # mining_figs[[9]],
  mining_figs[[10]],
  mining_figs[[11]],
  mining_figs[[12]],
  mining_figs[[13]],
  mining_figs[[14]],
  mining_figs[[15]],
  mining_figs[[16]],
  mining_figs[[17]],
  mining_figs[[18]],
  # mining_figs[[19]],
  mining_figs[[20]],
  mining_figs[[21]],
  mining_figs[[22]],
  mining_figs[[23]],
  mining_figs[[24]],
  mining_figs[[25]],
  mining_figs[[26]],
  mining_figs[[27]],
  # mining_figs[[28]],
  # mining_figs[[29]],
  mining_figs[[30]],
  mining_figs[[31]],
  # mining_figs[[32]],
  # mining_figs[[33]],
  # mining_figs[[34]],
  # mining_figs[[35]],
  # mining_figs[[36]],
  # mining_figs[[37]],
  # mining_figs[[38]],
  # mining_figs[[39]],
  # mining_figs[[40]],
  # mining_figs[[41]],
  # mining_figs[[42]],
  mining_figs[[43]],
  # mining_figs[[44]],
  mining_figs[[45]]
)


plotmaker = function(f){
  mineral = str_sub(f$Xlabel, 13)
  ff = f$graph + labs(title = mineral)+
     theme(axis.title.x = element_blank(),
            axis.title.y = element_blank())
  return(ff)
}

multi = map(all_figs, plotmaker) %>% wrap_plots(nrow = 7) +
  plot_annotation("Heterogeneous Treatment Effects by Mineral")

ggsave(file.path(root, 'Output/mining_het_te.pdf'), multi, device = cairo_pdf,
  height = 25, width = 15)


ggsave(file.path(root, 'Output/mining_het_te.png'), multi, height = 25, width = 15)
# %%
