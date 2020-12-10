#%%
rm(list = ls())
library(LalRUtils)

load_or_install(c('tidyverse','magrittr', 'lfe','janitor',
                  'data.table', 'knitr', 'stargazer2',
                  'ggstatsplot', 'interflex', 'tictoc', 'rio', 'binsreg'
                  ))
library(stargazer)
# devtools::install_github('xuyiqing/interflex') # it works
library(interflex)
####################################################
root = "./"
# root = yen_root
data = paste0(root, 'inp')
# setwd(data)
theme_set(lal_plot_theme())

ch.row <- function(name, yesno, format = 'latex') {
  if (format == "latex"){
    return(c(name, ifelse(yesno, "$\\checkmark$", "")))
  } else {
    return(c(name, ifelse(yesno, "✓", "")))
  }
}


# %% estimate het-TE by distance to mines - table 2

# %%
tic()
df = import("./tmp/est_clean2.rds") %>% setDT
toc()
#%%

df[, min_dist_to_border := NULL]
# columns to min over
(min_distances = str_subset(colnames(df), "^min_dist_to_") %>%
    .[2:length(.)])
df[, min_dist_computed := do.call(pmin, c(.SD, list(na.rm = T))),
   .SDcols = min_distances]

# replace with distance in km

df[, min_dist_computed := min_dist_computed *110]
df[, (min_distances) := lapply(.SD, function(x) x * 110),
   .SDcols = min_distances ]

#%%
df[, `:=`(D_f = as.factor(D),
          code_2011_f = as.factor(code_2011),
          year_f = as.factor(year)) ]

# NEW ADDED
df2[, min_dist_to_border := NULL]
# columns to min over
(min_distances = str_subset(colnames(df2), "^min_dist_to_") %>%
    .[2:length(.)])
df2[, min_dist_computed := do.call(pmin, c(.SD, list(na.rm = T))),
    .SDcols = min_distances]

# replace with distance in km

df2[, min_dist_computed := min_dist_computed *110]
df2[, (min_distances) := lapply(.SD, function(x) x * 110),
    .SDcols = min_distances ]

#%%
df2[, `:=`(D_f = as.factor(D),
           code_2011_f = as.factor(code_2011),
           year_f = as.factor(year)) ]
df2[, mine_dist_computed_inv := 1/min_dist_computed]


# Correlation between distance and ex-ante forest rates 
summary(lm(pref_mean ~ min_dist_to_mine, data = df))

df2 = df[pref == 1]

df2[, mine_dist_q := ntile(min_dist_to_mine, 3)]
df2[, mine_dist_d := ntile(min_dist_to_mine, 9)]
df2[, mine_dist_inv := 1/min_dist_to_mine]

df2[, `:=`(
  D_mine_1 = D * (mine_dist_q == 1),
  D_mine_2 = D * (mine_dist_q == 2),
  D_mine_3 = D * (mine_dist_q == 3))]

df3 = df2[min_dist_computed <= median(df2$min_dist_computed)]
df3[, mine_dist_50_b := ifelse(min_dist_computed>5, 1, 0)]
df3[, mine_dist_50_q := ntile(min_dist_to_mine, 3)]

df3[, `:=`(
  D_mine_50_1 = D * (mine_dist_50_q == 1),
  D_mine_50_2 = D * (mine_dist_50_q == 2),
  D_mine_50_3 = D * (mine_dist_50_q == 3))]

# %% 2wFE
tic()
m00 = felm(def_ha ~ D_mine_1 + D_mine_2 + D_mine_3 |
  village + yr | 0 | village, df2)
toc()
# %% 2wFE + linear time trends
tic()
m01 = felm(def_ha ~ D_mine_1 + D_mine_2 + D_mine_3 |
  village + yr + village:t | 0 | village, df2)
toc()
# %% Villge + state X year FEs
tic()
m02 = felm(def_ha ~ D_mine_1 + D_mine_2 + D_mine_3 |
  village + state*yr | 0 | village, df2)
toc()
# %%
tic()
m03 = felm(def_ha ~ D_mine_1 + D_mine_2 + D_mine_3 |
  village + village:t + styear | 0 | village, df2)
toc()

m04 = felm(def_ha ~ D_mine_1 + D_mine_2 + D_mine_3 |
             village + yr + village:t + styear | 0 | village, df2)

m00_1 = felm(def_ha ~ D*mine_dist_inv |
             village + yr | 0 | village, df2)

m01_1 = felm(def_ha ~ D*mine_dist_inv |
             village + yr + village:t | 0 | village, df2)

m02_1 = felm(def_ha ~ D*mine_dist_inv |
             village + state*yr | 0 | village, df2)

m03_1 = felm(def_ha ~ D*mine_dist_inv |
             village + village:t + styear | 0 | village, df2)

m00_2 = felm(def_ha ~ D_mine_1 + D_mine_2 + D_mine_3 |
               village + yr | 0 | block, df2)

m01_2 = felm(def_ha ~ D_mine_1 + D_mine_2 + D_mine_3 |
               village + yr + village:t | 0 | block, df2) # insig

m02_2 = felm(def_ha ~ D_mine_1 + D_mine_2 + D_mine_3 |
               village + state*yr | 0 | block, df2) # insig

m03_2 = felm(def_ha ~ D_mine_1 + D_mine_2 + D_mine_3 |
               village + village:t + styear | 0 | block, df2)

m00_3 = felm(def_ha ~ D*mine_dist_inv |
               village + yr | 0 | block, df2)

m01_3 = felm(def_ha ~ D*mine_dist_inv |
               village + yr + village:t | 0 | block, df2)

m02_3 = felm(def_ha ~ D*mine_dist_inv |
               village + state*yr | 0 | block, df2)

m03_3 = felm(def_ha ~ D*mine_dist_inv |
               village + village:t + styear | 0 | block, df2)

m00_6 = felm(def_ha ~ D*min_dist_computed |
               village + yr | 0 | village, df3[min_dist_computed >= 5])

m01_6 = felm(def_ha ~ D*min_dist_computed |
               village + yr + village:t | 0 | village, df3[min_dist_computed >= 5])

m02_6 = felm(def_ha ~ D*min_dist_computed |
               village + state*yr | 0 | village, df3[min_dist_computed >= 5])

m03_6 = felm(def_ha ~ D*min_dist_computed |
               village + village:t + styear | 0 | village, df3[min_dist_computed >= 5]) # interactions insig or even posi!

m00_7 = felm(def_ha ~ D*mine_dist_50_b |
               village + yr | 0 | village, df3)

m01_7 = felm(def_ha ~ D*mine_dist_50_b |
               village + yr + village:t | 0 | village, df3)

m02_7 = felm(def_ha ~ D*mine_dist_50_b |
               village + state*yr | 0 | village, df3)

m03_7 = felm(def_ha ~ D*mine_dist_50_b |
               village + village:t + styear | 0 | village, df3) # interactions all turn posi (sig)!

m00_new = felm(def_ha ~ D*pref_mean | village + yr | 0 | village, df2)

m01_new = felm(def_ha ~ D*pref_mean | village + yr + village:t | 0 | village, df2)

m02_new = felm(def_ha ~ D*pref_mean | village + state*yr | 0 | village, df2)

m03_new = felm(def_ha ~ D*pref_mean | village + village:t + styear | 0 | village, df2)

# %%
dvmean = round(mean(df2$def_ha), 2)
nvill  = nunique(df2$code_2011)
# %% export
mods = list(m00, m01, m02, m03)

covlabs = c("Scheduled X PESA X 1st Tercile", "Scheduled X PESA X 2nd Tercile",
  "Scheduled X PESA X 3rd Tercile")

mods_inv = list(m00_1, m01_1, m02_1, m03_1)
mods_block = list(m00_2, m01_2, m02_2, m03_2)
mods_inv_block = list(m00_3, m01_3, m02_3, m03_3)
mods_5_50_linear = list(m00_6, m01_6, m02_6, m03_6)
mods_5_50_binary = list(m00_7, m01_7, m02_7, m03_7)
mods_new = list(m00_new, m01_new, m02_new, m03_new)

stargazer(mods_new, keep.stat = c("N"), header = F,
          dep.var.labels = c("Annual Deforestation in Hectares"),
          style = "apsr",
          column.sep.width = "0pt",
          title = "Treatment Effects on Annual Deforestation by Previous Forest Rates",
          model.names = F,
          label = "table:regresmine10",
          notes = "Cluster-Robust Standard Errors (by village)",
          add.lines = list(
            ch.row('Village FE',      c(T,T,T,T), format = 'latex'),
            ch.row('Year FE',         c(T,T,F,F), format = 'latex'),
            ch.row('Village TT',      c(F,T,F,T), format = 'latex'),
            ch.row('State X Year FE', c(F,F,T,T), format = 'latex'),
            c("Dep. Var. Mean", rep(dvmean, 4)),
            c("N. Villages", rep(nvill, 4))
          ),
          type = 'latex')

stargazer(mods_inv, keep.stat = c("N"), header = F,
          dep.var.labels = c("Annual Deforestation in Hectares"),
          style = "apsr",
          column.sep.width = "0pt",
          title = "Treatment Effects on Annual Deforestation by Proximity to Nearest Mine",
          model.names = F,
          label = "table:regresmine2",
          notes = "Cluster-Robust Standard Errors (by village)",
          add.lines = list(
            ch.row('Village FE',      c(T,T,T,T), format = 'latex'),
            ch.row('Year FE',         c(T,T,F,F), format = 'latex'),
            ch.row('Village TT',      c(F,T,F,T), format = 'latex'),
            ch.row('State X Year FE', c(F,F,T,T), format = 'latex'),
            c("Dep. Var. Mean", rep(dvmean, 4)),
            c("N. Villages", rep(nvill, 4))
          ),
          type = 'latex')

stargazer(mods_block, keep.stat = c("N"), header = F,
          dep.var.labels = c("Annual Deforestation in Hectares"),
          style = "apsr",
          column.sep.width = "0pt",
          title = "Treatment Effects on Annual Deforestation by Distance to Nearest Mine (Block-level SE)",
          model.names = F,
          label = "table:regresmine3",
          notes = "Cluster-Robust Standard Errors (by village)",
          add.lines = list(
            ch.row('Village FE',      c(T,T,T,T), format = 'latex'),
            ch.row('Year FE',         c(T,T,F,F), format = 'latex'),
            ch.row('Village TT',      c(F,T,F,T), format = 'latex'),
            ch.row('State X Year FE', c(F,F,T,T), format = 'latex'),
            c("Dep. Var. Mean", rep(dvmean, 4)),
            c("N. Villages", rep(nvill, 4))
          ),
          type = 'latex')

stargazer(mods_inv_block, keep.stat = c("N"), header = F,
          dep.var.labels = c("Annual Deforestation in Hectares"),
          style = "apsr",
          column.sep.width = "0pt",
          title = "Treatment Effects on Annual Deforestation by Proximity to Nearest Mine (Block-level SE)",
          model.names = F,
          label = "table:regresmine4",
          notes = "Cluster-Robust Standard Errors (by village)",
          add.lines = list(
            ch.row('Village FE',      c(T,T,T,T), format = 'latex'),
            ch.row('Year FE',         c(T,T,F,F), format = 'latex'),
            ch.row('Village TT',      c(F,T,F,T), format = 'latex'),
            ch.row('State X Year FE', c(F,F,T,T), format = 'latex'),
            c("Dep. Var. Mean", rep(dvmean, 4)),
            c("N. Villages", rep(nvill, 4))
          ),
          type = 'latex')

stargazer(mods, keep.stat = c("N"), header = F,
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

stargazer(mods_5_50_linear, keep.stat = c("N"), header = F,
          dep.var.labels = c("Annual Deforestation in Hectares"),
          style = "apsr",
          column.sep.width = "0pt",
          title = "Treatment Effects on Annual Deforestation by Distance to Nearest Mine (5km-50km)",
          model.names = F,
          label = "table:regresmine5",
          notes = "Cluster-Robust Standard Errors (by village)",
          add.lines = list(
            ch.row('Village FE',      c(T,T,T,T), format = 'latex'),
            ch.row('Year FE',         c(T,T,F,F), format = 'latex'),
            ch.row('Village TT',      c(F,T,F,T), format = 'latex'),
            ch.row('State X Year FE', c(F,F,T,T), format = 'latex'),
            c("Dep. Var. Mean", rep(dvmean, 4)),
            c("N. Villages", rep(nvill, 4))
          ),
          type = 'latex')

stargazer(mods_5_50_binary, keep.stat = c("N"), header = F,
          dep.var.labels = c("Annual Deforestation in Hectares"),
          style = "apsr",
          column.sep.width = "0pt",
          title = "Treatment Effects on Annual Deforestation by Distance to Nearest Mine (5km below or above)",
          model.names = F,
          label = "table:regresmine6",
          notes = "Cluster-Robust Standard Errors (by village)",
          add.lines = list(
            ch.row('Village FE',      c(T,T,T,T), format = 'latex'),
            ch.row('Year FE',         c(T,T,F,F), format = 'latex'),
            ch.row('Village TT',      c(F,T,F,T), format = 'latex'),
            ch.row('State X Year FE', c(F,F,T,T), format = 'latex'),
            c("Dep. Var. Mean", rep(dvmean, 4)),
            c("N. Villages", rep(nvill, 4))
          ),
          type = 'latex')

# %%
#### ##    ## ######## ######## ########  ######## ##       ######## ##     ##
 ##  ###   ##    ##    ##       ##     ## ##       ##       ##        ##   ##
 ##  ####  ##    ##    ##       ##     ## ##       ##       ##         ## ##
 ##  ## ## ##    ##    ######   ########  ######   ##       ######      ###
 ##  ##  ####    ##    ##       ##   ##   ##       ##       ##         ## ##
 ##  ##   ###    ##    ##       ##    ##  ##       ##       ##        ##   ##
#### ##    ##    ##    ######## ##     ## ##       ######## ######## ##     ##


#%% binning estimator - figure 8 panel B

gc()
memory.limit(12000)

mining_new = interflex(Y = "def_ha", D = "D_f", X = "min_dist_computed",
                       FE = c("code_2011_f", "year"), # nbins = 10, Xunif = T,
                       cutoffs = seq(0, 100, 10),
                       xlab = "Distance", ylab = "Treatment Effect",
                       main = "Treatment Effect is strongest in villages close to mines",
                       data = df2[min_dist_computed <= 100], theme.bw = T,
                       estimator = 'binning')

mining_new$graph
mining_new$tests

mining_new_50_100 = interflex(Y = "def_ha", D = "D_f", X = "min_dist_computed",
                       FE = c("code_2011_f", "year"), # nbins = 10, Xunif = T,
                       cutoffs = seq(median(df2$min_dist_computed), 100, 7),
                       xlab = "Distance", ylab = "Treatment Effect",
                       main = "Treatment Effect is strongest in villages close to mines",
                       data = df2[min_dist_computed <= 100 & min_dist_computed > median(df2$min_dist_computed)], theme.bw = T,
                       estimator = 'binning')

mining_new_50_100$graph
mining_new_50_100$tests

mining_new_50 = interflex(Y = "def_ha", D = "D_f", X = "min_dist_computed",
                       FE = c("code_2011_f", "year"), # nbins = 10, Xunif = T,
                       cutoffs = seq(0, median(df2$min_dist_computed), 2),
                       xlab = "Distance", ylab = "Treatment Effect",
                       main = "Treatment Effect is strongest in villages close to mines",
                       data = df2[min_dist_computed <= median(df2$min_dist_computed)], theme.bw = T,
                       estimator = 'binning')

mining_new_50$graph
mining_new_50$tests

