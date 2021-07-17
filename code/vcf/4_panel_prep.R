# %% ####################################################
rm(list=ls())
library(LalRUtils)
LalRUtils::libreq(tidyverse, data.table, tictoc,
  rio, magrittr, janitor, fastDummies, fst)
set.seed(42)
theme_set(lal_plot_theme())
# %% ####################################################
dbox_root = '/home/alal/res/india_pesa_forests/'
root = dbox_root
data = file.path(root , 'inp')
tmp = file.path(root , 'tmp')
# %%
tic()
df = fread(file.path(tmp, "cell_panel_with_treatment_and_outcome.csv.gz"))
toc()
# %%
# cell identifier
df[, cellid := .GRP, .(row, col)]
# %% identifiers
df[, dist 	  := as.factor(DISTRICT)][,
     block 	  := as.factor(nameb)][,
     state 	  := as.factor(STATE_UT)]

# %% interpolate missing years by last value
obs_1994 = df[year == 1993][, year := 1994]
obs_2000 = df[year == 1999][, year := 2000]
df = rbind(df, obs_1994, obs_2000)
# %% # state X year FE
df[, styear := .GRP, by =  .(state, year)]
# weird id value
df[, `Unnamed: 0` := NULL]
df[, `:=`(
  first_pesa_exposure = case_when(
     STATE_UT == 'Andhra Pradesh'   ~ 2001 ,
     STATE_UT == 'Chhattisgarh'     ~ 2005 ,
     STATE_UT == 'Gujarat'          ~ 2001 ,
     STATE_UT == 'Jharkhand'        ~ 2010 ,
     STATE_UT == 'Maharashtra'      ~ 2007 ,
     STATE_UT == 'Orissa'           ~ 2002 ,
     STATE_UT == 'Madhya Pradesh'   ~ 2000 ,
     STATE_UT == 'Himachal Pradesh' ~ 2000 ,
     STATE_UT == 'Rajasthan'        ~ 2000 ,
     TRUE ~ 0))
]

# %%
df[, pesa_exposure  := 1* (year >= first_pesa_exposure)]
df[,
  D         := sch * pesa_exposure][,
  t         := year - 1995][,
  t2        := t^2]

df$D %>% tabyl
# %% demographic vars
df[, tot_non_sc_st := TOT_POP - (TOT_SC + TOT_ST)][,
     st_share      := TOT_ST/TOT_POP][,
     st_plurality  := fifelse(TOT_ST > max(TOT_SC, tot_non_sc_st), 1, 0)][,
     D_X_st_plurality := D * st_plurality]
df %>% glimpse
setnames(df, c("forest_cover", "green_cover", "built_cover"),
  c("forest_index", "green_index", "built_index"))

# %%
tic()
write_fst(df, file.path(tmp, 'vcf_cell_sample.fst')) # write file for future runs
toc()
# %%
