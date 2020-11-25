###################################################
rm(list=ls())
# install_github('apoorvalal/LalRUtils')
library(LalRUtils)
libreq(tidyverse,magrittr, data.table, rio, tictoc)
####################################################
#%% reshape - run once
widedat = fread('../inp/village_points_all_mines.csv')
widedat = lower_varnames(widedat) %>% setDT
#%%
mine_distances = colnames(widedat) %>% str_subset("min_dist_*.")
widedat %>% glimpse

(defors = colnames(widedat) %>% str_subset('deforest') %>% sort)
keepvars = c(
    'code_2011', 'state_ut', 'district', 'sub_dist', 'name', 'nameb',
    'tot_hh', 'tot_pop', 'tot_sc', 'tot_st', 'tot_w',
    mine_distances,
    'sch', 'nearest_segment', 'lat', 'lon',
    defors,
    'pref_max', 'pref_count', 'pref_mean', 'pref_min'
)
dat = widedat[, ..keepvars]
dat[, code_2011 := as.numeric(code_2011)]
# dat[, .(code_2011, vcode)]

# %% dedupe - throws out edge cases
dat_dedup = dat[, .SD[1],
  by = .(code_2011, state_ut, district, sub_dist, name)]

#%% # wide to long reshape for deforestation count
tic()
dat_long = dat_dedup %>%
  tidyr::gather(deforestation, def_cells, deforest_0:deforest_9) %>% setDT
toc()

#%% # break deforestation column into year + get rid of prefix
dat_long %<>% tidyr::separate(col = deforestation, into = c(NA, 'y'), sep = -2)
# throw out underscore in single digits, convert to numeric
dat_long %>% mutate(year = 2000 + as.numeric(str_replace(y, "_", ''))) %>%
  select(-y) %>% setDT ->
  dat_long_clean
#%%
# arrange by village + year
setorder(dat_long_clean, state_ut, district, sub_dist, name, code_2011)
#%%
############################################################
# Slice analysis sample + define treatment
############################################################
#%%
# dat_long_clean = fread('Intermediate/villages_long2.csv')
dat_long_clean = dat_long_clean[code_2011 != "" & year %in% 2001:2017][,
  def := ifelse(is.na(def_cells), 0, def_cells)]
# tabulate missings
#%% # drop strange villages 4 X 17 in total
dat_long_clean = dat_long_clean[!(state_ut %in%
    c("", 'Part of Madhya Pradesh', 'Part of Dadra & Nagar Haveli', 'Puducherry'))][
      name != ""]

# "Andhra Pradesh"   "Bihar"      "Chhattisgarh"     "Gujarat"
# "Himachal Pradesh" "Jharkhand"  "Karnataka"        "Madhya Pradesh"
# "Maharashtra"      "Odisha"     "Rajasthan"        "West Bengal"
# "Punjab"
#%%
# generate flags
toc()
dat_long_clean[, `:=`(
  pesa_exposure = case_when(
     state_ut == 'Andhra Pradesh'   & year %in% 2001:2017 ~ 1,
     state_ut == 'Chhattisgarh'     & year %in% 2005:2017 ~ 1,
     state_ut == 'Gujarat'          & year %in% 2001:2017 ~ 1,
     state_ut == 'Himachal Pradesh' & year %in% 2000:2017 ~ 1,
     state_ut == 'Jharkhand'        & year %in% 2010:2017 ~ 1,
     state_ut == 'Madhya Pradesh'   & year %in% 2000:2017 ~ 1,
     state_ut == 'Maharashtra'      & year %in% 2007:2017 ~ 1,
     state_ut == 'Odisha'           & year %in% 2002:2017 ~ 1,
     state_ut == 'Rajasthan'        & year %in% 2000:2017 ~ 1,
     TRUE ~ 0),
    distance = ifelse(sch == 1, min_dist_to_border, -1 * min_dist_to_border))
]
toc()
#%% drop very big cities and unpopulated places
dat_long_clean = dat_long_clean[tot_pop > 0 & tot_pop < 1000000]
# dedupe
dat_long_clean = dat_long_clean[, .SD[1], by = .(code_2011, year)]
#%%
tic()
saveRDS(dat_long_clean, '../tmp/villages_estimation_sample.rds') # write file for future runs
export(dat_long_clean, '../tmp/villages_estimation_sample.dta') # for stata
toc()
