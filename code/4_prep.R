# %%
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
##     ##  ######  ########
##     ## ##    ## ##
##     ## ##       ##
##     ## ##       ######
 ##   ##  ##       ##
  ## ##   ##    ## ##
   ###     ######  ##

tic()
vcf_data = read_fst(file.path(tmp, 'vcf_cell_sample.fst')) %>% setDT # write file for future runs
toc()
# %% sort
setorder(vcf_data, cellid, year)
vcf_data[, blk   := .GRP, by =  .(block, state, year)]
# %% sanity check
# vcf_data[, sum_index := forest_index + green_index + built_index]
# vcf_data$sum_index %>% summary
# %% # construct ex-ante cover using 5 year window preceding treatment
slice = vcf_data[,
  .(cellid, row, col,
    forest_index, green_index, built_index, sch, D, state, year, styear, blk, first_pesa_exposure)]
slice[, first_pesa_exposure := max(first_pesa_exposure, na.rm = T), state]
slice[, five_yr_win := fifelse(year >= (first_pesa_exposure - 5) & year < first_pesa_exposure,
  1, 0)]
ex_ante = slice[five_yr_win == 1,
  .(ex_ante_forest_5y = mean(forest_index),
    ex_ante_green_5y  = mean(green_index),
    ex_ante_built_5y  = mean(built_index)),
  cellid]
# %% # merge it onto main
vcf_data = merge(slice, ex_ante, by = 'cellid')
# %% merge in 1990 cover too
cover_1990 = vcf_data[year == 1990, .(cover_1990 = forest_index), cellid]
vcf_data = merge(vcf_data, cover_1990, by = 'cellid')
# %%
write_fst(vcf_data, file.path(tmp, 'vcf_reg_ready.fst'))
# %%
 ######   ########  ######
##    ##  ##       ##    ##
##        ##       ##
##   #### ######   ##
##    ##  ##       ##
##    ##  ##       ##    ##
 ######   ##        ######
tic()
gfc = readRDS(file.path(root, 'tmp/villages_estimation_sample.rds')) %>% setDT
toc()

# %%
kv = c("def_ha", "sch", "D", "village", "state", "year", "styear", "t", "block", "pref", "pref_bin", "pesa_exposure")
gfc = gfc[, ..kv]
# %%
gfc[, gfc3  := (state == "Chhattisgarh" | state == "Jharkhand" | state == "Maharashtra" | state == "Odisha")]
# %% write both datasets to memory
save(gfc, vcf_data, file = file.path(tmp, "regdata.rds"))
# %%
