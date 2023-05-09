# %%
library(LalRUtils)
libreq(tidyverse, data.table, zoo, tictoc, fst, fixest, PanelMatch, patchwork,
  rio, magrittr, janitor, did, panelView, ggiplot, vtable)
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
vcf_data = read_fst(file.path(tmp, 'vcf_cell_sample.fst')) %>% setDT
toc()
# %% sort
setorder(vcf_data, cellid, year)
vcf_data[, blk := .GRP, by = .(block, state)]

# %% subset
kv = c(
  # id
  "cellid", "row", "col", "x", "y",
  # admin
  "sch", "state", "year", "styear", "blk", "block", "t", "t2",
  # outcomes
  "forest_index", "green_index", "built_index",
  # treatment
  "first_pesa_exposure", "D",
  # census
  "TOT_POP", "TOT_SC", "TOT_ST", "st_share", "st_plurality"
)

slice = vcf_data[, ..kv]
setcolorder(slice, kv)


# %% construct ex-ante cover using 5 year window preceding treatment
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

# %% merge in night lights
nl_wide = fread(file.path(tmp, "pixel_level_nightlights_1992_2018.csv"))
oldnames = nl_wide %>%
  colnames %>%
  grep(pattern = "percentile", value = T)
newnames = oldnames %>% sub("percentile_95", "p95", .)
setnames(nl_wide, oldnames, newnames)
nl_wide[, V1 := NULL]
# long
long_nl = melt(nl_wide, id.vars = c("row", "col"))
long_nl[, c('yr', 'stat') := tstrsplit(variable, "_")][,
  yr := substr(yr, 2, 5) %>% as.numeric][,
  stat := paste0("nl_", stat)][,
  variable := NULL][value >= 63, value := value]
# wide again at pixel X year level
long_nl = long_nl %>%
  dcast(row + col + yr ~ stat, value.var = 'value')

# %% merge on nightlights (missing prior to 1992)
vcf_data = merge(vcf_data, long_nl,
  by.x = c("row", "col", "year"),
  by.y = c("row", "col", "yr"),
  all.x = TRUE
  )

# %% write out
write_fst(vcf_data, file.path(tmp, 'vcf_reg_ready.fst'))
fwrite(vcf_data,    file.path(tmp, 'vcf_reg_ready.csv.gz'))
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
gfc %>% glimpse
# %%
kv = c("def_ha", "sch", "D", "village", "state", "year", "styear",
  "t", "block", "pref", "pref_bin", "pesa_exposure",
  "min_dist_to_mine", "pref_mean")
gfc = gfc[, ..kv]
# %% identifying variation states
gfc[, gfc3  := (state == "Chhattisgarh" | state == "Jharkhand" | state == "Maharashtra" | state == "Odisha")]
# %% write both datasets to memory
save(gfc, vcf_data, file = file.path(tmp, "regdata.rds"))
# %%
