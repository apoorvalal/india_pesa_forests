# %% ####################################################
rm(list = ls())
library(LalRUtils)

LalRUtils::libreq(tidyverse, data.table, fst, collapse, fixest, rio, readxl,
  magrittr, janitor, tictoc, modelsummary, hablar, patchwork, RPushbullet,
  IRdisplay, interflex, binsreg)

theme_set(lal_plot_theme()) # add _d() for dark
options(repr.plot.width=12, repr.plot.height=9)
options(ggplot2.discrete.fill = RColorBrewer::brewer.pal(9, "Set1"))
options(ggplot2.discrete.colour = RColorBrewer::brewer.pal(9, "Set1"))
options(ggplot2.continuous.fill = "viridis"); options(ggplot2.continuous.colour = "viridis")
set.seed(42)
chr = function(...) as.character(...) %>% display_html()
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
df[, blk   := .GRP, by =  .(block, state, year)]
df[, styear := .GRP, by =  .(state, year)]

# %% mining distances
mine_dist = file.path(root, "tmp/fishnet_w_mine_distances.csv") |> fread()
df = merge(df, mine_dist, by.x = c("row", "col"), by.y = c("row", "col"))

# %% ####################################################
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
xcreg = regsamp[year == first_pesa_exposure - 1][min_dist_to_mine <= 1]
xcreg[, del_forest := ex_ante_forest - forest_index]
# %% mining summary figure - binned scatterplot - fig8 panel A ;
gen = binsreg(y = xcreg$del_forest, x = xcreg$min_dist_to_mine,
  nbins = 30, polyreg = 3)
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
regsamp[, `:=`(id_f = as.factor(cellid),
              sty_f = as.factor(styear),
              year_f = as.factor(year),
              D_f    = as.factor(D)
              )]
regsamp[, distance := min_dist_to_mine * 110]
intsamp = regsamp[distance <= 100]
#%% binning estimator - figure 8 panel B
mining_all = interflex(Y = "forest_index", D = "D_f", X = "distance",
  FE = c("id_f", "sty_f"), cl = "id_f",
  data = intsamp, theme.bw = T,
  Xlabel = 'Distance', Dlabel = "PESA",
  Ylabel = 'Forest Index (vcf)',
  cutoffs = seq(0, 100, 5),
  estimator = 'binning', CI = FALSE
)
mining_all$graph
# %%
gc()
ggsave(file.path(root, "out/Interflex_main_vcf.pdf"), mining_all$graph,
       device = cairo_pdf)
# %%

##       ########    ###     ######  ########  ######
##       ##         ## ##   ##    ## ##       ##    ##
##       ##        ##   ##  ##       ##       ##
##       ######   ##     ##  ######  ######    ######
##       ##       #########       ## ##             ##
##       ##       ##     ## ##    ## ##       ##    ##
######## ######## ##     ##  ######  ########  ######

# 2. Mining Leases and Prospecting Licences – This section contains data for
# mining leases and prospecting licences granted/ executed to States. It also
# provides data mineral wise leases and by the type of organisation.
# https://www.epwrfits.in/MineralsStatistics.aspx

leases = fread(file.path(root, "inp/EPWRF_MiningLicenses.csv")) %>% clean_names
leases %>% str

leases = leases[2:37] %>% remove_empty('cols') %>% remove_empty('rows')
leases_long = data.table::melt(leases, id.vars = 'v1', variable.name = 'state',
  value.name = "permitsGranted")
leases_long[, y := as.numeric(v1)]
leases_long = leases_long[y %between% c(1993, 2012)]
leases_long[, stNmiss := sum(!is.na(permitsGranted)), state]
leases_long = leases_long[stNmiss != 0]
# %%
leases_long[, first_pesa_exposure := case_when(
     state == 'andhra_pradesh'   ~ 2001 ,
     state == 'chhattisgarh'     ~ 2005 ,
     state == 'gujarat'          ~ 2001 ,
     state == 'jharkhand'        ~ 2010 ,
     state == 'maharashtra'      ~ 2007 ,
     state == 'odisha'           ~ 2002 ,
     state == 'madhya_pradesh'   ~ 2000 ,
     state == 'himachal_pradesh' ~ 2000 ,
     state == 'rajasthan'        ~ 2000)][,
  treat := ifelse(y >= first_pesa_exposure, 1, 0)]
# %%

# %%
ggplot(data = leases_long[!(state %in% c("all_india", "delhi")) & !is.na(permitsGranted)],
        aes(x = y, y = permitsGranted, colour = as.factor(treat), group = state)) +
    geom_point(size = 0.5) + geom_line() + facet_wrap(~ state, scale = "free_y") +
    scale_color_manual(name = "Treatment Status",
      values = c("#d7191c","#fdae61","#000000"),
      labels = c("Pre", "Post", "NS"))

# %%
leases_long[is.na(treat), treat := 0]
feols(permitsGranted ~ treat | y + state, leases_long)

# %%
########  ####  ######  ########
##     ##  ##  ##    ##    ##
##     ##  ##  ##          ##
##     ##  ##   ######     ##
##     ##  ##        ##    ##
##     ##  ##  ##    ##    ##
########  ####  ######     ##

########     ###    ##    ## ######## ##
##     ##   ## ##   ###   ## ##       ##
##     ##  ##   ##  ####  ## ##       ##
########  ##     ## ## ## ## ######   ##
##        ######### ##  #### ##       ##
##        ##     ## ##   ### ##       ##
##        ##     ## ##    ## ######## ########

dist_lev = file.path(root, "inp/india-mines/mine_prod_district.dta") %>%
  import %>% setDT

dist_lev %>% glimpse

sdc = c("prod", "value")
garbage_minerals = c('limestone', 'fire clay', 'clay', 'quartz glass/silica sand',
                'mica', 'ochre', 'talc', 'gypsum', 'salt')
state_aggs = dist_lev[!(mineral %in% garbage_minerals),
  lapply(.SD, sum), by = .(smi_state_name, year),
  .SDcols = sdc]
# %%
state_aggs[,
  first_pesa_exposure := case_when(
     smi_state_name == 'andhra pradesh'   ~ 2001 ,
     smi_state_name == 'chhattisgarh'     ~ 2005 ,
     smi_state_name == 'gujarat'          ~ 2001 ,
     smi_state_name == 'jharkhand'        ~ 2010 ,
     smi_state_name == 'maharashtra'      ~ 2007 ,
     smi_state_name == 'orissa'           ~ 2002 ,
     smi_state_name == 'madhya pradesh'   ~ 2000 ,
     smi_state_name == 'himachal pradesh' ~ 2000 ,
     smi_state_name == 'rajasthan'        ~ 2000)
][, pesa := ifelse(year >= first_pesa_exposure, 1, 0)]
# %%
pesa_states = state_aggs[!is.na(first_pesa_exposure)]
ggplot(pesa_states, aes(year, log(prod), colour = as.factor(pesa))) +
  geom_point() + facet_wrap(~ smi_state_name, scales = "free")
