# %% ####################################################
rm( list = ls() )
library(tidyverse)
library(PanelMatch)
library(data.table)
library("zoo")
library("tictoc")
library('fst')
library("fixest")
library("PanelMatch")
library("patchwork")
library("rio")
library("magrittr")
library("janitor")
library('did')
library("panelView")
library("vtable")
library("RPushbullet")



##########################################################
###############  Setting working directory ###############
##########################################################

dbox_root = 'C:/Users/Anzony/Dropbox/india_pesa_forests'
sher_root = "/scratch/gpfs/ar8787/india_pesa_forests"
root = sher_root
tmp = file.path(root, "tmp")

##########################################################



##########################################################
################### VCF DATA        ######################
##########################################################

# Uploading Main data
tic()
load(file.path(tmp, "regdata.rds"))
toc()

# %% vcf prep
if (exists("vcf_data")) {
  vcf = vcf_data[year >= 1995]
  rm(vcf_data)
}
vcf[, t := year - 1995]
ex_ante_med = quantile(vcf$cover_1990, 0.5)
above_med = vcf[cover_1990 > ex_ante_med]
above_med[, never_treated := max(D) == 0, cellid]

above_med[, stf := as.factor(state)]
pm_data = as.data.frame(above_med)
unique(pm_data$state)

##########################################################



##########################################################
################# Land Conflict Data   ###################
##########################################################

# Get land_conflict data
# Root
tic()
land_conflict = file.path(root, "inp/land_conflict_watch")
land <- read.csv( file.path(land_conflict, "cleaned_data.csv"))
toc()



tic()
land_v2 <- land %>% 
  pivot_longer(
    !c(cellid, year), 
    names_to = "variables",
    values_to = "value"
  ) %>% 
  select( c( "cellid", "variables", "year", "value") ) %>% 
  group_by( cellid, variables ) %>%
  mutate( year_max_value = year[ which.max( value ) ], 
          max_value = value[ which.max( value ) ]) %>%
  ungroup() %>% 
  mutate( new_value = ifelse( year < year_max_value, 
                              value,  
                              max_value )  ) %>% 
  select( c( "cellid", "variables", "year", "new_value") ) %>% 
  pivot_wider( names_from = variables, values_from = new_value )
toc()

##########################################################



##########################################################
######################## PMGSY Data   ####################
##########################################################

# PMGSY data at village level
tic()
pmgsy_path = file.path(root, "inp/pmgsy_91vars" )
data1 <- read.csv( file.path( pmgsy_path, "pmgsy_91vars.csv"))
data1_copy <- copy(data1)
toc()

# Generating the years for the dataset
for ( year in 1995:2016 ){
  data1_copy[ as.character( year ) ] = 0
}
data1 <- data1 %>% 
  mutate( tdist_100_missing = ifelse( is.na( tdist_100 ), 
                                      1, 0 ) )

pmsgy_final <- data1_copy %>% 
  select( xv, yv, "1995":"2016" ) %>% 
  pivot_longer( "1995":"2016", "year" ) %>% 
  select( xv:year ) %>% 
  distinct() %>% 
  mutate(ID = row_number() ) %>%
  left_join( data1, 
             by = c("xv" = "xv", "yv" = "yv" ) ) %>% 
  mutate( roadcomp = ifelse( !is.na( roadcomp_yr ), 
                             ifelse( year < roadcomp_yr, 0, 1 ), 
                             0  ) , 
          roadsanc = ifelse( !is.na( road_sanc_year ), 
                             ifelse( year < road_sanc_year, 0, 1 ), 
                             0  ), 
          ns_pc91_vd_tar_road = ifelse( !is.na( ns_pc91_vd_tar_road ), 
                                        ns_pc91_vd_tar_road, 0 ), 
          ns_pc91_vd_power_all = ifelse( !is.na( ns_pc91_vd_power_all ), 
                                         ns_pc91_vd_power_all, 0 )
          ) %>% 
  transform( year = as.numeric( year ) ) %>% 
  select( -state )



# Collapsing at cellid and year level for all the variables
key_vars <- c( "roadcomp", "roadsanc", "high_lit_91", 
               "ns_pc91_vd_tar_road", "ns_pc91_vd_power_all", 
               "scst91_high" )

cbind(
  lapply(
    lapply(pmsgy_final, is.na)
    , sum)
)

names(pmsgy_final)
pmsgy_cellid_1 <- pmsgy_final %>% 
  select( cellid, year, roadcomp, roadsanc, high_lit_91, 
          ns_pc91_vd_tar_road, ns_pc91_vd_power_all, scst91_high ) %>% 
  pivot_longer( roadcomp:scst91_high, names_to = "vars" ) %>% 
  group_by( cellid, year, vars, value ) %>% 
  tally() %>% 
  pivot_wider( id_cols  = c( "cellid", "year" ), 
               names_from = c("vars", "value"), 
               values_from  = "n" )

pmsgy_cellid_2 <- pmsgy_final %>% 
  select( cellid, year, tdist_100 ) %>% 
  group_by( cellid, year ) %>% 
  summarise( tdist_100_avg = mean(tdist_100 , na.rm = TRUE ) )

# Merging the two outputs
pmsgy_cellid <- pmsgy_cellid_1 %>% 
  left_join( pmsgy_cellid_2, 
             by = c( "cellid" = "cellid", 
                     "year" = "year" ) )

# the mean is not taking into consideration 
# the na, only counting the values
pmsgy_final %>% 
  filter( cellid == 29 ) %>% 
  filter( year == 2000 )

pmsgy_cellid_2 %>% 
  filter( cellid == 29 ) %>% 
  filter( year == 2000 )

# Check the number of missing values in pmsgy_final
cbind(
  lapply(
    lapply(pmsgy_cellid, is.na)
    , sum)
)

##########################################################




##########################################################
###################### Merging Data   ####################
##########################################################

# Merging using cell id and year
# Year exposure is the first year of treatment
# Get the first year of exposure for each CELLID
# Generating the lags for each cell id using first_pesa_exposure
# Keep only the first 4 lags
# keep the variables we want their lag values
# (con_Overall30, con_Mining30 and con_Forestry30)
df_final <- pm_data %>%
  left_join( land_v2, by = c("cellid" = "cellid", "year" = "year") ) %>%
  mutate( lag = year - first_pesa_exposure ) %>%
  filter(lag < 0 & lag > -5 ) %>%
  select( cellid,  lag, con_Mining50 ) %>% 
  reshape( idvar = "cellid", timevar = "lag", direction = "wide") %>% 
  rename_with( ~ gsub(".-", "_", .x, fixed = TRUE ) ) %>% 
  left_join( pm_data, by = c("cellid" = "cellid" ) ) %>%
  left_join( land_v2, by = c("cellid" = "cellid", "year" = "year") ) %>%
  left_join( pmsgy_cellid , by = c("cellid" = "cellid", 
                                   "year" = "year") ) %>% # replace nan with 0
  mutate( roadcomp_1 = ifelse( !is.na( roadcomp_1 ), 
                               roadcomp_1, 
                               0  ) , 
          roadsanc_1 = ifelse( !is.na( roadsanc_1 ), 
                               roadsanc_1, 
                               0  ), 
          ns_pc91_vd_tar_road_1 = ifelse( !is.na( ns_pc91_vd_tar_road_1 ), 
                                          ns_pc91_vd_tar_road_1, 
                                          0 ), 
          ns_pc91_vd_power_all_1 = ifelse( !is.na( ns_pc91_vd_power_all_1 ), 
                                           ns_pc91_vd_power_all_1, 
                                           0 ), 
          scst91_high_1 = ifelse( !is.na( scst91_high_1 ), 
                                  scst91_high_1, 
                                  0 ),
          high_lit_91_1 = ifelse( !is.na( high_lit_91_1 ), 
                                  high_lit_91_1, 
                                  0 ),
  ) %>% 
  mutate( roadcomp_dummy = ifelse( roadcomp_1 > 0 , 
                                   1, 
                                   0  ), 
          roadsanc_dummy = ifelse( roadsanc_1 > 0 , 
                                   1, 
                                   0  ), 
          ns_pc91_vd_tar_road_dummy = ifelse( ns_pc91_vd_tar_road_1 > 0, 
                                              1, 
                                              0 ), 
          ns_pc91_vd_power_all_dummy = ifelse( ns_pc91_vd_power_all_1 > 0, 
                                               1, 
                                               0 ), 
          scst91_high_dummy = ifelse( scst91_high_1 > 0 , 
                                      1, 
                                      0 ),
          high_lit_91_dummy = ifelse( high_lit_91_1 > 0 , 
                                      1, 
                                      0 ) ) %>% 
  transform( num_state = as.numeric( stf ) ) 

# Save output
write.csv(df_final , file.path( tmp, "7_panelmatch/v2_state_mining_exact_match/7_panelmatch_data.csv" ), row.names=FALSE)

