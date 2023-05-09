
# %% ####################################################
rm(list = ls())
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
library("sdcLog")
# library( librarian )


######
# Setting working directory
######

# %% ####################################################
dbox_root = '~/Dropbox/india_pesa_forests'
sher_root = "/scratch/gpfs/ar8787/india_pesa_forests"
root = sher_root
tmp = file.path(root, "tmp")

####################################################
# Uploading Main data
####################################################

tic()
load(file.path(tmp, "regdata.rds"))
toc()

####################################################


####################################################
# vcf prep
####################################################

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

####################################################



####################################################
# Get land_conflict data
####################################################

tic()
land_conflict = file.path(root, "inp/land_conflict_watch")
land <- read.csv( file.path(land_conflict, "cleaned_data.csv"))
toc()

####################################################

####################################################
# Changing Values in Land Data
####################################################
# There is something that looks curious in land_conflict data
# Lets see only for one cellid

land %>% 
  filter( cellid == 209 ) %>% 
  select( c( "cellid", "year", "con_Overall30" ) )

# cellid year con_Overall30
# 1     209 1995             0
# 2     209 1996             0
# 3     209 1997             0
# 4     209 1998             0
# 5     209 1999             0
# 6     209 2000             0
# 7     209 2001             0
# 8     209 2002             0
# 9     209 2003             0
# 10    209 2004             0
# 11    209 2005             0
# 12    209 2006             0
# 13    209 2007             0
# 14    209 2008             0
# 15    209 2009             1
# 16    209 2010             0
# 17    209 2011             0
# 18    209 2012             0
# 19    209 2013             0
# 20    209 2014             0
# 21    209 2015             0
# 22    209 2016             0

# It looks like the treatment is only for one year
# It should keep be treated after that year
# We expect the following output for the treated units
# cellid year con_Overall30
# 1     209 1995             0
# 2     209 1996             0
# 3     209 1997             0
# 4     209 1998             0
# 5     209 1999             0
# 6     209 2000             0
# 7     209 2001             0
# 8     209 2002             0
# 9     209 2003             0
# 10    209 2004             0
# 11    209 2005             0
# 12    209 2006             0
# 13    209 2007             0
# 14    209 2008             0
# 15    209 2009             1
# 16    209 2010             1
# 17    209 2011             1
# 18    209 2012             1
# 19    209 2013             1
# 20    209 2014             1
# 21    209 2015             1
# 22    209 2016             1

# For the cellids that were never treated they should be the same
# We only have to change the treated units

# Before changing data we want to check something
# What is the min and max year for each cellid?
land %>% 
  group_by( cellid ) %>% 
  summarise( max_year = max( year ), 
             min_year = min(year ) ) %>% 
  distinct( max_year, min_year)
# According to the output 
# we have the same max and min year for all cellids 
# Checking code for two cellids
land %>% 
  filter( cellid %in% c(1, 209) ) %>% 
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
  pivot_wider( names_from = variables, values_from = new_value ) %>%
  select( c( "cellid", "year", "con_Overall30", "con_Power30" ) ) %>% 
  print(n = 1000)

land %>% 
  filter( cellid %in% c(1, 209) ) %>% 
  select( c( "cellid", "year", "con_Overall30", "con_Power30" ) )

# Changing alld data and save in a new object
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

####################################################
# Checking Unique Ids before merging
####################################################
library( eeptools )
isid_pm  <- isid( pm_data, c( "year", "cellid" ), verbose = FALSE )
isid_land_v2  <- isid( land_v2, c( "year", "cellid" ), verbose = FALSE )
print( paste( "year and Cellid uniquesly identify <br>
              observations in pm_data", isid_pm ) )
print( paste( "year and Cellid uniquesly identify observations
              in land_v2_conflict_watch", isid_land_v2 ) )

####################################################################

####################################################################
# Merging with land_v2 Data
####################################################################
df <- pm_data %>%
  left_join( land_v2, by = c("cellid" = "cellid", "year" = "year") )
####################################################################

####################################################################
# Generating lag variables
####################################################################
# The lag varaible correspond the the value of the variable
# the year before of the first pesa exposure
# for each cellid
library(data.table)

# We wanted to know the first year of exposure
# means the first year of treatment
st1 <- df %>%
  filter( D == 1 ) %>%
  group_by( state ) %>%
  summarise_at(vars(year ),
               list(minyear = min)) %>%
  right_join( df, by = "state" )

sum(st1$minyear == st1$first_pesa_exposure) == dim(st1)[1]
# We are sure that first pesa exposure is the first year of treatment

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
  select( cellid,  lag, con_Overall30, con_Mining30, con_Forestry30 ) %>% 
  reshape( idvar = "cellid", timevar = "lag", direction = "wide") %>% 
  rename_with( ~ gsub(".-", "_", .x, fixed = TRUE ) ) %>% 
  left_join( pm_data, by = c("cellid" = "cellid" ) ) %>%
  left_join( land_v2, by = c("cellid" = "cellid", "year" = "year") ) %>%
  transform( num_state = as.numeric( stf ) )



########     ###    ##    ## ######## ##       ##     ##    ###    ########  ######  ##     ##
##     ##   ## ##   ###   ## ##       ##       ###   ###   ## ##      ##    ##    ## ##     ##
##     ##  ##   ##  ####  ## ##       ##       #### ####  ##   ##     ##    ##       ##     ##
########  ##     ## ## ## ## ######   ##       ## ### ## ##     ##    ##    ##       #########
##        ######### ##  #### ##       ##       ##     ## #########    ##    ##       ##     ##
##        ##     ## ##   ### ##       ##       ##     ## ##     ##    ##    ##    ## ##     ##
##        ##     ## ##    ## ######## ######## ##     ## ##     ##    ##     ######  ##     ##



######################################################
# Preparing data to panelmatch
######################################################
names(df_final)
cols_use_match <- c("cellid", "year","con_Overall30_4",
                    "con_Mining30_4", "con_Overall30", "con_Mining30", "con_Forestry30",
                    "con_Forestry30_4", "con_Overall30_3",
                    "con_Mining30_3",  "con_Forestry30_3",
                    "con_Overall30_2","con_Mining30_2"  ,
                    "con_Forestry30_2", "con_Overall30_1",
                    "con_Mining30_1" , "con_Forestry30_1",
                    "D", "num_state", "forest_index", "con_Industry30",
                    "con_Infrastructure30", "con_LandUse30", "con_Power30",
                    "nl_p95")

df_match <- df_final[ , cols_use_match ]
unique(df_final$state)
exact_match_vars <- c("num_state")

######################################################


################################################################
# Matching con_Infrastructure30 Using PSCORE 4 Years
################################################################
tic()
match_ps_vcf_con_Infrastructure30 = PanelMatch(
  lag = 4, time.id = "year", unit.id = "cellid",
  treatment = "D", refinement.method = "ps.weight",
  data = df_match,
  covs.formula = ~ I(lag(forest_index, 1:4)) 
  + I(lag(nl_p95, 1:4))  ,
  exact.match.variables = exact_match_vars,
  outcome.var = "con_Infrastructure30",
  size.match = 5, qoi = "att",
  match.missing = FALSE, listwise.delete = TRUE,
  lead = 0:4, forbid.treatment.reversal = F,
  use.diagonal.variance.matrix = T
)

toc()
save(match_ps_vcf_con_Infrastructure30,
     file = file.path(root, "tmp/panel_match_v2/state_exact_match/panelmatch_ps_vcf_con_Infrastructure30.RData"))

match_cbps_vcf_con_Infrastructure30 = PanelMatch(
  lag = 4, time.id = "year", unit.id = "cellid",
  treatment = "D", refinement.method = "CBPS.weight",
  data = df_match,
  covs.formula = ~ I(lag(forest_index, 1:4)) 
  + I(lag(nl_p95, 1:4)),
  exact.match.variables = exact_match_vars,
  outcome.var = "con_Infrastructure30",
  size.match = 5, qoi = "att",
  match.missing = FALSE, listwise.delete = TRUE,
  lead = 0:4, forbid.treatment.reversal = F,
  use.diagonal.variance.matrix = T
)

save(match_cbps_vcf_con_Infrastructure30,
     file = file.path(root, "tmp/panel_match_v2/state_exact_match/panelmatch_cbps_vcf_con_Infrastructure30.RData"))

save(match_ps_vcf_con_Infrastructure30, 
     match_cbps_vcf_con_Infrastructure30,
     file = file.path(root, "tmp/panel_match_v2/state_exact_match/7_panelmatch_dist_state_exact_infrastructure.RData"))

################################################################
