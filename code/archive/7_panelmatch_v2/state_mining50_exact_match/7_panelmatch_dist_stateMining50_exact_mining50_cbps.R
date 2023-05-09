
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
# library( librarian )


######
# Setting working directory
######

# %% ####################################################
dbox_root = 'C:/Users/Anzony/Dropbox/india_pesa_forests'
sher_root = "/scratch/gpfs/ar8787/india_pesa_forests"
root = dbox_root
tmp = file.path(root, "tmp")
state_mining50_exact_match <- file.path( tmp, "panel_match_v2/state_mining50_exact_match")

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
  filter( cellid == 3812 ) %>% 
  select( c( "cellid", "year", "con_Mining50" ) )

# cellid year con_Mining50
# 1    3812 1995            0
# 2    3812 1996            0
# 3    3812 1997            0
# 4    3812 1998            0
# 5    3812 1999            0
# 6    3812 2000            0
# 7    3812 2001            0
# 8    3812 2002            0
# 9    3812 2003            0
# 10   3812 2004            0
# 11   3812 2005            0
# 12   3812 2006            1
# 13   3812 2007            0
# 14   3812 2008            0
# 15   3812 2009            0
# 16   3812 2010            0
# 17   3812 2011            0
# 18   3812 2012            0
# 19   3812 2013            0
# 20   3812 2014            0
# 21   3812 2015            0
# 22   3812 2016            0

# It looks like the treatment is only for one year
# It should keep be treated after that year
# We expect the following output for the treated units
# cellid year con_Mining50
# 1    3812 1995            0
# 2    3812 1996            0
# 3    3812 1997            0
# 4    3812 1998            0
# 5    3812 1999            0
# 6    3812 2000            0
# 7    3812 2001            0
# 8    3812 2002            0
# 9    3812 2003            0
# 10   3812 2004            0
# 11   3812 2005            0
# 12   3812 2006            1
# 13   3812 2007            1
# 14   3812 2008            1
# 15   3812 2009            1
# 16   3812 2010            1
# 17   3812 2011            1
# 18   3812 2012            1
# 19   3812 2013            1
# 20   3812 2014            1
# 21   3812 2015            1
# 22   3812 2016            1

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
  filter( cellid %in% c(3812, 209) ) %>% 
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
  select( c( "cellid", "year", "con_Mining50" ) ) %>% 
  print(n = 1000)

land %>% 
  filter( cellid %in% c(3812, 209) ) %>% 
  select( c( "cellid", "year", "con_Mining50" ) )

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

land_v2 %>% 
  filter( cellid %in% c(3812, 209) ) %>% 
  group_by( cellid,  con_Mining50 ) %>% 
  tally()

land %>% 
  filter( cellid %in% c(3812, 209) ) %>% 
  group_by( cellid,  con_Mining50 ) %>% 
  tally()


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
  select( cellid,  lag, con_Mining50 ) %>% 
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
cols_use_match <- c("cellid", "year","nl_p95",
                    "D", "num_state", "forest_index", 
                    "con_Mining50",  "con_Mining50_4",   
                    "con_Mining50_3", "con_Mining50_2", 
                    "con_Mining50_1" )

df_match <- df_final[ df_final$state == "Rajasthan" , cols_use_match ]
unique(df_final$state)
exact_match_vars <- c("num_state", "con_Mining50_4",   
                      "con_Mining50_3", "con_Mining50_2", 
                      "con_Mining50_1")
names(df_match)
######################################################



################################################################
# Matching con_Mining50 Using CBPSCORE 4 Years
################################################################
match_cbps_vcf_con_Mining50 = PanelMatch(
  lag = 4, time.id = "year", unit.id = "cellid",
  treatment = "D", refinement.method = "CBPS.weight",
  data = df_match,
  covs.formula = ~ I(lag(forest_index, 1:4)) 
  + I(lag(nl_p95, 1:4))  ,
  exact.match.variables = exact_match_vars,
  outcome.var = "con_Mining50",
  size.match = 5, qoi = "att",
  match.missing = FALSE, listwise.delete = TRUE,
  lead = 0:4, forbid.treatment.reversal = F,
  use.diagonal.variance.matrix = T
)

save(match_cbps_vcf_con_Mining50,
     file = file.path( state_mining50_exact_match, "panelmatch_cbps_vcf_con_Mining50.RData" ) )

################################################################
