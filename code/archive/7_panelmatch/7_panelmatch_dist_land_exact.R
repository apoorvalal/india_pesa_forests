# Run on cluster
# takes around 9 hours on slurm job with specs

# SBATCH --begin=now
# SBATCH --time=12:00:00
# SBATCH --partition=hns
# SBATCH --mem=40G
# SBATCH --ntasks-per-node=12
# SBATCH --mail-type=ALL

# Notes about the PanelMatch Function
# You may see the following warning message
# 
# Warning message:
# In panel_match(lag, time.id, unit.id, treatment, refinement.method,  :
#  non-numeric data exists. Only numeric (including binary) data can be used for refinement and calculations
# 
# About this warning message, it comes from this code
# 
# if (any(c("character", "factor") %in% sapply(df2, class)))
# {
#  warning("non-numeric data exists. Only numeric (including binary) data can be used for refinement and calculations")
# }
# 
# It checks in the all the columns of dataset, 
# not only the columns we use. 
# Keep the columns you use (all of them were numeric columns, 
# state also) and you will not get the warning message



# %% ####################################################

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
dbox_root = '~/Dropbox/india_pesa_forests'
sher_root = "/home/ar8787/forests_analysis"
root = sher_root
tmp = file.path(root, "tmp")

####################################################
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

# Get land_conflict data
# Root
tic()
land_conflict = file.path(root, "input/land_conflict_watch")
land <- read.csv( file.path(land_conflict, "cleaned_data.csv"))
toc()



# Checking Unique Ids before merging
library( eeptools )
isid_pm  <- isid( pm_data, c( "year", "cellid" ), verbose = FALSE )
isid_land  <- isid( land, c( "year", "cellid" ), verbose = FALSE )
print( paste( "year and Cellid uniquesly identify
              observations in pm_data", isid_pm ) )
print( paste( "year and Cellid uniquesly identify observations
              in land_conflict_watch", isid_land ) )

####################################################################

# Merging with Land Data
tic()
df <- pm_data %>%
  left_join( land, by = c("cellid" = "cellid", "year" = "year") )

# Generating lag variables.
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
df2 <- df %>%
  mutate( lag = year - first_pesa_exposure ) %>%
  filter(lag < 0 & lag > -5 ) %>%
  select( cellid,  lag, con_Overall30, con_Mining30, con_Forestry30 )

# Reshape for get each lag as columns for each var
df3 <- reshape(df2, idvar = "cellid", timevar = "lag", direction = "wide")

# Rename variables
colnames(df3) <- c("cellid" ,"con_Overall30_4", "con_Mining30_4",
                   "con_Forestry30_4", "con_Overall30_3",  "con_Mining30_3",
                   "con_Forestry30_3", "con_Overall30_2","con_Mining30_2"  ,
                   "con_Forestry30_2", "con_Overall30_1",  "con_Mining30_1" ,
                   "con_Forestry30_1" )

# Merge with the original data
df_final <- df %>%
  left_join( df3, by = "cellid" )

# Transform state to numeric value
# To avoid warning message
df_final <- transform( df_final, num_state = as.numeric( stf ) )
colnames(df_final)


########     ###    ##    ## ######## ##       ##     ##    ###    ########  ######  ##     ##
##     ##   ## ##   ###   ## ##       ##       ###   ###   ## ##      ##    ##    ## ##     ##
##     ##  ##   ##  ####  ## ##       ##       #### ####  ##   ##     ##    ##       ##     ##
########  ##     ## ## ## ## ######   ##       ## ### ## ##     ##    ##    ##       #########
##        ######### ##  #### ##       ##       ##     ## #########    ##    ##       ##     ##
##        ##     ## ##   ### ##       ##       ##     ## ##     ##    ##    ##    ## ##     ##
##        ##     ## ##    ## ######## ######## ##     ## ##     ##    ##     ######  ##     ##

# To avoid warning message keep only numeric columns
cols_use_match <- c("cellid", "year","con_Overall30_4",
                    "con_Mining30_4",
                    "con_Forestry30_4", "con_Overall30_3",
                    "con_Mining30_3",  "con_Forestry30_3",
                    "con_Overall30_2","con_Mining30_2"  ,
                    "con_Forestry30_2", "con_Overall30_1",
                    "con_Mining30_1" , "con_Forestry30_1",
                    "D", "num_state", "forest_index",
                    "nl_p95")

df_match <- df_final[ , cols_use_match]
exact_match_vars <- c("num_state", "con_Overall30_4",
                      "con_Mining30_4",
                      "con_Forestry30_4", "con_Overall30_3",
                      "con_Mining30_3",  "con_Forestry30_3",
                      "con_Overall30_2","con_Mining30_2"  ,
                      "con_Forestry30_2", "con_Overall30_1",
                      "con_Mining30_1" , "con_Forestry30_1")

######################################################
# # Before Match
tic()
match_nomatch_vcf_land_vars = PanelMatch(
  lag = 4, time.id = "year", unit.id = "cellid",
  treatment = "D", refinement.method = "none",
  data = df_match,
  covs.formula = ~ I(lag(forest_index, 1:4)) + I(lag(nl_p95, 1:4))  ,
  outcome.var = "forest_index",
  size.match = 5, qoi = "att",
  exact.match.variables = NULL ,
  match.missing = FALSE, listwise.delete = TRUE,
  lead = 0:4, forbid.treatment.reversal = F,
  use.diagonal.variance.matrix = T,
  matching = FALSE
)

save(match_nomatch_vcf_land_vars,
     file = file.path(root, "tmp/land_exact_match/panelmatch_nomatch_vcf_land_vars.RData"))
toc()

# Before Refinement
tic()
match_none_vcf_land_vars = PanelMatch(
  lag = 4, time.id = "year", unit.id = "cellid",
  treatment = "D", refinement.method = "none",
  data = df_match,
  covs.formula = ~ I(lag(forest_index, 1:4)) + I(lag(nl_p95, 1:4))  ,
  outcome.var = "forest_index",
  size.match = 5, qoi = "att",
  exact.match.variables = exact_match_vars ,
  match.missing = FALSE, listwise.delete = TRUE,
  lead = 0:4, forbid.treatment.reversal = F,
  use.diagonal.variance.matrix = T, 
  matching = TRUE
)

save(match_none_vcf_land_vars,
     file = file.path(root, "tmp/land_exact_match/panelmatch_none_vcf_land_vars.RData"))
toc()
######################################################
# Matching Using PSCORE 4 Years
tic()
match_ps_vcf_land_vars = PanelMatch(
  lag = 4, time.id = "year", unit.id = "cellid",
  treatment = "D", refinement.method = "ps.weight",
  data = df_match,
  covs.formula = ~ I(lag(forest_index, 1:4)) + I(lag(nl_p95, 1:4))  ,
  outcome.var = "forest_index",
  size.match = 5, qoi = "att",
  exact.match.variables = exact_match_vars ,
  match.missing = FALSE, listwise.delete = TRUE,
  lead = 0:4, forbid.treatment.reversal = F,
  use.diagonal.variance.matrix = T
)

save(match_ps_vcf_land_vars,
     file = file.path(root, "tmp/land_exact_match/panelmatch_ps_vcf_land_vars.RData"))
toc()
######################################################
# # Matching Using CBPS 4 years
tic()
match_cbps_vcf_land_vars = PanelMatch(
  lag = 4, time.id = "year", unit.id = "cellid",
  treatment = "D", refinement.method = "CBPS.weight",
  data = df_match,
  covs.formula = ~ I(lag(forest_index, 1:4)) + I(lag(nl_p95, 1:4))  ,
  outcome.var = "forest_index",
  size.match = 5, qoi = "att",
  exact.match.variables = exact_match_vars ,
  match.missing = FALSE, listwise.delete = TRUE,
  lead = 0:4, forbid.treatment.reversal = F,
  use.diagonal.variance.matrix = T
)

save(match_cbps_vcf_land_vars,
     file = file.path(root, "tmp/land_exact_match/panelmatch_cbps_vcf_land_vars.RData"))

toc()

# Save all objects in one 
save( match_nomatch_vcf_land_vars, 
      match_none_vcf_land_vars,
      match_ps_vcf_land_vars, 
      match_cbps_vcf_land_vars,
      df_match_no_land_vars, 
      file = file.path(root, "tmp/land_exact_match/7_panelmatch_dist_land_vars.RData"))

