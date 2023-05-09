
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
root = dbox_root
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
land_conflict = file.path(root, "inp/land_conflict_watch")
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
                    "con_Mining30_4", "con_Overall30", "con_Mining30", "con_Forestry30",
                    "con_Forestry30_4", "con_Overall30_3",
                    "con_Mining30_3",  "con_Forestry30_3",
                    "con_Overall30_2","con_Mining30_2"  ,
                    "con_Forestry30_2", "con_Overall30_1",
                    "con_Mining30_1" , "con_Forestry30_1",
                    "D", "num_state", "forest_index", "con_Industry30",
                    "con_Infrastructure30", "con_LandUse30", "con_Power30",
                    "nl_p95")

df_match <- df_final[ , cols_use_match]


######################################################

exact_match <- c( "num_state", "con_Overall30_1", "con_Overall30_2", "con_Overall30_3", "con_Overall30_4",
                  "con_Mining30_1", "con_Mining30_2", "con_Mining30_3", "con_Mining30_4",
                  "con_Forestry30_1", "con_Forestry30_2", "con_Forestry30_3", "con_Forestry30_4")

match_ps_vcf_con_Overall30 = PanelMatch(
  lag = 4, time.id = "year", unit.id = "cellid",
  treatment = "D", refinement.method = "ps.weight",
  data = df_match,
  covs.formula = ~ I(lag(forest_index, 1:4)) + I(lag(nl_p95, 1:4))  ,
  exact.match.variables = exact_match,
  outcome.var = "con_Overall30",
  size.match = 5, qoi = "att",
  match.missing = FALSE, listwise.delete = TRUE,
  lead = 0:4, forbid.treatment.reversal = F,
  use.diagonal.variance.matrix = T
)
save(match_ps_vcf_con_Overall30,
     file = file.path(root, "tmp/land_exact_match/panelmatch_ps_vcf_con_Overall30.RData"))

match_cbps_vcf_con_Overall30 = PanelMatch(
  lag = 4, time.id = "year", unit.id = "cellid",
  treatment = "D", refinement.method = "CBPS.weight",
  data = df_match,
  covs.formula = ~ I(lag(forest_index, 1:4)) + I(lag(nl_p95, 1:4))  ,
  exact.match.variables = exact_match,
  outcome.var = "con_Overall30",
  size.match = 5, qoi = "att",
  match.missing = FALSE, listwise.delete = TRUE,
  lead = 0:4, forbid.treatment.reversal = F,
  use.diagonal.variance.matrix = T
)
save(match_cbps_vcf_con_Overall30,
     file = file.path(root, "tmp/land_exact_match/panelmatch_cbps_vcf_con_Overall30.RData"))

# Forestry
match_ps_vcf_con_Forestry30 = PanelMatch(
  lag = 4, time.id = "year", unit.id = "cellid",
  treatment = "D", refinement.method = "ps.weight",
  data = df_match,
  covs.formula = ~ I(lag(forest_index, 1:4)) + I(lag(nl_p95, 1:4))  ,
  exact.match.variables =exact_match, 
  outcome.var = "con_Forestry30",
  size.match = 5, qoi = "att",
  match.missing = FALSE, listwise.delete = TRUE,
  lead = 0:4, forbid.treatment.reversal = F,
  use.diagonal.variance.matrix = T
)
save(match_ps_vcf_con_Forestry30,
     file = file.path(root, "tmp/land_exact_match/panelmatch_ps_vcf_con_Forestry30.RData"))

match_cbps_vcf_con_Forestry30 = PanelMatch(
  lag = 4, time.id = "year", unit.id = "cellid",
  treatment = "D", refinement.method = "CBPS.weight",
  data = df_match,
  covs.formula = ~ I(lag(forest_index, 1:4)) + I(lag(nl_p95, 1:4))  ,
  exact.match.variables = exact_match,
  outcome.var = "con_Forestry30",
  size.match = 5, qoi = "att",
  match.missing = FALSE, listwise.delete = TRUE,
  lead = 0:4, forbid.treatment.reversal = F,
  use.diagonal.variance.matrix = T
)
save(match_cbps_vcf_con_Forestry30,
     file = file.path(root, "tmp/land_exact_match/panelmatch_cbps_vcf_con_Forestry30.RData"))


# con_Industry30
match_ps_vcf_con_Industry30 = PanelMatch(
  lag = 4, time.id = "year", unit.id = "cellid",
  treatment = "D", refinement.method = "ps.weight",
  data = df_match,
  covs.formula = ~ I(lag(forest_index, 1:4)) + I(lag(nl_p95, 1:4))  ,
  exact.match.variables = exact_match,
  outcome.var = "con_Industry30",
  size.match = 5, qoi = "att",
  match.missing = FALSE, listwise.delete = TRUE,
  lead = 0:4, forbid.treatment.reversal = F,
  use.diagonal.variance.matrix = T
)
save(match_ps_vcf_con_Industry30,
     file = file.path(root, "tmp/land_exact_match/panelmatch_ps_vcf_con_Industry30.RData"))


match_cbps_vcf_con_Industry30 = PanelMatch(
  lag = 4, time.id = "year", unit.id = "cellid",
  treatment = "D", refinement.method = "CBPS.weight",
  data = df_match,
  covs.formula = ~ I(lag(forest_index, 1:4)) + I(lag(nl_p95, 1:4))  ,
  exact.match.variables = exact_match,
  outcome.var = "con_Industry30",
  size.match = 5, qoi = "att",
  match.missing = FALSE, listwise.delete = TRUE,
  lead = 0:4, forbid.treatment.reversal = F,
  use.diagonal.variance.matrix = T
)
save(match_cbps_vcf_con_Industry30,
     file = file.path(root, "tmp/land_exact_match/panelmatch_cbps_vcf_con_Industry30.RData"))


# con_Infrastructure30
match_ps_vcf_con_Infrastructure30 = PanelMatch(
  lag = 4, time.id = "year", unit.id = "cellid",
  treatment = "D", refinement.method = "ps.weight",
  data = df_match,
  covs.formula = ~ I(lag(forest_index, 1:4)) + I(lag(nl_p95, 1:4))  ,
  exact.match.variables = exact_match,
  outcome.var = "con_Infrastructure30",
  size.match = 5, qoi = "att",
  match.missing = FALSE, listwise.delete = TRUE,
  lead = 0:4, forbid.treatment.reversal = F,
  use.diagonal.variance.matrix = T
)
save(match_ps_vcf_con_Infrastructure30,
     file = file.path(root, "tmp/land_exact_match/panelmatch_ps_vcf_con_Infrastructure30.RData"))

match_cbps_vcf_con_Infrastructure30 = PanelMatch(
  lag = 4, time.id = "year", unit.id = "cellid",
  treatment = "D", refinement.method = "CBPS.weight",
  data = df_match,
  covs.formula = ~ I(lag(forest_index, 1:4)) + I(lag(nl_p95, 1:4))  ,
  exact.match.variables = exact_match, 
  outcome.var = "con_Infrastructure30",
  size.match = 5, qoi = "att",
  match.missing = FALSE, listwise.delete = TRUE,
  lead = 0:4, forbid.treatment.reversal = F,
  use.diagonal.variance.matrix = T
)

save(match_cbps_vcf_con_Infrastructure30,
     file = file.path(root, "tmp/land_exact_match/panelmatch_cbps_vcf_con_Infrastructure30.RData"))


# con_LandUse30
match_ps_vcf_con_LandUse30 = PanelMatch(
  lag = 4, time.id = "year", unit.id = "cellid",
  treatment = "D", refinement.method = "ps.weight",
  data = df_match,
  covs.formula = ~ I(lag(forest_index, 1:4)) + I(lag(nl_p95, 1:4))  ,
  exact.match.variables = exact_match, 
  outcome.var = "con_LandUse30",
  size.match = 5, qoi = "att",
  match.missing = FALSE, listwise.delete = TRUE,
  lead = 0:4, forbid.treatment.reversal = F,
  use.diagonal.variance.matrix = T
)
save(match_ps_vcf_con_LandUse30,
     file = file.path(root, "tmp/land_exact_match/panelmatch_ps_vcf_con_LandUse30.RData"))


match_cbps_vcf_con_LandUse30 = PanelMatch(
  lag = 4, time.id = "year", unit.id = "cellid",
  treatment = "D", refinement.method = "CBPS.weight",
  data = df_match,
  covs.formula = ~ I(lag(forest_index, 1:4)) + I(lag(nl_p95, 1:4))  ,
  exact.match.variables = exact_match, 
  outcome.var = "con_LandUse30",
  size.match = 5, qoi = "att",
  match.missing = FALSE, listwise.delete = TRUE,
  lead = 0:4, forbid.treatment.reversal = F,
  use.diagonal.variance.matrix = T
)

save(match_cbps_vcf_con_LandUse30,
     file = file.path(root, "tmp/land_exact_match/panelmatch_cbps_vcf_con_LandUse30.RData"))


# con_Mining30
match_ps_vcf_con_Mining30 = PanelMatch(
  lag = 4, time.id = "year", unit.id = "cellid",
  treatment = "D", refinement.method = "ps.weight",
  data = df_match,
  covs.formula = ~ I(lag(forest_index, 1:4)) + I(lag(nl_p95, 1:4))  ,
  exact.match.variables = exact_match, 
  outcome.var = "con_Mining30",
  size.match = 5, qoi = "att",
  match.missing = FALSE, listwise.delete = TRUE,
  lead = 0:4, forbid.treatment.reversal = F,
  use.diagonal.variance.matrix = T
)

save(match_ps_vcf_con_Mining30,
     file = file.path(root, "tmp/land_exact_match/panelmatch_ps_vcf_con_Mining30.RData"))


match_cbps_vcf_con_Mining30 = PanelMatch(
  lag = 4, time.id = "year", unit.id = "cellid",
  treatment = "D", refinement.method = "CBPS.weight",
  data = df_match,
  covs.formula = ~ I(lag(forest_index, 1:4)) + I(lag(nl_p95, 1:4))  ,
  exact.match.variables = exact_match, 
  outcome.var = "con_Mining30",
  size.match = 5, qoi = "att",
  match.missing = FALSE, listwise.delete = TRUE,
  lead = 0:4, forbid.treatment.reversal = F,
  use.diagonal.variance.matrix = T
)

save(match_cbps_vcf_con_Mining30,
     file = file.path(root, "tmp/land_exact_match/panelmatch_cbps_vcf_con_Mining30.RData"))



# con_Power30
match_ps_vcf_con_Power30 = PanelMatch(
  lag = 4, time.id = "year", unit.id = "cellid",
  treatment = "D", refinement.method = "ps.weight",
  data = df_match,
  covs.formula = ~ I(lag(forest_index, 1:4)) + I(lag(nl_p95, 1:4))  ,
  exact.match.variables = exact_match,
  outcome.var = "con_Power30",
  size.match = 5, qoi = "att",
  match.missing = FALSE, listwise.delete = TRUE,
  lead = 0:4, forbid.treatment.reversal = F,
  use.diagonal.variance.matrix = T
)

save(match_ps_vcf_con_Power30,
     file = file.path(root, "tmp/land_exact_match/panelmatch_ps_vcf_con_Power30.RData"))


match_cbps_vcf_con_Power30 = PanelMatch(
  lag = 4, time.id = "year", unit.id = "cellid",
  treatment = "D", refinement.method = "CBPS.weight",
  data = df_match,
  covs.formula = ~ I(lag(forest_index, 1:4)) + I(lag(nl_p95, 1:4))  ,
  exact.match.variables = exact_match,
  outcome.var = "con_Power30",
  size.match = 5, qoi = "att",
  match.missing = FALSE, listwise.delete = TRUE,
  lead = 0:4, forbid.treatment.reversal = F,
  use.diagonal.variance.matrix = T
)

save(match_cbps_vcf_con_Power30,
     file = file.path(root, "tmp/land_exact_match/panelmatch_cbps_vcf_con_Power30.RData"))
