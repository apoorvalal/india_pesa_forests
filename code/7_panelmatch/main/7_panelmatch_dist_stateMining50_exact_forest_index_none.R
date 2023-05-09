
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
root = sher_root
tmp = file.path(root, "tmp")
state_mining50_exact_match <- file.path( tmp, "7_panelmatch/v2_state_mining_exact_match")


####################################################
# Uploading Main data
####################################################

tic()
df_final <- read.csv( file.path(state_mining50_exact_match, "7_panelmatch_data.csv" ) )
toc()

####################################################



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

df_match <- df_final[ , cols_use_match ]
unique(df_final$state)
exact_match_vars <- c("num_state", "con_Mining50_4",   
                      "con_Mining50_3", "con_Mining50_2", 
                      "con_Mining50_1")
names(df_match)
######################################################



################################################################
# Matching forest_index Using noneCORE 4 Years
################################################################
tic()
match_none_vcf_forest_index = PanelMatch(
  lag = 4, time.id = "year", unit.id = "cellid",
  treatment = "D", refinement.method = "none",
  data = df_match,
  covs.formula = ~ I(lag(forest_index, 1:4)) 
                  + I(lag(nl_p95, 1:4))  ,
  outcome.var = "forest_index",
  size.match = 5, qoi = "att",
  match.missing = FALSE, listwise.delete = TRUE,
  lead = 0:4, forbid.treatment.reversal = F,
  verbose = F,
  use.diagonal.variance.matrix = T,
  exact.match.variables = NULL,
  matching = FALSE
)
toc()

save(match_none_vcf_forest_index,
     file = file.path(state_mining50_exact_match, 
                      "panelmatch_none_vcf_forest_index.RData"))


################################################################
