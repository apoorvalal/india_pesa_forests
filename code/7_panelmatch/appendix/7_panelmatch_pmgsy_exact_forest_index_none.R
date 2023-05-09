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


######
# Setting working directory
######

# %% ####################################################
dbox_root = 'C:/Users/Anzony/Dropbox/india_pesa_forests'
sher_root = "/scratch/gpfs/ar8787/india_pesa_forests"
root = sher_root
tmp = file.path(root, "tmp")
pmgsy_exact_match <- file.path( tmp, "7_panelmatch_v3/pmgsy_exact_match")



tic()
df_final <- read.csv( file.path(tmp, "7_panelmatch_v3/7_panelmatch_data.csv"))
toc()
names(df_final)
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
                    "con_Mining50", "roadsanc_1", "roadcomp_1", 
                    "con_Mining50_1", "con_Mining50_2", "con_Mining50_3", 
                    "con_Mining50_4", 
                    "scst91_high_dummy", 
                    "ns_pc91_vd_tar_road_dummy", 
                    "ns_pc91_vd_power_all_dummy", 
                    "high_lit_91_dummy" )

df_match <- df_final[ , cols_use_match ]
unique(df_final$state)
exact_match_vars <- c("num_state", "con_Mining50_1", 
                      "con_Mining50_2", "con_Mining50_3", 
                      "con_Mining50_4", 
                      "scst91_high_dummy", 
                      "ns_pc91_vd_tar_road_dummy", 
                      "ns_pc91_vd_power_all_dummy", 
                      "high_lit_91_dummy" )
names(df_match)
######################################################



# 
# df_match %>% 
#   select( scst91_high_dummy, 
#           ns_pc91_vd_tar_road_dummy, 
#           ns_pc91_vd_power_all_dummy, 
#           high_lit_91_dummy, sch ) %>% 
#   pivot_longer( !c(sch), 
#                 names_to = "variables",
#                 values_to = "value" ) %>% 
#     group_by( sch, variables, value ) %>% 
#     count()

# ( forest index, nightlights, road sanctioned, and road completed )
# exact match dummy
# high_lit_91 ns_pc91_vd_tar_road ns_pc91_vd_power_all scst91_high

################################################################
# Matching forest_index Using noneCORE 4 Years
################################################################

match_none_vcf_forest_index = PanelMatch(
  lag = 4, time.id = "year", unit.id = "cellid",
  treatment = "D", refinement.method = "none",
  data = df_match,
  covs.formula = ~ I(lag(forest_index, 1:4)) 
  + I(lag(nl_p95, 1:4)) + I(lag(roadcomp_1, 1:4)) ,
  outcome.var = "forest_index",
  size.match = 5, qoi = "att",
  match.missing = FALSE, listwise.delete = TRUE,
  lead = 0:4, forbid.treatment.reversal = F,
  verbose = F,
  use.diagonal.variance.matrix = T,
  exact.match.variables = NULL,
  matching = FALSE
)

save(match_none_vcf_forest_index,
     file = file.path(pmgsy_exact_match, "panelmatch_none_vcf_forest_index.RData"))