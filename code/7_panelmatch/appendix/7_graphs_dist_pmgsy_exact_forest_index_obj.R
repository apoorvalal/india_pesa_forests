########################################################################
########################### Libraries #################################
########################################################################

# Run on cluster
# takes around 3 hours on slurm job with specs

# SBATCH --begin=now
# SBATCH --time=12:00:00
# SBATCH --partition=hns
# SBATCH --mem=40G
# SBATCH --ntasks-per-node=12
# SBATCH --mail-type=ALL

# rm(list = ls())
# library(devtools)
# devtools::install_github("apoorvalal/LalRUtils")

#Packages that we need
# install.packages("tidyverse",  repos = "https://cloud.r-project.org" )
# install.packages("data.table",  repos = "https://cloud.r-project.org" )
# install.packages("zoo",  repos = "https://cloud.r-project.org" )
# install.packages("tictoc",  repos = "https://cloud.r-project.org" )
# install.packages("fst",  repos = "https://cloud.r-project.org" )
# install.packages("fixest",  repos = "https://cloud.r-project.org" )
# install.packages("patchwork",  repos = "https://cloud.r-project.org" )
# install.packages("rio",  repos = "https://cloud.r-project.org" )
# install.packages("magrittr",  repos = "https://cloud.r-project.org" )
# install.packages("janitor",  repos = "https://cloud.r-project.org" )
# install.packages("did",  repos = "https://cloud.r-project.org" )
# install.packages("panelView",  repos = "https://cloud.r-project.org" )
# install.packages("vtable",  repos = "https://cloud.r-project.org" )
# install.packages("RPushbullet",  repos = "https://cloud.r-project.org" )
# install.packages("magrittr",  repos = "https://cloud.r-project.org" )
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
# library(LalRUtils)
# # sessionInfo()
# libreq(
#   tidyverse, data.table, zoo, tictoc, fst, fixest, PanelMatch, patchwork,
#   rio, magrittr, janitor, did, panelView, vtable, RPushbullet
# )
# set.seed(42)
# theme_set(lal_plot_theme())

########################################################################



########################################################################
########################### Main Paths #################################
########################################################################

dbox_root = 'C:/Users/Anzony/Dropbox/india_pesa_forests'
sher_root = "/scratch/gpfs/ar8787/india_pesa_forests"
root = sher_root
tmp = file.path(root, "tmp")
pmgsy_exact_match <- file.path( tmp, "7_panelmatch/v3_pmgsy_exact_match")
df_final <- read.csv( file.path(pmgsy_exact_match, "7_panelmatch_data.csv" ) )

######################################################



######################################################
############## Preparing data to panelmatch ##########
######################################################

names(df_final)
cols_use_match <- c("cellid", "year","nl_p95",
                    "D", "num_state", "forest_index", 
                    "roadsanc_1", "roadcomp_1", 
                    "con_Mining50",  "con_Mining50_4",   
                    "con_Mining50_3", "con_Mining50_2", 
                    "con_Mining50_1", 
                    "scst91_high_dummy", 
                    "ns_pc91_vd_tar_road_dummy", 
                    "ns_pc91_vd_power_all_dummy", 
                    "high_lit_91_dummy", 
                    "scst91_high_1", 
                    "ns_pc91_vd_tar_road_1", 
                    "ns_pc91_vd_power_all_1", 
                    "high_lit_91_1", "tdist_100_avg", 
                    "forest_index_1", "forest_index_2", 
                    "forest_index_3", "forest_index_4", 
                    "nl_p95_1", "nl_p95_2", "nl_p95_3", 
                    "nl_p95_4", "roadcomp_1_1", "roadcomp_1_2", 
                    "roadcomp_1_3", "roadcomp_1_4", 
                    "roadsanc_1_1", "roadsanc_1_2", 
                    "roadsanc_1_3", "roadsanc_1_4")

df_match <- df_final[  , cols_use_match ]
unique(df_final$state)
exact_match_vars <- c("num_state")
names(df_match)

######################################################

########################################################################



########################################################################
##########################   Import data ###############################
########################################################################

load( file.path( pmgsy_exact_match, "panelmatch_cbps_vcf_forest_index.RData" ) )
load( file.path( pmgsy_exact_match, "panelmatch_ps_vcf_forest_index.RData" ) )
load( file.path( pmgsy_exact_match, "panelmatch_none_vcf_forest_index.RData" ) )

########################################################################




########################################################################
#################### Balance Plot ######################################
########################################################################

# Figure 5
cov_vars <- c( "forest_index", "nl_p95", "roadcomp_1", "roadsanc_1", "con_Mining50" )
balance_nomatch <- get_covariate_balance(match_none_vcf_forest_index$att,
                                         data = df_match,
                                         covariates = cov_vars,
                                         plot = F,
                                         use.equal.weights = TRUE)
balance_norefinement <- get_covariate_balance(matched.sets = match_ps_vcf_forest_index$att,
                                              data = df_match,
                                              covariates = cov_vars,
                                              use.equal.weights = TRUE)
balance_ps <- get_covariate_balance(match_ps_vcf_forest_index$att,
                                    data = df_match,
                                    covariates = cov_vars,
                                    plot = F)
balance_cbps <- get_covariate_balance(match_cbps_vcf_forest_index$att,
                                      data = df_match,
                                      covariates = cov_vars,
                                      plot = F )

# Save objects for plot
save_obj_path <- file.path( pmgsy_exact_match, 
                            "fig5_state_exact_forest_index_obj.RData" )
save( balance_nomatch, 
      balance_norefinement, 
      balance_ps, 
      balance_cbps, 
      file = save_obj_path )

########################################################################





#######################################################
##################### Figure 4 AJSP ###################
#######################################################
cov_vars <- c( "forest_index_1", "forest_index_2", 
               "forest_index_3", "forest_index_4", 
               "nl_p95_1", "nl_p95_2", "nl_p95_3", 
               "nl_p95_4", "roadcomp_1_1", "roadcomp_1_2", 
               "roadcomp_1_3", "roadcomp_1_4", 
               "roadsanc_1_1", "roadsanc_1_2", 
               "roadsanc_1_3", "roadsanc_1_4", 
               "con_Mining50_4", "con_Mining50_3", 
               "con_Mining50_2", "con_Mining50_1",
               "scst91_high_dummy", 
               "ns_pc91_vd_tar_road_dummy", 
               "ns_pc91_vd_power_all_dummy", 
               "high_lit_91_dummy", "tdist_100_avg" )
matched_set_list <- list( match_ps_vcf_forest_index$att,
                          match_cbps_vcf_forest_index$att )
refined_balance <- list()
for (i in 1:length(matched_set_list)) {
  refined_balance[[i]] <- get_covariate_balance(matched.sets = matched_set_list[[i]],
                                                data = df_match,
                                                covariates = cov_vars )
}
non_refined_balance <- get_covariate_balance(matched.sets = matched_set_list[[1]],
                                             data = df_match,
                                             covariates = cov_vars,
                                             use.equal.weights = TRUE)
benchmark <- as.vector(non_refined_balance)
compared <- sapply( refined_balance, function(x) x <- x[1:(nrow(x)), ] )


# Save objects for plot
save_obj_path <- file.path( pmgsy_exact_match, "fig4_state_exact_forest_index_obj.RData" )
save( refined_balance, 
      non_refined_balance, 
      benchmark, 
      compared, 
      file = save_obj_path )

########################################################################



########################################################################
########################## Balance PLot ################################
########################################################################

ps_results_vcf = PanelEstimate(sets = match_ps_vcf_forest_index,
                               data = df_match,
                               number.iterations = 1000)
cbps_results_vcf = PanelEstimate(sets = match_cbps_vcf_forest_index,
                                 data = df_match,
                                 number.iterations = 1000 )
# Get Variables
matchrestable = \(x, v) data.frame(v, with(x, cbind(lead, estimates, standard.error)))
variables_vec <- c("0forest_index" )
results_list <- list( list( ps_results_vcf,
                            cbps_results_vcf ) )

length(results_list)
datalist = list()
for (i in 1:length(variables_vec)) {
  print(i)
  matchEst = rbind(
    matchrestable( results_list[[i]][[1]], "1Pscore" ),
    matchrestable( results_list[[i]][[2]], "2CBPS" )
  )
  matchEst$variable <- variables_vec[i]
  datalist[[i]] <- matchEst
}
df_aux1 <- bind_rows(datalist, .id = "column_label")  %>% filter( lead > -1 )

df_aux1$variable <- factor(df_aux1$variable, levels = c("0forest_index" ),
                           labels = c( "Forest Index" ) )


# Save objects for plot
save_obj_path <- file.path( pmgsy_exact_match, "fig7_state_exact_forest_index_obj.RData" )
save( ps_results_vcf, 
      cbps_results_vcf, 
      df_aux1,
      file = save_obj_path )

########################################################################

