# Run on cluster
# takes around 9 hours on slurm job with specs

# SBATCH --begin=now
# SBATCH --time=12:00:00
# SBATCH --partition=hns
# SBATCH --mem=40G
# SBATCH --ntasks-per-node=12
# SBATCH --mail-type=ALL

# %% ####################################################
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
# rm(list = ls())
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
'~/Dropbox/india_pesa_forests'
# %% ####################################################
dbox_root = '~/Dropbox/india_pesa_forests'
sher_root = "/home/ar8787/forests_analysis"
root = dbox_root
tmp = file.path(root, "tmp")
setwd( root )

# Import data
########################################################################
load( file.path( tmp, "land_exact_match/panelmatch_nomatch_vcf_land_vars.RData" ) )
load( file.path( tmp, "land_exact_match/panelmatch_cbps_vcf_land_vars.RData" ) )
load( file.path( tmp, "land_exact_match/panelmatch_ps_vcf_land_vars.RData" ) )
load( file.path( tmp, "land_exact_match/panelmatch_none_vcf_land_vars.RData" ) )

load( file.path( tmp, "land_exact_match/panelmatch_ps_vcf_con_Overall30.RData" ) )
load( file.path( tmp, "land_exact_match/panelmatch_cbps_vcf_con_Overall30.RData" ) )
load( file.path( tmp, "land_exact_match/panelmatch_ps_vcf_con_Forestry30.RData" ) )
load( file.path( tmp, "land_exact_match/panelmatch_cbps_vcf_con_Forestry30.RData" ) )
load( file.path( tmp, "land_exact_match/panelmatch_ps_vcf_con_Industry30.RData" ) )
load( file.path( tmp, "land_exact_match/panelmatch_cbps_vcf_con_Industry30.RData" ) )
load( file.path( tmp, "land_exact_match/panelmatch_ps_vcf_con_Infrastructure30.RData" ) )
load( file.path( tmp, "land_exact_match/panelmatch_cbps_vcf_con_Infrastructure30.RData" ) )
load( file.path( tmp, "land_exact_match/panelmatch_ps_vcf_con_LandUse30.RData" ) )
load( file.path( tmp, "land_exact_match/panelmatch_cbps_vcf_con_LandUse30.RData" ) )
load( file.path( tmp, "land_exact_match/panelmatch_ps_vcf_con_Mining30.RData" ) )
load( file.path( tmp, "land_exact_match/panelmatch_cbps_vcf_con_Mining30.RData" ) )
load( file.path( tmp, "land_exact_match/panelmatch_ps_vcf_con_Power30.RData" ) )
load( file.path( tmp, "land_exact_match/panelmatch_cbps_vcf_con_Power30.RData" ) )

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

# To avoid warning message keep only numeric columns
cols_use_match <- c("cellid", "year","con_Overall30_4",
                    "con_Mining30_4", "con_Overall30", "con_Mining30", "con_Forestry30",
                    "con_Forestry30_4", "con_Overall30_3",
                    "con_Mining30_3",  "con_Forestry30_3",
                    "con_Overall30_2","con_Mining30_2"  ,
                    "con_Forestry30_2", "con_Overall30_1",
                    "con_Mining30_1" , "con_Forestry30_1", "stf",
                    "D", "num_state", "forest_index", "con_Industry30",
                    "con_Infrastructure30", "con_LandUse30", "con_Power30",
                    "nl_p95")

df_match <- df_final[ , cols_use_match]

########################################################################
# Changing Object Names
match_ps_vcf_land_vars <- match_ps_vcf4
match_cbps_vcf_land_vars <- match_cbps_vcf4



########################################################################




# #########################################################################
# # Distribution of Match Observations
# ########################################################################
# ## Figure 3
png(file = file.path("out_sg/distribution_land_exact.png") ,width=4.25,height=3.25,units="in",res=800 )
plot( match_ps_vcf4$att, col= "white", border = "black"  , main="")
dev.off()
# #########################################################################
# 
# 
# ########################################################################
# # Balance Plot
# ########################################################################
cov_vars <- c( "forest_index", "nl_p95")
balance_nomatch <- get_covariate_balance(match_nomatch_vcf_land_vars$att,
                                         data = df_match,
                                         covariates = cov_vars,
                                         plot = F,
                                         use.equal.weights = TRUE)
balance_norefinement <- get_covariate_balance(matched.sets = match_ps_vcf_land_vars$att,
                                              data = df_match,
                                              covariates = cov_vars,
                                              use.equal.weights = TRUE)
balance_ps <- get_covariate_balance(match_ps_vcf_land_vars$att,
                                    data = df_match,
                                    covariates = cov_vars,
                                    plot = F)
balance_cbps <- get_covariate_balance(match_cbps_vcf_land_vars$att,
                                      data = df_match,
                                      covariates = cov_vars,
                                      plot = F )

# Figure 5
# dev.off()
png(file = file.path("out_sg/fig5_land_exact.png"), width=5.25,height=2.25,units="in",res=400)

m <- matrix( c( 1, 2, 3, 4, 5, 5, 5, 5 ), nrow = 2,
             ncol = 4, byrow = TRUE )
layout(mat = m, heights = c( 0.7, 0.3 ) )
par( mar = c( 1.5, 4, 4, 1 ), cex = 0.45 )
# Before Matching
graphics::matplot(balance_nomatch, type = "l",
                  col = c(rgb(red = 1, green = 0, blue = 0),
                          rgb(red = 0, green = 0, blue = 0)),
                  pch = c(1, 2),
                  lty = c(1,2),
                  xaxt = "n",
                  ylab = "Standardized Mean Differences",
                  ylim = c( -0.4, 0.4 ),
                  xlim = c( 0, 5 ),
                  lwd = 1 )
axis(side = 1, at= c( 1, 2, 3, 4 ),
     labels = c( -4, -3, -2, -1 ) )
rect( -1, -0.2, 6 , 0.2,
      col = rgb( 0.5, 0.5, 0.5, 0.1 ),  border = F )
abline(v=4, col= "#5A5C5D", lty = 2, lwd = 0.5 )
abline(h = 0, col= "black", lty = 2, lwd = 0.5 )
title("Before Matching", line = 0.7, font.main = 1, cex.main = 1.12 )
# No refinement
graphics::matplot(balance_norefinement, type = "l",
                  col = c(rgb(red = 1, green = 0, blue = 0),
                          rgb(red = 0, green = 0, blue = 0)),
                  pch = c(1, 2),
                  lty = c(1,2),
                  ylab = "",
                  xaxt = "n",
                  ylim = c( -0.4, 0.4 ),
                  xlim = c( 0, 5 ),
                  lwd = 1 )
axis(side = 1, at= c( 1, 2, 3, 4 ),
     labels = c( -4, -3, -2, -1 ) )
rect( -1, -0.2, 6 , 0.2,
      col = rgb( 0.5, 0.5, 0.5, 0.1 ),  border = F )
abline(v=4, col= "#5A5C5D", lty = 2, lwd = 0.5 )
abline(h = 0, col= "black", lty = 2, lwd = 0.5 )
title("Before Refinement", line = 0.7, font.main = 1, cex.main = 1.12 )
# Pscore Matching
graphics::matplot(balance_ps, type = "l",
                  col = c(rgb(red = 1, green = 0, blue = 0),
                          rgb(red = 0, green = 0, blue = 0)),
                  lty = c(1,2),
                  xaxt = "n",
                  ylab = "",
                  ylim = c( -0.4, 0.4 ),
                  xlim = c( 0, 5 ),
                  lwd = 1 )
axis(side = 1, at= c( 1, 2, 3, 4 ),
     labels = c( -4, -3, -2, -1 ) )
rect( -1, -0.2, 6 , 0.2,
      col = rgb( 0.5, 0.5, 0.5, 0.1 ),  border = F )
abline(v=4, col= "#5A5C5D", lty = 2, lwd = 0.5 )
abline(h = 0, col= "black", lty = 2, lwd = 0.5 )
title("Propensity Score Matching",
      line = 0.7, font.main = 1,  cex.main = 1.12 )
# CBPS Matching
graphics::matplot(balance_cbps, type = "l",
                  col = c(rgb(red = 1, green = 0, blue = 0),
                          rgb(red = 0, green = 0, blue = 0)),
                  xaxt = "n",
                  ylab = "",
                  ylim = c( -0.4, 0.4 ),
                  xlim = c( 0, 5 ),
                  lwd = 1 )
axis(side = 1, at= c( 1, 2, 3, 4 ),
     labels = c( -4, -3, -2, -1 ) )
rect( -1, -0.2, 6 , 0.2,
      col = rgb( 0.5, 0.5, 0.5, 0.1 ),  border = F )
abline(v=4, col= "#5A5C5D", lty = 2, lwd = 0.5 )
abline(h = 0, col= "black", lty = 2, lwd = 0.5 )
title("Covariate Balancing \nPropensity Score", line = 0.7,
      font.main = 1,  cex.main = 1.12 )

# Final Plot to add Legend
plot(1, type = "n", axes=FALSE,
     xlab="", ylab="" )
legend( "top", inset = -0.2,
        legend = c("Forest Index", "Nightlights"),
        col = c(1:2, "black"),
        lty = c(2,1),
        horiz = T,
        lwd = 1,
        bty = "n",
        box.lwd = 0,
        cex = 1,
        box.col = "white",
        bg = "white" )
title(xlab = "Years since PESA",
      outer = TRUE, line = -5.5 )

dev.off()
# ########################################################################
# 
# 
# ########################################################################
# ## Figure 4 AJPS
matched_set_list <- list( match_ps_vcf4$att,
                          match_cbps_vcf4$att )
refined_balance <- list()
for (i in 1:length(matched_set_list)) {
  refined_balance[[i]] <- get_covariate_balance(matched.sets = matched_set_list[[i]],
                                                data = df_match,
                                                covariates = c( "forest_index", "nl_p95" ))
}
non_refined_balance <- get_covariate_balance(matched.sets = matched_set_list[[1]],
                                             data = df_match,
                                             covariates = c( "forest_index", "nl_p95" ),
                                             use.equal.weights = TRUE)
benchmark <- as.vector(non_refined_balance)
compared <- sapply(refined_balance, function(x) x <- x[1:(nrow(x)), ])

# dev.off()
png(file = file.path("out_sg/fig4_land_exact.png") ,width=3.5,height=2.25,units="in",res=400)
par( mar = c( 4, 6, 1, 1 ), cex = 0.5 )
graphics::plot(abs(as.numeric(benchmark)),
               abs(as.numeric(compared[,1])),
               pch = 19,
               lwd = 0.01,
               xlab = "Before Refinement \nStandardized Mean Difference of Covariates",
               ylab = "After Refinement \nStandardized Mean Difference of Covariates",
               main = "",
               cex = 1,
               xlim = c(0, 0.8),
               ylim = c(0, 0.8) )
pchs <- c( 1, 4 )
if (length(refined_balance) > 1) {
  for (j in 2:length(refined_balance)) {
    print(j )
    graphics::points(abs(as.numeric(benchmark)),
                     abs(as.numeric(compared[,j])),
                     pch = pchs[j], lwd = 1.2,
                     cex = 1.5 )
  }
}
abline(h = 0, lty = "dashed")
abline(0, 1, lty = 2, col = "red")

# add legend
legend(x = 0, y = 0.8,
       legend = c("Propensity Score",
                  "Covariate Balancing Propensity Score"),
       y.intersp = 1,
       x.intersp = 1, xjust = 0,
       pch = c(19, 4), pt.cex = 1.2,
       bty = "n", ncol = 1,
       cex = 1, bg = "white")
dev.off()
# 
# 
# ########################################################################
# # %% event study fig
# ########################################################################
ps_results_vcf = PanelEstimate(sets = match_ps_vcf_land_vars,
                               data = df_match,
                               number.iterations = 1000)
cbps_results_vcf = PanelEstimate(sets = match_cbps_vcf_land_vars,
                                 data = df_match,
                                 number.iterations = 1000 )
# Get Variables
matchrestable = \(x, v) data.frame(v, with(x, cbind(lead, estimates, standard.error)))
variables_vec <- c("0Forest Index" )
results_list <- list( list( ps_results_vcf,
                            cbps_results_vcf ) )

length(results_list)
datalist = list()
for (i in 1:length(variables_vec)) {
  print(i)
  matchEst = rbind(
    matchrestable(results_list[[i]][[1]], "1Pscore"),
    matchrestable(results_list[[i]][[2]], "2CBPS")
  )
  matchEst$variable <- variables_vec[i]
  datalist[[i]] <- matchEst
}
df_aux1 <- bind_rows(datalist, .id = "column_label")  %>% filter( lead > -1 )

df_aux1$variable <- factor(df_aux1$variable, levels = c("0Forest Index" ),
                           labels = c("Forest Index" ) )


# ### Only "Forest Index"
(f2 = ggplot(df_aux1 %>% filter( variable == "Forest Index" ) ,
             aes(x= lead,
                 y = estimates,
                 colour = v,
                 shape = v ) ) +
    geom_pointrange(position = position_dodge2(width = .2),
                    aes(ymin = estimates - 1.96 * `standard.error`,
                        ymax = estimates + 1.96 * `standard.error`),
                    alpha = 1) +
    scale_color_manual( values = c("red", "blue"),
                        labels = c( "Propensity Score", "Covariate Balancing Propensity Score" ) ) +
    scale_shape_manual( values=c( 1, 4 ),
                        guide = "none" ) +
    guides( color = guide_legend( override.aes = list( shape = c(1, 4 )))) +
    geom_hline( yintercept = 0, linetype = 'dotted' ) +
    scale_color_manual( values = c("red", "blue"),
                        labels = c( "Propensity Score", "Covariate Balancing Propensity Score" ) ) +
    labs(y = "Treatment Effect on Forest Index",
         x = "Years since PESA",
         colour = "") +
    theme(panel.grid.major = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.position = "bottom"
          )
)

ggsave(file.path("out_sg/fig7_land_exact.png"), f2, device = png)

########################################################################
# Balance plot
########################################################################

# Generating variables

# con_Overall30 - Outcome
ps_results_vcf_con_Overall30 = PanelEstimate(sets = match_ps_vcf_con_Overall30,
                                             data = df_match,
                                             number.iterations = 1000)
cbps_results_vcf_con_Overall30 = PanelEstimate(sets = match_cbps_vcf_con_Overall30,
                                               data = df_match,
                                               number.iterations = 1000)

# con_Forestry30 - Outcome
ps_results_vcf_con_Forestry30 = PanelEstimate(sets = match_ps_vcf_con_Forestry30,
                                              data = df_match,
                                              number.iterations = 1000)
cbps_results_vcf_con_Forestry30 = PanelEstimate(sets = match_cbps_vcf_con_Forestry30,
                                                data = df_match,
                                                number.iterations = 1000)

# con_Industry30 - Outcome
ps_results_vcf_con_Industry30 = PanelEstimate(sets = match_ps_vcf_con_Industry30,
                                              data = df_match,
                                              number.iterations = 1000)
cbps_results_vcf_con_Industry30 = PanelEstimate(sets = match_cbps_vcf_con_Industry30,
                                                data = df_match,
                                                number.iterations = 1000)

# con_Infrastructure30 - Outcome
ps_results_vcf_con_Infrastructure30 = PanelEstimate(sets = match_ps_vcf_con_Infrastructure30,
                                                    data = df_match,
                                                    number.iterations = 1000)
cbps_results_vcf_con_Infrastructure30 = PanelEstimate(sets = match_cbps_vcf_con_Infrastructure30,
                                                      data = df_match,
                                                      number.iterations = 1000)

# con_LandUse30 - Outcome
ps_results_vcf_con_LandUse30 = PanelEstimate(sets = match_ps_vcf_con_LandUse30,
                                             data = df_match,
                                             number.iterations = 1000)
cbps_results_vcf_con_LandUse30 = PanelEstimate(sets = match_cbps_vcf_con_LandUse30,
                                               data = df_match,
                                               number.iterations = 1000)

# con_Mining30 - Outcome
ps_results_vcf_con_Mining30 = PanelEstimate(sets = match_ps_vcf_con_Mining30,
                                            data = df_match,
                                            number.iterations = 1000)
cbps_results_vcf_con_Mining30 = PanelEstimate(sets = match_cbps_vcf_con_Mining30,
                                              data = df_match,
                                              number.iterations = 1000)

# con_Power30 - Outcome
ps_results_vcf_con_Power30 = PanelEstimate(sets = match_ps_vcf_con_Power30,
                                           data = df_match,
                                           number.iterations = 1000)
cbps_results_vcf_con_Power30 = PanelEstimate(sets = match_cbps_vcf_con_Power30,
                                             data = df_match,
                                             number.iterations = 1000)
########################################################################

# Get Variables
variables_vec <- c( "1Overall", "3Forestry",
                   "5Industry", "6Infrastructure", "4Land Use",
                   "2Mining", "7Power" )


results_list <- list(list(ps_results_vcf_con_Overall30, cbps_results_vcf_con_Overall30),
                     list(ps_results_vcf_con_Forestry30, cbps_results_vcf_con_Forestry30),
                     list(ps_results_vcf_con_Industry30, cbps_results_vcf_con_Industry30),
                     list(ps_results_vcf_con_Infrastructure30, cbps_results_vcf_con_Infrastructure30),
                     list(ps_results_vcf_con_LandUse30, cbps_results_vcf_con_LandUse30),
                     list(ps_results_vcf_con_Mining30, cbps_results_vcf_con_Mining30),
                     list(ps_results_vcf_con_Power30, cbps_results_vcf_con_Power30))

matchrestable = \(x, v) data.frame(v, with(x, cbind(lead, estimates, standard.error)))
length(results_list)
datalist = list()
for (i in 1:length(variables_vec)) {
  print(i)
  matchEst = rbind(
    matchrestable(results_list[[i]][[1]], "1Pscore"),
    matchrestable(results_list[[i]][[2]], "2CBPS")
  )
  matchEst$variable <- variables_vec[i]
  datalist[[i]] <- matchEst
}
df_aux <- bind_rows(datalist, .id = "column_label")

df_aux$variable <- factor(df_aux$variable, levels = c("1Overall", "2Mining",
                                                      "3Forestry", "4Land Use", "5Industry",
                                                      "6Infrastructure",
                                                      "7Power" ),
                          labels = c( "Overall", "Mining", "Forestry",
                                     "Land Use", "Industry", "Infrastructure",
                                     "Power" ) )


### All the variables but "Forest Index"
(f1 = ggplot(df_aux ,
             aes(x= lead,
                 y = estimates,
                 colour = v,
                 shape = v ) ) +
    geom_pointrange(position = position_dodge2(width = .2),
                    aes(ymin = estimates - 1.96 * `standard.error`,
                        ymax = estimates + 1.96 * `standard.error`),
                    alpha = 0.6) +
    scale_color_manual( values = c("red", "blue"),
                        labels = c( "Propensity Score", "Covariate Balancing Propensity Score" ) ) +
    scale_shape_manual( values=c( 1, 4 ),
                        guide = "none" ) +
    guides( color = guide_legend( override.aes = list( shape = c(1, 4 )))) +
    facet_wrap( ~ variable, ncol = 4, scales='free') +
    geom_hline( yintercept = 0, linetype = 'dotted' ) +
    labs(y = "Treatment Effect",
         x = "Years since PESA",
         colour = "") +
    theme(panel.grid.major = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"), 
          legend.position = "bottom"
          )
)


ggsave(file.path("out_sg/fig8_land_exact.png"), f1, device = png)
