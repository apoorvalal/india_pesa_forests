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
state_exact_match_v2 <- file.path(tmp, "panel_match_v2/state_exact_match")
out_sg <- file.path( root, "out_sg/panel_match_v2/state_exact_match" ) 

########################################################################
# Import Objects for plot 8
########################################################################

load( file.path( state_exact_match_v2, "figure8_objects.RData" ) )

########################################################################


########################################################################
# Generating Dataframe Before Plot
########################################################################

variables_vec <- c("1Overall", "3Forestry",
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

df_aux$variable <- factor(df_aux$variable, 
                          levels = c("1Overall", "2Mining",
                                    "3Forestry", "4Land Use", "5Industry",
                                    "6Infrastructure",
                                    "7Power" ),
                          labels = c("Overall", "Mining", "Forestry",
                                     "Land Use", "Industry", "Infrastructure",
                                     "Power" ) )

########################################################################
# Generating PLot
# Treatment effects for land conflict variables
########################################################################

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

ggsave(file.path( out_sg, "figure8.png"), f1, device = png, 
       width = 10, height = 6, dpi = 150, units = "in" )
