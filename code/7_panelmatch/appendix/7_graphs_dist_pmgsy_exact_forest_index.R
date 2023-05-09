####################################################
################# Import Libraries #################
####################################################

# Run on Computer
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

####################################################



####################################################
#################### Paths to data #################
####################################################

dbox_root = 'C:/Users/Anzony/Dropbox/india_pesa_forests'
sher_root = "/scratch/gpfs/ar8787/india_pesa_forests"
root = sher_root
tmp = file.path(root, "tmp")
outsg_match <- file.path(root, "out_sg/7_panelmatch/v3_pmgsy_exact_match")
pmgsy_exact_match <- file.path( tmp, "7_panelmatch/v3_pmgsy_exact_match" )

####################################################



########################################################################
############################ Import data ###############################
########################################################################

load( file.path( pmgsy_exact_match, "panelmatch_ps_vcf_forest_index.RData" ) )
load( file.path( pmgsy_exact_match, 
                 "fig5_state_exact_forest_index_obj.RData" ) )
load( file.path( pmgsy_exact_match, 
                 "fig4_state_exact_forest_index_obj.RData" ) )
load( file.path( pmgsy_exact_match, 
                 "fig7_state_exact_forest_index_obj.RData" ) )

########################################################################



#########################################################################
# Figure 3 - Distribution of Match Observations
########################################################################

png(file = file.path( outsg_match , 
                      "distribution_forest_index.png") , 
    width = 4.25, height = 3.25, units = "in", res = 800 )

match_sizes_ps_att <- summary( match_ps_vcf_forest_index$att )$overview$matched.set.size
par( cex = 0.7 )
match_sizes_ps_att_1 <- match_sizes_ps_att[ match_sizes_ps_att > 0 ]
hist( match_sizes_ps_att_1, 
     col= "white", border = "black", freq = TRUE, 
     breaks = seq( min(match_sizes_ps_att_1), 5500, by = 100 ), 
     xlim = c(1, 2000), ylim = c( 0, 3000 ), 
     xlab = "", main = "",  yaxt = 'n', xaxt = "n", 
     ylab = "Frequency", lty = 1 )
lines(x = c(0,0), 
      y = c(0, length(match_sizes_ps_att[match_sizes_ps_att==0])), 
      lwd = 4,
      col = "#ffc6c4")
axis(side = 2, at = seq( 0, 3000, 1000 ), 
     labels = seq( 0, 3000, 1000) )
axis(side = 1, at = seq( 0, 2000, 400 ), 
     labels = seq( 0, 2000, 400) )
dev.off()

#########################################################################


########################################################################
###################### Figure 5 - Balance Plot #########################
########################################################################

# Generating the plot
# Generating the plot
png(file = file.path( outsg_match, "fig5_forest_index.png"), 
    width = 5.25, height = 2.25, units = "in", res = 400)
m <- matrix( c( 1, 2, 3, 4, 5, 5, 5, 5 ), nrow = 2,
             ncol = 4, byrow = TRUE )
layout(mat = m, heights = c( 0.7, 0.3 ) )
par( mar = c( 1.5, 4, 4, 1 ), cex = 0.45 )
# Before Matching
graphics::matplot(balance_nomatch[, -5], type = "p",
                  col = alpha(c(rgb(red = 1, green = 0, blue = 0),
                                rgb(red = 0, green = 0, blue = 0), 
                                rgb(red = 0, green = 0, blue = 0), 
                                rgb(red = 0, green = 0, blue = 0), 
                                rgb(red = 0, green = 0, blue = 0)), 0.6),
                  pch = c(1, 2, 4, 5, 0 ),
                  xaxt = "n",
                  ylab = "Standardized Mean Differences",
                  ylim = c( -0.5, 0.5 ),
                  xlim = c( 0, 5 ),
                  lwd = 1 )
graphics::matplot(balance_nomatch[, -5], type = "l",
                  col = alpha(c(rgb(red = 1, green = 0, blue = 0),
                                rgb(red = 0, green = 0, blue = 0), 
                                rgb(red = 0, green = 0, blue = 0), 
                                rgb(red = 0, green = 0, blue = 0), 
                                rgb(red = 0, green = 0, blue = 0)), 0.4),
                  lty = c(1 ),
                  xaxt = "n",
                  ylab = "Standardized Mean Differences",
                  ylim = c( -0.5, 0.5 ),
                  xlim = c( 0, 5 ),
                  lwd = 1 , add = T )
axis(side = 1, at= c( 1, 2, 3, 4 ),
     labels = c( -4, -3, -2, -1 ) )
rect( -1, -0.2, 6 , 0.2,
      col = rgb( 0.5, 0.5, 0.5, 0.1 ),  
      border = NA )
abline(v=4, col= "#5A5C5D", lty = 2, lwd = 0.5 )
abline(h = 0, col= "black", lty = 2, lwd = 0.5 )
title("Before Matching", line = 0.7, font.main = 1, cex.main = 1.12 )
# No refinement
graphics::matplot(balance_norefinement[, -5], type = "p",
                  col = alpha(c(rgb(red = 1, green = 0, blue = 0),
                                rgb(red = 0, green = 0, blue = 0), 
                                rgb(red = 0, green = 0, blue = 0), 
                                rgb(red = 0, green = 0, blue = 0), 
                                rgb(red = 0, green = 0, blue = 0)), 0.6),
                  pch = c(1, 2, 4, 5, 0 ),
                  xaxt = "n",
                  ylab = "Standardized Mean Differences",
                  ylim = c( -0.5, 0.5 ),
                  xlim = c( 0, 5 ),
                  lwd = 1 )
graphics::matplot(balance_norefinement[, -5], type = "l",
                  col = alpha(c(rgb(red = 1, green = 0, blue = 0),
                                rgb(red = 0, green = 0, blue = 0), 
                                rgb(red = 0, green = 0, blue = 0), 
                                rgb(red = 0, green = 0, blue = 0), 
                                rgb(red = 0, green = 0, blue = 0)), 0.4),
                  lty = c(1 ),
                  xaxt = "n",
                  ylab = "Standardized Mean Differences",
                  ylim = c( -0.5, 0.5 ),
                  xlim = c( 0, 5 ),
                  lwd = 1 , add = T )
axis(side = 1, at= c( 1, 2, 3, 4 ),
     labels = c( -4, -3, -2, -1 ) )
rect( -1, -0.2, 6 , 0.2,
      col = rgb( 0.5, 0.5, 0.5, 0.1 ),  
      border = NA )
abline(v=4, col= "#5A5C5D", lty = 2, lwd = 0.5 )
abline(h = 0, col= "black", lty = 2, lwd = 0.5 )
title("Before Refinement", line = 0.7, font.main = 1, cex.main = 1.12 )
# Pscore Matching
graphics::matplot(balance_ps[, -5], type = "p",
                  col = alpha(c(rgb(red = 1, green = 0, blue = 0),
                                rgb(red = 0, green = 0, blue = 0), 
                                rgb(red = 0, green = 0, blue = 0), 
                                rgb(red = 0, green = 0, blue = 0), 
                                rgb(red = 0, green = 0, blue = 0)), 0.6),
                  pch = c(1, 2, 4, 5, 0 ),
                  xaxt = "n",
                  ylab = "Standardized Mean Differences",
                  ylim = c( -0.5, 0.5 ),
                  xlim = c( 0, 5 ),
                  lwd = 1 )
graphics::matplot(balance_ps[, -5], type = "l",
                  col = alpha(c(rgb(red = 1, green = 0, blue = 0),
                                rgb(red = 0, green = 0, blue = 0), 
                                rgb(red = 0, green = 0, blue = 0), 
                                rgb(red = 0, green = 0, blue = 0), 
                                rgb(red = 0, green = 0, blue = 0)), 0.4),
                  lty = c(1 ),
                  xaxt = "n",
                  ylab = "Standardized Mean Differences",
                  ylim = c( -0.5, 0.5 ),
                  xlim = c( 0, 5 ),
                  lwd = 1 , add = T )
axis(side = 1, at= c( 1, 2, 3, 4 ),
     labels = c( -4, -3, -2, -1 ) )
rect( -1, -0.2, 6 , 0.2,
      col = rgb( 0.5, 0.5, 0.5, 0.1 ),  
      border = NA )
abline(v=4, col= "#5A5C5D", lty = 2, lwd = 0.5 )
abline(h = 0, col= "black", lty = 2, lwd = 0.5 )
title("Propensity Score Matching",
      line = 0.7, font.main = 1,  cex.main = 1.12 )
# CBPS Matching
graphics::matplot(balance_cbps[, -5], type = "p",
                  col = alpha(c(rgb(red = 1, green = 0, blue = 0),
                                rgb(red = 0, green = 0, blue = 0), 
                                rgb(red = 0, green = 0, blue = 0), 
                                rgb(red = 0, green = 0, blue = 0), 
                                rgb(red = 0, green = 0, blue = 0)), 0.6),
                  pch = c(1, 2, 4, 5, 0 ),
                  xaxt = "n",
                  ylab = "Standardized Mean Differences",
                  ylim = c( -0.5, 0.5 ),
                  xlim = c( 0, 5 ),
                  lwd = 1 )
graphics::matplot(balance_cbps[, -5], type = "l",
                  col = alpha(c(rgb(red = 1, green = 0, blue = 0),
                                rgb(red = 0, green = 0, blue = 0), 
                                rgb(red = 0, green = 0, blue = 0), 
                                rgb(red = 0, green = 0, blue = 0)), 0.4),
                  lty = c(1 ),
                  xaxt = "n",
                  ylab = "Standardized Mean Differences",
                  ylim = c( -0.5, 0.5 ),
                  xlim = c( 0, 5 ),
                  lwd = 1 , add = T )
axis(side = 1, at= c( 1, 2, 3, 4 ),
     labels = c( -4, -3, -2, -1 ) )
rect( -1, -0.2, 6 , 0.2,
      col = rgb( 0.5, 0.5, 0.5, 0.1 ),  
      border = NA )
abline(v=4, col= "#5A5C5D", lty = 2, lwd = 0.5 )
abline(h = 0, col= "black", lty = 2, lwd = 0.5 )
title("Covariate Balancing \nPropensity Score", line = 0.7,
      font.main = 1,  cex.main = 1.12 )

# Final Plot to add Legend
plot(1, type = "n", axes=FALSE,
     xlab="", ylab="" )
legend( "top", inset = -0.2,
        legend = c("Forest Index", "Nightlights", 
                   "Road Completed", "Road Sanctioned" ),
        col = c(rgb(red = 1, green = 0, blue = 0),
                rgb(red = 0, green = 0, blue = 0), 
                rgb(red = 0, green = 0, blue = 0), 
                rgb(red = 0, green = 0, blue = 0)),
        pch = c(1, 2, 4, 5, 0 ),
        lty = c(1 ),
        horiz = T,
        bty = "n",
        box.lwd = 0,
        cex = 1,
        box.col = "white",
        bg = "white" )
title(xlab = "Years since PESA",
      outer = TRUE, line = -5.8 )

dev.off()

########################################################################





########################################################################
################################# Figure 4 AJSP ########################
########################################################################

# Generate Plot
png(file = file.path( outsg_match , "fig4_forest_index.png") , 
    width = 3.5, height = 2.25, units = "in", res = 400 )
par( mar = c( 4, 6, 1, 1 ), cex = 0.5 )
graphics::plot(abs(as.numeric(benchmark)),
               abs(as.numeric(compared[,1])),
               pch = 19,
               lwd = 0.01,
               col = alpha( c(rgb(red = 0, green = 0, blue = 0)), 0.2 ) ,
               xlab = "Standardized Mean Difference\n Before Refinement",
               ylab = "Standardized Mean Difference\n After Refinement",
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
                     col = alpha( c(rgb(red = 0, green = 0, blue = 0)), 0.2 ) ,
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

########################################################################




########################################################################
# Balance PLot
########################################################################

### Only "Forest Index"
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
         legend.position = "bottom", 
         text = element_text( size = 16 )
   )
)

ggsave(file.path( outsg_match, "fig7_forest_index.png"), 
       f2, device = png, 
       width = 10, height = 6, dpi = 150, units = "in")


########################################################################