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
code = file.path(root, "code")


# First, get matched sets that will be used for the paper 
source( file.path( code, "7_panelmatch/7_panelmatch_dist_state_exact_land_yvars.R" ) )
source( file.path( code, "7_panelmatch/7_panelmatch_graphs_state_exact.R" ) )

source( file.path( code, "7_panelmatch/7_panelmatch_dist_land_exact_land_yvars.R" ) )
source( file.path( code, "7_panelmatch/7_panelmatch_graphs_land_exact.R" ) )







