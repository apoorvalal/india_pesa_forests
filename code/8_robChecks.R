# %% ####################################################
rm(list=ls())
library(LalRUtils)

libreq(tidyverse, data.table, zoo, tictoc, fst, fixest,
	PanelMatch, patchwork, augsynth,
	rio, magrittr, janitor, did, panelView, RPushbullet)
set.seed(42)
theme_set(lal_plot_theme())
notif = \(x) pbPost("note", x)
# %% ####################################################
dbox_root = '/home/alal/res/india_pesa_forests'
sher_root = "/home/users/apoorval/aa_scratch/forests_analysis"
root = dbox_root
tmp  = file.path(root, "tmp")
# %%
tic()
load(file.path(tmp, "regdata.rds"))
toc()

# %% VCF prep
vcf_data = vcf_data[year>=1995]
vcf_data[, t := year - 1995]
ex_ante_med = quantile(vcf_data$cover_1990, 0.5)
vcf_data = vcf_data[cover_1990 > ex_ante_med]
vcf_data[, never_treated := max(D) == 0, cellid]

# %%
tic()
ppool_syn = multisynth(forest_index ~ D,
	cellid, year, vcf_data)

toc()

# %%
