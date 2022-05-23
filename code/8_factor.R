# %% ####################################################
rm(list=ls())
set.seed(42)
library(LalRUtils)
libreq(data.table, tictoc,
       fixest, PanelMatch, patchwork, gsynth, fect,
       RPushbullet)
notif = \(x) pbPost(type = "note", body=x)

# %% ####################################################
dbox_root = '/home/alal/res/india_pesa_forests'
sher_root = "/home/groups/gulzar/india_pesa_forests"
root = sher_root
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
# minimal version
mindata = vcf_data[, .(forest_index, D, cellid, year, styear)]

# %%
#### ######## ########
 ##  ##       ##
 ##  ##       ##
 ##  ######   ######
 ##  ##       ##
 ##  ##       ##
#### ##       ########
nc = 24

# %% run on cluster - runtime ~2 hrs w 24 cores
ife1 = F
if(ife1){
    outife = fect(forest_index ~ D, data = mindata,
          index=c("cellid", "year"),
          force = "two-way", method = "ife",
          # pick number of factors by CV
          CV = TRUE, r = c(0, 4),
          # bootstrap
          se = TRUE, nboots = 200, parallel = TRUE, cores = nc)
    notif("IFE 1 done")
    save(outife, file = file.path(tmp, "factormod1.rds"))
}

# %%
