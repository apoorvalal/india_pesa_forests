#############################
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
load(file.path(tmp, "factormod1.rds"))

# %%
