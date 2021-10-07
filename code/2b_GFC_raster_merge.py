import glob, os
import numpy as np
import pandas as pd

import matplotlib.pyplot as plt
import seaborn as sns
import geopandas as gpd
from pathlib import Path
from rasterstats import zonal_stats
import rasterio
from rasterio.merge import merge
from rasterio.plot import show
# run for jupyter notebook
from IPython.core.interactiveshell import InteractiveShell
InteractiveShell.ast_node_interactivity = 'all'

# %%
dbox_root = '/home/alal/res/india_pesa_forests/'
root = Path(dbox_root)
rasters = root / 'inp/GFC_Rasters/'
# %%
vil_treat  = gpd.read_parquet(root/"inp/spatial/villages_sch_coded.spq")
# vil_treat = gpd.read_file(root/"inp/spatial/Village_stack_treatment.gpkg")
vil_treat.info(max_cols = 150)

# %% Collapsed="false"
deforestation = rasters / 'Hansen_GFC-2019-v1.7_lossyear_30N_080E.tif'
ex_ante       = rasters / 'Hansen_GFC-2019-v1.7_treecover2000_30N_080E.tif'

with rasterio.open(deforestation) as src:
    print(src.profile)

with rasterio.open(ex_ante) as src:
    print(src.profile)

# %%
##        #######   ######   ######  ##    ## ########    ###    ########
##       ##     ## ##    ## ##    ##  ##  ##  ##         ## ##   ##     ##
##       ##     ## ##       ##         ####   ##        ##   ##  ##     ##
##       ##     ##  ######   ######     ##    ######   ##     ## ########
##       ##     ##       ##       ##    ##    ##       ######### ##   ##
##       ##     ## ##    ## ##    ##    ##    ##       ##     ## ##    ##
########  #######   ######   ######     ##    ######## ##     ## ##     ##

def_master = rasters / '_Hansen_GFC_lossyear_all.tif'
with rasterio.open(def_master) as src:
    print(src.profile)

# %%
%%time
all_def = gpd.GeoDataFrame.from_features(zonal_stats(vil_treat, def_master,
            prefix = 'deforest_', stats='count', nodata=-1,
            categorical=True, geojson_out=True))
all_def.crs = vil_treat.crs
# %%
all_def.head()
# %%
all_def.to_parquet(root/'inp/spatial/village_stack_deforestation.spq')
# %%
######## ##     ##    ###    ##    ## ######## ########
##        ##   ##    ## ##   ###   ##    ##    ##
##         ## ##    ##   ##  ####  ##    ##    ##
######      ###    ##     ## ## ## ##    ##    ######
##         ## ##   ######### ##  ####    ##    ##
##        ##   ##  ##     ## ##   ###    ##    ##
######## ##     ## ##     ## ##    ##    ##    ########
ea_master = rasters / '_Hansen_GFC_treecover2000_all.tif'

with rasterio.open(ea_master) as src:
    print(src.profile)
# %%
exante = gpd.GeoDataFrame.from_features(zonal_stats(vil_treat, ea_master,
            prefix = 'preF_', nodata=-1, geojson_out = True))
exante.crs = vil_treat.crs

# %%
exante.head()
exante.shape

# %%
exante.to_file(root/'inp/spatial/village_stack_ex_ante.gpkg', driver = "GPKG")
# %%
