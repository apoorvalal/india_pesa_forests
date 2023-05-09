# %% library loads
import glob
from pathlib import Path
import json
import joblib
from joblib import Parallel, delayed
from numpy.core.fromnumeric import compress
# pyscience imports
import pandas as pd
import matplotlib

import matplotlib.pyplot as plt
font = {'family' : 'IBM Plex Sans Condensed',
                'weight' : 'normal',
                'size'   : 10}
plt.rc('font', **font)
plt.rcParams['figure.figsize'] = (10, 10)
matplotlib.style.use(['seaborn-talk', 'seaborn-ticks', 'seaborn-whitegrid'])
%matplotlib inline
%config InlineBackend.figure_format = 'retina'

# geodata packages
# raster packages
from fiona.crs import from_epsg
from rasterio.mask import mask
from rasterstats import zonal_stats
from shapely.geometry import box
import geopandas as gpd
import georasters as gr
import rasterio as rio

# show all output
from IPython.core.interactiveshell import InteractiveShell
InteractiveShell.ast_node_interactivity = 'all'

# %%
%load_ext watermark
%watermark
%watermark --iversions
# %%
dbox_root = '/home/alal/res/india_pesa_forests/'
root = Path(dbox_root)
data = root / 'inp'
spatial = data /'spatial'
nldata = data / 'nightlights'
# %%
%%time
grid_merged = gpd.read_parquet(root/"tmp/raster_fishnet.spq")

# %%
gridgdf = grid_merged[['row', 'col', 'geometry']]
# %% test out geometry
grid_merged.plot(column = 'sch', cmap = "Set1", edgecolor = 'k')

# %% bounding box
bbox = box(68.1766451354, 7.96553477623, 97.4025614766, 35.4940095078)
geo = gpd.GeoDataFrame({'geometry': bbox}, index=[0], crs=from_epsg(4326))
print(geo)
getFeatures = lambda gdf: [json.loads(gdf.to_json())['features'][0]['geometry']]
coords = getFeatures(geo)
print(coords)


# %%
def clip_and_zonal_nightlights(y):
  satellite = 'calDMSP' if y <= 2013 else 'simVIIRS'
  nlpath = nldata/f'Harmonized_DN_NTL_{y}_{satellite}.tif'
  nl = rio.open(nlpath)
  # clip the raster with bounding box
  out_img, out_transform = mask(dataset=nl, shapes=coords, crop=True)
  out_meta = nl.meta.copy()
  out_meta.update({"driver": "GTiff",
                  "height": out_img.shape[1],
                  "width": out_img.shape[2],
                  "transform": out_transform})
  # Calculate zonal statistics
  nl_zonal = gpd.GeoDataFrame.from_features(
                zonal_stats(
                  gridgdf,
                  np.squeeze(out_img),
                  affine=out_transform,
                  stats=['min', 'max', 'mean', 'percentile_95', 'median'],
                  nodata=-99,
                  prefix=f"y{y}_",
                  geojson_out=True
                ))
  # return bare dataframe
  return pd.DataFrame(nl_zonal.drop(columns='geometry'))


# %%
%%time
list_of_nl0 = [clip_and_zonal_nightlights(y) for y in range(1992, 2019)]
# %% parallelize
%%time
list_of_nl = Parallel(n_jobs=16)(delayed(clip_and_zonal_nightlights)(y) for y in range(1992, 2019))
# %%
from functools import reduce
nl_wide = reduce(lambda x, y: pd.merge(x, y, on = ['row', 'col']), list_of_nl)

# %%
nl_wide.columns
# %%
nl_wide.to_csv(root/'tmp/pixel_level_nightlights_1992_2018.csv')
# %%
