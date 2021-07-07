# ---
# jupyter:
#   jupytext:
#     text_representation:
#       extension: .py
#       format_name: percent
#       format_version: '1.3'
#       jupytext_version: 1.5.0
#   kernelspec:
#     display_name: gis
#     language: python
#     name: gds
# ---

# %% Collapsed="false"
import os
from numpy import *
import pandas as pd

from pathlib import Path

import matplotlib.pyplot as plt
import seaborn as sns
import geopandas as gpd
plt.style.use("seaborn-darkgrid")
# plt.style.use("dark_background") # dark bg plots
sns.set(style="ticks", context="talk")
# # %matplotlib inline
# run for jupyter notebook
from IPython.core.interactiveshell import InteractiveShell
InteractiveShell.ast_node_interactivity = 'all'

# %% Collapsed="false"
# disable nonsensical pandas warning 
pd.options.mode.chained_assignment = None

# %% [markdown] Collapsed="false"
# ### Folder Structure

# %% Collapsed="false"
#%% Read in data
root = Path('/home/alal/Dropbox/1_Research/India_Forests/')
code = root / 'Code'
data = root / 'Data'

# %% Collapsed="false"
# %cd $data

# %% [markdown] Collapsed="false"
# ### Ingest Block level shapefile

# %% Collapsed="false"
block = gpd.read_file(data/'Spatial/Vectors/2001/BLOCKMAP.shp')

state = gpd.read_file(data/'Spatial/Vectors/2001/STATE.shp')

# %% Collapsed="false"
block.head()
block.info()

# %% Collapsed="false"
states = [a for a in block.STATE_UT.unique() if a is not None]
sort(states)

# %% Collapsed="false"
state_abrs = dict([
    ('Orissa', 'or'),
    ('Andhra Pradesh', 'ap'),
    ('Jharkhand', 'jk'),
    ('Gujarat', 'gu'),
    ('Maharashtra', 'mh'),
    ('Chhattisgarh', 'ch'),
    ('Madhya Pradesh', 'mp'),
    ('Rajasthan', 'rj'),
    ('Himachal Pradesh', 'hp')
])

sched_area_states = state_abrs.values()

# %% [markdown] Collapsed="false"
# # data prep

# %% Collapsed="false"
block['named'] = block.DISTRICT.str.lower()
block['nameb'] = block.NAME.str.lower()

# %% Collapsed="false"
block['state'] = block.STATE_UT
block["state"].replace(state_abrs, inplace = True)

# %% Collapsed="false"
block['state'].unique()

# %% Collapsed="false"
# initialise values to 0
block['sch'] = 0


# %% [markdown] Collapsed="false" toc-hr-collapsed=true
# # Assign Treatment 

# %% [markdown] Collapsed="false"
# ### Function that takes list of districts and (district, block) pairs and modifies dataframe inplace

# %% Collapsed="false"
def sched_labeller(d, st, d_sch, b_sch, d_block_samp=None):
    """
    function to label blocks as scheduled and block_samp based on list 
    of district names and a dict
    """
    df = d.loc[d.state == st] # subset to state
    ############################## 
    # district level classifications
    ############################## 
    # slower but noisier prep
    for dist in d_sch:
        df.loc[df.named == dist, 'sch'] = 1 
        print(f'Changed {df.loc[df.named == dist].shape[0]} obs for district {dist}')
    
    ############################## 
    # Block sample - legacy
    ############################## 
    # if d_block_samp is not None: # if block name 
    #    df.loc[df.named.isin(d_block_samp), 'block_samp'] = 1 
    ############################## 
    # district - block sample
    ############################## 
    districts = df.named.unique() 
    blocks    = df.nameb.unique()
    # iterate through dict of (district, block) pairs and flag sch = 1 for each 
    for i in b_sch:
        d, b = i[0], i[1] # district, block pair
        error_pairs = []
        if not (d in districts and b in blocks):
            error_pairs.append(i)
        df.loc[(df.named == d) & (df.nameb == b),  'sch'] = 1 
        # noisy
        print(f'Changed {df.loc[(df.named == d) & (df.nameb == b)].shape[0]} obs for district = {d}, block = {b}')
    print('---------- MISSING PAIRS ----------')
    print(error_pairs)
    print('------------------------------------')
    return df


# %% [markdown] Collapsed="false"
# ### Orissa

# %% [markdown] Collapsed="false"
# * Source: Ministry of Tribal Affairs http://tribal.nic.in/Content/ScheduledAreasinOrissaSSAreas.aspx
# https://web.archive.org/web/20170819102032/http://tribal.nic.in:80/Content/ScheduledAreasinOrissaSSAreas.aspx
# * History: The Scheduled area in the State of Orissa was originally specified by the Scheduled Areas (Part A States) Order, 1950 (Constitution Order, 9) dated 23.1.1950 and the Scheduled Areas (Part B States) Order, 1950, (Constitution Order, 26) dated 7.12.1950 and has been respecified by the Scheduled Areas (States of Bihar Gujarat, Madhya Pradesh and Orissa) Order, 1977, (Constitution Order, 109) dated 31.12.1977 after rescinding the earlier Orders in so far as they related to the State of Orissa.

# %% Collapsed="false"
sort(block.loc[block.state == "or"].nameb.unique())
block.loc[(block.state == "or") & (block.nameb == 'surada')].named.unique()

# %% Collapsed="false"
# Orissa
or_d_sch = ["mayurbhanj", "sundargarh", "koraput"]

or_b_sch = [("sambalpur", "kochinda"), 
                 ("kendujhar", "kendujhargarh"), # found 
                 ("kendujhar", "telkoi"), 
                 ("kendujhar", "champua"),
                 ("kendujhar", "banspal"), 
                 ("baudh", "kantamal"),
                 ("kandhamal", "g.udayagiri"),  # fixed
                 ("kandhamal", "baliguda"),
                 ("kalahandi", "thuamul-rampur"), # fixed
                 ("kalahandi", "lanjigarh"),
                 ("baleshwar", "nilagiri"), 
                 ("gajapati", "r.udaygiri"), # fixed
                 ("gajapati", "guma"), 
                 ("gajapati", "rayagada"),
                 ("gajapati", "parlakhemundi (gosani)"), # fixed
                 ("ganjam", "surada")]

or2 = sched_labeller(block, 'or', or_d_sch, or_b_sch) #, or_d_block_samp)
or2.sch.value_counts()

# %% [markdown] Collapsed="false"
# ### Andhra
#
# https://web.archive.org/web/20170818085914/http://tribal.nic.in/Content/ScheduledAreasinAndhraPradeshSSAreas.aspx
#
# https://pesadarpan.gov.in/en_US/fifth-schedule-areas?p_p_id=122_INSTANCE_kKN0LGcIxmYl&p_p_lifecycle=0&p_p_state=normal&p_p_mode=view&p_p_col_id=_118_INSTANCE_NK1DjTMey0mQ__column-1&p_p_col_count=1&p_r_p_564233524_resetCur=true&p_r_p_564233524_categoryId=27580

# %% Collapsed="false"
sort(block.loc[(block.state == "ap")].named.unique())
sort(block.loc[(block.state == "ap")].nameb.unique())

# %% Collapsed="false"
#%% Andhra
ap_d_sch = ["visakhapatnam", "east godavari"]
ap_b_sch = [("adilabad", "adilabad"), ("adilabad", "boath"),
                 ("adilabad", "asifabad"), ("adilabad", "sirpur"),
                 ("adilabad", "luxettipet"), ("warangal", "mulug"),
                 ("warangal", "narsampet"), ("khammam", "palwancha"),
                 ("khammam", "yellandu")]

ap2 = sched_labeller(block, 'ap', ap_d_sch, ap_b_sch) # ap_d_block_samp)
ap2.sch.value_counts()

# %% [markdown] Collapsed="false"
# ### Jharkhand
# https://web.archive.org/web/20170819035933/http://tribal.nic.in/Content/ScheduledAreasinJharkhandSSAreas.aspx

# %% Collapsed="false"
sort(block.loc[(block.state == "jk")].named.unique())
sort(block.loc[(block.state == "jk")].nameb.unique())

# %% Collapsed="false"
#%% Jharkhand
jk_d_sch = [
    "ranchi", "lohardaga", "gumla", "purbi singhbhum", "pashchimi singhbhum",
    "sahibganj", "pakaur", "dumka"
]
jk_b_sch = [("gumla", "simdega"), ("palamu", "latehar"),
                 ("palamu", "satbarwa"), ("godda", "sundarpahari"),
                 ("godda", "boarijor"), ("dumka", "jamtara"),
                 ("garhwa", "bhandaria")]

jk2 = sched_labeller(block, 'jk', jk_d_sch, jk_b_sch) #, jk_d_block_samp)
jk2.sch.value_counts()

# %% [markdown] Collapsed="false"
# ### Gujarat
#
# https://web.archive.org/web/20170819103039/http://tribal.nic.in/Content/ScheduledAreasinGujarat.aspx
# https://pesadarpan.gov.in/web/guest/hidden/-/asset_publisher/PSDIGLsdo3bO/content/scheduled-areas-in-gujarat/26993?redirect=https://pesadarpan.gov.in/fifth-schedule-areas?p_p_id=122_INSTANCE_kKN0LGcIxmYl&p_p_lifecycle=0&p_p_state=normal&p_p_mode=view&p_p_col_id=_118_INSTANCE_NK1DjTMey0mQ__column-1&p_p_col_count=1&p_r_p_564233524_resetCur=true&p_r_p_564233524_categoryId=27020&entry_id=41141&show_back=true

# %% Collapsed="false"
sort(block.loc[(block.state == "gu")].named.unique())
sort(block.loc[(block.state == "gu")].nameb.unique())

# %% Collapsed="false"
#%%# Gujarat
gu_d_sch = []

gu_b_sch = [
       ("the dangs", "the dangs"),  # fixed, sep the dangs
       ("bharuch", "jhagadia"), 
       ("bharuch", "valia"), 
       ("dohad", "devgadbaria"), 
       ("dohad", "dohad"), 
       ("dohad", "jhalod"), 
       ("dohad", "limkheda"), 
    ("dohad", "devgadbaria"), 
       ("narmada", "dediapada"), 
       ("narmada", "nandod"), 
       ("narmada", "sagbara"), 
       ("panch mahals", "santrampur"), 
       ("surat", "uchchhal"), 
       ("surat", "vyara"), 
       ("surat", "mahuva"), 
       ("surat", "mandvi"),
       ("surat", "nizar"),
       ("surat", "mangrol"), 
       ("surat", "songadh"),
       ("surat", "valod"),
       ("surat", "bardoli"),    
       ("vadodara", "chhota udaipur"), 
       ("vadodara", "nasvadi"), 
       ("valsad", "dharampur"), 
       ("valsad", "pardi"), 
       ("valsad", "umbergaon"),
       ("navsari", "bansda"),
       ("navsari", "chikhli") 
       
]


gu2 = sched_labeller(block, 'gu', gu_d_sch, gu_b_sch)
gu2.sch.value_counts()

# %% [markdown] Collapsed="false"
# ### Maharashtra
#
# https://web.archive.org/web/20170818101527/http://tribal.nic.in/Content/ScheduledAreasinMaharashtraSSAreas.aspx

# %% Collapsed="false"
#%%# Maharashtra
mh_d_sch = []

mh_b_sch =  [("thane", "palghar"), ("thane", "vasai"), 
             ("thane", "bhiwandi"), ("thane", "murbad"), 
             ("nashik", "dindori"), ("nashik", "igatpuri"),
     ("nashik", "nashik"), ("nashik", "baglan"), ("dhule", "sakri"),
     ("dhule", "shirpur"), ("nandurbar", "nandurbar"),
     ("nandurbar", "shahade"), ("jalgaon", "chopda"), ("jalgaon", "raver"),
     ("jalgaon", "yawal"), ("pune", "ambegaon"), ("pune", "junnar"),
     ("nanded", "kinwat"), ("yavatmal", "maregaon"), ("yavatmal", "ralegaon"),
     ("yavatmal", "kelapur"), ("yavatmal", "ghatanji"),
     ("gadchiroli", "gadchiroli"), ("gadchiroli", "armori"),
     ("gadchiroli", "chamorshi"), ("chandrapur", "rajura"),
     ("ahmadnagar", "akola")]
mh2 = sched_labeller(block, 'mh', mh_d_sch, mh_b_sch)
mh2.sch.value_counts()

# %% [markdown] Collapsed="false"
# ### Chhatisgarh
#
# https://web.archive.org/web/20170818065659/http://tribal.nic.in/Content/ScheduledAreasinChhattisgarh.aspx
#

# %% Collapsed="false"
sort(block.query("state == 'ch'").named.unique())
sort(block.query("state == 'ch'").nameb.unique())

# %% Collapsed="false"
#%%# Chhatisgarh
ch_d_sch = ['bastar', 'koriya', 'surguja', 'kanker', 'dantewada',
           'korba', 'jashpur']

ch_b_sch = [("jashpur", "jashpurnagar"), 
            ("raigarh", "kharsia"), 
            ("bilaspur", "kota"), 
            ("bilaspur", "gaurella no.1"), 
            ("bilaspur", "gaurella no.2"), 
            ("bilaspur", "marwahi"), 
            ("korba", "katghora"), 
            ("durg", "dondi"), 
            ("rajnandgaon", "chowki"), 
            ("rajnandgaon", "mohla"), 
            ("rajnandgaon", "manpur"), 
            ("raipur", "mainpur"), 
            ("raipur", "chhura"), 
            ("raipur", "gariaband"), 
            ("dhamtari", "sihwa(nagri)")]

ch2 = sched_labeller(block, 'ch', ch_d_sch, ch_b_sch)
ch2.sch.value_counts()

# %% [markdown] Collapsed="false"
# ### Madhya Pradesh
#
# https://pesadarpan.gov.in/web/guest/hidden/-/asset_publisher/PSDIGLsdo3bO/content/scheduled-areas-in-madhya-pradesh/26993?redirect=https://pesadarpan.gov.in/fifth-schedule-areas?p_p_id=122_INSTANCE_kKN0LGcIxmYl&p_p_lifecycle=0&p_p_state=normal&p_p_mode=view&p_p_col_id=_118_INSTANCE_NK1DjTMey0mQ__column-1&p_p_col_count=1&p_r_p_564233524_resetCur=true&p_r_p_564233524_categoryId=27580&entry_id=41171&show_back=true

# %% Collapsed="false"
sort(block.query("state == 'mp'").named.unique())
sort(block.query("state == 'mp'").nameb.unique())

# %% Collapsed="false"
#%%# Madhya Pradesh
mp_d_sch = ['jhabua', 'mandla']

mp_b_sch = [("shahdol", "pushparajgarh"),
            ("shahdol", "sohagpur"),
            ("shahdol", "jaisinghnagar"), 
            ("dhar", "sardarpur"), 
            ("dhar", "dhar"), 
            ("dhar", "manawar"), 
            ("dhar", "kukshi"), 
            ("barwani", "barwani"), 
            ("barwani", "rajpur"), 
            ("barwani", "sendhwa"), 
            ("west nimar", "bhikangaon"), 
            ("west nimar", "maheshwar"), 
            ("east nimar", "khalwa"), 
            ("east nimar", "khaknar"), 
            ("ratlam", "sailana"), 
            ("betul", "betul"), 
            ("betul", "bhainshdehi"), 
            ("seoni", "kurai"), 
            ("seoni", "lakhnadon"), 
            ("balaghat", "baihar"), 
            ("hoshangabad", "kesala"), 
            ("sidhi", "kusmi"), 
            ("sheopur", "karahal"), 
            ("chhindwara", "tamia"), 
            ("chhindwara", "jamai(junnardeo)"), 
            ("chhindwara", "harrai"), 
            ("chhindwara", "bichhua")]

mp2 = sched_labeller(block, 'mp', mp_d_sch, mp_b_sch)
mp2.sch.value_counts()

# %% [markdown] Collapsed="false"
# ### Rajasthan
#
# https://web.archive.org/web/20170910234200/http://tribal.nic.in:80/Content/ScheduledAreasinRajasthanSSAreas.aspx
#
# https://pesadarpan.gov.in/hidden/-/asset_publisher/PSDIGLsdo3bO/content/scheduled-areas-in-rajasthan/26993?p_p_lifecycle=0&p_p_state=normal&p_p_mode=view&p_p_col_id=column-2&p_p_col_count=1&p_r_p_564233524_resetCur=true&p_r_p_564233524_categoryId=27581&entry_id=41201&show_back=true

# %% Collapsed="false"
sort(block.query("state == 'rj'").named.unique())
sort(block.query("state == 'rj'").nameb.unique())

# %% Collapsed="false"
#%%# Rajasthan
rj_d_sch = ['banswara', 'dungarpur']

rj_b_sch = [("udaipur", "girwa")]
#             ("udaipur", "bujdha"),
#             ("udaipur", "dodavli"), 
#             ("udaipur", "alsigdh"),
#             ("udaipur", "pduna"), 
#             ("udaipur", "chawand"),
#             ("udaipur", "saru"), 
#             ("udaipur", "tidi"), 
#             ("udaipur", "jawas"),
#             ("udaipur", "barapal"), 
#             ("udaipur", "titrdi"),
#             ("udaipur", "wati"), 
#             ("udaipur", "chansada"),
#             ("udaipur", "javad"), 
#             ("udaipur", "dantisr"),
#             ("udaipur", "lkdwas")]
rj2 = sched_labeller(block, 'rj', rj_d_sch, rj_b_sch)
rj2.sch.value_counts()

# %% [markdown] Collapsed="false"
# ### Himachal Pradesh
#
# https://pesadarpan.gov.in/web/guest/hidden/-/asset_publisher/PSDIGLsdo3bO/content/scheduled-areas-in-himachal-pradesh/26993?redirect=https://pesadarpan.gov.in/fifth-schedule-areas?p_p_id=122_INSTANCE_kKN0LGcIxmYl&p_p_lifecycle=0&p_p_state=normal&p_p_mode=view&p_p_col_id=_118_INSTANCE_NK1DjTMey0mQ__column-1&p_p_col_count=1&p_r_p_564233524_resetCur=true&p_r_p_564233524_categoryId=27022&entry_id=41151&show_back=true
#

# %% Collapsed="false"
sort(block.query("state == 'hp'").named.unique())
sort(block.query("state == 'hp'").nameb.unique())

# %% Collapsed="false"
#%%# Himanchal Pradesh
hp_d_sch = ['lahul and spiti', 'kinnaur']
hp_b_sch = [('chamba', 'pangi'), ('chamba', 'brahmaur')]

hp2 = sched_labeller(block, 'hp', hp_d_sch, hp_b_sch)
hp2.sch.value_counts()

# %% [markdown] Collapsed="false"
# ## Write file

# %% Collapsed="false"
block2 = pd.concat(
    [or2, ap2, jk2, gu2, ch2, mp2, mh2, hp2, rj2]
    )

# %% Collapsed="false"
block2.to_file(data/"Spatial/Processed/BLOCKS_sch_coded.geojson", 
              'GeoJSON')

# %% [markdown] Collapsed="false"
# # Plot Treatment

# %% Collapsed="false"
xmin, ymin, xmax, ymax= block2.total_bounds
f, ax = plt.subplots(1, figsize=(12,12))
block2.plot(column='sch', categorical=True, legend=True,
            edgecolor='k',linewidth=0.3,ax=ax)
state.plot(facecolor = 'none', categorical=True, legend=True,
            edgecolor='r',linewidth=1,ax=ax)
ax.set_xlim(xmin, xmax)
ax.set_ylim(ymin, ymax)
ax.set_axis_off()


# %% Collapsed="false"
def plot_treatment(statename, colname='sch'):
    """
    Function to slice geodataframe and plot treatment by state
    """
    df = block2.query('state == "{0}"'.format(statename))
    f, ax = plt.subplots(1, figsize=(9,9))
    df.plot(column=colname, categorical=True, legend=True, 
            cmap = 'viridis', ax=ax)
    plt.suptitle('treatment:{0} ; state:{1}'.format(colname, statename),color='r')
    ax.set_axis_off()


# %% Collapsed="false"
plot_treatment('or')

# %% Collapsed="false"
plot_treatment('ap')

# %% Collapsed="false"
plot_treatment('jk')

# %% Collapsed="false"
plot_treatment('gu')

# %% Collapsed="false"
plot_treatment('mh')

# %% Collapsed="false"
plot_treatment('ch')

# %% Collapsed="false"
plot_treatment('mp')

# %% Collapsed="false"
plot_treatment('hp')

# %% Collapsed="false"
plot_treatment('rj')

# %% Collapsed="false"
treatblocks = block2.loc[block2.sch == 1]
treatblocks.shape
block.C_CODE01.nunique()

# %% [markdown] Collapsed="false"
# # (IGNORE FOR NEW PREP) Create decimal degree buffers around treated block boundaries
#

# %% [raw] Collapsed="false"
# %cd {data / 'Spatial/Processed/'}

# %% [raw] Collapsed="false"
# def buffer_creator(distance):
#     treatbuf = treatblocks.boundary.buffer(distance)
#     treated_buffers = gpd.GeoDataFrame(treatbuf).rename(columns={0:'geometry'}).set_geometry('geometry')
#     treated_buffers.crs = treatblocks.crs
#     block_buf = gpd.overlay(block2, treated_buffers, how='intersection')
#     # Dissolve back into blocks 
#     block_buf2 = block_buf.dissolve(by='C_CODE01')
#     return block_buf2

# %% [raw] Collapsed="false"
# buf = buffer_creator(1.0)

# %% [raw] Collapsed="false"
# f, ax = plt.subplots(1, figsize=(12,12))
# buf.plot(column='sch', categorical=True, legend=True, edgecolor='r',linewidth=0.3,ax=ax)
# plt.suptitle('1 degree buffers',color='r')
# ax.set_axis_off()

# %% [raw] Collapsed="false"
# buf.to_file('Blocks_proximate_treat_10.shp')

# %% [raw] Collapsed="false"
# buf = buffer_creator(0.5)

# %% [raw] Collapsed="false"
# f, ax = plt.subplots(1, figsize=(12,12))
# buf.plot(column='sch', categorical=True, legend=True, edgecolor='r',linewidth=0.3,ax=ax)
# plt.suptitle('0.5 degree buffers',color='r')
# ax.set_axis_off()

# %% [raw] Collapsed="false"
# buf.to_file('Blocks_proximate_treat_05.shp')
