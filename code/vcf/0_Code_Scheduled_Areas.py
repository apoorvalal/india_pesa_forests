# %%
import os
from numpy import *
import pandas as pd

from pathlib import Path

import seaborn as sns
import geopandas as gpd
import contextily as cx

# viz
import matplotlib
import matplotlib.pyplot as plt
import seaborn as sns
from plotnine import *
font = {'family' : 'IBM Plex Sans',
                'weight' : 'normal',
                'size'   : 10}
plt.rc('font', **font)
plt.rcParams['figure.figsize'] = (10, 10)
matplotlib.style.use(['seaborn-talk', 'seaborn-ticks', 'seaborn-whitegrid'])
%matplotlib inline
%config InlineBackend.figure_format = 'retina'

# plt.style.use("dark_background") # dark bg plots
sns.set(style="ticks", context="talk")
# # %matplotlib inline
# run for jupyter notebook
from IPython.core.interactiveshell import InteractiveShell
InteractiveShell.ast_node_interactivity = 'all'
# %%
# disable nonsensical pandas warning
pd.options.mode.chained_assignment = None
#%% Read in data
root = Path('/home/alal/Dropbox/1_Research/india_pesa_forests/')
code = root / 'Code'
data = root / 'inp'
%cd $data
# %%
census_2001 = data/"india_spatial_master/data/CENSUS/India_Census_Villages_2001"
adm_2001    = data/"india_spatial_master/data/2001_admin/"
%ls $adm_2001

# %%
block = gpd.read_file(adm_2001/'BLOCKMAP.shp')
state = gpd.read_file(adm_2001/'STATE.shp')

# %%
block.head()
block.info()

# %%
states = [a for a in block.STATE_UT.unique() if a is not None]
sort(states)

# %%
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

# %% data prep
block['named'] = block.DISTRICT.str.lower()
block['nameb'] = block.NAME.str.lower()

# %%
block['state'] = block.STATE_UT
block["state"].replace(state_abrs, inplace = True)

# %%
block['state'].unique()

# %%
# initialise values to 0
block['sch'] = 0


# %% # # Assign Treatment
# Function that takes list of districts and (district, block) pairs and modifies dataframe inplace
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


# %%
# ### Orissa

# %% [markdown]
# * Source: Ministry of Tribal Affairs http://tribal.nic.in/Content/ScheduledAreasinOrissaSSAreas.aspx
# https://web.archive.org/web/20170819102032/http://tribal.nic.in:80/Content/ScheduledAreasinOrissaSSAreas.aspx
# * History: The Scheduled area in the State of Orissa was originally specified by the Scheduled Areas (Part A States) Order, 1950 (Constitution Order, 9) dated 23.1.1950 and the Scheduled Areas (Part B States) Order, 1950, (Constitution Order, 26) dated 7.12.1950 and has been respecified by the Scheduled Areas (States of Bihar Gujarat, Madhya Pradesh and Orissa) Order, 1977, (Constitution Order, 109) dated 31.12.1977 after rescinding the earlier Orders in so far as they related to the State of Orissa.

# %%
sort(block.loc[block.state == "or"].nameb.unique())
block.loc[(block.state == "or") & (block.nameb == 'surada')].named.unique()

# %%
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

# %% [markdown]
# ### Andhra
#
# https://web.archive.org/web/20170818085914/http://tribal.nic.in/Content/ScheduledAreasinAndhraPradeshSSAreas.aspx
#
# https://pesadarpan.gov.in/en_US/fifth-schedule-areas?p_p_id=122_INSTANCE_kKN0LGcIxmYl&p_p_lifecycle=0&p_p_state=normal&p_p_mode=view&p_p_col_id=_118_INSTANCE_NK1DjTMey0mQ__column-1&p_p_col_count=1&p_r_p_564233524_resetCur=true&p_r_p_564233524_categoryId=27580

# %%
sort(block.loc[(block.state == "ap")].named.unique())
sort(block.loc[(block.state == "ap")].nameb.unique())

# %%
#%% Andhra
ap_d_sch = ["visakhapatnam", "east godavari"]
ap_b_sch = [("adilabad", "adilabad"), ("adilabad", "boath"),
                 ("adilabad", "asifabad"), ("adilabad", "sirpur"),
                 ("adilabad", "luxettipet"), ("warangal", "mulug"),
                 ("warangal", "narsampet"), ("khammam", "palwancha"),
                 ("khammam", "yellandu")]

ap2 = sched_labeller(block, 'ap', ap_d_sch, ap_b_sch) # ap_d_block_samp)
ap2.sch.value_counts()

# %% [markdown]
# ### Jharkhand
# https://web.archive.org/web/20170819035933/http://tribal.nic.in/Content/ScheduledAreasinJharkhandSSAreas.aspx

# %%
sort(block.loc[(block.state == "jk")].named.unique())
sort(block.loc[(block.state == "jk")].nameb.unique())

# %% Jharkhand
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
# %%
export_jk = jk2[['state', 'named', 'nameb', 'sch']]
export_jk.to_csv(root/"tmp/jharkhand_blocks.csv")
# %% [markdown]
# ### Gujarat
#
# https://web.archive.org/web/20170819103039/http://tribal.nic.in/Content/ScheduledAreasinGujarat.aspx
# https://pesadarpan.gov.in/web/guest/hidden/-/asset_publisher/PSDIGLsdo3bO/content/scheduled-areas-in-gujarat/26993?redirect=https://pesadarpan.gov.in/fifth-schedule-areas?p_p_id=122_INSTANCE_kKN0LGcIxmYl&p_p_lifecycle=0&p_p_state=normal&p_p_mode=view&p_p_col_id=_118_INSTANCE_NK1DjTMey0mQ__column-1&p_p_col_count=1&p_r_p_564233524_resetCur=true&p_r_p_564233524_categoryId=27020&entry_id=41141&show_back=true

# %%
sort(block.loc[(block.state == "gu")].named.unique())
sort(block.loc[(block.state == "gu")].nameb.unique())

# %%
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

# %% [markdown]
# ### Maharashtra
#
# https://web.archive.org/web/20170818101527/http://tribal.nic.in/Content/ScheduledAreasinMaharashtraSSAreas.aspx

# %%
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

# %% [markdown]
# ### Chhatisgarh
#
# https://web.archive.org/web/20170818065659/http://tribal.nic.in/Content/ScheduledAreasinChhattisgarh.aspx
#

# %%
sort(block.query("state == 'ch'").named.unique())
sort(block.query("state == 'ch'").nameb.unique())

# %%
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

# %% [markdown]
# ### Madhya Pradesh
#
# https://pesadarpan.gov.in/web/guest/hidden/-/asset_publisher/PSDIGLsdo3bO/content/scheduled-areas-in-madhya-pradesh/26993?redirect=https://pesadarpan.gov.in/fifth-schedule-areas?p_p_id=122_INSTANCE_kKN0LGcIxmYl&p_p_lifecycle=0&p_p_state=normal&p_p_mode=view&p_p_col_id=_118_INSTANCE_NK1DjTMey0mQ__column-1&p_p_col_count=1&p_r_p_564233524_resetCur=true&p_r_p_564233524_categoryId=27580&entry_id=41171&show_back=true

# %%
sort(block.query("state == 'mp'").named.unique())
sort(block.query("state == 'mp'").nameb.unique())

# %%
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

# %% [markdown]
# ### Rajasthan
#
# https://web.archive.org/web/20170910234200/http://tribal.nic.in:80/Content/ScheduledAreasinRajasthanSSAreas.aspx
#
# https://pesadarpan.gov.in/hidden/-/asset_publisher/PSDIGLsdo3bO/content/scheduled-areas-in-rajasthan/26993?p_p_lifecycle=0&p_p_state=normal&p_p_mode=view&p_p_col_id=column-2&p_p_col_count=1&p_r_p_564233524_resetCur=true&p_r_p_564233524_categoryId=27581&entry_id=41201&show_back=true

# %%
sort(block.query("state == 'rj'").named.unique())
sort(block.query("state == 'rj'").nameb.unique())

# %%
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

# %% [markdown]
# ### Himachal Pradesh
#
# https://pesadarpan.gov.in/web/guest/hidden/-/asset_publisher/PSDIGLsdo3bO/content/scheduled-areas-in-himachal-pradesh/26993?redirect=https://pesadarpan.gov.in/fifth-schedule-areas?p_p_id=122_INSTANCE_kKN0LGcIxmYl&p_p_lifecycle=0&p_p_state=normal&p_p_mode=view&p_p_col_id=_118_INSTANCE_NK1DjTMey0mQ__column-1&p_p_col_count=1&p_r_p_564233524_resetCur=true&p_r_p_564233524_categoryId=27022&entry_id=41151&show_back=true
#

# %%
sort(block.query("state == 'hp'").named.unique())
sort(block.query("state == 'hp'").nameb.unique())

#%%# Himanchal Pradesh
hp_d_sch = ['lahul and spiti', 'kinnaur']
hp_b_sch = [('chamba', 'pangi'), ('chamba', 'brahmaur')]

hp2 = sched_labeller(block, 'hp', hp_d_sch, hp_b_sch)
hp2.sch.value_counts()

# %%
block2 = pd.concat(
    [or2, ap2, jk2, gu2, ch2, mp2, mh2, hp2, rj2]
    )

# %%
%%time
block2.to_parquet(root/"tmp/BLOCKS_sch_coded.spq")

# %% [markdown]
# # Plot Treatment

xmin, ymin, xmax, ymax= block2.total_bounds
f, ax = plt.subplots(1, figsize=(12,12))
block2.plot(column='sch', categorical=True, legend=True, alpha = 0.8,
            cmap = 'Set1', edgecolor='k',linewidth=0.3,ax=ax)
state.plot(facecolor = 'none', categorical=True, legend=True,
            edgecolor='y',linewidth=1,ax=ax)
ax.set_xlim(xmin, xmax)
ax.set_ylim(ymin, ymax)
ax.set_axis_off()
cx.add_basemap(ax, crs = state.crs.to_string(),
    source = cx.providers.Stamen.TonerLite)
ax.set_title('Fifth Schedule Areas')
ax.set_axis_off()
f.savefig(root/'out/treatmap/scheduled_areas_map.pdf')
f.savefig(root/'out/treatmap/scheduled_areas_map.png')


# %%
def plot_treatment(statename, colname='sch'):
    """
    Function to slice geodataframe and plot treatment by state
    """
    df = block2.query('state == "{0}"'.format(statename))
    f, ax = plt.subplots(1, figsize=(9,9))
    df.plot(column=colname, categorical=True, legend=True,
            cmap = 'viridis', ax=ax)
    plt.suptitle('scheduled areas \n {0}'.format(statename))
    ax.set_axis_off()
    f.savefig(root/f"out/treatmap/treatmap_{statename}.pdf")


# %%
plot_treatment('or')

# %%
plot_treatment('ap')

# %%
plot_treatment('jk')

# %%
plot_treatment('gu')

# %%
plot_treatment('mh')

# %%
plot_treatment('ch')

# %%
plot_treatment('mp')

# %%
plot_treatment('hp')

# %%
plot_treatment('rj')

# %%
