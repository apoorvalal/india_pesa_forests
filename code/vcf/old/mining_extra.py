# 3_mining_prep run before this
minerals = mines.mineral.unique()
# %%
def distance_compute(m):
    mines_subset = mines.loc[mines.mineral == m]
    distance_to_mine = lambda x: mines_subset.distance(x).min()
    d = vil_g.geometry.apply(distance_to_mine)

# %%
# %%time
distances = Parallel(n_jobs = 10, verbose = 50)(delayed(
    distance_compute)(m) for m in minerals
    )
# ### collate and write
distances_df = pd.concat(distances, axis = 1)
distances_df.columns = ['min_dist_to_' + re.sub("( |/)", "", m) for m in minerals]

# %%
vil2 = pd.concat([vil_g, distances_df], axis = 1)

# %% [markdown]
# ### Write

# %%
vil2 = pd.DataFrame(vil2.drop('geometry', axis = 1))

# %% jupyter={"outputs_hidden": true}
vil2.to_csv(data/"Intermediate/village_points_all_mines.csv")

# %% [markdown]
# # Intensive margin - count number of mines in buffer

# %% [markdown]
# ## 5 km

# %%
sindex = mines.sindex
len(sindex.leaves())

# %%
# %%time
results_list = []
# iterate over the points
for index, row in vil_g.iterrows():
    buffer = row['geometry'].buffer(0.05)  # buffer
    # find approximate matches with r-tree, then precise matches from those approximate ones
    possible_matches_index = list(sindex.intersection(buffer.bounds))
    possible_matches = mines.iloc[possible_matches_index]
    precise_matches = possible_matches[possible_matches.intersects(buffer)]
    results_list.append(len(precise_matches))

# %%
results_list[:5]

# %%
vil_g['mines_in_5k'] = pd.Series(results_list)

# %%
sns.distplot(vil_g.mines_in_5k)

# %%
vil_g.mines_in_5k.describe()

# %% [markdown]
# ## 10 km

# %%
# %%time
results_list = []
# iterate over the points
for index, row in vil_g.iterrows():
    buffer = row['geometry'].buffer(0.1)  # buffer
    # find approximate matches with r-tree, then precise matches from those approximate ones
    possible_matches_index = list(sindex.intersection(buffer.bounds))
    possible_matches = mines.iloc[possible_matches_index]
    precise_matches = possible_matches[possible_matches.intersects(buffer)]
    results_list.append(len(precise_matches))

# %%
results_list[:5]

# %%
vil_g['mines_in_10k'] = pd.Series(results_list)

# %%
sns.distplot(vil_g.mines_in_10k)

# %%
vil_g.mines_in_10k.describe()

# %% [markdown]
# ## 50 km

# %%
# %%time
results_list = []
# iterate over the points
for index, row in vil_g.iterrows():
    buffer = row['geometry'].buffer(0.5)  # buffer
    # find approximate matches with r-tree, then precise matches from those approximate ones
    possible_matches_index = list(sindex.intersection(buffer.bounds))
    possible_matches = mines.iloc[possible_matches_index]
    precise_matches = possible_matches[possible_matches.intersects(buffer)]
    results_list.append(len(precise_matches))

# %%
results_list[:5]

# %%
vil_g['mines_in_50k'] = pd.Series(results_list)

# %%
sns.distplot(vil_g.mines_in_50k)

# %%
vil_g.mines_in_50k.describe()

# %%

# %%
# %%time
results_list = []
# iterate over the points
for index, row in vil_g.iterrows():
    buffer = row['geometry'].buffer(0.1)  # buffer
    # find approximate matches with r-tree, then precise matches from those approximate ones
    possible_matches_index = list(sindex.intersection(buffer.bounds))
    possible_matches = mines.iloc[possible_matches_index]
    precise_matches = possible_matches[possible_matches.intersects(buffer)]
    results_list.append(len(precise_matches))

# %%
results_list[:5]

# %%
vil_g['mines_in_10k'] = pd.Series(results_list)

# %%
sns.distplot(vil_g.mines_in_10k)

# %%
vil_g.mines_in_10k.describe()

# %%
# %%time
pd.DataFrame(vil_g.drop('geometry', axis = 1)).to_csv(data/"Intermediate/village_points_mine_counts.csv")

# %% [markdown]
# # Map v2 with village points

# %%
states = gpd.read_file("/home/alal/Dropbox/_Data/India_spatial_pipeline/data/ADM/IND_adm1.shp")

# %%
f, ax = plt.subplots(1,figsize = (10, 12), dpi = 200)
states.plot(facecolor = 'None',linewidth = 0.5, edgecolor = 'k', ax = ax)
vil_g.plot(column = 'mines_in_10k',  markersize = 0.5 , ax = ax, cmap = 'viridis')
mines.loc[mines.mineral.isin(top20)].plot(column = 'mineral', categorical = True,
            markersize = 0.8 , ax = ax,
            cmap = 'tab20')
ax.set_axis_off()
ax.set_title("Villages with Mine Proximity \n Number of mines within 10 km radius \n State borders overlaid")

# %%
f.savefig(root/'out/mine_density_map.pdf')

f.savefig(root/'out/mine_density_map.png')
