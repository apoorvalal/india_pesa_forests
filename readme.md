# Replication archive (Gulzar, Lal, Pasquale 2020)

## Data Prep (requires GFC and ML Infomap data)
+ `0_mosaic_forests` mosaics rasters from the GFC databases
+ `1_Code_Scheduled_Areas` takes block level shapefiles and *codes the treatment*. Makes figure 2 (left)
+ `2_scheduled_villages` merges the treatment to village polygons
+ `3_raster_merge` aggregates the deforestation data to the vilalge level
+ `4_final_prep` computes distance to scheduled/non-scheduled borders
+ `5_mining_prep` computes distance to mines. makes figure A2
+ `6_prep_estimation` reads wide village level data with deforestation and distances and reshapes to long, and performs the final subset steps


## Core replication scripts
+ `7_estimation` prepares the data for the tables and runs regressions. Makes table 1, A3, A4, fig 5
+ `8_dyn_effects` makes fig 3, fig 4, fig 6, fig A4
+ `8_summary_plots_and_mechanisms` makes table A1, A2, fig 8 panel A, fig 8 panel B, table 2, fig A3, 
+ `9_panelmatch` performs panel matching. makes figure 7, figure A1.
