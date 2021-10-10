

# %%
###     ######    ######   ######## ####  ######    ######
## ##   ##    ##  ##    ##  ##        ##  ##    ##  ##    ##
##   ##  ##        ##        ##        ##  ##        ##
##     ## ##   #### ##   #### ######    ##  ##   ####  ######
######### ##    ##  ##    ##  ##        ##  ##    ##        ##
##     ## ##    ##  ##    ##  ##        ##  ##    ##  ##    ##
##     ##  ######    ######   ##       ####  ######    ######
regsamp = df[year>=1995]
regsamp[, time := year - first_pesa_exposure]
ex_ante_med = quantile(regsamp$ex_ante_forest, 0.5)
# %% aggregate trends
summaries = regsamp[ex_ante_forest >= ex_ante_med][,
  .(avg = mean(forest_index, na.rm = T)), by = .(time, sch)][,
  group_avg := mean(avg), by = sch][,
  demeaned_avg := avg - group_avg][,
  sch2 := ifelse(sch == 0, "Non-Scheduled", "Scheduled")]
(agg_forest_index = summaries[time %between% c(-10, 10)] %>%
  ggplot(aes(x = time, y = demeaned_avg, colour = as.factor(sch2))) +
  geom_point() +
  geom_smooth(data = summaries[time %between% c(-10, -1)],size = 0.5, method = 'lm', se = F) +
  geom_smooth(data = summaries[time %between% c(0, 10)],  size = 0.5, method = 'lm', se = F) +
  scale_colour_brewer(palette = "Set1") +
  geom_vline(xintercept = -0.5, linetype = 'dotted', alpha = 0.6) +
  labs(title = "Forest", y = "Residual Forest", x = "Event Time", colour = "")
)

# %%
regsamp[, time := year - first_pesa_exposure]
summaries = regsamp[ex_ante_forest >= ex_ante_med][, .(avg = mean(green_index, na.rm = T)), by = .(time, sch)][,
  group_avg := mean(avg), by = sch][,
  demeaned_avg := avg - group_avg][,
  sch2 := ifelse(sch == 0, "Non-Scheduled", "Scheduled")]
(agg_green_index = summaries[time %between% c(-10, 10)] %>%
  ggplot(aes(x = time, y = demeaned_avg, colour = as.factor(sch2))) +
  geom_point() +
  geom_smooth(data = summaries[time %between% c(-10, -1)],size = 0.5, method = 'lm', se = F) +
  geom_smooth(data = summaries[time %between% c(0, 10)],  size = 0.5, method = 'lm', se = F) +
  scale_colour_brewer(palette = "Set1") +
  geom_vline(xintercept = -0.5, linetype = 'dotted', alpha = 0.6) +
  theme(legend.pos = "None") +
  labs(title = "Non-Forest Green", y = "Residual Non-Forest Green", x = "Event Time", colour = "")
)

# %%
regsamp[, time := year - first_pesa_exposure]
summaries = regsamp[ex_ante_forest >= ex_ante_med][,
  .(avg = mean(built_index, na.rm = T)), by = .(time, sch)][,
  group_avg := mean(avg), by = sch][,
  demeaned_avg := avg - group_avg][,
  sch2 := ifelse(sch == 0, "Non-Scheduled", "Scheduled")]
(agg_built_index = summaries[time %between% c(-10, 10)] %>%
  ggplot(aes(x = time, y = demeaned_avg, colour = as.factor(sch2))) +
  geom_point() + theme(legend.pos = "None") +
  geom_smooth(data = summaries[time %between% c(-10, -1)],size = 0.5, method = 'lm', se = F) +
  geom_smooth(data = summaries[time %between% c(0, 10)],  size = 0.5, method = 'lm', se = F) +
  scale_colour_brewer(palette = "Set1") +
  geom_vline(xintercept = -0.5, linetype = 'dotted', alpha = 0.6) +
  labs(title = "Built", y = "Residual Non-Forest Green", x = "Event Time", colour = "")
)


# %%
fig_all = agg_forest_index / agg_green_index / agg_built_index

ggsave(file.path(root, "out/agg_trends_vcf.pdf"), device = cairo_pdf, height = 10)

# %%
