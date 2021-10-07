
# %% treatment timing figure
state_status = df[year>=1990][,
  .(out = 1, treat = max(D)), .(state, year)]

f0 = panelView(out ~ treat,
  data = as.data.frame(state_status),
  index = c("state","year"),
  xlab = "Year", ylab = "State",
  main = "",
  by.timing = TRUE, legendOff = TRUE,
  background = "white")
f0 = f0 +
  lal_plot_theme(textangle = 90) + theme(legend.pos = "None") + labs(x = "", y = "") +
  geom_vline(xintercept = 11.5, color = 'red', size = 1.2) +
  annotate("text", x = 8, y = 1, label = "GFC Coverage Begins", color = 'red', size = 5.0)
ggsave(file.path(root, "out/panelview_vcf_ann.pdf"), height = 6, width = 12, device = cairo_pdf)


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
