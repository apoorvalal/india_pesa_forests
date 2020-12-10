#%%
rm(list = ls())

###############################################################
###############################################################
# Please add this code to the respective files
###############################################################
###############################################################

# %%

#%% mine distance
##     ## #### ##    ## #### ##    ##  ######
###   ###  ##  ###   ##  ##  ###   ## ##    ##
#### ####  ##  ####  ##  ##  ####  ## ##
## ### ##  ##  ## ## ##  ##  ## ## ## ##   ####
##     ##  ##  ##  ####  ##  ##  #### ##    ##
##     ##  ##  ##   ###  ##  ##   ### ##    ##
##     ## #### ##    ## #### ##    ##  ######

# %% mining summary figure - binned scatterplot - fig8 panel A ;
dat = import(file.path(data, 'villages_estimation_sample.rds'))
late_states = dat[state_ut %in% c("Chhattisgarh", "Maharashtra", "Jharkhand") & year == 2001]

# %%
late_states = late_states[tot_pop > 0 & tot_pop < 1000000]
late_states$def_ha = late_states$def * 0.09

figsamp = late_states[min_dist_to_mine <= 1]
figsamp[, min_dist_to_mine  := min_dist_to_mine * 100]

gen = binsreg(y = figsamp$def_ha, x = figsamp$min_dist_to_mine,
  w = figsamp[, .(pref_count, pref_mean)],
  nbins = 20, polyreg = 2)

# %%
f = gen$bins_plot + lal_plot_theme() +
  labs(y = 'deforestated area in 2001 (residualised)', x = 'Distance')

ggsave(file.path(out, 'deforestation_v_mines.pdf'), f, device = cairo_pdf)

# %% estimate het-TE by distance to mines - table 2

# %%
tic()
estim_samp = df %>% setDT
toc()
#%%
estim_samp2 = estim_samp[pref == 1]
estim_samp2[, mine_dist_q := ntile(min_dist_to_mine, 3)]

estim_samp2[, `:=`(
  D_mine_1 = D * (mine_dist_q == 1),
  D_mine_2 = D * (mine_dist_q == 2),
  D_mine_3 = D * (mine_dist_q == 3))]

# Alternative measure of mine proximity
varlist  = names(estim_samp2)[which( colnames(estim_samp2) == "min_dist_to_chromite"):which( colnames(estim_samp2)=="min_dist_to_diamond")]

for (d in c(3, 5, 10, 15)) {
  estim_samp2$radCount = 0
  for (v in varlist) {
       estim_samp2[["radCount"]] = ifelse(estim_samp2[[v]] <= d, 
                                          estim_samp2[["radCount"]] + 1, 
                                          estim_samp2[["radCount"]]) 
       }
  nam <- paste("radius", d, sep = "")
  assign(nam, estim_samp2$radCount)
}

estim_samp2$radius3  = radius3
estim_samp2$radius5  = radius5
estim_samp2$radius10 = radius10
estim_samp2$radius15 = radius15

###### Alternative definition: 

estim_samp2[, rad5_q := ntile(radius5, 3)]

estim_samp2[, `:=`(
  rad_mine_1 = D * (rad5_q == 1),
  rad_mine_2 = D * (rad5_q == 2),
  rad_mine_3 = D * (rad5_q == 3))]

varlist <- names(estim_samp2)[95:98]

# %% 2wFE
tic()
r00 = felm(def_ha ~ rad_mine_1 + rad_mine_2 + rad_mine_3 | 
           village + yr | 0 | village, estim_samp2)
toc()
# %% 2wFE + linear time trends
tic()
r01 = felm(def_ha ~ rad_mine_1 + rad_mine_2 + rad_mine_3 | 
             village + yr + village:t | 0 | village, estim_samp2)
toc()
# %% Villge + state X year FEs
tic()
r02 = felm(def_ha ~ rad_mine_1 + rad_mine_2 + rad_mine_3 | 
             village + state*yr | 0 | village, estim_samp2)
toc()
# %%
tic()
r03 = felm(def_ha ~ rad_mine_1 + rad_mine_2 + rad_mine_3 | 
             village + village:t + styear | 0 | village, estim_samp2)
toc()

######

# %%
dvmean = round(mean(estim_samp2$def_ha), 2)
nvill  = nunique(estim_samp2$code_2011)

mods = list(r00, r01, r02, r03)

covlabs = c("Scheduled X PESA X 1st Tercile", "Scheduled X PESA X 2nd Tercile",
            "Scheduled X PESA X 3rd Tercile")

stargazer(mods, keep.stat = c("N"),
          covariate.labels = covlabs,
          dep.var.labels = c("Annual Deforestation in Hectares"),
          style = "apsr",
          column.sep.width = "0pt",
          title = "Treatment Effects on Annual Deforestation by Distance to Nearest Mine",
          model.names = F,
          label = "table:regresmine",
          notes = "Cluster-Robust Standard Errors (by village)",
          type = 'latex', out = file.path(root, "out/mine_conc_alt.pdf"))

#%%
estim_samp2[, `:=`(D_f = as.factor(D),
                  code_2011_f = as.factor(code_2011),
                  year_f = as.factor(year)) ]

#%% Alternative estimator - binning estimator - figure 8 panel B
tic()
mining_alt = interflex(Y = "def_ha", D = "D_f", X = "radius5",
                       FE = c("code_2011_f", "year"), 
                       nbins = 5, 
                       xlab = "Mine Concentration (r = 5 km)", ylab = "Treatment Effect",
                       data = estim_samp2, theme.bw = T,
                       estimator = 'binning', CI = FALSE)
toc()

mining_alt$graph

# %%
ggsave(file.path(root, "out/Interflex_alt.pdf"), mining_alt$graph,
       device = cairo_pdf)


######## ##     ##  ######  ######## ##     ## ########
##       ##     ## ##    ##    ##    ##     ## ##     ##
##       ##     ## ##          ##    ##     ## ##     ##
######   ##     ##  ######     ##    ##     ## ##     ##
##        ##   ##        ##    ##    ##     ## ##     ##
##         ## ##   ##    ##    ##    ##     ## ##     ##
########    ###     ######     ##     #######  ########



# %% fig 6
mini = -6
maxi = 12
indics <- paste0(c(paste0("`lag_", seq(mini, -2), "`"),
                   paste0("lag_", seq(0, maxi))),
                 collapse = " + ")
# make the formula we want to run
form <- as.formula(paste0("def_ha ~ ", indics, "| village + styear | 0 | village"))

# %%
ev_study_fitter = function(lb, data = widedat, formula = form){
  data[pref_bin %in% lb:10]  %>% felm(formula, data = .)  %>%
    broom::tidy(., conf.int = T) %>%
    mutate(time = c(mini:(-2), 0:maxi)) %>%
    select(time, estimate, conf.low, conf.high) %>%
    bind_rows(tibble( time = -1, estimate = 0, conf.low = 0, conf.high = 0 )) %>%
    mutate(band_groups = case_when(
      time < -1 ~ "pre",
      time >= 0 ~ "post",
      time == -1 ~ ""
    ))  -> output
  return(output)
}
ev_study_plotter = function(estimates){
  estimates %>%
    ggplot(aes(x = time, y = estimate)) +
    geom_pointrange(aes(ymin = conf.low, ymax = conf.high, color = band_groups),
                    show.legend = FALSE) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = -0.5, linetype = "dashed") +
    scale_colour_brewer(palette = "Set1") +
    scale_x_continuous(breaks = seq(-10, 10, by = 2)) +
    labs(x = "Relative Time", y = "Estimate") +
    ylim(-.1, .1) +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          axis.title = element_text(size = 18),
          axis.text = element_text(size = 16)) ->
    twfe_event
  return(twfe_event)
}

ev_study = function(lb, data = widedat, formula = form){
  tic()
  est = ev_study_fitter(lb, data, formula)
  toc()
  est %>% ev_study_plotter
}

# %%
# ev_study(5)
# %%
event_studies = seq(0, 8, 2) %>%
  map(., ev_study)
event_studies[[1]] = event_studies[[1]] + labs(title = "Entire Sample", x = "")
event_studies[[2]] = event_studies[[2]] + labs(title = "2nd Decile and Up", x = "")
event_studies[[3]] = event_studies[[3]] + labs(title = "4nd Decile and Up", x = "")
event_studies[[4]] = event_studies[[4]] + labs(title = "6nd Decile and Up", x = "")
event_studies[[5]] = event_studies[[5]] + ggtitle("8nd Decile and Up")
# %%

(twfe_event = event_studies %>%
    wrap_plots(nrow = 2) + plot_annotation(
      title = 'Event Study: coefficient for treated X year in relative time',
      subtitle = 'increasing order of ex-ante forest levels',
    ))

widedat_late = widedat[which(widedat$state == "Jharkhand" | widedat$state == "Maharashtra"), ]
widedat_erly = widedat[which(widedat$state == "Odisha"), ]

ev_study_lt  = function(lb, data = widedat_late, formula = form){
  tic()
  est = ev_study_fitter(lb, data, formula)
  toc()
  est %>% ev_study_plotter
}

ev_study_ey  = function(lb, data = widedat_erly, formula = form){
  tic()
  est = ev_study_fitter(lb, data, formula)
  toc()
  est %>% ev_study_plotter
}

event_studies_lt      = seq(0) %>%
  map(., ev_study_lt)
event_studies_lt[[1]] = event_studies_lt[[1]] + labs(title = "Jharkhand and Maharashtra", x = "")

event_studies_ey      = seq(0) %>%
  map(., ev_study_ey)
event_studies_ey[[1]] = event_studies_ey[[1]] + labs(title = "Odisha", x = "")

event_studies_lt      = event_studies_lt[[1]]
(twfe_event_lt = event_studies_lt %>%
    wrap_plots(nrow = 1) + plot_annotation(
      title = 'Event Study: coefficient for treated X year in relative time'
    ))

event_studies_ey      = event_studies_ey[[1]]
(twfe_event_ey = event_studies_ey %>%
    wrap_plots(nrow = 1) + plot_annotation(
      title = 'Event Study: coefficient for treated X year in relative time'
    ))


# %%
ggsave("C:/Users/sriva/Desktop/GulzarReplication/out/odisha.pdf", twfe_event_ey, device = cairo_pdf,
       width = 15, height = 12)
ggsave("C:/Users/sriva/Desktop/GulzarReplication/out/jhmaha.pdf", twfe_event_lt, device = cairo_pdf,
       width = 15, height = 12)
