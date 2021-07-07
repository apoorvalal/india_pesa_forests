#%%
rm(list = ls())
library(LalRUtils)
libreq(tidyverse, magrittr, rio, lfe, data.table, texreg, knitr, stargazer, broom,
      fastDummies, janitor, patchwork, tictoc, fect, panelView, PanelMatch, fixest,
      arrow, interflex)
theme_set(lal_plot_theme())

root = "../"
out = '../out'
data = paste0(root, 'inp')
options(repr.plot.width=12, repr.plot.height=9)

#%% ####################################################
######## #### ##     ## #### ##    ##  ######
   ##     ##  ###   ###  ##  ###   ## ##    ##
   ##     ##  #### ####  ##  ####  ## ##
   ##     ##  ## ### ##  ##  ## ## ## ##   ####
   ##     ##  ##     ##  ##  ##  #### ##    ##
   ##     ##  ##     ##  ##  ##   ### ##    ##
   ##    #### ##     ## #### ##    ##  ######
tic()
dat_est = import('../inp/villages_estimation_sample.rds')
toc()
#%%
dat_est %>% nrow
# dedupe
dat_est = dat_est[, .SD[1], by = .(code_2011, year)]
dat_est = dat_est[code_2011 != '']
#%%
dat = dat_est[, .SD[1], by = .(state_ut, year)]
####################################################
# %% fig 3
dat[,def_ha  :=  def * 0.09]

(p = panelView(def_ha ~ pesa_exposure,
              legend.labs = c("Pre PESA election", "Post PESA election"),
  data = dat, index = c("state_ut","year"), xlab = "Year", ylab = "State", by.timing = T,
  main = "", theme.bw = T, background = 'white')
  # main = 'PESA Adoption Timing', by.treatment = TRUE)
)

# %%
ggsave(file.path(out, 'PESA_adopt2.pdf'), p, device = cairo_pdf, width = 10, height = 6)
# %%



######## ##     ## ######## ##    ## ########
##       ##     ## ##       ###   ##    ##
##       ##     ## ##       ####  ##    ##
######   ##     ## ######   ## ## ##    ##
##        ##   ##  ##       ##  ####    ##
##         ## ##   ##       ##   ###    ##
########    ###    ######## ##    ##    ##
#%% event time dummies
tic()
df = import("../inp/est_clean2.rds") %>% setDT
toc()
# %%
n = colnames(df)
n %>% grep('dist_to', .) %>% n[.] -> dropcols
df[, (dropcols) := NULL]
# %%
df = df[state_ut %in% c("Odisha", "Chhattisgarh", "Maharashtra", "Jharkhand")]
df[,
  treat_year :=  case_when(
     state_ut == 'Chhattisgarh'     ~ 2005,
     state_ut == 'Jharkhand'        ~ 2010,
     state_ut == 'Maharashtra'      ~ 2007,
     state_ut == 'Odisha'           ~ 2002)]
# %%
###     ######    ######
## ##   ##    ##  ##    ##
##   ##  ##        ##
##     ## ##   #### ##   ####
######### ##    ##  ##    ##
##     ## ##    ##  ##    ##
##     ##  ######    ######

# %% fig 4
pref_samp = widedat[pref == 1]
pref_samp[, rel_year := year - treat_year]
pref_samp %>% glimpse
# %%
summaries = pref_samp[, .(avg = mean(def_ha, na.rm = T)), by = .(rel_year, sch)]
summaries[, group_avg := mean(avg), by = sch]
summaries[, demeaned_avg := avg - group_avg]
summaries[, sch2 := ifelse(sch == 0, "Non-Scheduled", "Scheduled")]

(summaries [rel_year %in% -5:5] %>%
  ggplot(
    aes(x = rel_year, y = demeaned_avg, colour = as.factor(sch2))) +
  geom_point() +
  geom_smooth(data = summaries[rel_year %in% -5:-1], size = 0.5, method = 'lm', se = F) +
  geom_smooth(data = summaries[rel_year %in% 0:5],   size = 0.5, method = 'lm', se = F) +
  scale_colour_brewer(palette = "Set1") +
  geom_vline(xintercept = -0.5, linetype = 'dotted', alpha = 0.6) +
  scale_x_continuous(breaks = -5:5) +
  labs(title = "",
      y = "Residual Deforestation", x = "Event Time",
      colour = "") -> agg_trends
)

# %%
ggsave(file.path(out, 'levels_time_trends.pdf'), agg_trends, device = cairo_pdf,
  width = 10, height = 8)

# %%
  ######## ##     ##  ######  ######## ##     ## ########
  ##       ##     ## ##    ##    ##    ##     ## ##     ##
  ##       ##     ## ##          ##    ##     ## ##     ##
  ######   ##     ##  ######     ##    ##     ## ##     ##
  ##        ##   ##        ##    ##    ##     ## ##     ##
  ##         ## ##   ##    ##    ##    ##     ## ##     ##
  ########    ###     ######     ##     #######  ########

#%% event time dummies
tic()
df = import("../tmp/est_clean2.rds") %>% setDT
toc()
# %%
n = colnames(df)
n %>% grep('dist_to', .) %>% n[.] -> dropcols
df[, (dropcols) := NULL]
# %%
df[,
  treat_year :=  case_when(
     state_ut == 'Andhra Pradesh'   ~ 2001,
     state_ut == 'Chhattisgarh'     ~ 2005,
     state_ut == 'Gujarat'          ~ 2001,
     state_ut == 'Himachal Pradesh' ~ 2000,
     state_ut == 'Jharkhand'        ~ 2010,
     state_ut == 'Madhya Pradesh'   ~ 2000,
     state_ut == 'Maharashtra'      ~ 2007,
     state_ut == 'Odisha'           ~ 2002,
     state_ut == 'Rajasthan'        ~ 2000,
     TRUE ~ 0)
]

df[, `:=`(
  vilf = as.factor(code_2011),
  yf   = as.factor(year) )]


# %% subset to pref # %% create dummies
df[, lag := if_else(sch == 1, year - treat_year, 0)]
df %>% dummy_cols(select_columns = "lag",
                       ignore_na = TRUE) -> widedat
lagcols = names(widedat) %>% str_subset("lag")
# %%
widedat %>% glimpse

# %% fig 6
mini = -6
maxi = 6
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

# %%
ggsave(file.path(out, 'Dyn_treat_eff_big.pdf'), twfe_event, device = cairo_pdf,
  width = 15, height = 12)
#######################################################################

# %%
######## ########     ###
##       ##     ##   ## ##
##       ##     ##  ##   ##
######   ########  ##     ##
##       ##   ##   #########
##       ##    ##  ##     ##
##       ##     ## ##     ##
df2 = copy(df)
# %%
(form <- as.formula(paste0("def_ha ~ ", indics, "| vilf + styear | 0 | nameb")))

# %% FRA label
# event study around FRA
df2[, lag := if_else(sch == 1, year - 2008, 0)]
df2 %>% dummy_cols(select_columns = "lag", ignore_na = TRUE) -> wide_df2
lagcols = names(wide_df2) %>% str_subset("lag")
mini = -6; maxi = 6
indics <- paste0(c(paste0("`lag_", seq(mini, -2), "`"),
                   paste0("lag_", seq(0, maxi))),
                 collapse = " + ")
# %%

# %%
FRA_evstudy = ev_study(5, wide_df2)
FRA_evstudy

# %%
wide_df2$state_ut %>% tabyl

(fra_ev_or = ev_study(5, wide_df2[state_ut == "Odisha"]))
(fra_ev_jh = ev_study(5, wide_df2[state_ut == "Jharkhand"]))
(fra_ev_ch = ev_study(5, wide_df2[state_ut == "Chhattisgarh"]))
(fra_ev_mh = ev_study(5, wide_df2[state_ut == "Maharashtra"]))
(fra_ev_mp = ev_study(5, wide_df2[state_ut == "Madhya Pradesh"]))
(fra_ev_hp = ev_study(5, wide_df2[state_ut == "Himachal Pradesh"]))
# %%
(fra_plots = (fra_ev_or + ggtitle("Orissa") | fra_ev_jh + ggtitle("Jharkhand")) /
  (fra_ev_or + ggtitle("Chhattisgarh") | fra_ev_mh + ggtitle("Maharashtra")) /
  (fra_ev_mp + ggtitle("Madhya Pradesh") | fra_ev_hp + ggtitle("Himachal Pradesh")) +
  plot_annotation(title = 'Event Study plots: Forest Rights Act (2008)')
)

# %%
ggsave(file.path(out, 'fra_event_studies.pdf'), fra_plots, device = cairo_pdf,
  width = 15, height = 12)
# %%
