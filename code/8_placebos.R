# %% ####################################################
rm(list=ls())
library(LalRUtils)
libreq(data.table, fixest, ggplot2, patchwork, tictoc, RPushbullet)
set.seed(42)
theme_set(lal_plot_theme())
notif = function(x) pbPost("note", x)

# %% ####################################################
dbox_root = '/home/alal/res/india_pesa_forests'
root = dbox_root
data = file.path(root, 'inp')
tmp  = file.path(root, 'tmp')

tic()
load(file.path(tmp, "regdata.rds"))
toc()

# %% subset to primary sample
vcf_data = vcf_data[year>=1990]
vcf_data[, t := year - 1990]
ex_ante_med = quantile(vcf_data$cover_1990, 0.5)
above_med = vcf_data[cover_1990 > ex_ante_med]
above_med[, never_treated := max(D) == 0, cellid]

# %%  timing placebo
# extract upper bound for random timing
ub_by_cell = above_med[, .(ub = first_pesa_exposure[1]), cellid]
# %% function to randomly draw treatment timing in pre-period, re-estimate
permuteTiming = \(){
  # draw random year between 1990 and actual treatment date to switch on treatment
  ub_by_cell[, pseudo_treat := Map(\(ub) sample(1990:ub, 1), ub), cellid]
  # merge it back and compute treatment effect
  dftemp = merge(above_med, ub_by_cell, by = 'cellid')
  # fake treatment timing, real scheduled status
  dftemp[, Dfake := (year >= pseudo_treat) * sch]
  m2 = feols(forest_index ~ Dfake | cellid[t] + styear, data = dftemp, cluster = "blk")
  m2$coefficients
}

# %%
pseudo_effects = mcReplicate(1000, permuteTiming(), mc.cores = 4)

true_effect = 0.36
f1 = ggplot(data.frame(pseudo_effects), aes(x = pseudo_effects)) +
  geom_density() +
  geom_vline(xintercept = true_effect, colour = 'red') +
  xlim(c(NA, 0.4))+
  labs(x = "Placebo effect", y = "Density",
      subtitle = "Placebo treatment timing") +
  annotate(geom = "text", x = 0.35, y = 5, label = "Observed effect",
      color = "blue", angle = 90)
ggsave(file.path(root, "out/placebo_timing_dist.pdf"), f1,
  width = 6, height = 5,
  device = cairo_pdf)

# %% Placebo 2: permute scheduled labels
permuteSched = \(){
  # draw scheduled status with empirical probability of scheduled in state
  sch_prob_by_state = above_med[, sch[1], .(cellid, state)][, schprob := mean(V1), state]
  sch_prob_by_state[, pseudo_sch := Map(\(p) rbinom(1, 1, prob = p), schprob), cellid]
  # merge it back and compute treatment effect
  dftemp = merge(above_med, sch_prob_by_state[, .(cellid, pseudo_sch)], by = 'cellid')
  # real treatment timing, fake scheduled status
  dftemp[, Dfake := (year >= first_pesa_exposure) * pseudo_sch]
  m2 = feols(forest_index ~ Dfake | cellid[t] + styear, data = dftemp, cluster = "blk")
  m2$coefficients
}

pseudo_effects2 = mcReplicate(1000, permuteSched(), mc.cores = 6)

true_effect = 0.36
f2 = ggplot(data.frame(pseudo_effects), aes(x = pseudo_effects)) +
  geom_density() +
  geom_vline(xintercept = true_effect, colour = 'red') +
  xlim(c(NA, 0.4))+
  labs(x = "Placebo effect", y = "Density",
      subtitle = "Placebo scheduled status") +
  annotate(geom = "text", x = 0.35, y = 5, label = "Observed effect",
      color = "blue", angle = 90)

ggsave(file.path(root, "out/placebo_sched_dist.pdf"), f2,
  width = 6, height = 5,
  device = cairo_pdf)

