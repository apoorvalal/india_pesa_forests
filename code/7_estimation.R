# %% ####################################################
rm(list = ls())
library(LalRUtils)
LalRUtils::libreq(tidyverse, data.table, glue, tictoc,
                  lfe, rio, foreach, magrittr, janitor,
                  stargazer2, modelsummary, texreg)
theme_set(lal_plot_theme())
options(repr.plot.width=12, repr.plot.height=9)
set.seed(42)
# %% ####################################################
root = "../"
data = glue("{root}inp")
out  = "../out/"

# %% export utils
ch.row <- function(name, yesno, format = 'latex') {
  if (format == "latex"){
    return(c(name, ifelse(yesno, "$\\checkmark$", "")))
  } else {
    return(c(name, ifelse(yesno, "✓", "")))
  }
}


########  ########  ######## ########
##     ## ##     ## ##       ##     ##
##     ## ##     ## ##       ##     ##
########  ########  ######   ########
##        ##   ##   ##       ##
##        ##    ##  ##       ##
##        ##     ## ######## ##
#  run once , write out to binary for analysis

# %% read in data
tic()
df = import(file.path(data, 'villages_estimation_sample.rds')) %>% setDT
toc()
df %>% glimpse

# %%

# %% identifiers
df[, dist := as.factor(district)][,
     block := as.factor(nameb)][,
     sub_dist2 := as.factor(sub_dist)][,
     state := as.factor(state_ut)][,
     village := as.factor(code_2011)]
# state X year FE
df[, styear := .GRP, by =  .(state, year)]

# %% outcome and time variables prep
df[, def_ha := def * 0.09][,
  D         := sch * pesa_exposure][,
  t         := year - 2000][,
  t2        := t^2]

# %% demographic vars
df[, tot_non_sc_st := tot_pop - (tot_sc + tot_st)][,
     st_share      := tot_st/tot_pop][,
     st_plurality  := fifelse(tot_st > max(tot_sc, tot_non_sc_st), 1, 0)][,
     D_X_st_plurality := D * st_plurality]

# %% pre-period forest cover deciles and main analysis cutoff
df[, pref_bin := ntile(pref_mean, 10)]
# cutoff for 2%
df[, pref := fifelse(pref_mean >= 2, 1, 0)]


df$yr = as.factor(df$year)
# write out intermediate file
saveRDS(df, file.path(root, 'tmp/est_clean2.rds'))

# %%

########  ########  ######    ######
##     ## ##       ##    ##  ##    ##
##     ## ##       ##        ##
########  ######   ##   ####  ######
##   ##   ##       ##    ##        ##
##    ##  ##       ##    ##  ##    ##
##     ## ########  ######    ######

# %%
tic()
df = import(file.path(root, 'tmp/est_clean2.rds')) %>% setDT
toc()

# %% main regressions
df2 = df[pref == 1]

# %% 2wFE
tic()
m00 = felm(def_ha ~ D | village + yr | 0 | village, df2)
toc()
# %% 2wFE + linear time trends
tic()
m01 = felm(def_ha ~ D | village + yr + village:t | 0 | village, df2)
toc()
# %% Villge + state X year FEs
tic()
m02 = felm(def_ha ~ D | village + state*yr | 0 | village, df2)
toc()
# %%
tic()
m03 = felm(def_ha ~ D | village + village:t + styear | 0 | village, df2)
toc()

# %%
dvmean = round(mean(df2$def_ha), 2)
nvill  = nunique(df2$code_2011)
# %% export
mods = list(m00, m01, m02, m03)

# table 1
stargazer(mods, keep.stat = c("N"),
        covariate.labels = c("Scheduled X PESA"),
        dep.var.labels = c("Annual Deforestation in Hectares"),
        style = "apsr", type = 'latex',
        column.sep.width = "0pt",
        title = "Main Effects (Difference in Differences)",
        model.names = F,
        label = "table:regres",
        notes = "Cluster-Robust Standard Errors (by village)",
        add.lines = list(
        ch.row('Village FE',      c(T,T,T,T), format = 'latex'),
        ch.row('Year FE',         c(T,T,F,F), format = 'latex'),
        ch.row('Village TT',      c(F,T,F,T), format = 'latex'),
        ch.row('State X Year FE', c(F,F,T,T), format = 'latex'),
        c("Dep. Var. Mean", rep(dvmean, 4)),
        c("N. Villages", rep(nvill, 4))
        ),
        out = file.path(out, 'fe_estimates_village.tex'))

########   #######  ########  ##     ##  ######  ########
##     ## ##     ## ##     ## ##     ## ##    ##    ##
##     ## ##     ## ##     ## ##     ## ##          ##
########  ##     ## ########  ##     ##  ######     ##
##   ##   ##     ## ##     ## ##     ##       ##    ##
##    ##  ##     ## ##     ## ##     ## ##    ##    ##
##     ##  #######  ########   #######   ######     ##


# %% cluster by block
# %% 2wFE
tic()
m10 = felm(def_ha ~ D | village + yr | 0 | block, df2)
toc()
# %% 2wFE + linear time trends
tic()
m11 = felm(def_ha ~ D | village + yr + village:t | 0 | block, df2)
toc()
# %% Villge + state X year FEs
tic()
m12 = felm(def_ha ~ D | village + state*yr | 0 | block, df2)
toc()
# %%
tic()
m13 = felm(def_ha ~ D | village + village:t + styear | 0 | block, df2)
toc()

# %% table A3
mods1 = list(m10, m11, m12, m13)
stargazer(mods1, keep.stat = c("N"),
        covariate.labels = c("Scheduled X PESA"),
        dep.var.labels = c("Annual Deforestation in Hectares"),
        style = "apsr",
        column.sep.width = "0pt",
        title = "Village level regressions clustered at block level",
        model.names = F,
        label = "table:regres2",
        notes = "Cluster-Robust Standard Errors (by block)",
        add.lines = list(
          ch.row('Village FE',      c(T,T,T,T), format = 'latex'),
          ch.row('Year FE',         c(T,T,F,F), format = 'latex'),
          ch.row('Village TT',      c(F,T,F,T), format = 'latex'),
          ch.row('State X Year FE', c(F,F,T,T), format = 'latex'),
          c("Dep. Var. Mean", rep(dvmean, 4)),
          c("N. Villages", rep(nvill, 4))
        ),
        type = 'latex', out = file.path(out, 'fe_estimates_block.tex'))

# %% full sample
tic()
m20 = felm(def_ha ~ D | village + yr | 0 | block, df)
toc()
# %% 2wFE + linear time trends
tic()
m21 = felm(def_ha ~ D | village + yr + village:t | 0 | block, df)
toc()
# %% Villge + state X year FEs
tic()
m22 = felm(def_ha ~ D | village + state*yr | 0 | block, df)
toc()
# %%
tic()
m23 = felm(def_ha ~ D | village + village:t + styear | 0 | block, df)
toc()

# %% table A4
dvmean = round(mean(df$def_ha), 2)
nvill  = nunique(df$code_2011)

mods1 = list(m20, m21, m22, m23)
stargazer(mods1, keep.stat = c("N"),
        covariate.labels = c("Scheduled X PESA"),
        dep.var.labels = c("Annual Deforestation in Hectares"),
        style = "apsr",
        column.sep.width = "0pt",
        title = "Village level regressions for full sample (including villages with no ex-ante forest cover)",
        model.names = F,
        label = "table:fullsamp",
        notes = "Cluster-Robust Standard Errors (by block)",
        add.lines = list(
          ch.row('Village FE',      c(T,T,T,T), format = 'latex'),
          ch.row('Year FE',         c(T,T,F,F), format = 'latex'),
          ch.row('Village TT',      c(F,T,F,T), format = 'latex'),
          ch.row('State X Year FE', c(F,F,T,T), format = 'latex'),
          c("Dep. Var. Mean", rep(dvmean, 4)),
          c("N. Villages", rep(nvill, 4))
        ),
        type = 'latex', out = file.path(out, 'fe_full_estimates_village.tex'))

# %%

# %%
tic()
df = import(file.path(data, 'Intermediate/est_clean2.rds')) %>% setDT
toc()


fitter = function(cutoff){
  dat = df[pref_bin >= cutoff]
  m = felm(def_ha ~ D | village + village:t + styear | 0 | village, dat)
  summary(m)$coefficients[, 1:2]
}

# %%
tic()
cutoff_res = map(1:10, fitter)
toc()

# %% fig 5
ests = rbindlist(lapply(cutoff_res, as.data.frame.list)) %>% setDT
ests$n = 1:10
ests[, col := ifelse(n == 5, 'red', 'blue')]
colnames(ests)[1:2] = c('beta', 'se')



(p = ggplot(ests, aes(n, beta, group = 1)) +
  geom_point(aes(colour = as.factor(col)), size = 3) +
  geom_line(linetype = "dotted") +
  geom_errorbar(aes(ymax = beta + 1.96 * se,
    ymin = beta - 1.96 * se), colour = '#556B2F', alpha = 0.6, width = 0) +
  geom_hline(yintercept = 0) +
  scale_x_continuous(breaks = 1:10) +
  scale_colour_brewer(palette = 'Set1') +
  labs(title = '',
   y = "PESA Effect", x = "Cutoff Decile",
  caption = '') + theme(legend.position = 'none')
)

ggsave(file.path(out, 'varying_cutoff2.pdf'), p,
  width = 10, height = 6, device = cairo_pdf)

# %%
