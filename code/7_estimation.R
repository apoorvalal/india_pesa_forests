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
out  = glue("{root}out/")

# %% export utils
ch.row <- function(name, yesno, format = 'latex') {
  if (format == "latex"){
    return(c(name, ifelse(yesno, "$\\checkmark$", "")))
  } else {
    return(c(name, ifelse(yesno, "✓", "")))
  }
}


########  ########  ######    ######
##     ## ##       ##    ##  ##    ##
##     ## ##       ##        ##
########  ######   ##   ####  ######
##   ##   ##       ##    ##        ##
##    ##  ##       ##    ##  ##    ##
##     ## ########  ######    ######

# %%
tic()
df = import(file.path(root, 'tmp/villages_estimation_sample.rds')) %>% setDT
toc()

# %% main regressions
df2 = df[pref == 1]
df3 = df2[state == "Chhattisgarh" | state == "Jharkhand" | state == "Maharashtra" | state == "Odisha"]

# %% 2wFE
tic()
m00 = felm(def_ha ~ D | village + yr | 0 | village, df2)
toc()
# %% 2wFE + linear time trends
tic()
m01 = felm(def_ha ~ D | village + yr + village:t | 0 | village, df2)
toc()
# %% with effective sample for cols 3, 4
tic()
m002 = felm(def_ha ~ D | village + state*yr | 0 | village, df3)
toc()
# %%
tic()
m003 = felm(def_ha ~ D | village + village:t + styear | 0 | village, df3)
toc()
# %%
dvmean = round(mean(df2$def_ha), 2)
nvill  = nunique(df2$code_2011)
# for state-year FE specification
dvmean2 = round(mean(df3$def_ha), 2)
nvill2  = nunique(df3$code_2011)
# %% export
mods = list(m00, m01, m002, m003)
# table 1
stargazer(mods)

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
        c("Dep. Var. Mean", rep(dvmean, 2), rep(dvmean2, 2)),
        c("N. Villages", rep(nvill, 2), rep(nvill2, 2))
        ),
        out = file.path(out, 'fe_estimates_village.tex'))

# %%
########  ##    ##  ######  ########    ###    ######## ########
##     ##  ##  ##  ##    ##    ##      ## ##      ##    ##
##     ##   ####   ##          ##     ##   ##     ##    ##
########     ##     ######     ##    ##     ##    ##    ######
##     ##    ##          ##    ##    #########    ##    ##
##     ##    ##    ##    ##    ##    ##     ##    ##    ##
########     ##     ######     ##    ##     ##    ##    ########

CH = df3[state == "Chhattisgarh"]
tic()
m_CH = felm(def_ha ~ D | village + village:t + styear | 0 | village, CH)
toc()
# %%
tic()
m_MH = felm(def_ha ~ D | village + village:t + styear | 0 | village, MH)
toc()
# %%
OR = df3[state == "Odisha"]
tic()
m_OR = felm(def_ha ~ D | village + village:t + styear | 0 | village, OR)
toc()
# %%
JH = df3[state == "Jharkhand"]
tic()
m_JH = felm(def_ha ~ D | village + village:t + styear | 0 | village, JH)
toc()
# %%
NJH = df3[state != "Jharkhand"]
tic()
m_NJH = felm(def_ha ~ D | village + village:t + styear | 0 | village, NJH)
toc()
# %%
# st_mods = list(m_CH, m_JH, m_MH, m_OR)
st_mods = list(m_JH, m_NJH)
# table by state
dvmean_JH = round(mean(JH$def_ha), 2)
nvill_JH  = nunique(JH$code_2011)
dvmean_NJH = round(mean(NJH$def_ha), 2)
nvill_NJH  = nunique(NJH$code_2011)

# %%
dvmean_CH = round(mean(CH$def_ha), 2)
nvill_CH  = nunique(CH$code_2011)
dvmean_MH = round(mean(MH$def_ha), 2)
nvill_MH  = nunique(MH$code_2011)
dvmean_OR = round(mean(OR$def_ha), 2)
nvill_OR  = nunique(OR$code_2011)

# %%
stargazer(st_mods, keep.stat = c("N"),
          covariate.labels = c("Scheduled X PESA"),
          dep.var.labels = c("Annual Deforestation in Hectares"),
          column.labels = c("Jharkhand", "CH/MH/OR"),
          title = "Main Effects in 4 treated states separated by Jharkahnd and Chhattisgarh, Maharashtra, and Odisha",
          model.names = F,
          label = "table:regres_by_state",
          style = "apsr", type = 'latex',
          notes = "Cluster-Robust Standard Errors (by village)",
          add.lines = list(
        ch.row('Village FE',      c(T,T), format = 'latex'),
        ch.row('Village TT',      c(T,T), format = 'latex'),
        ch.row('State X Year FE', c(T,T), format = 'latex'),
            c("Dep. Var. Mean", dvmean_JH, dvmean_NJH),
            c("N. Villages",     nvill_JH,  nvill_NJH)
          ), out = file.path(out, 'fe_estimates_village_state.tex'))

# %%
.082/0.08
0.063/.21
# stargazer(st_mods, keep.stat = c("N"),
#           covariate.labels = c("Scheduled X PESA"),
#           dep.var.labels = c("Annual Deforestation in Hectares"),
#           column.labels = c("Chhattisgarh", "Jharkhand", "Maharashtra", "Odisha"),
#           title = "Main Effects by State",
#           model.names = F,
#           label = "table:regres_by_state",
#           style = "apsr", type = 'latex',
#           notes = "Cluster-Robust Standard Errors (by village)",
#           add.lines = list(
#         ch.row('Village FE',      c(T,T,T,T), format = 'latex'),
#         ch.row('Village TT',      c(T,T,T,T), format = 'latex'),
#         ch.row('State X Year FE', c(T,T,T,T), format = 'latex'),
#             c("Dep. Var. Mean", dvmean_CH, dvmean_JH, dvmean_MH, dvmean_OR),
#             c("N. Villages",    nvill_CH,  nvill_JH,  nvill_MH, nvill_OR)
#           ), out = file.path(out, 'fe_estimates_village_state.tex'))
#




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
