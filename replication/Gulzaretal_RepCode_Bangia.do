********** GULZAR REPLICATION ***********
*
* Code for slides 10-15
*
* Sachet Bangia 120620
*****************************************

** Exploring the data

// keep if pref
// tab sch
// /*
//         sch |      Freq.     Percent        Cum.
// ------------+-----------------------------------
//           0 |    626,212       69.80       69.80
//           1 |    270,980       30.20      100.00
// ------------+-----------------------------------
//       Total |    897,192      100.00
//
// */
// gen schpesa = sch & pesa_exposure
//
// //Unexpected: deforestation happening in a very small subsample each year
// gen def_0 = def_ha==0
// tab def_0 t
// /*
// tab def_0
//
//       def_0 |      Freq.     Percent        Cum.
// ------------+-----------------------------------
//           0 |    129,863       14.47       14.47
//           1 |    767,329       85.53      100.00
// ------------+-----------------------------------
//       Total |    897,192      100.00
// */
//
// egen tag_vill = tag(village)
// egen mean_zero = mean(def_0), by(village)
//
// /*
// tab mean_zero if tag_vill
//
//   mean_zero |      Freq.     Percent        Cum.
// ------------+-----------------------------------
//           0 |        207        0.39        0.39
//    .0588235 |        262        0.50        0.89
//    .1176471 |        295        0.56        1.45
//    .1764706 |        385        0.73        2.18
//    .2352941 |        488        0.92        3.10
//    .2941177 |        572        1.08        4.19
//    .3529412 |        692        1.31        5.50
//    .4117647 |        804        1.52        7.02
//    .4705882 |        954        1.81        8.83
//    .5294118 |      1,146        2.17       11.00
//    .5882353 |      1,365        2.59       13.59
//    .6470588 |      1,621        3.07       16.66
//    .7058824 |      1,824        3.46       20.11
//    .7647059 |      2,381        4.51       24.62
//    .8235294 |      3,274        6.20       30.83
//    .8823529 |      4,845        9.18       40.01
//    .9411765 |      8,286       15.70       55.71
//           1 |     23,375       44.29      100.00
// ------------+-----------------------------------
//       Total |     52,776      100.00
// */
//
// replace all_zero = mean_zero == 1
//
// /*
//
// tab all_zero if tag_vill
//
//    all_zero |      Freq.     Percent        Cum.
// ------------+-----------------------------------
//           0 |     29,401       55.71       55.71
//           1 |     23,375       44.29      100.00
// ------------+-----------------------------------
//       Total |     52,776      100.00
//
// */
//
// //degree 10 polynomial:
//
// gen pref_mean2 = pref_mean*pref_mean
// gen pref_mean3 = pref_mean2*pref_mean
// gen pref_mean4 = pref_mean3*pref_mean
// gen pref_mean5 = pref_mean4*pref_mean
// gen pref_mean6 = pref_mean5*pref_mean
// gen pref_mean7 = pref_mean6*pref_mean
// gen pref_mean8 = pref_mean7*pref_mean
// gen pref_mean9 = pref_mean8*pref_mean
// gen pref_mean10 = pref_mean9*pref_mean
//
// label def sch 0 "Non-scheduled" 1 "Scheduled"
// label def exp 0 "Not exposed" 1 "Exposed"
// label values sch sch
// label values pesa_exposure exp
//
// //
// preserve
// keep if t==1
// reg def_0 i.sch##i.pesa_exposure##c.pref_mea*  //played around with def_0/all_zero/def_ha as outcomes
// predict yhat
// scatter yhat pref_mean, by(sch pesa_exposure)
// restore

********** R Code:
// I edited the main table code to (1) change outcome to binary 0/1 deforestation, and/or (2) subset the data to run one state at a time.

# %% Main regressions w sample restricted to 4 states for columns 3 and 4
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
# %% Villge + state X year FEs
tic()
m02 = felm(def_ha ~ D | village + state*yr | 0 | village, df3)
toc()
# %%
tic()
m03 = felm(def_ha ~ D | village + village:t + styear | 0 | village, df3)
toc()

# %%
dvmean = round(mean(df2$def_ha), 2)
nvill  = nunique(df2$code_2011)
dvmean2 = round(mean(df3$def_ha), 2)
nvill2  = nunique(df3$code_2011)
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
        c("Dep. Var. Mean", rep(dvmean, 2), rep(dvmean2, 2)),
        c("N. Villages", rep(nvill, 2), rep(nvill2, 2))
        ),
        out = file.path(out, 'fe_estimates_village.tex'))

# %% Main regressions by state
df2 = df[pref == 1]
Chhattisgarh = df2[state=="Chhattisgarh"]
Jharkhand = df2[state=="Jharkhand"]
Maharashtra = df2[state=="Maharashtra"]
Odisha = df2[state=="Odisha"]

# %% 2wFE
m2 = felm(def_ha ~ D | village + village:t + styear | 0 | village, Chhattisgarh)
m5 = felm(def_ha ~ D | village + village:t + styear | 0 | village, Jharkhand)
m7 = felm(def_ha ~ D | village + village:t + styear | 0 | village, Maharashtra)
m8 = felm(def_ha ~ D | village + village:t + styear | 0 | village, Odisha)


dvmean2 = round(mean(Chhattisgarh$def_ha), 2)
nvill2  = nunique(Chhattisgarh$code_2011)
dvmean5 = round(mean(Jharkhand$def_ha), 2)
nvill5  = nunique(Jharkhand$code_2011)
dvmean7 = round(mean(Maharashtra$def_ha), 2)
nvill7  = nunique(Maharashtra$code_2011)
dvmean8 = round(mean(Odisha$def_ha), 2)
nvill8  = nunique(Odisha$code_2011)

# %% export
mods = list(m2, m5, m7, m8)

# table 1 by state
stargazer(mods, keep.stat = c("N"),
          covariate.labels = c("Scheduled X PESA"),
          dep.var.labels = c("Annual Deforestation in Hectares"),
          style = "apsr", type = 'latex',
          column.sep.width = "0pt",
          object.names = TRUE,
          column.labels = c("Chhattisgarh", "Jharkhand", "Maharashtra", "Odisha"),
          title = "Main Effects by State",
          model.names = F,
          label = "table:regres",
          notes = "Cluster-Robust Standard Errors (by village)",
          add.lines = list(
            c("Dep. Var. Mean", dvmean2, dvmean5, dvmean7, dvmean8),
            c("N. Villages", nvill2, nvill5, nvill7, nvill8)
          ),
          out = file.path(out, 'fe_estimates_village_byState.tex'))

#####

# Main table with binary outcome

df2$def_0 <- df2$def_ha==0
# %% 2wFE
tic()
m00 = felm(def_0 ~ D | village + yr | 0 | village, df2)
toc()
# %% 2wFE + linear time trends
tic()
m01 = felm(def_0 ~ D | village + yr + village:t | 0 | village, df2)
toc()
# %% Villge + state X year FEs
tic()
m02 = felm(def_0 ~ D | village + state*yr | 0 | village, df2)
toc()
# %%
tic()
m03 = felm(def_0 ~ D | village + village:t + styear | 0 | village, df2)
toc()

# %%
dvmean = round(mean(df2$def_0), 2)
nvill  = nunique(df2$code_2011)
dvmean2 = round(mean(df3$def_0), 2)
nvill2  = nunique(df3$code_2011)

# %% export
mods = list(m00, m01, m02, m03)

# table 1 with binary outcome
stargazer(mods, keep.stat = c("N"),
          covariate.labels = c("Scheduled X PESA"),
          dep.var.labels = c("Indicator for no deforestation"),
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
          out = file.path(out, 'fe_estimates_village_dummy.tex'))


# %% Binary outcome by state
Chhattisgarh = df2[state=="Chhattisgarh"]
Jharkhand = df2[state=="Jharkhand"]
Maharashtra = df2[state=="Maharashtra"]
Odisha = df2[state=="Odisha"]

# %% 2wFE
m2 = felm(def_0 ~ D | village + village:t + styear | 0 | village, Chhattisgarh)
m5 = felm(def_0 ~ D | village + village:t + styear | 0 | village, Jharkhand)
m7 = felm(def_0 ~ D | village + village:t + styear | 0 | village, Maharashtra)
m8 = felm(def_0 ~ D | village + village:t + styear | 0 | village, Odisha)


dvmean2 = round(mean(Chhattisgarh$def_0), 2)
nvill2  = nunique(Chhattisgarh$code_2011)
dvmean5 = round(mean(Jharkhand$def_0), 2)
nvill5  = nunique(Jharkhand$code_2011)
dvmean7 = round(mean(Maharashtra$def_0), 2)
nvill7  = nunique(Maharashtra$code_2011)
dvmean8 = round(mean(Odisha$def_0), 2)
nvill8  = nunique(Odisha$code_2011)

# %% export
mods = list(m2, m5, m7, m8)

# table 1 with binary outcome by state
stargazer(mods, keep.stat = c("N"),
          covariate.labels = c("Scheduled X PESA"),
          dep.var.labels = c("Indicator for no deforestation"),
          style = "apsr", type = 'latex',
          column.sep.width = "0pt",
          object.names = TRUE,
          column.labels = c("Chhattisgarh", "Jharkhand", "Maharashtra", "Odisha"),
          title = "Main Effects by State",
          model.names = F,
          label = "table:regres",
          notes = "Cluster-Robust Standard Errors (by village)",
          add.lines = list(
            c("Dep. Var. Mean", dvmean2, dvmean5, dvmean7, dvmean8),
            c("N. Villages", nvill2, nvill5, nvill7, nvill8)
          ),
          out = file.path(out, 'fe_estimates_village_dummy_byState.tex'))

#
