*********************************
**** Post-hoc power analysis ****
*********************************
use "Z:\Projects\R Projects\transparency_submission\4 results_prepared.dta", clear

* Parameters for Power calculation observed in experiment
* Overall sample size N: 214
* Observed means and standard errors: see Table 2 (Manuscript)
* 1.67 (2.68), 3.24 (3.21), 2.49 (2.95), 2.92 (3.19), 2.85 (2.95)
* Observed n per experimental group: see Table 2 (Manuscript)
* 45, 46, 43, 39, 41

* Parameters for Power calculation assumed
* alpha: 0.05
* beta: 0.80

**** Finding 1: There is a default effect on contributions for a default, a default with added purpose, as well as for a default with both types of transparency.
** Calculate POWER for given sample size, effect size, and alpha
power twomeans 1.67 3.24, sd1(2.68) sd2(3.21) n1(45) n2(46) // 0.71
power twomeans 1.67 2.49, sd1(2.68) sd2(2.95) n1(45) n2(43) // 0.27
power twomeans 1.67 2.92, sd1(2.68) sd2(3.19) n1(45) n2(39) // 0.48
power twomeans 1.67 2.85, sd1(2.68) sd2(2.95) n1(45) n2(41) // 0.48

** Calculate MINIMUM DETECTABLE EFFECT SIZE for given sample size, alpha, and beta
power twomeans 1.67, sd1(2.68) sd2(3.21) power(0.8) n1(45) n2(46) // m2 = 3.42
power twomeans 1.67, sd1(2.68) sd2(2.95) power(0.8) n1(45) n2(43) // m2 = 3.38
power twomeans 1.67, sd1(2.68) sd2(3.19) power(0.8) n1(45) n2(39) // m2 = 3.51
power twomeans 1.67, sd1(2.68) sd2(2.95) power(0.8) n1(45) n2(41) // m2 = 3.40

** Calculate REQUIRED SAMPLE SIZE (in experimental group) for given effect size, alpha, and beta
power twomeans 1.67 3.24, sd1(2.68) sd2(3.21) n1(45) compute(n2) // n2 = 70
power twomeans 1.67 2.49, sd1(2.68) sd2(2.95) n1(45) compute(n2) // n2 = not achievable
power twomeans 1.67 2.92, sd1(2.68) sd2(3.19) n1(45) compute(n2) // n2 = 304
power twomeans 1.67 2.85, sd1(2.68) sd2(2.95) n1(45) compute(n2) // n2 = 789


**** Findings 2-4: Omnibus hypothesis (excluding the Control group)
kwallis Contribution, by(TreatmentnoCf) // p = 0.773
oneway Contribution TreatmentnoCf // within-groups variance 9.4849

** Calculate POWER for given sample size, effect size, and alpha
power oneway 3.24 2.49 2.92 2.85, alpha (0.05) varerror(9.48) ngroups(4) n1(46) n2(43) n3(39) n4(41) // power = 0.1376

** Calculate MINIMUM DETECTABLE EFFECT SIZE for given sample size, alpha, and beta
power oneway, varerror(9.48) ngroups(4) alpha(0.05) power(0.8) n1(46) n2(43) n3(39) n4(41) // delta = 0.2570, Var_m = 0.6262

** Calculate REQUIRED SAMPLE SIZE for given effect size, alpha, and beta
power oneway 3.24 2.49 2.92 2.85, alpha(0.05) power(0.8) varerror(9.48) // N = 1460, N per group = 365

