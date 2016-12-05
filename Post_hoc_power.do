*********************************
**** Post-hoc power analysis ****
*********************************
use "Z:\Projects\Transparency\Submission\Data\4 results_prepared.dta", clear

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
** Control vs. Default
reg Contribution Def DefInf DefPur DefInfPur i.ImportanceD i.Genderf c.Age i.PastParticipationf i.EU_ETS_Useful, robust // r2f = 0.1739
reg Contribution DefInf DefPur DefInfPur i.ImportanceD i.Genderf c.Age i.PastParticipationf i.EU_ETS_Useful, robust // r2r = 0.1523
** Calculate POWER for given sample size, effect size, and alpha
powerreg, r2f(0.1739) r2r(0.1523) alpha(0.05) nvar(6) n(214) // Taking estimates from OLS linear Regression
* power = 0.6534 (p = 0.009)

** Calculate REQUIRED SAMPLE SIZE for given effect size, alpha, and beta
powerreg, r2f(0.1739) r2r(0.1523) alpha(0.05) nvar(6) // Taking estimates from OLS linear Regression
* n = 304

** Control vs. Default+Info
reg Contribution Def DefPur DefInfPur i.ImportanceD i.Genderf c.Age i.PastParticipationf i.EU_ETS_Useful, robust // r2r = 0.1717
** Calculate POWER for given sample size, effect size, and alpha
powerreg, r2f(0.1739) r2r(0.1717) alpha(0.05) nvar(6) n(214) // Taking estimates from OLS linear Regression
* power = 0.1168 (p = 0.237)

** Calculate REQUIRED SAMPLE SIZE for given effect size, alpha, and beta
powerreg, r2f(0.1739) r2r(0.1717) alpha(0.05) nvar(6) // Taking estimates from OLS linear Regression
* n = 2944

** Control vs. Default+Purpose
reg Contribution Def DefInf DefInfPur i.ImportanceD i.Genderf c.Age i.PastParticipationf i.EU_ETS_Useful, robust // r2r = 0.1521
** Calculate POWER for given sample size, effect size, and alpha
powerreg, r2f(0.1739) r2r(0.1521) alpha(0.05) nvar(6) n(214) // Taking estimates from OLS linear Regression
* power = 0.6574 (p = 0.006)

** Calculate REQUIRED SAMPLE SIZE for given effect size, alpha, and beta
powerreg, r2f(0.1739) r2r(0.1521) alpha(0.05) nvar(6) // Taking estimates from OLS linear Regression
n = 296

** Control vs. Default+Info+Purpose
reg Contribution Def DefInf DefPur i.ImportanceD i.Genderf c.Age i.PastParticipationf i.EU_ETS_Useful, robust // r2r = 0.1591
** Calculate POWER for given sample size, effect size, and alpha
powerreg, r2f(0.1739) r2r(0.1591) alpha(0.05) nvar(6) n(214) // Taking estimates from OLS linear Regression
* power = 0.4957 (p = 0.034)

** Calculate REQUIRED SAMPLE SIZE for given effect size, alpha, and beta
powerreg, r2f(0.1739) r2r(0.1591) alpha(0.05) nvar(6) // Taking estimates from OLS linear Regression
n = 440


**** Findings 2-4: Omnibus hypothesis
kwallis Contribution, by(TreatmentnoCf) // p = 0.773
oneway Contribution TreatmentnoCf // p = 0.722

** Calculate POWER for given sample size, effect size, and alpha
power oneway 3.24 2.49 2.92 2.85, alpha (0.05) varerror(9) ngroups(4) n1(46) n2(43) n3(39) n4(41) // Excluding the Control group
* power = 0.1428

** Calculate MINIMUM DETECTABLE EFFECT SIZE for given sample size, alpha, and beta
power oneway, varerror(9) ngroups(4) alpha(0.05) power(0.8) n1(46) n2(43) n3(39) n4(41) // Excluding the Control group
* delta = 0.2570, Var_m = 0.5945

** Calculate REQUIRED SAMPLE SIZE for given effect size, alpha, and beta
power oneway 3.24 2.49 2.92 2.85, alpha(0.05) power(0.8) varerror(9)
* N = 1388, N per group = 347

