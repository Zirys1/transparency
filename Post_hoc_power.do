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
tobit Contribution Def DefInf DefPur DefInfPur i.ImportanceD i.Genderf c.Age i.PastParticipationf i.EU_ETS_Useful, robust ll(0) // r2f = 0.0545
tobit Contribution DefInf DefPur DefInfPur i.ImportanceD i.Genderf c.Age i.PastParticipationf i.EU_ETS_Useful, robust ll(0) // r2r = 0.0465
powerreg, r2f(0.0545) r2r(0.0465) alpha(0.05) nvar(6) n(214) // Taking estimates from Tobit Regression
* power = 0.2679 (p = 0.009)

reg Contribution Def DefInf DefPur DefInfPur i.ImportanceD i.Genderf c.Age i.PastParticipationf i.EU_ETS_Useful, robust // r2f = 0.1739
reg Contribution DefInf DefPur DefInfPur i.ImportanceD i.Genderf c.Age i.PastParticipationf i.EU_ETS_Useful, robust // r2r = 0.1523
powerreg, r2f(0.1739) r2r(0.1523) alpha(0.05) nvar(6) n(214) // Taking estimates from OLS linear Regression
* power = 0.6534 (p = 0.009)

** Control vs. Default+Info
tobit Contribution Def DefPur DefInfPur i.ImportanceD i.Genderf c.Age i.PastParticipationf i.EU_ETS_Useful, robust ll(0) // r2r = 0.0530
powerreg, r2f(0.0545) r2r(0.0530) alpha(0.05) nvar(6) n(214) // Taking estimates from Tobit Regression
* power = 0.0893 (p = 0.237)

reg Contribution Def DefPur DefInfPur i.ImportanceD i.Genderf c.Age i.PastParticipationf i.EU_ETS_Useful, robust // r2r = 0.1717
powerreg, r2f(0.1739) r2r(0.1717) alpha(0.05) nvar(6) n(214) // Taking estimates from OLS linear Regression
* power = 0.1168 (p = 0.237)

** Control vs. Default+Purpose
tobit Contribution Def DefInf DefInfPur i.ImportanceD i.Genderf c.Age i.PastParticipationf i.EU_ETS_Useful, robust ll(0) // r2r = 0.0462
powerreg, r2f(0.0545) r2r(0.0462) alpha(0.05) nvar(6) n(214) // Taking estimates from Tobit Regression
* power = 0.2761 (p = 0.006)

reg Contribution Def DefInf DefInfPur i.ImportanceD i.Genderf c.Age i.PastParticipationf i.EU_ETS_Useful, robust // r2r = 0.1521
powerreg, r2f(0.1739) r2r(0.1521) alpha(0.05) nvar(6) n(214) // Taking estimates from OLS linear Regression
* power = 0.6574 (p = 0.006)

** Control vs. Default+Info+Purpose
tobit Contribution Def DefInf DefPur i.ImportanceD i.Genderf c.Age i.PastParticipationf i.EU_ETS_Useful, robust ll(0) // r2r = 0.0495
powerreg, r2f(0.0545) r2r(0.0495) alpha(0.05) nvar(6) n(214) // Taking estimates from Tobit Regression
* power = 0.1850 (p = 0.034)

reg Contribution Def DefInf DefPur i.ImportanceD i.Genderf c.Age i.PastParticipationf i.EU_ETS_Useful, robust // r2r = 0.1591
powerreg, r2f(0.1739) r2r(0.1591) alpha(0.05) nvar(6) n(214) // Taking estimates from OLS linear Regression
* power = 0.4957 (p = 0.034)

**** Findings 2-4: Omnibus hypothesis
kwallis Contribution, by(TreatmentnoCf) // p = 0.773
oneway Contribution TreatmentnoCf // p = 0.722
power oneway 3.24 2.49 2.92 2.85, alpha (0.05) varerror(9) n1(46) n2(43) n3(39) n4(41) // Excluding the Control group
* power = 0.1428

* Pairwise tests
* Regression analysis with treatment base = Default
**** Finding 2: Informing participants that the default may have an influence on their decision does not significantly decrease contributions compared to when they are not informed.


**** Finding 3:Informing participants about the default’s purpose does not significantly increase contributions compared to when they are not informed.


**** Finding 4: Informing participants that the default may have an influence on their decision, as well as of the default’s purpose does not decrease or increase contributions, compared to the other types of transparency (including no transparency at all).





*********************************
***** General Power Analyses ****
*********************************

**** ANOVA (omnibus) hypothesis test ****
** Calculate POWER for given sample size, effect size, and alpha
power oneway 1.67 3.24 2.49 2.92 2.85, alpha (0.05) varerror(9) n1(45) n2(46) n3(43) n4(39) n5(41)
* power = 0.5419

** Calculate MINIMUM DETECTABLE EFFECT SIZE for given sample size, alpha, and beta
power oneway, varerror(9) ngroups(5) alpha (0.05) power(0.8) n1(45) n2(46) n3(43) n4(39) n5(41)
* delta = 0.2389, Var_m = 0.5135 
 
** Calculate REQUIRED SAMPLE SIZE for given effect size, alpha, and beta
power oneway 1.67 3.24 2.49 2.92 2.85, alpha (0.05) power (0.8) varerror(9)
* N = 380, N per group = 76



**** Regression hypotheses tests **** PROBLEM: do not refer to the specific hypotheses we test (rather: test how much variance is explained by treatment-variable, i.e. all treatments)
** Calculate POWER for given sample size, effect size, and alpha
powerreg, r2f(0.0545) r2r(0.0426) alpha(0.05) nvar(6) n(214) // Taking estimates from Tobit Regression
* power = 0.3722
powerreg, r2f(0.0545) r2r(0.0426) alpha(0.05) nvar(6) n(214) ntest(4) // Taking estimates from Tobit Regression, assuming each hypothesis test of Teatment against control is 1 separate test
* power = 0.2169
powerreg, r2f(0.1739) r2r(0.1384) alpha(0.05) nvar(6) n(214) // Taking estimates from OLS linear Regression
* power = 0.8551
powerreg, r2f(0.1739) r2r(0.1384) alpha(0.05) nvar(6) n(214) ntest(4) // Taking estimates from OLS linear Regression, assuming each hypothesis test of Teatment against control is 1 separate test
* power = 0.6628


** Calculate REQUIRED SAMPLE SIZE for given effect size, alpha, and beta
powerreg, r2f(0.0545) r2r(0.0426) alpha(0.05) nvar(6) // Taking estimates from Tobit Regression
* n = 624
powerreg, r2f(0.0545) r2r(0.0426) alpha(0.05) nvar(6) ntest(4) // Taking estimates from Tobit Regression, assuming each hypothesis test of Teatment against control is 1 separate test
* n = 960
powerreg, r2f(0.1739) r2r(0.1384) alpha(0.05) nvar(6) // Taking estimates from OLS linear Regression
* n = 184
powerreg, r2f(0.1739) r2r(0.1384) alpha(0.05) nvar(6) ntest(4) // Taking estimates from OLS linear Regression, assuming each hypothesis test of Teatment against control is 1 separate test
* n = 280
