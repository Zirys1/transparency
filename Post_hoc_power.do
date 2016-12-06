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

**** Finding 1: There is a default effect on contributions for a default, a default with added purpose, as well as for a default with both types of transparency.
** Calculate POWER for given sample size, effect size, and alpha
* Analysis conducted with G*Power 
* Test family: t tests
* Statistical test: Means: Wilcoxon-Mann-Whitney test (two groups))
* Type of power analysis: Post hoc: Compute achieved power - given alpha, sample size, and effect size
* Tail(s): Two
* Parent distribution: Normal
* Effect size (calculated from means and sd's above): 0.53, 0.29, 0.42, 0.42 
* alpha error prob: 0.05
* Sample size group 1: 45
* Sample size group 2: 46, 43, 39, 41


**** Findings 2-4: Omnibus hypothesis (excluding the Control group)
kwallis Contribution, by(TreatmentnoCf) // p = 0.773
** WITHIN-GROUP VARIANCE
oneway Contribution TreatmentnoCf // within-groups variance 9.4849	   p = 0.722
** OBSERVED EFFECT-SIZE
estat esize // eta-squared = 0.0080045 --> f = 0.0898

** Calculate POWER for given sample size, effect size, and alpha
power oneway 3.24 2.49 2.92 2.85, alpha (0.05) varerror(9.48) ngroups(4) n1(46) n2(43) n3(39) n4(41) // power = 0.1376

** Calculate MINIMUM DETECTABLE EFFECT SIZE for given sample size, alpha, and beta
power oneway, varerror(9.48) ngroups(4) alpha(0.05) power(0.8) n1(46) n2(43) n3(39) n4(41) // f = 0.2570, Var_m = 0.6262


