/* Power simulations according to Engel, but with
Control
Def
DefInf
13.03.2017
*/

* What is the power of our tests for the effects we find in the experiment?
* With actual sample size
* This is not necessarily the most interesting question we want to pose (this is the post-hoc power analyis, though)
set more off
program drop _all 
program define power,rclass // starts definition of program

	***** data generating process

	clear
	set more off
	set obs 134 // Control, Def, DefInf
	gen id = _n
	gen treat = . // Definition of treatment factor variables 
	replace treat = 0 if id < 46 // Control
	replace treat = 1 if id > 45 & id < 92 // Def
	replace treat = 2 if id > 91 & id < 135 // DefInf

	gen contr = . // definition of contribution variable for each treatment group, according to the means and sd's of laboratory experiment
	replace contr = 1.67 + rnormal(0, 2.68) if treat == 0 // Control
	replace contr = 3.24 + rnormal(0, 3.21) if treat == 1 // Def
	replace contr = 2.49 + rnormal(0, 2.95) if treat == 2 // DefInf

	replace contr = 0 if contr < 0 // limit variable to positive values (distribution has property of excess zero's (censoring) - requiring Tobit model)


	***** statistics

	tobit contr i.treat, ll(0) // tobit model with left-censoring at 0
	test 1.treat = 0 // Control == Def
	return scalar comp1 = r(p) // 
	test 1.treat = 2.treat // Def == DefInf
	return scalar comp2 = r(p)
	test 2.treat = 0 // Control == DefInf
	return scalar comp3 = r(p)
end
	
// simulate does Monte Carlo simulations, so probably just repeats the above process (bootstrapping) and then gives the coefficients and confidence intervals
simulate comp1 = r(comp1) comp2 = r(comp2) comp3 = r(comp3), reps(10000) seed(123456): power // power is the program we just defined above

/* The following reports all p-values that are below threshold of .05 */
su comp1 if comp1 < .05 // beta = 0.74
su comp2 if comp2 < .05 // beta = 0.24
su comp3 if comp3 < .05 // beta = 0.26




***** which effect size is identified at the customary level of beta (power) = .8?
// Also: Which mean difference is identified?
// i.e. approximately 80/100 simulated p-values are below .05


program drop _all
program define power,rclass

	***** data generating process

	clear
	set more off
	set obs 134 // Control, Def, DefInf
	gen id = _n
	gen treat = . // Definition of treatment factor variables 
	replace treat = 0 if id < 46 // Control
	replace treat = 1 if id > 45 & id < 92 // Def
	replace treat = 2 if id > 91 & id < 135 // DefInf

	gen contr = . // effects detectable with power = .8
	replace contr = 0 + rnormal(0, 2.68) if treat == 0 // Control
	replace contr = 3 + rnormal(0, 3.21) if treat == 1 // Def
	replace contr = 1 + rnormal(0, 2.95) if treat == 2 // DefInf

	replace contr = 0 if contr < 0


	***** statistics

	tobit contr i.treat, ll(0) // tobit model with left-censoring at 0
	test 1.treat = 0 // Control == Def
	return scalar comp1 = r(p) // 
	test 1.treat = 2.treat // Def == DefInf
	return scalar comp2 = r(p)
	test 2.treat = 0 // Control == DefInf
	return scalar comp3 = r(p)
end
	

simulate comp1 = r(comp1) comp2 = r(comp2) comp3 = r(comp3), reps(10000) seed(123456): power

su comp1 if comp1 < .05 // beta = 0.998
su comp2 if comp2 < .05 // beta = 0.869 
su comp3 if comp3 < .05 // beta = 0.36

// i.e. we could detect a difference between Def and DefInf of roughly 2 EURO, with power of .8
// this is a large effect size



** Say we want to be able to detect a difference of 1 EUR at beta = .8, and alpha = .05?
** Would we be able to do this with 70 observations in each of the three groups?

program drop _all
program define power,rclass

	***** data generating process

	clear
	set more off
	set obs 210 // Control, Def, DefInf
	gen id = _n
	gen treat = . // Definition of treatment factor variables 
	replace treat = 0 if id < 71 // Control
	replace treat = 1 if id > 70 & id < 141 // Def
	replace treat = 2 if id > 140 & id < 211 // DefInf

	gen contr = . // effects detectable with power = .8
	replace contr = 0 + rnormal(0, 2.68) if treat == 0 // Control
	replace contr = 2 + rnormal(0, 3.21) if treat == 1 // Def
	replace contr = 1 + rnormal(0, 2.95) if treat == 2 // DefInf

	replace contr = 0 if contr < 0


	***** statistics

	tobit contr i.treat, ll(0) // tobit model with left-censoring at 0
	test 1.treat = 0 // Control == Def
	return scalar comp1 = r(p) // 
	test 1.treat = 2.treat // Def == DefInf
	return scalar comp2 = r(p)
	test 2.treat = 0 // Control == DefInf
	return scalar comp3 = r(p)
end
	

simulate comp1 = r(comp1) comp2 = r(comp2) comp3 = r(comp3), reps(100) seed(123456): power

su comp1 if comp1 < .05 // beta = 0.9835
su comp2 if comp2 < .05 // beta = 0.5246
su comp3 if comp3 < .05 // beta = 0.5396

// No, we would not be able to detect 1 EUR difference, only power = 0.5

// for 80 per observation: beta = .61

** What if we would not increase Control, but only Def and DefInf by 40 each?
program drop _all
program define power,rclass

	***** data generating process

	clear
	set more off
	set obs 214 // Control, Def, DefInf
	gen id = _n
	gen treat = . // Definition of treatment factor variables 
	replace treat = 0 if id < 46 // Control
	replace treat = 1 if id > 45 & id < 132 // Def
	replace treat = 2 if id > 131 & id < 215 // DefInf

	gen contr = . // effects detectable with power = .8
	replace contr = 0 + rnormal(0, 2.68) if treat == 0 // Control
	replace contr = 2 + rnormal(0, 3.21) if treat == 1 // Def
	replace contr = 1 + rnormal(0, 2.95) if treat == 2 // DefInf

	replace contr = 0 if contr < 0


	***** statistics

	tobit contr i.treat, ll(0) // tobit model with left-censoring at 0
	test 1.treat = 0 // Control == Def
	return scalar comp1 = r(p) // 
	test 1.treat = 2.treat // Def == DefInf
	return scalar comp2 = r(p)
	test 2.treat = 0 // Control == DefInf
	return scalar comp3 = r(p)
end

simulate comp1 = r(comp1) comp2 = r(comp2) comp3 = r(comp3), reps(100) seed(123456): power

su comp1 if comp1 < .05 // beta = 0.99
su comp2 if comp2 < .05 // beta = 0.54
su comp3 if comp3 < .05 // beta = 0.44

// This would not bring much in terms of power, if we want to detect a MDE of 1 EUR.


** What, in terms of effect size, would our experiment be able to detect with 
// +40 observations in Def and DefInf
program drop _all
program define power,rclass

	***** data generating process

	clear
	set more off
	set obs 214 // Control, Def, DefInf
	gen id = _n
	gen treat = . // Definition of treatment factor variables 
	replace treat = 0 if id < 46 // Control
	replace treat = 1 if id > 45 & id < 132 // Def
	replace treat = 2 if id > 131 & id < 215 // DefInf

	gen contr = . // effects detectable with power = .8
	replace contr = 0 + rnormal(0, 2.68) if treat == 0 // Control
	replace contr = 2.35 + rnormal(0, 3.21) if treat == 1 // Def
	replace contr = 1 + rnormal(0, 2.95) if treat == 2 // DefInf

	replace contr = 0 if contr < 0


	***** statistics

	tobit contr i.treat, ll(0) // tobit model with left-censoring at 0
	test 1.treat = 0 // Control == Def
	return scalar comp1 = r(p) // 
	test 1.treat = 2.treat // Def == DefInf
	return scalar comp2 = r(p)
	test 2.treat = 0 // Control == DefInf
	return scalar comp3 = r(p)
end
	

simulate comp1 = r(comp1) comp2 = r(comp2) comp3 = r(comp3), reps(10000) seed(123456): power

su comp1 if comp1 < .05 // beta = 0.99
su comp2 if comp2 < .05 // beta = 0.825 
su comp3 if comp3 < .05 // beta = 0.45

// i.e. we could detect a difference between Def and DefInf of roughly 1.35 EURO, with power of .8
// this is an effect size of 1.35/2.947 = 0.458, which we would be able to detect.
// Here wee calculated pooled SD = (2.68 + 3.21 + 2.95)/3 = 2.947
// 0.439 is a bit below medium (currently, it is .66)



** What, in terms of effect size, would our experiment be able to detect with 
// 70 observations in each Con, Def, DefInf?
program drop _all
program define power,rclass

	***** data generating process

	clear
	set more off
	set obs 210 // Control, Def, DefInf
	gen id = _n
	gen treat = . // Definition of treatment factor variables 
	replace treat = 0 if id < 71 // Control
	replace treat = 1 if id > 70 & id < 141 // Def
	replace treat = 2 if id > 140 & id < 211 // DefInf

	gen contr = . // effects detectable with power = .8
	replace contr = 0 + rnormal(0, 2.68) if treat == 0 // Control
	replace contr = 2.35 + rnormal(0, 3.21) if treat == 1 // Def
	replace contr = 1 + rnormal(0, 2.95) if treat == 2 // DefInf

	replace contr = 0 if contr < 0


	***** statistics

	tobit contr i.treat, ll(0) // tobit model with left-censoring at 0
	test 1.treat = 0 // Control == Def
	return scalar comp1 = r(p) // 
	test 1.treat = 2.treat // Def == DefInf
	return scalar comp2 = r(p)
	test 2.treat = 0 // Control == DefInf
	return scalar comp3 = r(p)
end
	

simulate comp1 = r(comp1) comp2 = r(comp2) comp3 = r(comp3), nodots reps(100) seed(123456): power

su comp1 if comp1 < .05 // beta = 0.99
su comp2 if comp2 < .05 // beta = 0.75 
su comp3 if comp3 < .05 // beta = 0.54

// i.e. we could detect a difference between Def and DefInf of roughly 1.35 EURO, with power of .8
// this is an effect size of 1.35/2.947 = 0.458, which we would be able to detect.
// 0.439 is a medium effect size, so ok



**How many observations do we need in order to detect an MDE of 1 EUR? 
** 1 EUR would be a small effect size

program drop _all
program define power,rclass

	***** data generating process

	clear
	set more off
	set obs 420 // Control, Def, DefInf
	gen id = _n
	gen treat = . // Definition of treatment factor variables 
	replace treat = 0 if id < 101 // Control
	replace treat = 1 if id > 100 & id < 261 // Def
	replace treat = 2 if id > 260 & id < 421 // DefInf

	gen contr = . // effects detectable with power = .8
	replace contr = 0 + rnormal(0, 2.68) if treat == 0 // Control
	replace contr = 2 + rnormal(0, 3.21) if treat == 1 // Def
	replace contr = 1 + rnormal(0, 2.95) if treat == 2 // DefInf

	replace contr = 0 if contr < 0


	***** statistics

	tobit contr i.treat, ll(0) // tobit model with left-censoring at 0
	test 1.treat = 0 // Control == Def
	return scalar comp1 = r(p) // 
	test 1.treat = 2.treat // Def == DefInf
	return scalar comp2 = r(p)
	test 2.treat = 0 // Control == DefInf
	return scalar comp3 = r(p)
end
	

simulate comp1 = r(comp1) comp2 = r(comp2) comp3 = r(comp3), reps(10000) seed(123456): power

su comp1 if comp1 < .05 // beta = 1
su comp2 if comp2 < .05 // beta = 0.85
su comp3 if comp3 < .05 // beta = 0.79



