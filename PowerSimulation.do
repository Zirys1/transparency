/* Elena Reznichenko
Power Analysis
170303
*/


****** power to detect the effect in the observed data, were they representative for the population
***** with the actual sample sizes from the experiment

program drop _all 
program define power,rclass // starts definition of program

	***** data generating process

	clear
	set more off
	set obs 169 // does not take observations from control group (45) into account (overall 214 observations incl. control)
	gen id = _n
	gen treat = . // Definition of treatment factor variables 
	replace treat = 0 if id < 47 // Def
	replace treat = 1 if id > 46 & id < 90 // DefInf
	replace treat = 2 if id > 89 & id < 129 // DefPur
	replace treat = 3 if id > 128 //DefInfPur

	gen contr = . // definition of contribution variable for each treatment group, according to the means and sd's of laboratory experiment
	replace contr = 3.24 + rnormal(0, 3.21) if treat == 0 // Def
	replace contr = 2.49 + rnormal(0, 2.95) if treat == 1 // DefInf
	replace contr = 2.92 + rnormal(0, 3.19) if treat == 2 // DefPur
	replace contr = 2.85 + rnormal(0, 2.95) if treat == 3 // DefInfPur

	replace contr = 0 if contr < 0 // limit variable to positive values (distribution has property of excess zero's (censoring) - requiring Tobit model)


	***** statistics

	tobit contr i.treat, ll(0) // tobit model with left-censoring at 0
	test 1.treat = 0 // tests if coefficient is equal to 0 (significance test from regressio table, only F instead of t test) - Def == DefInf
	return scalar comp1 = r(p) // stores scalar in program for later use (I think)
	test 1.treat = 2.treat // DefInf == DefPur
	return scalar comp2 = r(p)
	test 1.treat = 3.treat // DefInf == DefInfPur
	return scalar comp3 = r(p)
	test 2.treat = 3.treat // DefPur == DefInfPur
	return scalar comp4 = r(p)
end
	
// simulate does Monte Carlo simulations, so probably just repeats the above process (bootstrapping) and then gives the coefficients and confidence intervals
simulate comp1 = r(comp1) comp2 = r(comp2) comp3 = r(comp3) comp4 = r(comp4), reps(10000) seed(123456): power // power is the program we just defined above

/* The following reports all p-values that are below threshold of .05 */
su comp1 if comp1 < .05 // summarize statistics only when p-value below 5% threshold
su comp2 if comp2 < .05
su comp3 if comp3 < .05
su comp4 if comp4 < .05

/* Findings (concerning power):
In 4576 (of 10000) cases does either difference be significant. 
2211 for Def == 0,
1053 for Def == DefInf
766 for Def == DefPur
546 for Def == DefInfPur

Interpretation:

*/


***** with a (much) larger sample

program drop _all
program define power,rclass

	***** data generating process

	clear
	set more off
	set obs 8000 // basically does the same as above only with much more observations, albeit statistics similar to ours. Basically what Bilel tried to do by doubling data
	gen id = _n
	gen treat = .
	replace treat = 0 if id < 2000
	replace treat = 1 if id > 1999 & id < 4000
	replace treat = 2 if id > 3999 & id < 6000
	replace treat = 3 if id > 5999

	gen contr = .
	replace contr = 3.24 + rnormal(0, 3.21) if treat == 0
	replace contr = 2.49 + rnormal(0, 2.95) if treat == 1
	replace contr = 2.92 + rnormal(0, 3.19) if treat == 2
	replace contr = 2.85 + rnormal(0, 2.95) if treat == 3

	replace contr = 0 if contr < 0


	***** statistics

	tobit contr i.treat, ll(0)
	test 1.treat = 0
	return scalar comp1 = r(p)
	test 1.treat = 2.treat
	return scalar comp2 = r(p)
	test 1.treat = 3.treat
	return scalar comp3 = r(p)
	test 2.treat = 3.treat
	return scalar comp4 = r(p)
end
	

simulate comp1 = r(comp1) comp2 = r(comp2) comp3 = r(comp3) comp4 = r(comp4), reps(10000) seed(123456): power

su comp1 if comp1 < .05
su comp2 if comp2 < .05
su comp3 if comp3 < .05
su comp4 if comp4 < .05

/* Findings:
10000 for Def == 0,
9961 for Def == DefInf
9614 for Def == DefPur
1682 for Def == DefInfPur

Interpretation:
With a sample this large, our experiment could detect deviations from zero of Def (based on mean and sd from our experiment) in all of the cases
*/



****** an effect of which size could be detected with the actual sample size, and with the standard deviations from the observed data
***** large effect size: Cohen's d: |mean0 - mean1|/sd = .8 (ASSUMED AS POPULATION EFFECT SIZE)
// for SD's using (3.21 + 2.95 + 3.19 + 2.95)/4 = 3.075 (you can do better by weighting by size of subsample, or simple using the overall sd)
// With subsample weighting: 3.21*(46/169) + 2.95*(43/169) + 3.19*(39/169) + 2.95*(41/169) = 3.076154
// .8 * 3.075 = 2.46 , i.e.  mean0 - mean1 = 2.46, i.e. the difference we could detect with our sample and power of 0.8
// an effect this big you would have easily identified (even at beta <= .5)

program drop _all
program define power,rclass

	***** data generating process

	clear // same data generation as above
	set more off
	set obs 169
	gen id = _n
	gen treat = .
	replace treat = 0 if id < 47
	replace treat = 1 if id > 46 & id < 90
	replace treat = 2 if id > 89 & id < 129
	replace treat = 3 if id > 128

	gen contr = .
	replace contr = 0 + rnormal(0, 3.21) if treat == 0   // here are the changes (Def)
	replace contr = 2.46 + rnormal(0, 2.95) if treat == 1 // this would be the effect we could easily identify with our sample an .8 power
	replace contr = 4.92 + rnormal(0, 3.19) if treat == 2
	replace contr = 7.38 + rnormal(0, 2.95) if treat == 3

	replace contr = 0 if contr < 0


	***** statistics
	// one comparison less since 1 vs. 3 (DefPur) would double the effect size

	tobit contr i.treat, ll(0)
	test 1.treat = 0
	return scalar comp1 = r(p)
	test 1.treat = 2.treat
	return scalar comp2 = r(p)
	test 2.treat = 3.treat
	return scalar comp3 = r(p)
end
	

simulate comp1 = r(comp1) comp2 = r(comp2) comp3 = r(comp3) comp4 = r(comp4), reps(10000) seed(123456): power

su comp1 if comp1 < .05
su comp2 if comp2 < .05
su comp3 if comp3 < .05

/* Findings:
9330 for Def == 0,
9533 for Def == DefInf
9471 for DefInf == DefPur

Interpretation:
*/


// My question based on this approach would be: Which minimum effect could be detected with power of .8 with a sample of 70 per treatment (or at least for Control, Def, and DefInf)?




***** medium effect size: Cohen's d: |mean0 - mean1|/sd = .5 (ASSUMED AS POPULATION EFFECT SIZE)
// using (3.21 + 2.95 + 3.19 + 2.95)/4 = 3.075 (you can do better by weighting by size of subsample, or simple using the overall sd)
// .5 * 1.5375 = 
// only the comparsion between 

program drop _all
program define power,rclass

	***** data generating process

	clear
	set more off
	set obs 169
	gen id = _n
	gen treat = .
	replace treat = 0 if id < 47
	replace treat = 1 if id > 46 & id < 90
	replace treat = 2 if id > 89 & id < 129
	replace treat = 3 if id > 128

	gen contr = .
	replace contr = 0 + rnormal(0, 3.21) if treat == 0   // here are the changes
	replace contr = 1.5375 + rnormal(0, 2.95) if treat == 1
	replace contr = 3.075 + rnormal(0, 3.19) if treat == 2
	replace contr = 4.6125 + rnormal(0, 2.95) if treat == 3

	replace contr = 0 if contr < 0


	***** statistics

	tobit contr i.treat, ll(0)
	test 1.treat = 0
	return scalar comp1 = r(p)
	test 1.treat = 2.treat
	return scalar comp2 = r(p)
	test 2.treat = 3.treat
	return scalar comp3 = r(p)
end
	

simulate comp1 = r(comp1) comp2 = r(comp2) comp3 = r(comp3), reps(10000) seed(123456): power

su comp1 if comp1 < .05
su comp2 if comp2 < .05
su comp3 if comp3 < .05

/* Findings:
5416 for Def == 0,
6209 for Def == DefInf
5942 for DefInf == DefPur

Interpretation:
*/



***** which effect size is identified at the customary level of beta (power) = .8?
// i.e. approximately 80/100 simulated p-values are below .05


program drop _all
program define power,rclass

	***** data generating process

	clear
	set more off
	set obs 169
	gen id = _n
	gen treat = .
	replace treat = 0 if id < 47
	replace treat = 1 if id > 46 & id < 90
	replace treat = 2 if id > 89 & id < 129
	replace treat = 3 if id > 128

	gen contr = .
	replace contr = 0 + rnormal(0, 3.21) if treat == 0   // here are the changes (the mean differences?)
	replace contr = 2 + rnormal(0, 2.95) if treat == 1
	replace contr = 4 + rnormal(0, 3.19) if treat == 2
	replace contr = 6 + rnormal(0, 2.95) if treat == 3

	replace contr = 0 if contr < 0


	***** statistics

	tobit contr i.treat, ll(0)
	test 1.treat = 0
	return scalar comp1 = r(p)
	test 1.treat = 2.treat
	return scalar comp2 = r(p)
	test 2.treat = 3.treat
	return scalar comp3 = r(p)
end
	

simulate comp1 = r(comp1) comp2 = r(comp2) comp3 = r(comp3), reps(10000) seed(123456): power

su comp1 if comp1 < .05
su comp2 if comp2 < .05
su comp3 if comp3 < .05

// 2/3.075 is very close to two thirds, i.e. to an effect size of .66
// i.e. we could detect changes that are roughly 2 EURO
// What is the minimum change (mean0 - mean1) we are interested in? 1 EURO? 50 cent?


***** which sample size would be needed to identify a medium sized (.5) effect at beta = .8 ?

program drop _all
program define power,rclass

	***** data generating process

	clear
	set more off
	set obs 280  // here and in the treatment assignment are the changes
	gen id = _n
	gen treat = .
	replace treat = 0 if id < 71
	replace treat = 1 if id > 70 & id < 141
	replace treat = 2 if id > 140 & id < 211
	replace treat = 3 if id > 210

	gen contr = .
	replace contr = 0 + rnormal(0, 3.21) if treat == 0 
	replace contr = 1.5375 + rnormal(0, 2.95) if treat == 1
	replace contr = 3.075 + rnormal(0, 3.19) if treat == 2
	replace contr = 4.6125 + rnormal(0, 2.95) if treat == 3

	replace contr = 0 if contr < 0


	***** statistics

	tobit contr i.treat, ll(0)
	test 1.treat = 0
	return scalar comp1 = r(p)
	test 1.treat = 2.treat
	return scalar comp2 = r(p)
	test 2.treat = 3.treat
	return scalar comp3 = r(p)
end
	

simulate comp1 = r(comp1) comp2 = r(comp2) comp3 = r(comp3), reps(10000) seed(123456): power

su comp1 if comp1 < .05
su comp2 if comp2 < .05
su comp3 if comp3 < .05

/* Interpretation:
With approximately 70 observations per treatment, we could identify a medium sized (.5) effect at beta = .8
*/
