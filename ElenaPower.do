/* Elena Reznichenko
Power Analysis
170303
*/


****** power to detect the effect in the observed data, were they representative for the population
***** with the actual sample sizes from the experiment

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
	

simulate comp1 = r(comp1) comp2 = r(comp2) comp3 = r(comp3) comp4 = r(comp4), reps(100) seed(123456): power

su comp1 if comp1 < .05
su comp2 if comp2 < .05
su comp3 if comp3 < .05
su comp4 if comp4 < .05




***** with a (much) larger sample

program drop _all
program define power,rclass

	***** data generating process

	clear
	set more off
	set obs 8000
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
	

simulate comp1 = r(comp1) comp2 = r(comp2) comp3 = r(comp3) comp4 = r(comp4), reps(100) seed(123456): power

su comp1 if comp1 < .05
su comp2 if comp2 < .05
su comp3 if comp3 < .05
su comp4 if comp4 < .05



****** an effect of which size could be detected with the actual sample size, and with the standard deviations from the observed data
***** large effect size: Cohen's d: |mean0 - mean1|/sd = .8
// using (3.21 + 2.95 + 3.19 + 2.95)/4 = 3.075 (you can do better by weighting by size of subsample, or simple using the overall sd)
// .8 * 3.075 = 2.46
// an effect this big you would have easily identified (even at beta <= .5)

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
	replace contr = 2.46 + rnormal(0, 2.95) if treat == 1
	replace contr = 4.92 + rnormal(0, 3.19) if treat == 2
	replace contr = 7.38 + rnormal(0, 2.95) if treat == 3

	replace contr = 0 if contr < 0


	***** statistics
	// one comparison less since 1 vs. 3 would double the effect size

	tobit contr i.treat, ll(0)
	test 1.treat = 0
	return scalar comp1 = r(p)
	test 1.treat = 2.treat
	return scalar comp2 = r(p)
	test 2.treat = 3.treat
	return scalar comp3 = r(p)
end
	

simulate comp1 = r(comp1) comp2 = r(comp2) comp3 = r(comp3) comp4 = r(comp4), reps(100) seed(123456): power

su comp1 if comp1 < .05
su comp2 if comp2 < .05
su comp3 if comp3 < .05



***** medium effect size: Cohen's d: |mean0 - mean1|/sd = .5
// using (3.21 + 2.95 + 3.19 + 2.95)/4 = 3.075 (you can do better by weighting by size of subsample, or simple using the overall sd)
// .5 * 3.075 = 1.5375
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
	

simulate comp1 = r(comp1) comp2 = r(comp2) comp3 = r(comp3), reps(100) seed(123456): power

su comp1 if comp1 < .05
su comp2 if comp2 < .05
su comp3 if comp3 < .05



***** which effect size is identified at the customary level of beta = .8?
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
	replace contr = 0 + rnormal(0, 3.21) if treat == 0   // here are the changes
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
	

simulate comp1 = r(comp1) comp2 = r(comp2) comp3 = r(comp3), reps(100) seed(123456): power

su comp1 if comp1 < .05
su comp2 if comp2 < .05
su comp3 if comp3 < .05

// 2/3.075 is very close to two thirds, i.e. to an effect size of .66



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
	

simulate comp1 = r(comp1) comp2 = r(comp2) comp3 = r(comp3), reps(100) seed(123456): power

su comp1 if comp1 < .05
su comp2 if comp2 < .05
su comp3 if comp3 < .05
