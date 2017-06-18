set more off
/* 
- Can Nudges Be Transparent and Yet Effective? -
---------------- Power Analysis ----------------
------------------ 15.06.2017 ------------------
*/

/* *****************************************************************************
********************************************************************************
Outline:
First, we calculate the ex-post/ observed power of the tests in our paper 
for the following tests and contrasts:

1) Concerning Finding 1:
Underlying model: Tobit-Model (in the Manuscript the power was calculated using a MWU test. However,
we simulate the power for a Tobit Model here, because this is the model we ultimately
base our findings on.

a) Control vs. Def
b) Control vs. Def+Information
c) Control vs. Def+Purpose
d) Control vs. Def+Information+Purpose

2) Concerning Findings 2-4:
Underlying model: Tobit-Model

a) Default vs. Default+Information
b) Default vs. Default+Purpose
c) Default+Info vs. Default+Purpose vs. Default+Info+Purpose
*/

/*
We have the following data from experiment 1 (see Table 2):
Control					n = 45		Contribution (Mean) = 1.67		Contribution (SD) = 2.68			
Default					n = 46		Contribution (Mean) = 3.24		Contribution (SD) = 3.21	
Default+Info			n = 43		Contribution (Mean) = 2.49		Contribution (SD) = 2.95	
Default+Purpose			n = 39		Contribution (Mean) = 2.92		Contribution (SD) = 3.19	
Default+Info+Purpose	n = 41		Contribution (Mean) = 2.85		Contribution (SD) = 2.95
--------------------------------------------------------------------------------------------	
OVERALL					n = 214		Contribution (Mean) = 2.63		Contribution (SD) = 3.02	
*/

/* We set the critical alpha level to 0.05 */


program drop _all 
program define power,rclass // starts definition of program

	** generating a data set based on the data from the experiment

	clear
	set more off
	set obs 214 
	gen id = _n
	gen treat = . // Definition of treatment factor variables 
	replace treat = 0 if id < 46 // Con
	replace treat = 1 if id > 45 & id < 92 // Def
	replace treat = 2 if id > 91 & id < 135 // DefInf
	replace treat = 3 if id > 134 & id < 174 // DefPur
	replace treat = 4 if id > 173 //DefInfPur

	** defining mean contribution amounts and SD's per treatment group
	** Assuming a left censored normal distribution
	gen contr = . 
	replace contr = 1.67 + rnormal(0, 2.68) if treat == 0 // Con
	replace contr = 3.24 + rnormal(0, 3.21) if treat == 1 // Def
	replace contr = 2.49 + rnormal(0, 2.95) if treat == 2 // DefInf
	replace contr = 2.92 + rnormal(0, 3.19) if treat == 3 // DefPur
	replace contr = 2.85 + rnormal(0, 2.95) if treat == 4 // DefInfPur

	/* draw from normal distribution but set to INTEGERS as in experiment*/
	replace contr = 0 if contr <= 0.5 // left-censoring
	replace contr = 1 if contr > 0.5 & contr <= 1.5
	replace contr = 2 if contr > 1.5 & contr <= 2.5
	replace contr = 3 if contr > 2.5 & contr <= 3.5
	replace contr = 4 if contr > 3.5 & contr <= 4.5
	replace contr = 5 if contr > 4.5 & contr <= 5.5
	replace contr = 6 if contr > 5.5 & contr <= 6.5
   	replace contr = 7 if contr > 6.5 & contr <= 7.5
	replace contr = 8 if contr > 7.5 & contr <= 8.5
	replace contr = 9 if contr > 8.5 & contr <= 9.5
	replace contr = 10 if contr > 9.5 // right-censoring (not accounted for in model)
	
	

	** Test contrasts shown in 1) in Tobit model
	
	tobit contr i.treat, ll(0)
	test 0.treat == 1.treat // a) Control vs. Default
	return scalar comp1 = r(p) 
	test 0.treat == 2.treat // b) Control vs. Default+Information
	return scalar comp2 = r(p) 
	test 0.treat == 3.treat // c) Control vs. Default+Purpose
	return scalar comp3 = r(p) 
	test 0.treat == 4.treat // d) Control vs. Default+Information+Purpose
	return scalar comp4 = r(p) 
	
	
	** Test contrasts shown in 2) in Tobit model
	
	test 1.treat == 2.treat // Default vs. Default+Information
	return scalar comp5 = r(p) 
	test 1.treat == 3.treat // b) Default vs. Default+Purpose
	return scalar comp6 = r(p) 
	test 2.treat == 3.treat == 4.treat // c) Default+Info vs. Default+Purpose vs. Default+Info+Purpose
	return scalar comp7 = r(p) 

end

/* Simulation of the above program. Number of times X specified in reps(X).
Should at best be 20000, but takes a long time. */
simulate comp1 = r(comp1) comp2 = r(comp2) comp3 = r(comp3) comp4 = r(comp4) comp5 = r(comp5) comp6 = r(comp6) comp7 = r(comp7), reps(100000) seed(123456): power // power is the program we just defined above


/* Calculate the number of times the Tobit model correctly finds a difference
significant at alpha 0.05. This is the power of the tests */

su comp1 if comp1 < .05 // summarize statistics only when p-value below 5% threshold
su comp2 if comp2 < .05
su comp3 if comp3 < .05
su comp4 if comp4 < .05
su comp5 if comp5 < .05
su comp6 if comp6 < .05
su comp7 if comp7 < .05




/* What is the minimum effect size (in EUR, and standardized as Cohen's d) that 
we can detect with a power of 80%, alpha = 0.05, assuming the above statistics? 

We focus on the most relevant contrast (according to reviewer 1)

2) 
a) Default vs. Default+Information
*/

program drop _all 
program define power,rclass // starts definition of program

	** generating a data set based on the data from the experiment

	clear
	set more off
	set obs 214 
	gen id = _n
	gen treat = . // Definition of treatment factor variables 
	replace treat = 0 if id < 46 // Con
	replace treat = 1 if id > 45 & id < 92 // Def
	replace treat = 2 if id > 91 & id < 135 // DefInf
	replace treat = 3 if id > 134 & id < 174 // DefPur
	replace treat = 4 if id > 173 //DefInfPur

	** defining mean contribution amounts and SD's per treatment group
	** Assuming a left censored normal distribution
	gen contr = . 
	replace contr = 1.67 + rnormal(0, 2.68) if treat == 0 // Con
	replace contr = 4 + rnormal(0, 3.21) if treat == 1 // Def
	replace contr = 2.1 + rnormal(0, 2.95) if treat == 2 // DefInf
	replace contr = 2.92 + rnormal(0, 3.19) if treat == 3 // DefPur
	replace contr = 2.85 + rnormal(0, 2.95) if treat == 4 // DefInfPur

	/* draw from normal distribution but set to INTEGERS as in experiment*/
	replace contr = 0 if contr <= 0.5 // left-censoring
	replace contr = 1 if contr > 0.5 & contr <= 1.5
	replace contr = 2 if contr > 1.5 & contr <= 2.5
	replace contr = 3 if contr > 2.5 & contr <= 3.5
	replace contr = 4 if contr > 3.5 & contr <= 4.5
	replace contr = 5 if contr > 4.5 & contr <= 5.5
	replace contr = 6 if contr > 5.5 & contr <= 6.5
   	replace contr = 7 if contr > 6.5 & contr <= 7.5
	replace contr = 8 if contr > 7.5 & contr <= 8.5
	replace contr = 9 if contr > 8.5 & contr <= 9.5
	replace contr = 10 if contr > 9.5 // right-censoring (not accounted for in model)
	
	

	tobit contr i.treat, ll(0)
	test 1.treat == 2.treat // b) Default vs. Default+Information
	return scalar comp1 = r(p) 
end


simulate comp1 = r(comp1), reps(100000) seed(123456): power 


/* Calculate the number of times the Tobit model correctly finds a difference
significant at alpha 0.05. */

su comp1 if comp1 < .05 


/* The null hypothesis is correctly rejected when the  difference is
 4 - 2.1 = 1.9 by the Tobit model > 80% of the time (power = 80%).
 The standardized effect size of an effect of 1.9 is:
 1.9/(3.09) = 0.6 (large)
*/




/* We now intend to design a new experiment with an improved power for the following
contrasts:

Default vs. Default+Info

In order to detect a statistically significant difference between the mean
difference between Def and Def+Inf observed in experiment 1, we'd need more than
250 observations in each of these groups to get 80% power. This is economically
infeasible for us. This would mean that we would need to invest more than 5000 EUR
in order to detect a standardized effect size of 
(3.24 - 2.49)/((3.21+2.95)/2) = 0.24, which is close to small according to
convention (Small, d <= 0.2, Cohen 1988)
See calculations below
*/


program drop _all 
program define power,rclass // starts definition of program

	** generating a data set based on the data from the experiment

	clear
	set more off
	set obs 500 
	gen id = _n
	gen treat = . // Definition of treatment factor variables 
*	replace treat = 0 if id < 41 // Con
	replace treat = 1 if id  < 251 // Def
	replace treat = 2 if id > 250 // DefInf


	** defining mean contribution amounts and SD's per treatment group
	** Assuming a left censored normal distribution
	gen contr = . 
	*replace contr = 1.67 + rnormal(0, 2.68) if treat == 0 // Con
	replace contr = 3.24 + rnormal(0, 3.21) if treat == 1 // Def
	replace contr = 2.49 + rnormal(0, 2.95) if treat == 2 // DefInf


	/* draw from normal distribution but set to INTEGERS as in experiment*/
	replace contr = 0 if contr <= 0.5 // left-censoring
	replace contr = 1 if contr > 0.5 & contr <= 1.5
	replace contr = 2 if contr > 1.5 & contr <= 2.5
	replace contr = 3 if contr > 2.5 & contr <= 3.5
	replace contr = 4 if contr > 3.5 & contr <= 4.5
	replace contr = 5 if contr > 4.5 & contr <= 5.5
	replace contr = 6 if contr > 5.5 & contr <= 6.5
   	replace contr = 7 if contr > 6.5 & contr <= 7.5
	replace contr = 8 if contr > 7.5 & contr <= 8.5
	replace contr = 9 if contr > 8.5 & contr <= 9.5
	replace contr = 10 if contr > 9.5 // right-censoring (not accounted for in model)
	
	

	tobit contr i.treat, ll(0)
	test 1.treat == 2.treat // b) Default vs. Default+Information
	return scalar comp1 = r(p) 
end


simulate comp1 = r(comp1), reps(100000) seed(123456): power 


/* Calculate the number of times the Tobit model correctly finds a difference
significant at alpha 0.05. */

su comp1 if comp1 < .05 

/* 500 Observations can detect the effect we observed in experiment 1 only 774 times
i.e. only has a power of 77.4%
*/


/*
We gather observations for the Control, Default, and Default+Info groups
to make the experiment also a viable replication of part of our original experiment 
We assume the same standard deviations as in our original experiment.

We calculate the needed sample size based on our reasoning that we want to 
be able to detect an effect of 1.15 EUR, the smallest effect interesting to us from
an economic perspective, with a power of 80% (alpha = 0.05). While this does not
mean that we are not able to detect smaller effect sizes, we want to be able to 
detect an effect of this size with a power of 80%.
A difference of 1 EUR corresponds to a standardized effect size of
1/((3.21+2.95)/2) = 0.32 (medium effect), assuming the variances from our first 
experiment.

NOTE: We intend to be able to detect the effect of 1.15 EUR after pooling data from 
both experiments.
*/

/* OPTION A: POOLED MDE */
program drop _all 
program define power,rclass // starts definition of program

	** generating a data set based on the data from the experiment

	clear
	set more off
	set obs 404 // (45 + 46 + 43 = 134 from first experiment, additionally 270 (40 + 115 + 115)
	gen id = _n
	gen treat = . // Definition of treatment factor variables 
	replace treat = 0 if id < 86 // Con
	replace treat = 1 if id  > 85 & id < 257 // Def
	replace treat = 2 if id > 256 // DefInf


	** defining mean contribution amounts and SD's per treatment group
	** Assuming a left censored normal distribution
	gen contr = . 
	replace contr = 1.67 + rnormal(0, 2.68) if treat == 0 // Con
	replace contr = 0 + rnormal(0, 3.21) if treat == 1 // Def
	replace contr = 1.15 + rnormal(0, 2.95) if treat == 2 // DefInf


	/* draw from normal distribution but set to INTEGERS as in experiment*/
	replace contr = 0 if contr <= 0.5 // left-censoring
	replace contr = 1 if contr > 0.5 & contr <= 1.5
	replace contr = 2 if contr > 1.5 & contr <= 2.5
	replace contr = 3 if contr > 2.5 & contr <= 3.5
	replace contr = 4 if contr > 3.5 & contr <= 4.5
	replace contr = 5 if contr > 4.5 & contr <= 5.5
	replace contr = 6 if contr > 5.5 & contr <= 6.5
   	replace contr = 7 if contr > 6.5 & contr <= 7.5
	replace contr = 8 if contr > 7.5 & contr <= 8.5
	replace contr = 9 if contr > 8.5 & contr <= 9.5
	replace contr = 10 if contr > 9.5 // right-censoring (not accounted for in model)
	
	

	tobit contr i.treat, ll(0)
	test 1.treat == 2.treat // b) Default vs. Default+Information
	return scalar comp1 = r(p) 
end


simulate comp1 = r(comp1), reps(100000) seed(123456): power 


/* Calculate the number of times the Tobit model correctly finds a difference
significant at alpha 0.05. */

su comp1 if comp1 < .05 


/* Pooled MDE of 1.15 at power 0.795
1.15/((3.21+2.95)/2) = 0.37 (medium)
*/


/* OPTION A: MDE of EXPERIMENT 2 */
program drop _all 
program define power,rclass // starts definition of program

	** generating a data set based on the data from the experiment

	clear
	set more off
	set obs 270 // (40 + 115 + 115)
	gen id = _n
	gen treat = . // Definition of treatment factor variables 
	replace treat = 0 if id < 41 // Con
	replace treat = 1 if id  > 40 & id < 156 // Def
	replace treat = 2 if id > 155 // DefInf


	** defining mean contribution amounts and SD's per treatment group
	** Assuming a left censored normal distribution
	gen contr = . 
	replace contr = 1.67 + rnormal(0, 2.68) if treat == 0 // Con
	replace contr = 0 + rnormal(0, 3.21) if treat == 1 // Def
	replace contr = 1.35 + rnormal(0, 2.95) if treat == 2 // DefInf


	/* draw from normal distribution but set to INTEGERS as in experiment*/
	replace contr = 0 if contr <= 0.5 // left-censoring
	replace contr = 1 if contr > 0.5 & contr <= 1.5
	replace contr = 2 if contr > 1.5 & contr <= 2.5
	replace contr = 3 if contr > 2.5 & contr <= 3.5
	replace contr = 4 if contr > 3.5 & contr <= 4.5
	replace contr = 5 if contr > 4.5 & contr <= 5.5
	replace contr = 6 if contr > 5.5 & contr <= 6.5
   	replace contr = 7 if contr > 6.5 & contr <= 7.5
	replace contr = 8 if contr > 7.5 & contr <= 8.5
	replace contr = 9 if contr > 8.5 & contr <= 9.5
	replace contr = 10 if contr > 9.5 // right-censoring (not accounted for in model)
	
	

	tobit contr i.treat, ll(0)
	test 1.treat == 2.treat // b) Default vs. Default+Information
	return scalar comp1 = r(p) 
end


simulate comp1 = r(comp1), reps(100000) seed(123456): power 


/* Calculate the number of times the Tobit model correctly finds a difference
significant at alpha 0.05. */

su comp1 if comp1 < .05 


/* MDE of NEW EXPERIMENT: 1.35 at power 0.795
1.35/((3.21+2.95)/2) = 0.44 (medium)
*/



/* OPTION B: POOLED MDE */
program drop _all 
program define power,rclass // starts definition of program

	** generating a data set based on the data from the experiment

	clear
	set more off
	set obs 269 // (45 + 46 + 43 = 134 from first experiment, additionally 135 (45 + 45 + 45)
	gen id = _n
	gen treat = . // Definition of treatment factor variables 
	replace treat = 0 if id < 91 // Con
	replace treat = 1 if id  > 90 & id < 187 // Def
	replace treat = 2 if id > 186 // DefInf


	** defining mean contribution amounts and SD's per treatment group
	** Assuming a left censored normal distribution
	gen contr = . 
	replace contr = 1.67 + rnormal(0, 2.68) if treat == 0 // Con
	replace contr = 0 + rnormal(0, 3.21) if treat == 1 // Def
	replace contr = 1.50 + rnormal(0, 2.95) if treat == 2 // DefInf


	/* draw from normal distribution but set to INTEGERS as in experiment*/
	replace contr = 0 if contr <= 0.5 // left-censoring
	replace contr = 1 if contr > 0.5 & contr <= 1.5
	replace contr = 2 if contr > 1.5 & contr <= 2.5
	replace contr = 3 if contr > 2.5 & contr <= 3.5
	replace contr = 4 if contr > 3.5 & contr <= 4.5
	replace contr = 5 if contr > 4.5 & contr <= 5.5
	replace contr = 6 if contr > 5.5 & contr <= 6.5
   	replace contr = 7 if contr > 6.5 & contr <= 7.5
	replace contr = 8 if contr > 7.5 & contr <= 8.5
	replace contr = 9 if contr > 8.5 & contr <= 9.5
	replace contr = 10 if contr > 9.5 // right-censoring (not accounted for in model)
	
	

	tobit contr i.treat, ll(0)
	test 1.treat == 2.treat // b) Default vs. Default+Information
	return scalar comp1 = r(p) 
end


simulate comp1 = r(comp1), reps(100000) seed(123456): power 


/* Calculate the number of times the Tobit model correctly finds a difference
significant at alpha 0.05. */

su comp1 if comp1 < .05 

/* Pooled MDE: 1.50, power = 0.817 
1.50/((3.21+2.95)/2) = 0.49 (medium, almost large)
*/



/* OPTION B: MDE of EXPERIMENT 2 */
program drop _all 
program define power,rclass // starts definition of program

	** generating a data set based on the data from the experiment

	clear
	set more off
	set obs 135 // (45 + 45 + 45)
	gen id = _n
	gen treat = . // Definition of treatment factor variables 
	replace treat = 0 if id < 46 // Con
	replace treat = 1 if id  > 45 & id < 91 // Def
	replace treat = 2 if id > 90 // DefInf


	** defining mean contribution amounts and SD's per treatment group
	** Assuming a left censored normal distribution
	gen contr = . 
	replace contr = 1.67 + rnormal(0, 2.68) if treat == 0 // Con
	replace contr = 0 + rnormal(0, 3.21) if treat == 1 // Def
	replace contr = 2 + rnormal(0, 2.95) if treat == 2 // DefInf


	/* draw from normal distribution but set to INTEGERS as in experiment*/
	replace contr = 0 if contr <= 0.5 // left-censoring
	replace contr = 1 if contr > 0.5 & contr <= 1.5
	replace contr = 2 if contr > 1.5 & contr <= 2.5
	replace contr = 3 if contr > 2.5 & contr <= 3.5
	replace contr = 4 if contr > 3.5 & contr <= 4.5
	replace contr = 5 if contr > 4.5 & contr <= 5.5
	replace contr = 6 if contr > 5.5 & contr <= 6.5
   	replace contr = 7 if contr > 6.5 & contr <= 7.5
	replace contr = 8 if contr > 7.5 & contr <= 8.5
	replace contr = 9 if contr > 8.5 & contr <= 9.5
	replace contr = 10 if contr > 9.5 // right-censoring (not accounted for in model)
	
	

	tobit contr i.treat, ll(0)
	test 1.treat == 2.treat // b) Default vs. Default+Information
	return scalar comp1 = r(p) 
end


simulate comp1 = r(comp1), reps(100000) seed(123456): power 


/* Calculate the number of times the Tobit model correctly finds a difference
significant at alpha 0.05. */

su comp1 if comp1 < .05 

/* Pooled MDE: 1.50, power = 0.817 
2/((3.21+2.95)/2) = 0.65 (large)
so equal to experiment 1
*/
