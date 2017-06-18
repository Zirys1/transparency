/* 
- Can Nudges Be Transparent and Yet Effective? -
---------------- Power Analysis ----------------
------------------ 15.06.2017 ------------------
*/

/*
We have the following data (see Table 2 from Manuscript):
Control					n = 45		Contribution (Mean) = 1.67		Contribution (SD) = 2.68			
Default					n = 46		Contribution (Mean) = 3.24		Contribution (SD) = 3.21	
Default+Info			n = 43		Contribution (Mean) = 2.49		Contribution (SD) = 2.95	
Default+Purpose			n = 39		Contribution (Mean) = 2.92		Contribution (SD) = 3.19	
Default+Info+Purpose	n = 41		Contribution (Mean) = 2.85		Contribution (SD) = 2.95
--------------------------------------------------------------------------------------------	
OVERALL					n = 214		Contribution (Mean) = 2.63		Contribution (SD) = 3.02	
*/


/* Number of simulations set to 100,000, which takes a long time. Set to 1,000 for 
fast results, and to 20,000 for good results 
Seed is set to 123456
*/

*****************************
*****[1] OBSERVED POWER *****
*****************************
set more off
program drop _all 
program define power,rclass // starts definition of program

	** generating a data set based on the data from the experiment

	clear
	set more off
	set obs 214 // Defining number of observations from original study
	gen id = _n // Generating id variable
	gen treat = . // Definition of treatment factor variables 
	replace treat = 0 if id < 46 // Conrol
	replace treat = 1 if id > 45 & id < 92 // Default
	replace treat = 2 if id > 91 & id < 135 // Default+Information
	replace treat = 3 if id > 134 & id < 174 // Default+Purpose
	replace treat = 4 if id > 173 //Default+Information+Purpose

	** Defining mean contribution amounts and SD's per treatment group
	** Assuming a left censored normal distribution
	gen contr = . 
	replace contr = 1.67 + rnormal(0, 2.68) if treat == 0 // Con
	replace contr = 3.24 + rnormal(0, 3.21) if treat == 1 // Def
	replace contr = 2.49 + rnormal(0, 2.95) if treat == 2 // DefInf
	replace contr = 2.92 + rnormal(0, 3.19) if treat == 3 // DefPur
	replace contr = 2.85 + rnormal(0, 2.95) if treat == 4 // DefInfPur

	/* Set draws from normal distribution to INTEGERS, as in experiment*/
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
	
	
	** Test contrasts
	
	tobit contr i.treat, ll(0)
	test 0.treat == 1.treat // Control vs. Default
	return scalar comp1 = r(p) 
	test 0.treat == 2.treat // Control vs. Default+Information
	return scalar comp2 = r(p) 
	test 0.treat == 3.treat // Control vs. Default+Purpose
	return scalar comp3 = r(p) 
	test 0.treat == 4.treat // Control vs. Default+Information+Purpose
	return scalar comp4 = r(p) 
	test 1.treat == 2.treat // Default vs. Default+Information
	return scalar comp5 = r(p) 
	test 1.treat == 3.treat // Default vs. Default+Purpose
	return scalar comp6 = r(p) 
	test 2.treat == 3.treat == 4.treat // Default+Info vs. Default+Purpose vs. Default+Info+Purpose
	return scalar comp7 = r(p) 

end

/* Simulation of the above program. Number of times X specified in reps(X).
Should at best be 20000, but takes a long time. */
simulate comp1 = r(comp1) comp2 = r(comp2) comp3 = r(comp3) comp4 = r(comp4) comp5 = r(comp5) comp6 = r(comp6) comp7 = r(comp7), reps(100000) seed(123456): power // power is the program we just defined above


/* Calculate the number of times the Tobit model correctly finds a difference
significant at alpha 0.05. This is the power of the test */

su comp5 if comp5 < .05 // summarize statistics only when p-value below 5% threshold

/* Dividing the outcome by the number of simulation draws gives the power. 
With 100,000 simulation draws: 22355/100000 = 0.22355 */



*****************************************************
*****[2] MINIMUM DETECTABLE EFFECT AT 80% POWER *****
*****************************************************
set more off
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
********************************************************************************
** The following two means need to be calibrated until the difference can be detected with 80% power **
	replace contr = 4 + rnormal(0, 3.21) if treat == 1 // Def
	replace contr = 2.1 + rnormal(0, 2.95) if treat == 2 // DefInf
********************************************************************************
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
	test 1.treat == 2.treat // Default vs. Default+Information
	return scalar comp1 = r(p) 
end


simulate comp1 = r(comp1), reps(100000) seed(123456): power 

su comp1 if comp1 < .05 

/* The number of times a true effect of 4 - 2.1 = 1.9 between Def and DefInf is 
detected by deviding the number the outcome by the number of simulation draws. 
With 100,000 simulation draws:  82331/100000 = 0.82331*/




*************************************************************************************
*****[3] NUMBER OF OBSERVATIONS NEEDED TO DETECT OBSERVED EFFECT WITH 80% POWER *****
*************************************************************************************
set more off
program drop _all 
program define power,rclass // starts definition of program

	** generating a data set based on the data from the experiment

	clear
	set more off
********************************************************************************
** The number of observations need to be calibrated so observed effect is detected 80% of the time **
	set obs 500 
********************************************************************************
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
	test 1.treat == 2.treat // Default vs. Default+Information
	return scalar comp1 = r(p) 
end


simulate comp1 = r(comp1), reps(100000) seed(123456): power 

su comp1 if comp1 < .05 

/* The power a true effect equal to the observed effect in the experiment 
(3.24 - 2.49 = 0.75) is detected given 250 observations in each of the Default, 
and Default+Information group is calculated by dividing the outcome by the number
of simulation draws. 
With 100,000 simulation draws: 77771/100000 = 0.77771
*/



**************************************************************************************************
*****[4] POWER TO DETECT THE MINIMUM EFFECT OF INTEREST at 80% power (1.15 EUR) WITH POOLING *****
**************************************************************************************************
set more off
program drop _all 
program define power,rclass // starts definition of program

	** generating a data set based on the data from the experiment

	clear
	set more off
********************************************************************************
** The number of observations is set to observations from Experiment 1 and new experiment **
	set obs 404 // (45 + 46 + 43 = 134 from first experiment, additionally 270 (40 + 115 + 115)
********************************************************************************
	gen id = _n
	gen treat = . // Definition of treatment factor variables 
********************************************************************************
** Observations from Experiments 1 and 2 per group
	replace treat = 0 if id < 86 // Con
	replace treat = 1 if id  > 85 & id < 257 // Def
	replace treat = 2 if id > 256 // DefInf
********************************************************************************


	** defining mean contribution amounts and SD's per treatment group
	** Assuming a left censored normal distribution
	gen contr = . 
	replace contr = 1.67 + rnormal(0, 2.68) if treat == 0 // Con
********************************************************************************
** The following two means need to be calibrated until the difference can be detected with 80% power **
	replace contr = 0 + rnormal(0, 3.21) if treat == 1 // Def
	replace contr = 1.15 + rnormal(0, 2.95) if treat == 2 // DefInf
********************************************************************************

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
	test 1.treat == 2.treat // Default vs. Default+Information
	return scalar comp1 = r(p) 
end


simulate comp1 = r(comp1), reps(1000) seed(123456): power 


su comp1 if comp1 < .05 


/* The power to detect a hypothetical true effect of 1.15 with this test and these
assumptions is equal to the outcome above divided by the number of simulation draws
With 100,000 simulation draws: 78814/100000 = 0.78814
*/



*****************************************************************************************************
*****[5] POWER TO DETECT THE MINIMUM EFFECT OF INTEREST at 80% power (1.35 EUR) WITHOUT POOLING *****
*****************************************************************************************************
set more off
program drop _all 
program define power,rclass // starts definition of program

	** generating a data set based on the data from the experiment

	clear
	set more off
********************************************************************************
** The number of observations is set to observations from the new experiment **
	set obs 270 // (40 + 115 + 115)
********************************************************************************
	gen id = _n
	gen treat = . // Definition of treatment factor variables 
********************************************************************************
** Observations from Experiment 2 per group
	replace treat = 0 if id < 41 // Con
	replace treat = 1 if id  > 40 & id < 156 // Def
	replace treat = 2 if id > 155 // DefInf
********************************************************************************


	** defining mean contribution amounts and SD's per treatment group
	** Assuming a left censored normal distribution
	gen contr = . 
	replace contr = 1.67 + rnormal(0, 2.68) if treat == 0 // Con
********************************************************************************
** The following two means need to be calibrated until the difference can be detected with 80% power **
	replace contr = 0 + rnormal(0, 3.21) if treat == 1 // Def
	replace contr = 1.35 + rnormal(0, 2.95) if treat == 2 // DefInf
********************************************************************************


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
	test 1.treat == 2.treat // Default vs. Default+Information
	return scalar comp1 = r(p) 
end


simulate comp1 = r(comp1), reps(100000) seed(123456): power 


su comp1 if comp1 < .05 

/* The power to detect a hypothetical true effect of 1.35 with this model under
these assumption is equal to dividing the effect by the number of simulation draws
With 100,000 simulation draws: 80184/100000 = 0.80184
*/
