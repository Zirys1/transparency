************************
** Import of raw data **
************************
import excel "Z:\Projects\Transparency\Submission\Data\2 results_cleaned.xlsx", sheet("Sheet1") firstrow clear

************************
*** Data preparation ***
************************

* convert some variables to factor variables
encode Gender, generate(Genderf)
encode Treatment, generate(Treatmentf)
encode TreatmentnoC, generate(TreatmentnoCf)
encode PastParticipation, generate(PastParticipationf)
gen ImportanceD = 0
replace ImportanceD = 0 if Importance == 1
replace ImportanceD = 0 if Importance == 2
replace ImportanceD = 0 if Importance == 3
replace ImportanceD = 1 if Importance == 4
replace ImportanceD = 1 if Importance == 5

* relabel Treatmentf and TreatmentnoCf (so that Treatment factors in regression tables are shown in right order)
gen Treatmentf1 = 0
replace Treatmentf1 = 1 if Treatmentf == 1
replace Treatmentf1 = 2 if Treatmentf == 2
replace Treatmentf1 = 3 if Treatmentf == 3
replace Treatmentf1 = 4 if Treatmentf == 5
replace Treatmentf1 = 5 if Treatmentf == 4

* label factor variables
la def T 1 "Control" 2 "Default" 3 "Default+Info" 4 "Default+Purpose" 5 "Default+Info+Purpose"
label values Treatmentf1 T

* same as above
gen TreatmentnoCf1 = .
replace TreatmentnoCf1 = 1 if TreatmentnoCf == 1
replace TreatmentnoCf1 = 2 if TreatmentnoCf == 2
replace TreatmentnoCf1 = 3 if TreatmentnoCf == 4
replace TreatmentnoCf1 = 4 if TreatmentnoCf == 3

la def TnoC 1 "Default" 2 "Default+Info" 3 "Default+Purpose" 4 "Default+Info+Purpose"
label values TreatmentnoCf1 TnoC

* in order to be able to use Treatmentf1 also when Control group excluded
replace ReactanceM = . if Treatmentf1 == 1

* Treatment Dummies
gen Con = 0
replace Con = 1 if Treatmentf == 1
gen Def = 0
replace Def = 1 if Treatmentf == 2
gen DefInf = 0
replace DefInf = 1 if Treatmentf == 3
gen DefPur = 0
replace DefPur = 1 if Treatmentf == 5
gen DefInfPur = 0
replace DefInfPur = 1 if Treatmentf == 4

************************
**** Storing data ******
************************
save "Z:\Projects\Transparency\Submission\Data\4 results_prepared.dta", replace

************************
******* Load data ******
************************
use "Z:\Projects\Transparency\Submission\Data\4 results_prepared.dta", clear

***********************
**** Data analysis ****
***********************

*** Manuscript

** Regression tables
* Table 4
quietly tobit Contribution i.Treatmentf1, robust ll(0)
eststo tobit1
quietly tobit Contribution i.Treatmentf1 i.ImportanceD , robust ll(0)
eststo tobit2
quietly tobit Contribution i.Treatmentf1 i.ImportanceD i.Genderf c.Age i.PastParticipationf i.EU_ETS_Useful, robust ll(0)
eststo tobit3
* for rtf format output
esttab tobit1 tobit2 tobit3 using tobit.rtf, se star(x 0.10 * 0.05 ** 0.01 *** 0.001) nobase l nogaps
* tex format output
esttab tobit1 tobit2 tobit3 using tobit.tex, se star(x 0.10 * 0.05 ** 0.01 *** 0.001) nobase l nogaps
eststo clear

* LRtests
quietly tobit Contribution i.Treatmentf1 , ll(0) // robust not possible for lrtest
eststo t1
quietly tobit Contribution i.Treatmentf1 i.ImportanceD , ll(0) // robust not possible for lrtest
eststo t2
lrtest t1 t2 // model with Importance performs better

quietly tobit Contribution i.Treatmentf1 i.ImportanceD , ll(0) // robust not possible for lrtest
eststo t3
quietly tobit Contribution i.Treatmentf1 i.Genderf c.Age i.PastParticipationf i.EU_ETS_Useful i.ImportanceD, ll(0) // robust not possible for lrtest
eststo t4
lrtest t3 t4 // full model performs better

* Wald-test
quietly tobit Contribution i.Treatmentf1 i.Genderf c.Age i.PastParticipationf i.EU_ETS_Useful i.ImportanceD, ll(0) // robust not possible for lrtest
test 2.Treatmentf1 == 3.Treatmentf1 == 4.Treatmentf1 == 5.Treatmentf1

quietly tobit Contribution i.Def i.DefInf i.DefPur i.DefInfPur i.Genderf c.Age i.PastParticipationf i.EU_ETS_Useful i.ImportanceD, robust ll(0)
test 1.Def = 1.DefInf = 1.DefPur = 1.DefInfPur 

* Table 5
quietly ologit ThreatToFreedom i.Treatmentf1 i.ImportanceD i.Genderf c.Age i.PastParticipationf i.EU_ETS_Useful, vce(robust)
eststo ologit1
quietly ologit Anger i.Treatmentf1 i.ImportanceD i.Genderf c.Age i.PastParticipationf i.EU_ETS_Useful, vce(robust)
eststo ologit2
* for rtf format output
esttab ologit1 ologit2 using ologit.rtf, se star(x 0.10 * 0.05 ** 0.01 *** 0.001) nobase l nogaps
* tex format output
esttab ologit1 ologit2 using ologit.tex, se star(x 0.10 * 0.05 ** 0.01 *** 0.001) nobase l nogaps
eststo clear


* Table 6
quietly tobit Contribution i.Treatmentf1##c.ReactanceM, robust ll(0)
eststo interact1
quietly tobit Contribution i.Treatmentf1##c.ReactanceM i.ImportanceD , robust ll(0)
eststo interact2
quietly tobit Contribution i.Treatmentf1##c.ReactanceM i.ImportanceD i.Genderf c.Age i.PastParticipationf i.EU_ETS_Useful, robust ll(0)
eststo interact3
* for rtf format output
esttab interact1 interact2 interact3 using interact11.rtf, se star(x 0.10 * 0.05 ** 0.01 *** 0.001) nobase l nogaps
* tex format output
esttab interact1 interact2 interact3 using interact11.tex, se star(x 0.10 * 0.05 ** 0.01 *** 0.001) nobase l nogaps
eststo clear

* LRtests
quietly tobit Contribution i.Treatmentf1##c.ReactanceM , ll(0) // robust not possible for lrtest
eststo t1
quietly tobit Contribution i.Treatmentf1##c.ReactanceM i.ImportanceD , ll(0) // robust not possible for lrtest
eststo t2
lrtest t1 t2 // model with Importance performs better

quietly tobit Contribution i.Treatmentf1##c.ReactanceM i.ImportanceD , ll(0) // robust not possible for lrtest
eststo t3
quietly tobit Contribution i.Treatmentf1##c.ReactanceM i.Genderf c.Age i.PastParticipationf i.EU_ETS_Useful i.ImportanceD, ll(0) // robust not possible for lrtest
eststo t4
lrtest t3 t4 // full model performs better



* Figure B.5
quietly tobit Contribution i.Treatmentf1 i.ImportanceD i.Genderf c.Age i.PastParticipationf i.EU_ETS_Useful, robust ll(0)
eststo tobit3
quietly tobit Contribution ib2.Treatmentf1 i.ImportanceD i.Genderf c.Age i.PastParticipationf i.EU_ETS_Useful, robust ll(0)
eststo tobit3a
quietly tobit Contribution ib4.Treatmentf1 i.ImportanceD i.Genderf c.Age i.PastParticipationf i.EU_ETS_Useful, robust ll(0)
eststo tobit3b

coefplot tobit3 , xline(0) drop(_cons ?.ImportanceD ?.Genderf Age ?.PastParticipationf ?.EU_ETS_Usefulness) omitted base levels(95) xlabel(-4(1)4) saving(H1x) ///
headings(1.Treatmentf1 = "{it:Treatment effects:}")

coefplot tobit3a , xline(0) drop(_cons ?.ImportanceD ?.Genderf Age ?.PastParticipationf ?.EU_ETS_Usefulness) omitted base levels(95) xlabel(-4(1)4) saving(H2x) ///
headings(1.Treatmentf1 = "{it:Treatment effects:}")

coefplot tobit3b , xline(0) drop(_cons ?.ImportanceD ?.Genderf Age ?.PastParticipationf ?.EU_ETS_Usefulness) omitted base levels(95) xlabel(-4(1)4) saving(H4x) ///
headings(1.Treatmentf1 = "{it:Treatment effects:}")

quietly tobit Contribution i.TreatmentnoCf1##c.ReactanceM i.ImportanceD i.Genderf c.Age i.PastParticipationf i.EU_ETS_Useful, robust ll(0)
eststo interact3

coefplot interact3 , xline(0) drop(_cons ?.ImportanceD ?.Genderf Age ?.PastParticipationf ?.EU_ETS_Usefulness) omitted base levels(95) xlabel(-4(1)4) saving(H6x) ///
headings(1.TreatmentnoCf1 = "{it:Treatment effects:}" "Default" = "{it:Reactance interaction:}" "Reactance" = "{it:Reactance effect:}") ///
rename(1.TreatmentnoCf1#c.ReactanceM = "Default" 2.TreatmentnoCf1#c.ReactanceM = "Default+Info" ReactanceM = "Reactance" ///
3.TreatmentnoCf1#c.ReactanceM = "Default+Info+Purpose" 4.TreatmentnoCf1#c.ReactanceM = "Default+Purpose")

gr combine H1x.gph H2x.gph H4x.gph H6x.gph
eststo clear

* Figure B.6
quietly ologit ThreatToFreedom i.TreatmentnoCf1 i.ImportanceD i.Genderf c.Age i.PastParticipationf i.EU_ETS_Useful, vce(robust)
margins, dydx(*) predict(outcome(4)) post
eststo ThreatToFreedom
coefplot ThreatToFreedom, xline(0) levels(95) drop(_cons ?.ImportanceD ?.Genderf Age ?.PastParticipationf ?.EU_ETS_Usefulness) omitted base xlabel(-0.1(0.05)0.1) saving(H5a) ///
headings(1.TreatmentnoCf1 = "{it:Treatment effects:}")
eststo clear

* Figure B.7
quietly ologit Anger i.TreatmentnoCf1 i.ImportanceD i.Genderf c.Age i.PastParticipationf i.EU_ETS_Useful, vce(robust)
margins, dydx(*) predict(outcome(4)) post
eststo Anger
coefplot Anger, xline(0) levels(95) drop(_cons ?.ImportanceD ?.Genderf Age ?.PastParticipationf ?.EU_ETS_Usefulness) omitted base xlabel(-0.1(0.05)0.1) saving(H5b) ///
headings(1.TreatmentnoCf1 = "{it:Treatment effects:}")
eststo clear


