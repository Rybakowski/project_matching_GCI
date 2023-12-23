set matsize 10000
ssc install estout, replace

* Indicate below the folder where you have put all the ado files
adopath ++ "/Users/xdhaulfoeuille/Dropbox/Two-way fixed effects/Applications/Ado files"

* Indicate below the folder where you have put the .dta files
use "/Users/xdhaulfoeuille/Dropbox/Two-way fixed effects/Applications/Media and political persuasion/NTV_Aggregate_Data.dta", clear

qui{
* Data manipulation
drop if Votes_Edinstvo_1999   ==.
rename  Turnout_2003  Turnout2003
rename logpop98 logpop1999
rename logpop96 logpop1995
rename wage98_ln logwage1999
rename wage96_ln logwage1995

rename Votes_DVR_1995 Votes_SPS_1995
gen Watch_probit_2003=Watch_probit 
gen Watch_probit_1995=0
rename Watch_probit  Watch_probit_1999

reshape long Watch_probit_ Votes_SPS_  Turnout Votes_Yabloko_  Votes_KPRF_ ///
Votes_LDPR_ logpop logwage, i(tik_id)
xi i._j

label var Watch_probit_ "NTV coverage"
label var _I_j_1999 "Year 1999 dummy"

* Selecting the sample
keep if _I_j_1995==1|_I_j_1999==1
replace population1996=round(population1996)


* Basic unweighted regression (reproducing Enikolopov et al.'s Table 3)
xtreg Votes_SPS_ Watch_probit_  _I_j_1999  if _j!=2003, fe i(tik_id) ///
cluster (tik_id)
scalar b_unweighted=_b[Watch_probit_]
eststo

* Same regression, but weighted by population
areg Votes_SPS_ _I_j_1999 Watch_probit_ [fweight=population1996], ///
absorb(tik_id) cluster (tik_id)
scalar b_weighted=_b[Watch_probit_]
eststo

/* The following table produces the coefficients of the initial regression in 
  Enikolopov et al. (beta_fe and its s.e., end of p.4) and the regression
  weighted by population (see the 2nd paragraph p.5) */
}
esttab, se nostar mti(Unweighted Weighted) l noobs 

qui{
eststo clear

scalar diff=_b[Watch_probit_]-b_unweighted

/* Significant difference between unweighted and weighted?
 Computation of the t-statistic b/w the two coeff. (see the 2nd paragraph p.5)*/

set seed 1
matrix R=0
forvalue i=1/200{
preserve
bsample, cluster(tik_id)
areg Votes_SPS_ _I_j_1999 Watch_probit_, absorb(tik_id) cluster (tik_id)
scalar b_unweighted_bs=_b[Watch_probit_]
areg Votes_SPS_ _I_j_1999 Watch_probit_ [fweight=population1996], ///
absorb(tik_id) cluster (tik_id)
scalar diff_bs=_b[Watch_probit_]-b_unweighted_bs
matrix R=R\diff_bs
restore
}
preserve 
drop _all
svmat R
drop if _n==1
sum R1
}
di "The t-statistic b/w the coeffs of the weighted and unweighted regression" ///
" is equal to " diff/r(sd)
qui{
restore

set seed 1
}
/* Computing the weights attached to betaFE (see 1st paragraph p.5) + correlation 
   b/w these weights and population (see 2nd paragraph p.5) */
twowayfeweights Votes_SPS_ tik_id _I_j_1999 Watch_probit_, type(feTR) ///
test_random_weights(logpop)

adopath - 1
