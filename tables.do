*net install dlogit2.pkg 
**************
**# Table 1 **
**************

use "raw/encuesta/RedistributionFinal.dta", clear
local vars edad female yrs_educ married trabajo asegurado padres_indigenas
putexcel set "tables/table_1.xlsx", replace

putexcel A1 = "Variable"
putexcel A2 = "Age (years)"
putexcel A3 = "Women"
putexcel A4 = "Years of schooling"
putexcel A5 = "Married"
putexcel A6 = "Employed"
putexcel A7 = "Enrolled in Social Security (among those employed)"
putexcel A8 = "Indigenous parents"

putexcel B1 = "Sample mean"

local it = 1
local j = 2
foreach y of varlist `vars'{
	
	if inlist(`it', 2, 4, 5, 7){
		sum `y', format
		putexcel B`j' = `r(mean)', nformat(#.0%)
	}
	else if `it' == 6{
		
		sum `y' if trabajo == 1, format
		putexcel B`j' = `r(mean)', nformat(#.0%)

	}
	
	else{
		sum `y', format
		putexcel B`j' = `r(mean)', nformat(#.0)
	}
	local it = `it' + 1
	local j = `j' + 1
}

putexcel save

**************
**# Table 2 **
**************

use "raw/encuesta/RedistributionFinal.dta", clear

* Recoding variables
recode sec1_p_2 (1 = 0)(2 = 1)
recode sec5_p_55 (1 = 0)(2 = 1)
gen social_mobility = decil_actual - decil_14anios
recode sec3_p_36_3_a 1/2 = 0 3/5 = 1, generate(everyone)
recode sec3_p_36_3_b 1/2 = 0 3/5 = 1, generate(rich)

*renaming variables
rename sec1_p_2 sex
rename social_mobility mob
rename index_wealth wealth
rename sec5_p_55 inherited
rename sec5_p_58 exbeq
rename sec5_p_59 receive
rename sec3_p_25_2_3 epoor
rename sec3_p_25_3_3 emiddle
rename sec3_p_25_4_3 erich
rename sec3_p_25_5_3 evrich
rename index_poverty poverty
rename sec3_p_30_b inwealth
rename sec3_p_30_c corwealth
rename sec3_p_29_a ppoor
rename sec3_p_29_b prich
rename sec3_p_29_c pvrich
rename index_ineq pineq
rename index_trust trust
rename index_efficiency eff
rename index_social socialpro
rename index_econ econpro

local controles "sex mob wealth inherited exbeq receive epoor emiddle erich evrich poverty inwealth corwealth ppoor prich pvrich pineq trust eff socialpro econpro"

regress everyone `controles'  [aw=weight], beta robust
outreg2 using tables/table_2, dec(3) replace excel

regress rich `controles' [aw=weight], beta  robust
outreg2 using tables/table_2, dec(3) append excel

***************
**# Table S1 **
***************

dlogit2 everyone `controles' [aw=weight]
outreg2 using tables/table_s1, replace dec(3) excel

dlogit2 rich `controles' [aw=weight]
outreg2 using tables/table_s1, append dec(3) excel


***************
**# Table S2 **
***************
putexcel set "tables/table_s2.xlsx", replace
recode sec3_p_24_j (3/5 = 1) (1/2 = 0), generate(agree_legitimate)
recode sec3_p_24_m (3/5 = 1) (1/2 = 0), generate(agree_fair)
recode sec3_p_24_o (3/5 = 1) (1/2 = 0), generate(disagree_interfere)

putexcel A1 = "Tax"
putexcel B1 = "Support"
putexcel C1 = "Inherited wealth is as legitimate as self-made wealth."
putexcel D1 = "Inherited wealth is as legitimate as self-made wealth."
putexcel E1 = "It is fair to have an inheritance tax."
putexcel F1 = "It is fair to have an inheritance tax."
putexcel G1 = "Government shouldn't interfere in wealth transfers between parents and children."
putexcel H1 = "Government shouldn't interfere in wealth transfers between parents and children."

putexcel C2 = "Agree"
putexcel D2 = "Disagree"
putexcel E2 = "Agree"
putexcel F2 = "Disagree"
putexcel G2 = "Agree"
putexcel H2 = "Disagree"

putexcel A3 = "No threshold"
putexcel A4 = "No threshold"
putexcel A5 = "> USD $1 million (PPP)"
putexcel A6 = "> USD $1 million (PPP)"

putexcel B3 = "Does not support"
putexcel B4 = "Support"
putexcel B5 = "Does not support"
putexcel B6 = "Support"

* Inherited wealth is as legitimate as self-made wealth.
sum agree_legitimate if everyone == 0
local a = `r(mean)'
local d =  1- `r(mean)'
putexcel C3 = `a'
putexcel D3 = `d'

sum agree_legitimate if everyone == 1
local a = `r(mean)'
local d =  1- `r(mean)'
putexcel C4 = `a'
putexcel D4 = `d'

sum agree_legitimate if rich == 0
local a = `r(mean)'
local d =  1- `r(mean)'
putexcel C5 = `a'
putexcel D5 = `d'

sum agree_legitimate if rich == 1
local a = `r(mean)'
local d =  1- `r(mean)'
putexcel C6 = `a'
putexcel D6 = `d'

* It is fair to have an inheritance tax.
sum agree_fair if everyone == 0
local a = `r(mean)'
local d =  1- `r(mean)'
putexcel E3 = `a'
putexcel F3 = `d'

sum agree_fair if everyone == 1
local a = `r(mean)'
local d =  1- `r(mean)'
putexcel E4 = `a'
putexcel F4 = `d'

sum agree_fair if rich == 0
local a = `r(mean)'
local d =  1- `r(mean)'
putexcel E5 = `a'
putexcel F5 = `d'

sum agree_fair if rich == 1
local a = `r(mean)'
local d =  1- `r(mean)'
putexcel E6 = `a'
putexcel F6 = `d'

* It is fair to have an inheritance tax.
sum disagree_interfere if everyone == 0
local a = `r(mean)'
local d =  1- `r(mean)'
putexcel G3 = `a'
putexcel H3 = `d'

sum disagree_interfere if everyone == 1
local a = `r(mean)'
local d =  1- `r(mean)'
putexcel G4 = `a'
putexcel H4 = `d'

sum disagree_interfere if rich == 0
local a = `r(mean)'
local d =  1- `r(mean)'
putexcel G5 = `a'
putexcel H5 = `d'

sum disagree_interfere if rich == 1
local a = `r(mean)'
local d =  1- `r(mean)'
putexcel G6 = `a'
putexcel H6 = `d'

putexcel close

** Demographic variables
***************************
**# Romano-Wolf p-values **
***************************

use "raw/encuesta/RedistributionFinal.dta", clear

* Recoding variables
recode sec1_p_2 (1 = 0)(2 = 1)
recode sec5_p_55 (1 = 0)(2 = 1)
gen social_mobility = decil_actual - decil_14anios
recode sec3_p_36_3_a 1/2 = 0 3/5 = 1, generate(everyone)
recode sec3_p_36_3_b 1/2 = 0 3/5 = 1, generate(rich)

*renaming variables
rename sec1_p_2 sex
rename social_mobility mob
rename index_wealth wealth
rename sec5_p_55 inherited
rename sec5_p_58 exbeq
rename sec5_p_59 receive
rename sec3_p_25_2_3 epoor
rename sec3_p_25_3_3 emiddle
rename sec3_p_25_4_3 erich
rename sec3_p_25_5_3 evrich
rename index_poverty poverty
rename sec3_p_30_b inwealth
rename sec3_p_30_c corwealth
rename sec3_p_29_a ppoor
rename sec3_p_29_b prich
rename sec3_p_29_c pvrich
rename index_ineq pineq
rename index_trust trust
rename index_efficiency eff
rename index_social socialpro
rename index_econ econpro

* Dependent variables
local dep "everyone rich"

putexcel set "tables/rwolf.xlsx", replace

putexcel A1 = "Independent variable"
putexcel B1 = "No exemption p-value"
putexcel C1 = "No exemption rw p-value"
putexcel D1 = "1 million USD p-value"
putexcel E1 = "1 million USD rw p-value"
putexcel F1 = "Changes"

**# Demographic variables
local fact "sex mob wealth"
local controles "inherited exbeq receive epoor emiddle erich evrich poverty inwealth corwealth ppoor prich pvrich pineq trust eff socialpro econpro"

putexcel A2 = "Female"
putexcel A3 = "Social mobility"
putexcel A4 = "Wealth"

rwolf `dep' [aw=weight], indepvar(`fact') controls(`controles') method(reg) reps(500) seed(12345) robust verbose

ereturn list

local i = 2
foreach v of varlist `fact'{
	putexcel B`i' = matrix(e(RW_`v')[1,1])
	putexcel C`i' = matrix(e(RW_`v')[1,3])
	putexcel D`i' = matrix(e(RW_`v')[2,1])
	putexcel E`i' = matrix(e(RW_`v')[2,3])
	local i = `i' + 1
}


**# Self-interest
local fact "inherited exbeq receive"
local controles "sex mob wealth epoor emiddle erich evrich poverty inwealth corwealth ppoor prich pvrich pineq trust eff socialpro econpro"

putexcel A5 = "Has inherited"
putexcel A6 = "How much expects to bequeath"
putexcel A7 = "How much expects to receibe in inheritance"

rwolf `dep' [aw=weight], indepvar(`fact') controls(`controles') method(reg) reps(500) seed(12345) robust verbose

local i = `i'
foreach v of varlist `fact'{
	putexcel B`i' = matrix(e(RW_`v')[1,1])
	putexcel C`i' = matrix(e(RW_`v')[1,3])
	putexcel D`i' = matrix(e(RW_`v')[2,1])
	putexcel E`i' = matrix(e(RW_`v')[2,3])
	local i = `i' + 1
}

**# Peer effects
local fact "epoor emiddle erich evrich"
local controles "sex mob wealth inherited exbeq receive poverty inwealth corwealth ppoor prich pvrich pineq trust eff socialpro econpro"

putexcel A8 = "Tax evasion (poor households)"
putexcel A9 = "Tax evasion (middle-class households)"
putexcel A10 = "Tax evasion (rich households)"
putexcel A11 = "Tax evasion (very rich households)"

rwolf `dep' [aw=weight], indepvar(`fact') controls(`controles') method(reg) reps(500) seed(12345) robust verbose

local i = `i'
foreach v of varlist `fact'{
	putexcel B`i' = matrix(e(RW_`v')[1,1])
	putexcel C`i' = matrix(e(RW_`v')[1,3])
	putexcel D`i' = matrix(e(RW_`v')[2,1])
	putexcel E`i' = matrix(e(RW_`v')[2,3])
	local i = `i' + 1
}

**# Fairness
local fact "poverty inwealth corwealth"
local controles "sex mob wealth inherited exbeq receive epoor emiddle erich evrich ppoor prich pvrich pineq trust eff socialpro econpro"

putexcel A12 = "Perception of poverty as explainable by personal effort"
putexcel A13 = "Proportion of wealth of the rich by inheritance"
putexcel A14 = "Proportion of wealth of the rich by corruption"

rwolf `dep' [aw=weight], indepvar(`fact') controls(`controles') method(reg) reps(500) seed(12345) robust verbose

local i = `i'
foreach v of varlist `fact'{
	putexcel B`i' = matrix(e(RW_`v')[1,1])
	putexcel C`i' = matrix(e(RW_`v')[1,3])
	putexcel D`i' = matrix(e(RW_`v')[2,1])
	putexcel E`i' = matrix(e(RW_`v')[2,3])
	local i = `i' + 1
}


**# Distributive preferences
local fact "ppoor prich pvrich pineq"
local controles "sex mob wealth inherited exbeq receive epoor emiddle erich evrich poverty inwealth corwealth trust eff socialpro econpro"

putexcel A15 = "Proportion of poor households"
putexcel A16 = "Proportion of rich households"
putexcel A17 = "Proportion of very rich households"
putexcel A18 = "Preoccupation for inequality"

rwolf `dep' [aw=weight], indepvar(`fact') controls(`controles') method(reg) reps(500) seed(12345) robust verbose

local i = `i'
foreach v of varlist `fact'{
	putexcel B`i' = matrix(e(RW_`v')[1,1])
	putexcel C`i' = matrix(e(RW_`v')[1,3])
	putexcel D`i' = matrix(e(RW_`v')[2,1])
	putexcel E`i' = matrix(e(RW_`v')[2,3])
	local i = `i' + 1
}

**# Ideological factors
local fact "socialpro econpro"
local controles "sex mob wealth inherited exbeq receive epoor emiddle erich evrich poverty inwealth corwealth ppoor prich pvrich pineq trust eff"

putexcel A19 = "Social progressivism"
putexcel A20 = "Economic progressivism"

rwolf `dep' [aw=weight], indepvar(`fact') controls(`controles') method(reg) reps(500) seed(12345) robust verbose

local i = `i'
foreach v of varlist `fact'{
	putexcel B`i' = matrix(e(RW_`v')[1,1])
	putexcel C`i' = matrix(e(RW_`v')[1,3])
	putexcel D`i' = matrix(e(RW_`v')[2,1])
	putexcel E`i' = matrix(e(RW_`v')[2,3])
	local i = `i' + 1
}

**# Changes
forvalues i = 2/20{
	putexcel F`i' = formula(CONCAT(CONCAT("No exemption: ", IF(AND(B`i'<0.05, C`i'>= 0.05),1,0), "; "), CONCAT("1m+ USD: ", IF(AND(D`i'<0.05, E`i'>= 0.05),1,0), ".")))
}
