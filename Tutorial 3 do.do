*-------------------------------------------------------------------------------
* UNDERSTANDING SOCIAL PROTECTION, March 2021
* Tutorial: Microsimulation
*-------------------------------------------------------------------------------
* infile Tutorial_Dataset_mod2.dta
* outfile: Tutorial_Dataset_final.dta
* log: Microsimulation_Fantasia.log
* last update: 09/03/2021
/* 
	SECTIONS
	 (1) 	Introduction of Benefit Design
	 (2)  	Identification of Eligible Beneficiaries
	 (3)  	Calculation of Simulated Benefits
	 (4)  	Assigning Benefits to Eligible Beneficiaries
     (5)  	Market and Disposable Income
	 (6)  	Poverty and Inequality Impact
	 (7)  	Benefit Incidence Analysis 
	 (8)  	Poverty Efficiency
	 (9) 	Cost Effectiveness
	 (10) 	Graphing the Change                     	*/
*-------------------------------------------------------------------------------

set more off
clear

cd "C:\Users\chen\Desktop\SPP2021\4206"
use output\Tutorial_Dataset_mod2.dta
/*Note: Use the previous modified data for Fantasia 03/2021*/

cap log close
log using "Microsimulation_Fantasia1.log", replace

*-------------------------------------------------------------------------------
* (1) Benefit Design
*-------------------------------------------------------------------------------

/* We simulate three different benefits:
      1) Child benefit (Household level)
	  2) Unemployment assistance (Individual level)
	  3) Family income support (Household level)
	  
Cf. Document on CANVAS: Benefit design */

*-------------------------------------------------------------------------------
* (2) Identification of Eligible Beneficiaries
*-------------------------------------------------------------------------------

* (2.1) Child Benefit
gen child=(c306<=17)    //Assign value 1 to the ones c306<=17, 0 to c306>17
bys hhnumber: egen num_child=total(child)   //Calculate total number of children per household
	replace num_child=. if hh_age_mis==1    
	//Note: If there is member with missing age, we assume the number of child is missing
	
tab num_child [aw=relwt], m

gen elig_CB=.
	replace elig_CB=0 if num_child==0
	replace elig_CB=1 if num_child>0 & num_child<.
	replace elig_CB=0 if (num_child>0 & num_child<=.) & head!=1  
	//Note: Household level benefit, assign to head only
	label variable elig_CB "Eligibility (household head) - Child Benefit (CB)"
	label values elig_CB label_binary

tab elig_CB [aw=relwt], m

* (2.2) Unemployment Assistance
tab unemployed [aw=relwt] if c306>=18 & c306<=65, m  //unemployment defined previously

gen elig_UA=.
	replace elig_UA=0 if c306<18 | (c306>65 & c306<.) | unemployed==0 | (pc_income>=300 & pc_income<.)
	//Note: above shows all paralleled condition, pay attention to the use of | and ()
	replace elig_UA=1 if c306>= 18 & c306<=65 & unemployed==1 & pc_income<300
	replace elig_UA=. if age_mis==1 | unemployed==.
	label variable elig_UA "Eligibility (individual) - Unemployment Assistance (UA)"
	label values elig_UA label_binary
	
tab elig_UA [aw=relwt], m

* (2.3) Family Income Support
gen employed_2=employed
	replace employed_2=1 if c335==1   //Reference period: 7 days
	label define label_employed_2 0 "Not employed in preceding 7 days and not temporarily absent" 1 "Employed in preceding 7 days or only temporarily absent" 
	label values employed_2 label_employed_2
	lab var employed "Employed in preceding 7 days or only temporarily absent"
	
rename c344 employment_type
	label define label_employment_type 1 "Paid employee" 2 "Employer" 3 "Own-account worker" 4 "Unpaid family worker" 5 "Unpaid worker"
	label values employment_type employment_type_label
	lab var employment_type "Type of employment"

gen elig_FIS_ind=.
	replace elig_FIS_ind=0 if c306<18 | c306>65 | employed_2==0 | num_child==0 | (employment_type>3 & employment_type<.)
	replace elig_FIS_ind=1 if c306>=18 & c306<= 65 & employed_2==1 & employment_type<=3 & num_child>0
	replace elig_FIS_ind=. if hh_age_mis==1 | employed_2==.

tab elig_FIS_ind [aw=relwt], m

bys hhnumber: egen elig_FIS=max(elig_FIS_ind)  //Household level benefit, so has to aggregate
	replace elig_FIS=. if hh_age_mis==1
	replace elig_FIS=0 if elig_FIS==1 & head!=1
	label variable elig_FIS "Eligibility (household head) - Family Income Support (FIS)"
	label values elig_FIS label_binary
	
tab elig_FIS [aw=relwt], m
drop elig_FIS_ind

*-------------------------------------------------------------------------------
* (3) Calculation of Simulated Benefits
*-------------------------------------------------------------------------------

* (3.1) Child Benefit
gen CB=0
	replace CB=120 if num_child==1
	replace CB=120+(num_child-1)*60 if num_child== 2 | num_child==3
	replace CB=240+(num_child-3)*36 if num_child>3 & num_child!=.
	label variable CB "CB (per year)"
	//Note: Can do it via loop as well, here just an example. 
tab num_child CB

* (3.2) Unemployment Assistance
gen UA=240
	label variable UA "UA (per year)"
	
* (3.3) Family Income Support
gen FIS=0
	replace FIS=600 if num_child>0 & num_child<=4 
	replace FIS=1200 if num_child>4 & num_child<=9
	replace FIS=1800 if num_child>9
  	label variable FIS  "FIS (pear year)"
			
*-------------------------------------------------------------------------------
* (4) Assigning Benefits to Eligible Beneficiaries
*-------------------------------------------------------------------------------
/* Note: This part will remove the benefits for the non-beneficiaries*/
 
* (4.1) Child Benefit
	replace CB=0 if elig_CB==0
	replace CB=. if elig_CB==.

tab CB elig_CB [aw=relwt], m

* (4.2) Unemployment Assistance
	replace UA=0 if elig_UA==0 
	replace UA=. if elig_UA==.

tab UA elig_UA [aw=relwt], m

* (4.3) Family Income Support
codebook c1001_1_ c1001_1v c1001_2_ c1001_2v  //Household level labor income
	recode c1001_1_ c1001_2_ (1=1) (2=0) (8=.)
	replace c1001_1v=0 if c1001_1_==0
	replace c1001_2v=0 if c1001_2_==0
  
gen empl_income=c1001_1v+c1001_2v
twoway (scatter empl_inc hhnumber) 
	replace empl_income=. if empl_income>=60000  //Outliers 

  replace FIS=0 if elig_FIS==0
  replace FIS= (FIS-empl_inc)*0.6  //0.6 as the withdrawal rate
  replace FIS=0 if FIS<0 
  replace FIS =. if elig_FIS==. | empl_inc==.

tab FIS if elig_FIS==1 [aw=relwt], m
 
*-------------------------------------------------------------------------------
* (5) Market and Disposable Income
*-------------------------------------------------------------------------------

* Market income
gen market_inc=income
	label variable market_inc "Household market income (annual, Fantasian $)"

gen pc_market_inc= market_inc/famsize
	label variable pc_market_inc "Per capita market income (annual, Fantasian $)"

* Social transfers
local benefit CB UA FIS
foreach var in `benefit' {
	gen `var'_mis=0
		replace `var'_mis=1 if `var'==.
bys hhnumber: egen hh_`var'_mis = max(`var'_mis)

bys hhnumber: egen `var'_HH = total(`var')
  replace `var'_HH =.  if hh_`var'_mis==1
	label variable `var'_HH "Household `var' (annual, Fantasian $)"	   
}

local benefit_HH CB_HH UA_HH FIS_HH
foreach var in `benefit_HH' {
	gen pc_`var'=`var'/famsize
		label variable pc_`var' "Per capita `var' (annual, Fantasian $)"  
}

rename pc_CB_HH pc_CB
rename pc_UA_HH pc_UA
rename pc_FIS_HH pc_FIS

* Disposable income  //Note: assumption: No fixed expenditure that need to be deducted from
local benefit_HH CB_HH UA_HH FIS_HH
foreach var in `benefit_HH' {
	gen disp_inc_`var'=market_inc+`var' 
		label variable disp_inc_`var' "Household disposable income (`benefit_HH') (annual, Fantasian $)"
codebook disp_inc_`var'
}

gen disp_inc_all= market_inc+CB_HH+UA_HH+FIS_HH
	label variable disp_inc_all "Household disposable income (all social transfers) (annual, Fantasian $)"

*browse market_inc CB_HH disp_inc_CB UA_HH disp_inc_UA FIS_HH disp_inc_FIS disp_inc_all

local disp_inc disp_inc_CB disp_inc_UA disp_inc_FIS disp_inc_all
foreach var in `disp_inc' {
	gen pc_`var' = `var'/famsize
		label variable pc_`var' "Per capita disposable income (`var') (annual, Fantasian $)"
}

*-------------------------------------------------------------------------------
* (6) Poverty and Inequality Impact - Simple Before and After Comparison
*-------------------------------------------------------------------------------

poverty pc_market_inc [aw=relwt]

gen povline=300
	label variable povline "Poverty line (annual)"

local inc_meas pc_market_inc pc_disp_inc_CB pc_disp_inc_UA pc_disp_inc_FIS pc_disp_inc_all
foreach var in `inc_meas' {
	gen pov_`var' = (`var'< povline) if !missing(`var')
		label values pov_`var' label_binary
 
	fastgini `var' [pw=relwt]
	gen gini_`var' = r(gini)
}

    label variable pov_pc_market_inc "Poverty status - market income"
    label variable pov_pc_disp_inc_CB "Poverty status - disposable income (CB)"
    label variable pov_pc_disp_inc_UA "Poverty status - disposable income (UA)"
    label variable pov_pc_disp_inc_FIS "Poverty status - disposable income (FIS)"
	label variable pov_pc_disp_inc_all "Poverty status - disposable income (All)"
	
    label variable gini_pc_market_inc "Gini coefficient - market income"
    label variable gini_pc_disp_inc_all "Gini coefficient - disposable income (All)"
    label variable gini_pc_disp_inc_CB "Gini coefficient - disposable income (CB)"
    label variable gini_pc_disp_inc_UA "Gini coefficient - disposable income (UA)"
    label variable gini_pc_disp_inc_FIS "Gini coefficient - disposable income (FIS)"

sum pov_* [aw=relwt]	
sum gini_* [aw=relwt]	

/*local inc_meas pc_market_inc pc_disp_inc_CB pc_disp_inc_UA pc_disp_inc_FIS pc_disp_inc_all
foreach var in `inc_meas' {
	sepov `var' [pw=relwt], p(povline) 
	ineqdeco `var' [aw=relwt]
} */

*-------------------------------------------------------------------------------
* (7) Benefit Incidence Analysis
*-------------------------------------------------------------------------------

local benefit CB UA FIS
foreach var in `benefit' {
	gen IND_beneficiary_`var'=(`var'> 0 & `var'<.)
	replace IND_beneficiary_`var'= . if `var'_mis==1
	label variable IND_beneficiary_`var' "Individual Beneficiary - `var'"
	label values IND_beneficiary_`var' label_binary
 
bys hhnumber: egen HH_beneficiary_`var' = max(IND_beneficiary_`var')
	replace HH_beneficiary_`var' = . if hh_`var'_mis==1
 label variable HH_beneficiary_`var' "Household Beneficiary - `var'"
 label values HH_beneficiary_`var' label_binary
 }
 
* (7.1) Child Benefit
tab IND_beneficiary_CB [aw=relwt], m
tab HH_beneficiary_CB [aw=relwt] if head==1, m
tab HH_beneficiary_CB [aw=relwt], m

sum pc_CB [aw=relwt]
sum CB_HH [aw=relwt] if head==1
    /// Avg benefit per capita  - 32
	/// Avg benefit per household - 184

sum pc_CB [aw=relwt] if HH_beneficiary_CB==1
sum CB_HH [aw=relwt] if HH_beneficiary_CB==1 & head==1    
    /// Avg benefit per beneficiary (individual) - 37
    /// Avg benefit per beneficiary (household) -  242

* (7.2) Unemployment Assistance
tab IND_beneficiary_UA [aw=relwt], m
tab HH_beneficiary_UA [aw=relwt] if head==1, m
tab HH_beneficiary_UA [aw=relwt], m

sum UA [aw=relwt]
sum UA_HH [aw=relwt] if head==1
    /// Avg benefit per capita  - 2
	/// Avg benefit per household - 9

sum UA [aw=relwt] if IND_beneficiary_UA==1
sum UA_HH [aw=relwt] if HH_beneficiary_UA==1 & head==1    
    /// Avg benefit per beneficiary (individual) - 240
    /// Avg benefit per beneficiary (household) -  346

* (7.3) Family Income Support
tab IND_beneficiary_FIS [aw=relwt], m
tab HH_beneficiary_FIS [aw=relwt] if head==1, m
tab HH_beneficiary_FIS [aw=relwt], m
 	
sum pc_FIS [aw=relwt]
sum FIS_HH [aw=relwt] if head == 1
    /// Avg benefit per capita  - 22
	/// Avg benefit per household - 127

sum pc_FIS [aw=relwt] if HH_beneficiary_FIS == 1
sum FIS_HH [aw=relwt] if HH_beneficiary_FIS == 1 & head == 1    
    /// Avg benefit per beneficiary (individual) - 40
    /// Avg benefit per beneficiary (household) -  282

*-------------------------------------------------------------------------------
* (8) Poverty Efficiency
*-------------------------------------------------------------------------------

* Total weighted poverty gaps
local inc_meas pc_market_inc pc_disp_inc_CB pc_disp_inc_UA pc_disp_inc_FIS pc_disp_inc_all
foreach var in `inc_meas' {
	gen double PG_`var'_ind=povline-`var' if pov_`var'==1 
		replace PG_`var'_ind=0 if pov_`var'==0
 
	egen double PG_`var'=total(PG_`var'*relwt), m
		label variable PG_`var' "Weighted total poverty gap - `var'"
	drop PG_`var'_ind
}  

rename PG_pc_market_inc PG_market
rename PG_pc_disp_inc_all PG_all
rename PG_pc_disp_inc_CB PG_CB
rename PG_pc_disp_inc_UA PG_UA
rename PG_pc_disp_inc_FIS PG_FIS

* Total weighted amount of transfers
local inc_meas pc_CB pc_UA pc_FIS
foreach var in `inc_meas' {
	egen double TT_`var'=total(`var'*relwt)
		label variable TT_`var' "Weighted total amount of transfer - `var'"
}  

rename TT_pc_CB TT_CB
rename TT_pc_UA TT_UA
rename TT_pc_FIS TT_FIS

egen double TT_all = rowtotal(TT_CB TT_UA TT_FIS)
	label variable TT_all "Weighted total amount of transfers - all"
 
* Total weighted amount of transfers to the before-poor
local inc_meas pc_CB pc_UA pc_FIS
foreach var in `inc_meas' {
	egen double TTP_`var'_stp=total(`var'*relwt) if pov_pc_market_inc==1

	egen TTP_`var'=min(TTP_`var'_stp)
		label variable TTP_`var' "Weighted total amount of transfer to the before-poor - `var'"
	drop TTP_`var'_stp
 }  
 
rename TTP_pc_CB TTP_CB
rename TTP_pc_UA TTP_UA
rename TTP_pc_FIS TTP_FIS

egen double TTP_all = rowtotal(TTP_CB TTP_UA TTP_FIS)
	label variable TTP_all "Weighted total amount of transfer to the poor - all"
  
* Calculate A, B, C, and D (cf. microsimulation module I)   
local benefit CB UA FIS all
foreach var in `benefit' {
	gen double D_`var'=PG_`var'
	gen double A_`var'=PG_market-PG_`var'
   
	gen double ABC_`var'=TT_`var'
	gen double AB_`var'=TTP_`var'

	gen double B_`var'=AB_`var'-A_`var'
	gen double C_`var'=ABC_`var'-AB_`var'
}  

* Compute poverty efficiency measures (cf. microsimulation module I)     
local benefit CB UA FIS all
foreach var in `benefit' {   
	gen double PGE_`var'=A_`var'/(A_`var'+D_`var')
		label variable PGE_`var' "Poverty gap efficiency - `var'"
 
	gen double PRE_`var'=A_`var'/(ABC_`var')
		label variable PRE_`var' "Poverty reduction efficiency - `var'"
 
	gen double VEE_`var' = AB_`var'/(ABC_`var')
		label variable VEE_`var' "Vertical expenditure efficiency - `var'"
         
	gen double S_`var' = B_`var'/AB_`var'
		label variable S_`var' "Spillover - `var'"

	drop A_`var' D_`var' ABC_`var' AB_`var' B_`var' C_`var'	
 }
 
sum PGE* PRE* VEE* S* [aw=relwt]
 
*-------------------------------------------------------------------------------
* (9) Cost Effectiveness
*-------------------------------------------------------------------------------

* Administrative costs
gen double admin_cost_CB=TT_CB/100*5
	label variable admin_cost_CB "Weighted administrative cost - CB"
 
gen double admin_cost_UA=TT_UA/100*10
	label variable admin_cost_UA "Weighted administrative cost - UA"

gen double admin_cost_FIS=TT_FIS/100*7
	label variable admin_cost_FIS "Weighted administrative cost - FIS"
   
egen double admin_cost_all=rowtotal(admin_cost_CB admin_cost_UA admin_cost_FIS)
	label variable admin_cost_all "Weighted total administrative cost - all"

* Total costs
egen double tot_cost_CB = rowtotal(TT_CB admin_cost_CB)
	label variable tot_cost_CB "Weighted total program cost - CB"
	 
egen double tot_cost_UA = rowtotal(TT_UA admin_cost_UA)
	label variable tot_cost_UA "Weighted total program cost - UA"
 
egen double tot_cost_FIS = rowtotal(TT_FIS admin_cost_FIS)
	label variable tot_cost_FIS "Weighted total program cost - FIS"
	 
egen double tot_cost_all = rowtotal(tot_cost_CB tot_cost_UA tot_cost_FIS)
	label variable tot_cost_all "Weighted total program cost - all"

local disp_inc pc_disp_inc_CB pc_disp_inc_UA pc_disp_inc_FIS pc_disp_inc_all 
foreach var in `disp_inc' {

* Income difference
	gen incdiff_`var'=`var'-pc_market_inc
		label variable incdiff_`var' "Individual income difference - `var'"
	 
* Difference in poverty status
	sum pov_pc_market_inc [aw=relwt]
		scalar PHB=r(mean)
	sum pov_`var' [aw=relwt]
		scalar PHA=r(mean)
 
	gen povdiff_`var'=(PHB-PHA)*100
		label variable povdiff_`var' "Overall poverty headcount difference - `var'"
	 
* Difference in Gini coefficient
	sum gini_pc_market_inc [aw=relwt]
		scalar giniB=r(mean)
	sum gini_`var' [aw=relwt]
		scalar giniA=r(mean)
 
	gen ginidiff_`var'=(giniB - giniA)*100
		label variable ginidiff_`var' "Overall Gini difference - `var'"
} 

sum povdiff_* [aw=relwt] 
sum ginidiff* [aw=relwt]

* Cost efficiency measures
local disp_inc CB UA FIS all
foreach var in `disp_inc' {
	gen double CE_pov_`var'=tot_cost_`var'/(povdiff_pc_disp_inc_`var')
	label variable CE_pov_`var' "Cost Efficiency, Poverty - `var'"

	gen double CE_ineq_`var'=tot_cost_`var'/(ginidiff_pc_disp_inc_`var')
	label variable CE_ineq_`var' "Cost Efficiency, Inequality - `var'"
  } 
 
sum CE* [aw=relwt] 

*-------------------------------------------------------------------------------
* (10) Visualising the Change
*-------------------------------------------------------------------------------
/* In order to visualise changes, we collapse the dataset into 100 observations, using 
market_inc_PC to create percentiles. The reason for this are computation costs - with
large dataset, it will take very long to make the graphs. */

count if pc_market_inc==0 
			
egen rank =rank(-famsize) if pc_market_inc==0, unique
egen rank2=rank(pc_market_inc) if pc_market_inc!=0, unique  
	replace rank=rank2+14 if rank==.
	label variable rank "Ranking of individuals by per capita market income"
   
xtile pc_market_inc_100=rank [aw=relwt], nquantiles(100) 
	drop rank rank2
sum pc_market_inc_100, d
tab pc_market_inc_100, m

cumul pc_market_inc, gen(CDF_pc_market_inc)
	label variable CDF_pc_market_inc "Cumulative population share - per capita market income"
	
sort CDF_pc_market_inc
browse hhnumber pc_market_inc_100 pc_market_inc CDF_pc_market_inc

save "output/Tutorial_Dataset_final.dta", replace

collapse famsize elig_CB elig_UA elig_FIS CB UA FIS market_inc pc_market_inc CB_HH UA_HH FIS_HH ///
		 pc_CB pc_UA pc_FIS disp_inc_CB disp_inc_UA disp_inc_FIS disp_inc_all pc_disp_inc_CB pc_disp_inc_UA pc_disp_inc_FIS pc_disp_inc_all ///
		 povline pov_pc_market_inc gini_pc_market_inc pov_pc_disp_inc_CB gini_pc_disp_inc_CB pov_pc_disp_inc_UA ///
		 gini_pc_disp_inc_UA pov_pc_disp_inc_FIS gini_pc_disp_inc_FIS pov_pc_disp_inc_all gini_pc_disp_inc_all ///
		 IND_beneficiary_CB HH_beneficiary_CB IND_beneficiary_UA HH_beneficiary_UA IND_beneficiary_FIS HH_beneficiary_FIS CDF_pc_market_inc relwt [pweight=relwt], by(pc_market_inc_100)

*-------------------------------------------------------------------------------		 
* Bar graphs
*-------------------------------------------------------------------------------

* All social transfers
	* Complete distribution   
	graph bar (sum) pc_market_inc pc_CB pc_UA pc_FIS, stack over(pc_market_inc_100, label(labsize(zero)) sort((sum) ///
		pc_market_inc)) title("Per capita disposable income after social transfers", size(small)) ///
		legend(label(1 "Market income") label(2 "Child benefit") ///
		label(3 "Unemployment assistance") label(4 "Family income support")) ylabel(,labsize(small))

	* Bottom of the distribution
	graph bar (sum) pc_market_inc pc_CB pc_UA pc_FIS if pc_market_inc<1000, stack over(pc_market_inc_100, label(labsize(zero)) sort((sum) ///
		pc_market_inc)) title("Per capita disposable income after social transfers", size(small)) ///
		legend(label(1 "Market income") label(2 "Child benefit") ///
		label(3 "Unemployment assistance") label(4 "Family income support")) ylabel(,labsize(small))

* Individual transfers
	graph bar (sum) pc_market_inc pc_CB if pc_market_inc<1000, stack over(pc_market_inc_100, label(labsize(zero)) sort((sum) ///
		pc_market_inc)) title("Per capita disposable income after child benefit", size(small)) ///
		legend(label(1 "Market income") label(2 "Child benefit")) ylabel(,labsize(small))
		
	graph bar (sum) pc_market_inc pc_UA if pc_market_inc<1000, stack over(pc_market_inc_100, label(labsize(zero)) sort((sum) ///
		pc_market_inc)) title("Per capita disposable income after unemployment assistance", size(small)) ///
		legend(label(1 "Market income") label(2 "Unemployment assistance")) ylabel(,labsize(small))
		
	graph bar (sum) pc_market_inc pc_FIS if pc_market_inc<1000, stack over(pc_market_inc_100, label(labsize(zero)) sort((sum) ///
		pc_market_inc)) title("Per capita disposable income after family income support", size(small)) ///
		legend(label(1 "Market income") label(2 "Family income support")) ylabel(,labsize(small))
		
*-------------------------------------------------------------------------------		 
* Pen's Parade
*-------------------------------------------------------------------------------		
* All social transfers
	* Complete distribution 
	graph twoway ///
		(line pc_market_inc CDF_pc_market_inc) ///
		(line pc_disp_inc_all CDF_pc_market_inc), ///
		title("Per capita family market and disposable income distribution", size(small)) xtitle("Cumulative population share", size(small)) ///
		legend(size(vsmall) label(1 "Per capita family market income") ///
		label(2 "Per capita family disposable income")) ylabel(,labsize(small)) xlabel(,labsize(small))

	* Bottom of the distribution
	graph twoway ///
		(line pc_market_inc CDF_pc_market_inc) ///
		(line pc_disp_inc_all CDF_pc_market_inc) if pc_market_inc<1000, ///
		title("Per capita family market and disposable income distribution", size(small)) xtitle("Cumulative population share", size(small)) ///
		legend(size(vsmall) label(1 "Per capita family market income") ///
		label(2 "Per capita family disposable income")) ylabel(,labsize(small)) xlabel(,labsize(small))

* Individual transfers
	* Complete distribution
	graph twoway ///
		(line pc_market_inc CDF_pc_market_inc) ///
		(line pc_disp_inc_CB CDF_pc_market_inc)	///
		(line pc_disp_inc_UA CDF_pc_market_inc)	///
		(line pc_disp_inc_FIS CDF_pc_market_inc), ///
		title("Per capita family market and disposable income distribution", size(small)) xtitle("Cumulative population share", size(small)) ///
		legend(size(vsmall) label(1 "Per capita family market income") ///
		label(2 "Per capita disposable income - only CB") ///
		label(3 "Per capita disposable income - only UA") ///
		label(4 "Per capita disposable income - only FIS")) ///
		ylabel(,labsize(small)) xlabel(,labsize(small))

   * Bottom of the distribution
	graph twoway ///
		(line pc_market_inc CDF_pc_market_inc) ///
		(line pc_disp_inc_CB CDF_pc_market_inc)	///
		(line pc_disp_inc_UA CDF_pc_market_inc)	///
		(line pc_disp_inc_FIS CDF_pc_market_inc) if pc_market_inc<1000, ///
		title("Per capita family market and disposable income distribution", size(small)) xtitle("Cumulative population share", size(small)) ///
		legend(size(vsmall) label(1 "Per capita family market income") ///
		label(2 "Per capita disposable income - only CB") ///
		label(3 "Per capita disposable income - only UA") ///
		label(4 "Per capita disposable income - only FIS")) ///
		ylabel(,labsize(small)) xlabel(,labsize(small))
		
	graph twoway ///
		(line pc_market_inc CDF_pc_market_inc) ///
		(line pc_disp_inc_CB CDF_pc_market_inc)	///
		(line pc_disp_inc_UA CDF_pc_market_inc)	///
		(line pc_disp_inc_FIS CDF_pc_market_inc) if pc_market_inc<500, ///
		title("Per capita family market and disposable income distribution", size(small)) xtitle("Cumulative population share", size(small)) ///
		legend(size(vsmall) label(1 "Per capita family market income") ///
		label(2 "Per capita disposable income - only CB") ///
		label(3 "Per capita disposable income - only UA") ///
		label(4 "Per capita disposable income - only FIS")) ///
		ylabel(,labsize(small)) xlabel(,labsize(small))
