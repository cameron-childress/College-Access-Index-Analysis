/*
This code builds off of a previously developed longitudinal dataset compiling a variety of variables related to Pell student enrollment, 
Pell student share, institutional finances, and other variables. Most data pulled from IPEDS NCES, rankings generated using Barron's selectivity ratings. 

The previously developed dataset is organized in long form. Here, we reshape it to wide format to calculate percent change variables from 2011 to 2021, 
which will be used to generate rankings. The data will then be exported to an Excel file for group review, trend identification, and further analysis.

Code written by Cameron Childress

*/

*********************************************
***Analysis**********************************
*********************************************

/* set filepath macros */
local college_index_data "G:\Shared drives\ET Shared Drive (Staff)\Active Projects (Staff)\College Access Index 2023\data\clean\"

/* Load the prepared dataset */
use "`college_index_data'college_index_data", clear 

/* Filter data to focus on specific years for analysis */
keep if year == 2011 | year == 2021

/* Rename variables to prepare for reshape */
keep unitid instnm year rating rating_analysis state_flagship stabbr region sector control pell_total_undergrad pell_pct_undergrad eftotlt2 endow_per_fte

ds pell_total_undergrad pell_pct_undergrad eftotlt2 endow_per_fte
foreach var in `r(varlist)' {
	rename `var' `var'_
}

/* Reshape data to a wide format for comparative analysis */
reshape wide pell_total_undergrad pell_pct_undergrad eftotlt2_ endow_per_fte, i(unitid) j(year)

******************************************************
/* Manipulate variables to set up college rankings */
******************************************************

/* Calculate percent changes, percentage point changes, and total changes in undergrad Pell enrollment, Pell share */
gen change_ug_pell_num_2011_2021 = (pell_total_undergrad_2021 - pell_total_undergrad_2011)/pell_total_undergrad_2011

gen change_ug_enroll_2011_2021 = (eftotlt2_2021 - eftotlt2_2011)/ eftotlt2_2011

gen pct_pt_change_ug_pell_2011_2021 = pell_pct_undergrad_2021 - pell_pct_undergrad_2011

/* Generate rankings for 2021 Pell share, percentage point change in Pell share, percentage growth in Pell enrollment  */
egen pell_rank_ug_2021 = rank( pell_pct_undergrad_2021), field
bysort rating_analysis: egen pell_rank_ug_rating_2021 = rank(pell_pct_undergrad_2021), field

egen change_ug_pell_rank_2011_2021 = rank(change_ug_pell_num_2011_2021), field

/* Create wealth quintiles and rank within groups */
xtile wealth_group = endow_per_fte_2021, nq(5) 
bysort wealth_group: egen pell_rank_ug_wealth_2021 = rank(pell_pct_undergrad_2021), field

/* Create endowmnet per FTE enrollment buckets to organize schools */
gen endow_bucket = ""
replace endow_bucket = "Less than $100k" if endow_per_fte_2021 < 100000
replace endow_bucket = "$100k-$250k" if endow_per_fte_2021 >= 100000 & endow_per_fte_2021 < 250000
replace endow_bucket = "$250k-$500k" if endow_per_fte_2021 >= 250000 & endow_per_fte_2021 < 500000
replace endow_bucket = "$500k-$1 million" if endow_per_fte_2021 >= 500000 & endow_per_fte_2021 < 1000000
replace endow_bucket = "$1 million+" if endow_per_fte_2021 >= 1000000
replace endow_bucket = "N/A" if endow_per_fte_2021 == .

/* Create ranking by pell bucket */
bysort endow_bucket: egen pell_rank_ug_endow_2021 = rank(pell_pct_undergrad_2021), field

/* Create rankings by region and institutional control */
bysort region: egen pell_rank_ug_region_2021 = rank(pell_pct_undergrad_2021), field
bysort control: egen pell_rank_ug_cntrl_2021 = rank(pell_pct_undergrad_2021), field

/* clear rankings on inst. without data on endowment */
replace pell_rank_ug_endow_2021 = . if endow_per_fte_2021 == .
replace pell_rank_ug_wealth_2021 = . if endow_per_fte_2021 == .

/* Save as Stata .dta to merge in data for the next sheet */
save "`college_index_data'college_index_analysis", replace

/* Export "analysis" sheet into Excel */
export excel using "`college_index_data'college_index_data.xls", sheet("analysis") firstrow(variables) replace 

**********************************************************************************************************************
/* Calculate weighted pell share and race share for each Barron's cateogry, add to additional sheet on excel dataset */
**********************************************************************************************************************

/* Clear and load the prepared dataset */
use "`college_index_data'college_index_data", clear 

/* Merge in endowment buckets generated while creating the analysis sheet. Collapse data on Pell share, Underrepresented Minority (URM) student enrollment share, 
  Black student enrollment share, and Hispanic student enrollment share by bucket and year. Calculate weighted means by total undergraduate enrollment. */

merge m:1 unitid using "`college_index_data'college_index_analysis", keepusing(endow_bucket) nogen 

gen count = 1
collapse (count) count (mean) pell_pct_undergrad urm_pct_undergrad_t bkaa_pct_undergrad_t hisp_pct_undergrad_t [w=eftotlt2], by( endow_bucket year )

export excel using "`college_index_data'college_index_data.xls", sheet("endow_bucket_averages") firstrow(variables)

