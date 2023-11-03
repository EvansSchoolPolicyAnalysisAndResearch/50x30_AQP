clear
global directory			"\\netid.washington.edu\wfs\EvansEPAR\Project\EPAR\Working Files\335 - Ag Team Data Support\Waves"
/*global Nigeria_GHS_W1_created_data "$directory/Nigeria GHS/Nigeria GHS Wave 1/Final DTA Files - LM/created_data"
global Nigeria_GHS_W2_created_data  "$directory/Nigeria GHS/Nigeria GHS Wave 2/Final DTA Files - ALT/created_data"
global Nigeria_GHS_W3_created_data 		"$directory/Nigeria GHS/Nigeria GHS Wave 3/Final DTA files - ALT/created_data"
global Nigeria_GHS_W4_created_data 		"$directory/Nigeria GHS/Nigeria GHS Wave 4/Final DTA files_AT/created_data"
global Nigeria_GHS_W1_final_data "$directory/Nigeria GHS/Nigeria GHS Wave 1/Final DTA Files - LM/final_data"
global Nigeria_GHS_W2_final_data  "$directory/Nigeria GHS/Nigeria GHS Wave 2/Final DTA Files - ALT/final_data"
global Nigeria_GHS_W3_final_data 		"$directory/Nigeria GHS/Nigeria GHS Wave 3/Final DTA files - ALT/final_data"
global Nigeria_GHS_W4_final_data 		"$directory/Nigeria GHS/Nigeria GHS Wave 4/Final DTA files_AT/final_data"
*/

/*
global Nigeria_GHS_W1_created_data "$directory/Nigeria GHS/Nigeria GHS Wave 1/Final DTA Files - AGP/created_data"
global Nigeria_GHS_W2_created_data  "$directory/Nigeria GHS/Nigeria GHS Wave 2/Final DTA Files - AGP/created_data"
global Nigeria_GHS_W3_created_data 		"$directory/Nigeria GHS/Nigeria GHS Wave 3/Final DTA files - AGP/created_data"
global Nigeria_GHS_W4_created_data 		"$directory/Nigeria GHS/Nigeria GHS Wave 4/Final DTA files - AQP/created_data"
global Nigeria_GHS_W1_final_data "$directory/Nigeria GHS/Nigeria GHS Wave 1/Final DTA Files - AGP/final_data"
global Nigeria_GHS_W2_final_data  "$directory/Nigeria GHS/Nigeria GHS Wave 2/Final DTA Files - AGP/final_data"
global Nigeria_GHS_W3_final_data 		"$directory/Nigeria GHS/Nigeria GHS Wave 3/Final DTA files - AGP/final_data"
global Nigeria_GHS_W4_final_data 		"$directory/Nigeria GHS/Nigeria GHS Wave 4/Final DTA files - AQP/final_data"
*/

* HKS 6.16.23: We are moving away from using AgQueryPlus Specific folders (AGP) in favor of all of the latest data being stored in the "Final DTA Files" folder
global Nigeria_GHS_W1_created_data "$directory/Nigeria GHS/Nigeria GHS Wave 1/Final DTA Files/created_data"
global Nigeria_GHS_W2_created_data  "$directory/Nigeria GHS/Nigeria GHS Wave 2/Final DTA Files/created_data"
global Nigeria_GHS_W3_created_data 		"$directory/Nigeria GHS/Nigeria GHS Wave 3/Final DTA files/created_data"
global Nigeria_GHS_W4_created_data 		"$directory/Nigeria GHS/Nigeria GHS Wave 4/Final DTA files/created_data"
global Nigeria_GHS_W1_final_data "$directory/Nigeria GHS/Nigeria GHS Wave 1/Final DTA Files/final_data/As of June 23"
global Nigeria_GHS_W2_final_data  "$directory/Nigeria GHS/Nigeria GHS Wave 2/Final DTA Files/final_data"
global Nigeria_GHS_W3_final_data 		"$directory/Nigeria GHS/Nigeria GHS Wave 3/Final DTA files/final_data"
global Nigeria_GHS_W4_final_data 		"$directory/Nigeria GHS/Nigeria GHS Wave 4/Final DTA files/final_data"



global shiny_dir "\\netid.washington.edu\wfs\EvansEPAR\Project\EPAR\Working Files\420 - AgQuery Plus\AgQueryPlus\agquery\Data" 
global all_topcrops maize rice wheat sorgum millet /*mill*/ pmill cowpea grdnt beans yam swtptt cassav banana teff barley coffee sesame hsbean nueg cotton sunflr pigpea cocoa soy mangos mungbn avocad potato cashew jute
global nb_allcrops : list sizeof global(all_topcrops) // Gets the current length of the global macro list "topcropname_area" 
global all_topcrop_codes "1080 1110 2280 1070 1100 0 0 1060 1010 1120 2181 1020 2030 0 0 3060 2040 0 0 1050 0 2150 3040 2220 3160 0 3221 2180 3020 3100" //Note: Need to recode 3041 to 3040 //Also only applicable to Nigeria 
set obs $nb_allcrops //Update if number of crops changes
egen rnum = seq(), f(1) t($nb_allcrops)
gen crop_code = .
gen crop_name = ""
forvalues k=1(1)$nb_allcrops {
	local c : word `k' of $all_topcrop_codes
	local cn : word `k' of $all_topcrops
	replace crop_code = `c' if rnum==`k'
	replace crop_name = "`cn'" if rnum==`k'
} 
drop if crop_code==0 //Missing from NGA 
drop rnum
tempfile croplist
save `croplist'

global instr_abbr nga_w1 nga_w2 nga_w3 nga_w4
global instr Nigeria_GHS_W1 Nigeria_GHS_W2 Nigeria_GHS_W3 Nigeria_GHS_W4
global no_instr : list sizeof global(instr)
forvalues k=1(1)$no_instr {
    local c : word `k' of $instr_abbr
	local i : word `k' of $instr

//Plot variables
use "${`i'_created_data}/`i'_plot_labor_long.dta", clear
collapse (sum) days, by(hhid plot_id labor_type)
ren days days_
reshape wide days_, i(hhid plot_id) j(labor_type) string
egen labor_total = rowtotal(days*)
tempfile labor_days
save `labor_days' //Theoretically, we could change this so labor productivity is constructed during runtime and allow filtering/aggregation on exchange/family/hired

use "${`i'_created_data}/`i'_all_plots.dta", clear
capture ren crop_code_master crop_code
merge m:1 hhid plot_id using "${`i'_created_data}/`i'_plot_cost_inputs.dta", nogen //129 plots recorded input use but don't show up in the all plots file. Gah.
merge m:1 hhid plot_id using `labor_days', nogen keep(1 3) //8 plots with 0 recorded labor days.
//??
replace labor_total=labor_total*percent_inputs //These do all sum to 100
replace val_exp=val_exp*percent_inputs //Need to check and make sure this is actually correct
replace val_imp = val_imp*percent_inputs 
ren val_exp cost_expli 
gen cost_total = cost_expli+val_imp 
drop val_imp
ren quant_harv_kg kgs_harvest
gen yield_hv = kgs_harvest/ha_harvest
gen yield_pl = kgs_harvest/ha_planted
gen labor_productivity = value_harvest/labor_total
gen land_productivity_hv = value_harvest/ha_harvest 
gen land_productivity_pl = value_harvest/ha_planted
gen land_labor_ratio = labor_total/ha_planted
gen crop_exp_all = cost_expli 
gen crop_exp_ha = cost_expli/ha_planted 
gen crop_exp_kg = cost_expli/kgs_harvest 
merge m:1 hhid plot_id using "${`i'_created_data}/`i'_input_quantities.dta", nogen
drop if crop_code == . | ha_planted ==0 //Note to self: this needs fixing.

replace herb_rate = herb_rate*percent_inputs/ha_planted 
replace pest_rate = pest_rate*percent_inputs/ha_planted
//replace org_fert_rate = org_fert_rate*percent_inputs/ha_planted 
replace inorg_fert_rate=inorg_fert_rate*percent_inputs/ha_planted

recode *rate (.=0)
gen inorg_fert_rate_alluse_fert=inorg_fert_rate if inorg_fert_rate > 0
gen herb_rate_alluse_herb=herb_rate if herb_rate>0
gen pest_rate_alluse_pest=pest_rate if pest_rate>0
//gen org_fert_rate_alluse_fert = org_fert_rate if org_fert_rate>0
merge m:1 hhid using "${`i'_final_data}/`i'_household_variables.dta", nogen keep(1 3) keepusing(farm_size_agland weight geography year instrument fhh rural) //Plot vars should only have ag_hh, yeah?
merge m:1 hhid using "${`i'_created_data}/`i'_hhsize.dta", nogen keepusing(hh_members zone state lga ea weight_pop_tot weight_pop_rururb)
gen farmsize = "0 ha" if farm_size_agland == 0 | farm_size_agland == .
replace farmsize = ">0 - 2 ha" if farm_size_agland <= 2 & farm_size_agland > 0 & farm_size_agland !=.
replace farmsize = "2 - 4 ha" if farm_size_agland > 2 & farm_size_agland <= 4 & farm_size_agland !=.
replace farmsize = ">4 ha" if farm_size_agland > 4 & farm_size_agland !=.
drop farm_size_agland
//replace weight = weight*ha_planted //ALT 06.30.23: No longer necessary due to updates, I think. 
merge m:1 crop_code using `croplist', nogen keep(3) //Cutting things down for now.
ren ha_planted ha_planted_plot
ren ha_harvest ha_harvest_plot
ren kgs_harvest kgs_harvest_plot
capture numlabel state_id, r //Only applicable to W4; we'll just ignore the error on W2 and W3.
capture numlabel STATE, r
capture numlabel ZONE, r 
capture numlabel zone_id, r
capture numlabel zone, r //W3
capture numlabel state, r //W3
capture numlabel lga, r
capture numlabel LGA, r
/*ren zone div1
ren state div2
ren lga div4
ren ea div8*/
save "${`i'_created_data}/`c'_plot_vars.dta", replace
save "${shiny_dir}/`c'_plot_vars.dta", replace

//Person variables; could be changed so we can disaggregate among crops. 
use "${`i'_created_data}/`i'_farmer_fert_use.dta", clear
//merge 1:1 hhid indiv using "${`i'_created_data}/`i'_farmer_improvedseed_use.dta", nogen  keep(1 3)
keep hhid indiv female use_herb all_use_inorg_fert use_pest use_imprv_seed
gen hybrid_seed_use = . //No hybrid/imprved seed use in this wave.
//Renaming for consistency with output files
ren use_imprv_seed imprv_seed_use 
ren all_use_inorg_fert indiv_fert_use 
ren use_herb indiv_herb_use 
ren use_pest indiv_pest_use 
merge m:1 hhid using "${`i'_final_data}/`i'_household_variables.dta", nogen keepusing(farm_size_agland rural fhh weight geography year instrument)
merge m:1 hhid using "${`i'_created_data}/`i'_hhsize.dta", nogen keepusing(hh_members zone state lga ea weight_pop_tot weight_pop_rururb)
gen farmsize = "0 ha" if farm_size_agland == 0 | farm_size_agland == .
replace farmsize = ">0 - 2 ha" if farm_size_agland <= 2 & farm_size_agland > 0 & farm_size_agland !=.
replace farmsize = "2 - 4 ha" if farm_size_agland > 2 & farm_size_agland <= 4 & farm_size_agland !=.
replace farmsize = ">4 ha" if farm_size_agland > 4 & farm_size_agland !=.
drop farm_size_agland
capture numlabel state_id, r //Only applicable to W4; we'll just ignore the error on W2 and W3.
capture numlabel STATE, r
capture numlabel ZONE, r 
capture numlabel zone_id, r
capture numlabel zone, r //W3
capture numlabel state, r //W3
capture numlabel lga, r
capture numlabel LGA, r
/*ren zone div1
ren state div2
ren lga div4
ren ea div8
ren hhid div9*/
save "${`i'_created_data}/`c'_pers_vars.dta", replace
save "${shiny_dir}/`c'_pers_vars.dta", replace

//Household-Crop. Disaggregation by plot manager is possible but probably not best here.
use "${`i'_created_data}/`c'_plot_vars.dta", clear
collapse (sum) value_harvest kgs_harvest ha_harvest ha_planted cost_expli, by(hhid crop_code)
merge 1:1 hhid crop_code using "${`i'_created_data}/`i'_hh_crop_values_production.dta", nogen
drop if crop_code==.
gen grew_crop=1
fillin hhid crop_code
drop _fillin
recode grew_crop (.=0) 
merge m:1 hhid using "${`i'_final_data}/`i'_household_variables.dta", nogen keepusing(farm_size_agland rural fhh weight geography year instrument proportion_cropvalue_sold) 
merge m:1 hhid using "${`i'_created_data}/`i'_hhsize.dta", nogen keepusing(hh_members zone state lga ea weight_pop_tot weight_pop_rururb)
gen farmsize = "0 ha" if farm_size_agland == 0 | farm_size_agland == .
replace farmsize = ">0 - 2 ha" if farm_size_agland <= 2 & farm_size_agland > 0 & farm_size_agland !=.
replace farmsize = "2 - 4 ha" if farm_size_agland > 2 & farm_size_agland <= 4 & farm_size_agland !=.
replace farmsize = ">4 ha" if farm_size_agland > 4 & farm_size_agland !=.
drop farm_size_agland
//Renaming vars to avoid overlap between household and crop
ren value_harvest value_harvest_hh
ren kgs_harvest kgs_harvest_hh
ren ha_harvest ha_harvest_hh
ren ha_planted ha_planted_hh
ren cost_expli cost_expli_hh
gen cost_expli_hh_ha = cost_expli_hh/ha_planted
merge m:1 crop_code using `croplist', nogen keep(3) //Cutting things down for now.
capture numlabel state_id, r //Only applicable to W4; we'll just ignore the error on W2 and W3.
capture numlabel STATE, r
capture numlabel ZONE, r 
capture numlabel zone_id, r
capture numlabel zone, r //W3
capture numlabel state, r //W3
capture numlabel lga, r
capture numlabel LGA, r
/*ren zone div1
ren state div2
ren lga div4
ren ea div8
ren hhid div9*/
save "${`i'_created_data}/`c'_hh-crop_vars.dta", replace
save "${shiny_dir}/`c'_hh-crop_vars.dta", replace

//Household
use "${`i'_created_data}/`c'_plot_vars.dta", clear
collapse (sum) ha_harvest ha_planted, by(hhid)
tempfile farmvars
save `farmvars'

use "${`i'_created_data}/`i'_person_ids.dta", clear
gen women = female==1 & age > 15
gen men = female==0 & age > 15
gen children = age <=15
collapse (sum) men women children, by(hhid)
tempfile hhmbrs
save `hhmbrs'

use "${`i'_created_data}/`i'_input_use.dta", clear
recode use* (.=0)
//Livestock products
merge 1:1 hhid using "${`i'_created_data}/`i'_livestock_products.dta", nogen
merge 1:1 hhid using "${`i'_created_data}/`i'_TLU_Coefficients", nogen
merge 1:1 hhid using "${`i'_created_data}/`i'_livestock_sales.dta", nogen
merge 1:1 hhid using "${`i'_final_data}/`i'_household_variables.dta", nogen keepusing(adulteq ag_hh crop_hh agactivities_hh fishing_hh num_crops_hh wage_paid_aglabor /*cost_expli_hh cost_expli_hh_ha*/ livestock_hh land_productivity labor_productivity farm_size_agland multiple_crops /*
*/ 																					lvstck_holding_tlu w_share_nonfarm value_assets proportion_cropvalue_sold hrs_off_farm_pc_all hrs_off_farm_pc_any peraeq_cons percapita_cons daily_peraeq_cons daily_percap_cons percapita_income /*
*/																					crop_income fishing_income livestock_income self_employment_income poverty_under_1_9 w_share_agwage w_share_nonfarm w_share_fishing w_share_crop w_share_livestock w_share_self_employment w_share_nonagwage w_share_all_other /*
*/																					w_share_transfers agwage_income total_income nonfarm_income nonagwage_income all_other_income transfers_income share_livestock_prod_sold value_milk_produced value_eggs_produced /*
*/																					value_livestock_products value_livestock_sales rural fhh weight geography year instrument)
merge 1:1 hhid using "${`i'_created_data}/`i'_hhsize.dta", nogen keepusing(hh_members zone state lga ea weight_pop_tot weight_pop_rururb)
merge 1:1 hhid using `hhmbrs', nogen
merge 1:1 hhid using `farmvars'
capture confirm file "${`i'_created_data}/`i'_mpi_indicators.dta"
if !_rc {
	merge 1:1 hhid using "${`i'_created_data}/`i'_mpi_indicators.dta", nogen
}
gen farmsize = "0 ha" if farm_size_agland == 0 | farm_size_agland == .
replace farmsize = ">0 - 2 ha" if farm_size_agland <= 2 & farm_size_agland > 0 & farm_size_agland !=.
replace farmsize = "2 - 4 ha" if farm_size_agland > 2 & farm_size_agland <= 4 & farm_size_agland !=.
replace farmsize = ">4 ha" if farm_size_agland > 4 & farm_size_agland !=.
gen ssp = farm_size_agland <=4
replace ssp = . if farm_size_agland==. | farm_size_agland==0
ren land_productivity land_productivity_hh 
ren labor_productivity labor_productivity_hh
gen households=1
capture numlabel state_id, r //Only applicable to W4; we'll just ignore the error on W2 and W3.
capture numlabel STATE, r
capture numlabel ZONE, r 
capture numlabel zone_id, r
capture numlabel zone, r //W3
capture numlabel state, r //W3
capture numlabel lga, r
capture numlabel LGA, r
/*ren zone div1
ren state div2
ren lga div4
ren ea div8
ren hhid div9
*/
save "${`i'_created_data}/`c'_hh_vars.dta", replace //Need to come back to this one.
save "${shiny_dir}/`c'_hh_vars.dta", replace

/*
use "${`i'_created_data}/`c'_plot_vars.dta", clear
ren hhid div9
ren plot_id div10
save "${`i'_created_data}/`c'_plot_vars.dta", replace
save "${shiny_dir}/`c'_plot_vars.dta", replace
*/

use "${`i'_final_data}/`i'_household_variables.dta", clear
capture confirm file "${`i'_created_data}/`i'_mpi_indicators.dta"
if !_rc {
	merge 1:1 hhid using "${`i'_created_data}/`i'_mpi_indicators.dta", nogen
}
capture numlabel state_id, r //Only applicable to W4; we'll just ignore the error on W2 and W3.
capture numlabel STATE, r
capture numlabel ZONE, r 
capture numlabel zone_id, r
capture numlabel zone, r //W3
capture numlabel state, r //W3
capture numlabel lga, r
capture numlabel LGA, r
save "${shiny_dir}/`c'_household_variables.dta", replace


* Recode hhid as string for all outputs for rshiny
foreach file in hh_vars hh-crop_vars pers_vars plot_vars {
	use  "${shiny_dir}/`c'_`file'", clear
	tostring hhid, replace force
	save  "${shiny_dir}/`c'_`file'.dta", replace 
	}

	
* Recode farmsize as a factor
use "${shiny_dir}/`c'_hh_vars.dta", clear
*encode farmsize, gen(farmsize2)
gen farmsize2 = 1 if farmsize == "0 ha"
	replace farmsize2 = 2 if farmsize == "2 - 4 ha"
	replace farmsize2 = 3 if farmsize == ">0 - 2 ha"
	replace farmsize2 = 4 if farmsize == ">4 ha"
drop farmsize
ren farmsize2 farmsize
save "${shiny_dir}/`c'_hh_vars.dta", replace


}





//Geovariables - files differ a lot among waves, so no loop for us.
use "\\netid.washington.edu\wfs\EvansEPAR\Project\EPAR\Working Files\335 - Ag Team Data Support\Waves\Nigeria GHS\Nigeria GHS Wave 1\Raw DTA Files\v01_all_one_folder_LM\NGA_HouseholdGeovariables_Y1", clear
ren lat_dd_mod LAT
ren lon_dd_mod LON
keep lga ea LAT LON
duplicates drop lga ea LAT LON, force //ea+lga necessary to uniquely identify ea
gen year = "2010-11"
//decode lga, gen(div4)
//drop lga
//ren ea div8
decode lga, gen(lga2)
drop lga
ren lga2 lga
save "${shiny_dir}/Nigeria_w1_div8_coords.dta", replace
//collapse (mean) LAT LON, by(div8) //Simple way to guess at the center of an LGA based on sampled eas.
collapse (mean) LAT LON, by(lga)
save "${shiny_dir}/Nigeria_w1_div4_coords.dta", replace

use "\\netid.washington.edu\wfs\EvansEPAR\Project\EPAR\Working Files\335 - Ag Team Data Support\Waves\Nigeria GHS\Nigeria GHS Wave 2\Raw DTA Files\NGA_2012_GHSP-W2_v02_M\NGA_HouseholdGeovars_Y2.dta", clear
ren LAT_DD_MOD LAT
ren LON_DD_MOD LON
keep lga ea LAT LON
duplicates drop lga ea LAT LON, force //ea+lga necessary to uniquely identify ea
gen year = "2012-13"
//decode lga, gen(div4)
//drop lga
//ren ea div8
decode lga, gen(lga2)
drop lga
ren lga2 lga
save "${shiny_dir}/Nigeria_w2_div8_coords.dta", replace
collapse (mean) LAT LON, by(lga) //Simple way to guess at the center of an LGA based on sampled eas.
save "${shiny_dir}/Nigeria_w2_div4_coords.dta", replace

use "\\netid.washington.edu\wfs\EvansEPAR\Project\EPAR\Working Files\335 - Ag Team Data Support\Waves\Nigeria GHS\Nigeria GHS Wave 3\Raw DTA files\NGA_2015_GHSP-W3_v02_M_Stata\NGA_HouseholdGeovars_Y3.dta", clear
ren LAT_DD_MOD LAT
ren LON_DD_MOD LON
keep lga ea LAT LON
numlabel lga, r
duplicates drop lga ea LAT LON, force //ea+lga necessary to uniquely identify ea
gen year = "2015-16"
//decode lga, gen(div4)
//drop lga
//ren ea div8
decode lga, gen(lga2)
drop lga
ren lga2 lga
save "${shiny_dir}/Nigeria_w3_div8_coords.dta", replace
collapse (mean) LAT LON, by(lga) //Simple way to guess at the center of an LGA based on sampled eas.
save "${shiny_dir}/Nigeria_w3_div4_coords.dta", replace

use "\\netid.washington.edu\wfs\EvansEPAR\Project\EPAR\Working Files\335 - Ag Team Data Support\Waves\Nigeria GHS\Nigeria GHS Wave 4\Raw DTA Files\NGA_2018_GHSP-W4_v02_M_Stata12\nga_householdgeovars_y4.dta", clear
merge 1:1 hhid using "${Nigeria_GHS_W4_created_data}/Nigeria_GHS_W4_hhids.dta", nogen keep(3) 
ren lat_dd_mod LAT
ren lon_dd_mod LON
keep lga ea LAT LON
numlabel lga, r
duplicates drop lga ea LAT LON, force //ea+lga necessary to uniquely identify ea
gen year = "2018-19"
//decode lga, gen(div4)
//drop lga
//ren ea div8
decode lga, gen(lga2)
drop lga
ren lga2 lga
save "${shiny_dir}/Nigeria_w4_div8_coords.dta", replace
collapse (mean) LAT LON, by(lga) //Simple way to guess at the center of an LGA based on sampled eas.
save "${shiny_dir}/Nigeria_w4_div4_coords.dta", replace
