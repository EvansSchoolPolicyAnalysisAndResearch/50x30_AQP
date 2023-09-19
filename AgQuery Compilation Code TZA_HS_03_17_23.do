clear
global directory			"\\netid.washington.edu\wfs\EvansEPAR\Project\EPAR\Working Files\335 - Ag Team Data Support\Waves"

* For remote destop
*global directory			"\EPAR\Working Files\335 - Ag Team Data Support\Waves\"


* HS: For AgQueryPlus 2/15/23
global Tanzania_NPS_W1_created_data "$directory/Tanzania NPS/Tanzania NPS Wave 1/Final DTA Files - AGP/created_data"
global Tanzania_NPS_W2_created_data  	"$directory/Tanzania NPS/Tanzania NPS Wave 2/Final DTA Files - AT/created_data"
global Tanzania_NPS_W3_created_data 	"$directory/Tanzania NPS/Tanzania NPS Wave 3/Final DTA Files - AGP/created_data" 
global Tanzania_NPS_W4_created_data 	"$directory/Tanzania NPS/Tanzania NPS Wave 4/Final DTA files - AGP/created_data"
global Tanzania_NPS_W5_created_data		"$directory/Tanzania NPS/Tanzania NPS Wave 5/Final DTA Files - AGP/created_data" 

global Tanzania_NPS_W1_final_data		"$directory/Tanzania NPS/Tanzania NPS Wave 1/Final DTA Files - AGP/final_data"
global Tanzania_NPS_W2_final_data  		"$directory/Tanzania NPS/Tanzania NPS Wave 2/Final DTA Files - AT/final_data" 
global Tanzania_NPS_W3_final_data 		"$directory/Tanzania NPS/Tanzania NPS Wave 3/Final DTA Files - AGP/final_data"
global Tanzania_NPS_W4_final_data 		"$directory/Tanzania NPS/Tanzania NPS Wave 4/Final DTA Files - AGP/final_data"
global Tanzania_NPS_W5_final_data		"$directory/Tanzania NPS/Tanzania NPS Wave 5/Final DTA Files - AGP/final_data"
global shiny_dir "\\netid.washington.edu\wfs\EvansEPAR\Project\EPAR\Working Files\420 - AgQuery Plus\AgQueryPlus\agquery\Data" 


****** Create a CROP LIST dataset ******
* Declare a global macro with a list of variables (all top crops)
global all_topcrops maize rice wheat sorgum millet /*mill*/ pmill cowpea grdnt beans yam swtptt cassav banana teff barley coffee sesame hsbean nueg cotton sunflr pigpea cocoa soy mangos mungbn avocad potato cashew jute
global nb_allcrops : list sizeof global(all_topcrops) // Gets the current length of the global macro list "all_topcrops"
global all_topcrop_codes "11 12 16 13 15 14 32 43 31 24 22 21 71 0 17 54 42 0 0 50 41 34 56 47 73 0 72 23 46 62" //Recode bambara to groundnuts?
set obs $nb_allcrops // Update number of observations if number of crops changes
egen rnum = seq(), f(1) t($nb_allcrops) // sequence from 1 to length of nb_allcrops
gen crop_code = .
gen crop_name = ""


global hhid y1_hhid y2_hhid y3_hhid y4_hhid y5_hhid

* For each observation, create local 
forvalues k=1(1)$nb_allcrops { 
	local c : word `k' of $all_topcrop_codes
	local cn : word `k' of $all_topcrops
	replace crop_code = `c' if rnum==`k'
	replace crop_name = "`cn'" if rnum==`k'
} 

drop if crop_code==0 
	//Missing from country
drop rnum
tempfile croplist
save `croplist', replace


**** Household IDs *****
global instr_abbr tza_w1 tza_w2 tza_w3 tza_w4 tza_w5 // instrument abbrev
global instr Tanzania_NPS_W1 Tanzania_NPS_W2 Tanzania_NPS_W3  Tanzania_NPS_W4 Tanzania_NPS_W5 // instrument name
global no_instr : list sizeof global(instr) // instrument number



forvalues k=1(1)$no_instr { // in each iteration, "k" takes on a new isntrument number (lists the values 1 to "instrument number" (stepping up by 1))
    local c : word `k' of $instr_abbr
	local i : word `k' of $instr
	
	local h : word `k' of $hhid 
	
	disp in red "our hhid is `h'"
	
			disp in red "start of wave `i'"

	
	******************* Not in NGA Code *********************
	use "${`i'_created_data}/`i'_hhsize.dta", clear
	*capture ren *hhid hhid // standardize hhid variable
	*capture ren y2_hhid hhid // HS 11/10
	*if _rc drop *_hhid // HS 11/10
	tempfile hhsize
	save `hhsize'
	
	
	**** Geographic vars ****
	if `k'==1 {
		global hhgeovars region district ward ea
		} 
		else {
			global hhgeovars $hhgeovars
		}
	*****************************************

	
****** Plot ID, plot input, etc. **********
capture confirm file "${`i'_created_data}/`i'_all_plots.dta" // HS verifies that filename exists and is readable; otherwise issues error message
if !_rc { // if no error
	use "${`i'_created_data}/`i'_all_plots.dta", clear 
	rename kg_harvest kgs_harvest // not in NGA
	gen yield_hv = kgs_harvest/ha_harvest // not in NGA
	gen yield_pl = kgs_harvest/ha_planted // not in NGA

	gen land_productivity_hv = value_harvest/ha_harvest // not in NGA
	gen land_productivity_pl = value_harvest/ha_planted // not in NGA


	capture confirm file "${`i'_created_data}/`i'_plot_cost_inputs.dta" 
	if !_rc {
		capture merge m:1 `h' plot_id short using "${`i'_created_data}/`i'_plot_cost_inputs.dta", nogen // Merge all_plots with plot_cost_inputs
		if _rc merge m:1 `h' plot_id using "${`i'_created_data}/`i'_plot_cost_inputs.dta", nogen
					disp in red "start of wave `i'"


		/** Not in NGA *
		* HS commented out because it was choking, and these vars aren't used later in the code
		ren  val_exp
		gen crop_exp_all = val_exp // no val_exp in W1
		gen crop_exp_ha = val_exp/ha_planted 
		gen crop_exp_kg = val_exp/kgs_harvest 
		ren val_exp cost_expli 
		gen cost_total = cost_expli //Why do we do it this way? // original comment
		*/

		merge m:1 `h' plot_id short using "${`i'_created_data}/`i'_input_quantities.dta", nogen // Merge all_plots+plot_cost_inputs with input_quantities
		drop if crop_code == . | ha_planted ==0 //Note to self: this needs fixing. // original comment

		capture replace pestherb_rate = pestherb_rate*percent_inputs/ha_planted
		//replace herb_rate = herb_rate*percent_inputs/ha_planted // original comment
		//replace pest_rate = pest_rate*percent_inputs/ha_planted // original comment
		capture replace org_fert_rate = org_fert_rate*percent_inputs/ha_planted 
		capture replace inorg_fert_rate=inorg_fert_rate*percent_inputs/ha_planted

		recode *rate (.=0)
		gen inorg_fert_rate_alluse_fert=inorg_fert_rate if inorg_fert_rate > 0
		//gen herb_rate_alluse_herb=herb_rate if herb_rate>0 // original comment
		//gen pest_rate_alluse_pest=pest_rate if pest_rate>0 // original comment
		capture gen pestherb_rate_alluse_pestherb = pestherb_rate if pestherb_rate>0
		gen org_fert_rate_alluse_fert = org_fert_rate if org_fert_rate>0
	}

		
	capture merge m:1 `h' using "${`i'_final_data}/`i'_household_variables.dta", keep(1 3) keepusing(farm_size_agland weight geography year instrument weight) //Plot vars should only have ag_hh, yeah? // original comment
	merge m:1 `h' using `hhsize', nogen keepusing(hh_members $hhgeovars weight_pop_tot weight_pop_rururb)
	gen farmsize = "0 ha" if farm_size_agland == 0 | farm_size_agland == .
	replace farmsize = ">0 - 2 ha" if farm_size_agland <= 2 & farm_size_agland > 0 & farm_size_agland !=.
	replace farmsize = "2 - 4 ha" if farm_size_agland > 2 & farm_size_agland <= 4 & farm_size_agland !=.
	replace farmsize = ">4 ha" if farm_size_agland > 4 & farm_size_agland !=.
	drop farm_size_agland
	replace weight = weight*ha_planted
	replace weight_pop_tot = weight_pop_tot*ha_planted // at this point, we are not having weight_pop_tot
	replace weight_pop_rururb = weight_pop_rururb*ha_planted
	//merge m:1 crop_code using `croplist', nogen keep(3) //Cutting things down for now. // original comment
	* Standardize variable types
		tostring `h', replace force 
	save "${`i'_created_data}/`c'_plot_vars.dta", replace
	save "${shiny_dir}/`c'_plot_vars.dta", replace
}



//Person variables; could be changed so we can disaggregate among crops. 
capture confirm file "${`i'_created_data}/`i'_farmer_input_use.dta" 
if !_rc {
	use "${`i'_created_data}/`i'_farmer_input_use.dta", clear 
	merge 1:1 `h' indidy* using "${`i'_created_data}/`i'_farmer_improvedseed_use.dta", nogen  keep(1 3) keepusing(all_imprv_seed_use all_hybrid_seed_use)

	drop farmerid*
	ren indidy* indiv
	ren all_use_* *_use
	ren all_*_use *_use
	keep `h' indiv female *use
	
	ren inorg_fert_use indiv_fert_use 
	/*ren herb_use indiv_herb_use  // original comment
	ren pest_use indiv_pest_use */ // original comment
	capture ren pestherb_use indiv_pestherb_use
	ren org_fert_use indiv_orgfert_use
	recode *use (.=0)
	

	merge m:1 `h' using "${`i'_final_data}/`i'_household_variables.dta", nogen keepusing(farm_size_agland rural fhh weight geography year instrument)
	tempfile persvars
	save `persvars'
	use `persvars', clear
	
	merge m:1 `h' using `hhsize', keepusing(hh_members $hhgeovars weight_pop_tot weight_pop_rururb) 
	gen farmsize = "0 ha" if farm_size_agland == 0 | farm_size_agland == .
	replace farmsize = ">0 - 2 ha" if farm_size_agland <= 2 & farm_size_agland > 0 & farm_size_agland !=.
	replace farmsize = "2 - 4 ha" if farm_size_agland > 2 & farm_size_agland <= 4 & farm_size_agland !=.
	replace farmsize = ">4 ha" if farm_size_agland > 4 & farm_size_agland !=.
	drop farm_size_agland
		* Standardize variable types
		tostring `h', replace force 
	save "${`i'_created_data}/`c'_pers_vars.dta", replace
	save "${shiny_dir}/`c'_pers_vars.dta", replace
}



//Household-Crop. Disaggregation by plot manager is possible but probably not best here.
capture confirm file "${`i'_created_data}/`i'_hh_crop_values_production.dta" 
if !_rc {
	capture confirm file "${`i'_created_data}/`c'_plot_vars.dta" 
	if !_rc {
		use "${`i'_created_data}/`i'_hh_crop_values_production.dta", clear 
		tempfile cropfiles 
		save `cropfiles' 

		use "${`i'_created_data}/`c'_plot_vars.dta", clear 
		capture confirm var cost_expli // HS 2.21.23: W5 has trouble distinguishing between implicit vs explicit costs
		if _rc gen cost_expli=.
		capture collapse (sum) value_harvest kgs_harvest ha_harvest ha_planted cost_expli, by(`h' crop_code)
		
		merge 1:1 `h' crop_code using `cropfiles', nogen 
		drop if crop_code==. // HS 2.17.23: drops all of the unmatched from master
		gen grew_crop=1 
		fillin `h' crop_code 	
		recode grew_crop (.=0) // HS fill in where HH do not grow a crop with 0 so that we can later calculate a proportion of hh growing crop X
	
		merge m:1 `h' using "${`i'_final_data}/`i'_household_variables.dta", nogen keepusing(farm_size_agland rural fhh weight geography year instrument  proportion_cropvalue_sold) 
		gen farmsize = "0 ha" if farm_size_agland == 0 | farm_size_agland == .
		replace farmsize = ">0 - 2 ha" if farm_size_agland <= 2 & farm_size_agland > 0 & farm_size_agland !=.
		replace farmsize = "2 - 4 ha" if farm_size_agland > 2 & farm_size_agland <= 4 & farm_size_agland !=.
		replace farmsize = ">4 ha" if farm_size_agland > 4 & farm_size_agland !=.
		drop farm_size_agland
		//merge m:1 crop_code using `croplist', nogen keep(3) //Cutting things down for now. // original AT comment
		*drop if crop_code == . // HS added
			* Standardize variable types
		tostring `h', replace force 
		save "${`i'_created_data}/`c'_hh-crop_vars.dta", replace
		save "${shiny_dir}/`c'_hh-crop_vars.dta", replace
	}
}



//Household
use "${`i'_created_data}/`i'_person_ids.dta", clear 
gen women = female==1 & age > 15
gen men = female==0 & age > 15
gen children = age <=15
collapse (sum) men women children, by(`h')

capture confirm file "${`i'_created_data}/`i'_input_use.dta"
if !_rc {
	merge 1:1 `h' using "${`i'_created_data}/`i'_input_use.dta", nogen 
	recode use* (.=0)	
}




//Livestock products
//merge 1:1 *hhid using "${`i'_created_data}/`i'_livestock_products.dta", nogen //ALT: This needs fixing // original comment
capture confirm file "${`i'_created_data}/`i'_TLU_Coefficients"
	 if !_rc merge 1:1 `h' using "${`i'_created_data}/`i'_TLU_Coefficients", nogen
	 
capture confirm file  "${`i'_created_data}/`i'_livestock_sales.dta", nogen 
	if !_rc merge m:1 `h' using "${`i'_created_data}/`i'_livestock_sales.dta", nogen 
	capture ren hhid `h'
	merge 1:1 `h' using "${`i'_created_data}/`i'_hhsize.dta", nogen keepusing(hh_members $hhgeovars weight_pop_tot weight_pop_rururb) 

	
	* HS 2.16.23: Need to rename hours off farm for W1 
	preserve
	use "$directory\Tanzania NPS\Tanzania NPS Wave 1\Final DTA Files - AGP\final_data\Tanzania_NPS_W1_household_variables.dta", clear
	capture gen hrs_off_farm_pc_all = off_farm_hours_pc_all //capture because it only needed to be done once;
	capture gen hrs_off_farm_pc_any = off_farm_hours_pc_any
	save "$directory\Tanzania NPS\Tanzania NPS Wave 1\Final DTA Files - AGP\final_data\Tanzania_NPS_W1_household_variables.dta", replace
	restore


merge 1:1 `h' using "${`i'_final_data}/`i'_household_variables.dta", nogen keepusing(adulteq ag_hh crop_hh agactivities_hh fishing_hh num_crops_hh wage_paid_aglabor cost_expli_hh cost_expli_hh_ha livestock_hh land_productivity /*
*/ 																					labor_productivity farm_size_agland multiple_crops lvstck_holding_tlu w_share_nonfarm value_assets proportion_cropvalue_sold hrs_off_farm_pc_all /*
*/ 																					hrs_off_farm_pc_any peraeq_cons percapita_cons daily_peraeq_cons daily_percap_cons percapita_income crop_income fishing_income livestock_income /*
*/ 																					self_employment_income poverty_under_1_9 w_share_agwage w_share_nonfarm w_share_fishing w_share_crop w_share_livestock w_share_self_employment /*
*/ 																					w_share_nonagwage w_share_all_other w_share_transfers agwage_income total_income nonfarm_income nonagwage_income all_other_income transfers_income /*
*/ 																					share_livestock_prod_sold value_milk_produced value_eggs_produced value_livestock_products value_livestock_sales rural fhh weight geography year instrument)
gen farmsize = "0 ha" if farm_size_agland == 0 | farm_size_agland == .
replace farmsize = ">0 - 2 ha" if farm_size_agland <= 2 & farm_size_agland > 0 & farm_size_agland !=.
replace farmsize = "2 - 4 ha" if farm_size_agland > 2 & farm_size_agland <= 4 & farm_size_agland !=.
replace farmsize = ">4 ha" if farm_size_agland > 4 & farm_size_agland !=.
gen ssp = farm_size_agland <=4
replace ssp = . if farm_size_agland==. | farm_size_agland==0
ren land_productivity land_productivity_hh 
ren labor_productivity labor_productivity_hh
gen households=1
	* Standardize variable types
		tostring `h', replace force 
save "${`i'_created_data}/`i'_hh_vars.dta", replace //Need to come back to this one. // original AT comment
save "${shiny_dir}/`c'_hh_vars.dta", replace

		
/* From original TZA CC file - commented out by AT
use "${`c'_created_data}/`c'_plot_vars.dta", clear
ren hhid div9
ren plot_id div10
save "${`c'_created_data}/`c'_plot_vars.dta", replace
save "${shiny_dir}/`c'_plot_vars.dta", replace
*/
*Not in TZA :
*use "${`i'_created_data}/`i'_plot_cost_inputs_long.dta", clear
*keep if strmatch(input, "anml") & strmatch(exp, "imp")

*merge 1:1 *hhid plot_id using "${`i'_created_data}/`i'_plot_areas.dta", nogen
*keep if val > 0 & val!=.
*capture ren area_meas_hectares field_size
*collapse (sum) field_size, by(*hhid)
*ren field_size ha_anml_plowed
*tempfile anml_plow
*save `anml_plow'


//Livestock vars for filtering (inefficient!)
use "${`i'_created_data}/`i'_TLU_Coefficients.dta", clear
	tempfile tlus
	save `tlus'

/* From original TZA CC file - commented out by AT
use "${`i'_created_data}/`i'_livestock_products.dta", clear
capture ren *hhid hhid
if _rc drop *_hhid 
collapse (sum) milk_liters_produced earnings_milk_year (firstnm) price_per_unit_hh, by(hhid)
gen prop_dairy_sold = earnings_milk_year/(price_per_unit_hh*milk_liters_produced)
tempfile ls_prod
save `ls_prod'
*/

use "${`i'_final_data}/`i'_household_variables.dta", clear
 merge 1:1 `h' using `tlus',  keepusing(nb_cattle_today nb_smallrum_today nb_poultry_today nb_cows_today nb_chickens_today) nogen
//merge 1:1 *hhid using `ls_prod', keepusing(prop_dairy_sold /*prop_eggs_sold*/ /*share_livestock_prod_sold*/) nogen // original comment
*merge 1:1 *hhid using `anml_plow', nogen // original comment
ren share_livestock_prod_sold prop_anml_sold
ren nb_*_today nb_*
	* Standardize variable types
		tostring `h', replace force 
save "${shiny_dir}/`c'_household_variables.dta", replace
		disp in red "end of wave `i'"

		
			
* Recode farmsize as a factor
use "${shiny_dir}/`c'_hh-crop_vars.dta", clear
*encode farmsize, gen(farmsize2)
gen farmsize2 = 1 if farmsize == "0 ha"
	replace farmsize2 = 2 if farmsize == "2 - 4 ha"
	replace farmsize2 = 3 if farmsize == ">0 - 2 ha"
	replace farmsize2 = 4 if farmsize == ">4 ha"
drop farmsize
ren farmsize2 farmsize
save "${shiny_dir}/`c'_hh-crop_vars.dta", replace

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



	

* HKS 5/4/23: Standardize hhid for agqueryplus

/* hh_vars
use "${shiny_dir}/tza_w1_hh_vars.dta", clear
	ren *hhid hhid
	save "${shiny_dir}/tza_w1_hh_vars.dta", replace
use "${shiny_dir}/tza_w2_hh_vars.dta", clear
	ren *hhid hhid
	save "${shiny_dir}/tza_w2_hh_vars.dta", replace
use "${shiny_dir}/tza_w3_hh_vars.dta", clear
	ren y3_hhid hhid
	save "${shiny_dir}/tza_w3_hh_vars.dta", replace
use "${shiny_dir}/tza_w4_hh_vars.dta", clear
	ren y4_hhid hhid
	save "${shiny_dir}/tza_w4_hh_vars.dta", replace
use "${shiny_dir}/tza_w5_hh_vars.dta", clear
	ren y5_hhid hhid
	save "${shiny_dir}/tza_w5_hh_vars.dta", replace	


* hh_crop_vars
use "${shiny_dir}/tza_w1_hh-crop_vars.dta", clear
	ren *hhid hhid
	save "${shiny_dir}/tza_w1_hh-crop_vars.dta", replace
use "${shiny_dir}/tza_w2_hh-crop_vars.dta", clear
	ren *hhid hhid
	save "${shiny_dir}/tza_w2_hh-crop_vars.dta", replace
use "${shiny_dir}/tza_w3_hh-crop_vars.dta", clear
	ren y3_hhid hhid
	save "${shiny_dir}/tza_w3_hh-crop_vars.dta", replace
use "${shiny_dir}/tza_w4_hh-crop_vars.dta", clear
	ren y4_hhid hhid
	save "${shiny_dir}/tza_w4_hh-crop_vars.dta", replace
use "${shiny_dir}/tza_w5_hh-crop_vars.dta", clear
	ren y5_hhid hhid
	save "${shiny_dir}/tza_w5_hh-crop_vars.dta", replace	

* plot_vars
use "${shiny_dir}/tza_w1_plot_vars.dta", clear
	ren *hhid hhid
	save "${shiny_dir}/tza_w1_plot_vars.dta", replace
use "${shiny_dir}/tza_w2_plot_vars.dta", clear
	ren *hhid hhid
	save "${shiny_dir}/tza_w2_plot_vars.dta", replace
use "${shiny_dir}/tza_w3_plot_vars.dta", clear
	ren y3_hhid hhid
	save "${shiny_dir}/tza_w3_plot_vars.dta", replace
use "${shiny_dir}/tza_w4_plot_vars.dta", clear
	ren y4_hhid hhid
	save "${shiny_dir}/tza_w4_plot_vars.dta", replace
use "${shiny_dir}/tza_w5_plot_vars.dta", clear
	ren y5_hhid hhid
	save "${shiny_dir}/tza_w5_plot_vars.dta", replace	

* pers_vars
use "${shiny_dir}/tza_w1_pers_vars.dta", clear
	ren *hhid hhid
	save "${shiny_dir}/tza_w1_pers_vars.dta", replace
use "${shiny_dir}/tza_w2_pers_vars.dta", clear
	ren *hhid hhid
	save "${shiny_dir}/tza_w2_pers_vars.dta", replace
use "${shiny_dir}/tza_w3_pers_vars.dta", clear
	ren y3_hhid hhid
	save "${shiny_dir}/tza_w3_pers_vars.dta", replace
use "${shiny_dir}/tza_w4_pers_vars.dta", clear
	ren y4_hhid hhid
	save "${shiny_dir}/tza_w4_pers_vars.dta", replace
use "${shiny_dir}/tza_w5_pers_vars.dta", clear
	ren y5_hhid hhid
	save "${shiny_dir}/tza_w5_pers_vars.dta", replace	
*/	
	

use "\\netid.washington.edu\wfs\EvansEPAR\Project\EPAR\Working Files\335 - Ag Team Data Support\Waves\Tanzania NPS\Tanzania NPS Wave 1\Raw DTA Files\TZA_2008_NPS1_v02_M_STATA_English_labels\HH.Geovariables_Y1.dta", clear
merge 1:1 hhid using "${Tanzania_NPS_W1_created_data}/Tanzania_NPS_W1_hhids.dta", nogen keep(3)  
ren lat_modified LAT
ren lon_modified LON
//Village is considered same as ea in this wave, which is weird.
capture duplicates drop region district ward ea LAT LON, force
keep ward ea LAT LON
gen year = "2010-11"
save "${shiny_dir}/Tanzania_w1_div8_coords.dta", replace
*preserve
//collapse (mean) LAT LON, by(village)
//save "${shiny_dir}/Tanzania_w3_div7_coords.dta", replace
//restore
/*collapse (mean) LAT LON, by(ward) */ //HS 2.17.23 Commented out because lat/lon is unavailable
save "${shiny_dir}/Tanzania_w1_div5_coords.dta", replace

/*
\\netid.washington.edu\wfs\EvansEPAR\Project\EPAR\Working Files\335 - Ag Team Data Support\Waves\Tanzania NPS\Tanzania NPS Wave 1\Final DTA Files\created_data\Tanzania_NPS_W1_hhids.dta
\\netid.washington.edu\wfs\EvansEPAR\Project\EPAR\Working Files\335 - Ag Team Data Support\Waves\Tanzania NPS\Tanzania NPS Wave 1\Final DTA Files\created_data/Tanzania_NPS_W1_hhids.dta
*/

use "\\netid.washington.edu\wfs\EvansEPAR\Project\EPAR\Working Files\335 - Ag Team Data Support\Waves\Tanzania NPS\Tanzania NPS Wave 2\Raw DTA Files\TZA_2010_NPS-R2_v02_M_STATA8\HH.Geovariables_Y2.dta", clear
merge 1:1 y2_hhid using "${Tanzania_NPS_W2_created_data}/Tanzania_NPS_W2_hhids.dta", nogen keep(3) 
ren lat_modified LAT
ren lon_modified LON
//Village is considered same as ea in this wave, which is weird.
capture duplicates drop region district ward ea LAT LON, force
keep ward ea LAT LON
gen year = "2010-11"
save "${shiny_dir}/Tanzania_w2_div8_coords.dta", replace
//preserve
//collapse (mean) LAT LON, by(village)
//save "${shiny_dir}/Tanzania_w3_div7_coords.dta", replace
//restore
/*collapse (mean) LAT LON, by(ward) */ //HS 2.17.23 Commented out because lat/lon is unavailable
save "${shiny_dir}/Tanzania_w2_div5_coords.dta", replace


use "\\netid.washington.edu\wfs\EvansEPAR\Project\EPAR\Working Files\335 - Ag Team Data Support\Waves\Tanzania NPS\Tanzania NPS Wave 3\Raw DTA Files\TZA_2012_NPS-R3_v01_M_STATA8_English_labels\HouseholdGeovars_Y3", clear
merge 1:1 y3_hhid using "${Tanzania_NPS_W3_created_data}/Tanzania_NPS_W3_hhids.dta", nogen keep(3) 
ren lat_dd_mod LAT
ren lon_dd_mod LON
//Village is considered same as ea in this wave, which is weird.
capture duplicates drop region district ward ea LAT LON, force
keep ward ea LAT LON
gen year = "2012-13"
save "${shiny_dir}/Tanzania_w3_div8_coords.dta", replace
//preserve
//collapse (mean) LAT LON, by(village)
//save "${shiny_dir}/Tanzania_w3_div7_coords.dta", replace
//restore
/*collapse (mean) LAT LON, by(ward) */ //HS 2.17.23 Commented out because lat/lon is unavailable
save "${shiny_dir}/Tanzania_w3_div5_coords.dta", replace

use "\\netid.washington.edu\wfs\EvansEPAR\Project\EPAR\Working Files\335 - Ag Team Data Support\Waves\Tanzania NPS\Tanzania NPS Wave 3\Raw DTA Files\TZA_2012_NPS-R3_v01_M_STATA8_English_labels\HouseholdGeovars_Y3", clear
merge 1:1 y3_hhid using "${Tanzania_NPS_W3_created_data}/Tanzania_NPS_W3_hhids.dta", nogen keep(3) 
ren lat_dd_mod LAT
ren lon_dd_mod LON
//Village is considered same as ea in this wave, which is weird.
capture duplicates drop region district ward ea LAT LON, force
keep ward ea LAT LON
gen year = "2012-13"
save "${shiny_dir}/Tanzania_w3_div8_coords.dta", replace
//preserve
//collapse (mean) LAT LON, by(village)
//save "${shiny_dir}/Tanzania_w3_div7_coords.dta", replace
//restore
/*collapse (mean) LAT LON, by(ward) */ //HS 2.17.23 Commented out because lat/lon is unavailable
save "${shiny_dir}/Tanzania_w3_div5_coords.dta", replace

use "\\netid.washington.edu\wfs\EvansEPAR\Project\EPAR\Working Files\335 - Ag Team Data Support\Waves\Tanzania NPS\Tanzania NPS Wave 4\Raw DTA Files\TZA_2014_NPS-R4_v03_M_STATA11\com_sec_a1a2", clear
ren id_01 region
ren id_02 district
ren id_03 ward
drop village
ren id_04 village
ren id_05 ea 
ren y4_cluster clusterid
merge 1:1 clusterid using "\\netid.washington.edu\wfs\EvansEPAR\Project\EPAR\Working Files\335 - Ag Team Data Support\Waves\Tanzania NPS\Tanzania NPS Wave 4\Raw DTA Files\TZA_2014_NPS-R4_v03_M_STATA11\npsy4.ea.offset.dta"

//merge 1:m $hhgeovars using "${Tanzania_NPS_W4_created_data}/Tanzania_NPS_W4_hhids.dta", nogen keep(3) 
ren lat_modified LAT
ren lon_modified LON
duplicates drop $hhgeovars LAT LON, force
keep ward village ea LAT LON
gen year = "2014-16"
save "${shiny_dir}/Tanzania_w4_div8_coords.dta", replace
//preserve
/*collapse (mean) LAT LON, by(ward) */ //HS 2.17.23 Commented out because lat/lon is unavailable
//save "${shiny_dir}/Tanzania_w4_div7_coords.dta", replace
//restore
/*collapse (mean) LAT LON, by(ward) */ //HS 2.17.23 Commented out because lat/lon is unavailable
save "${shiny_dir}/Tanzania_w4_div5_coords.dta", replace

use "\\netid.washington.edu\wfs\EvansEPAR\Project\EPAR\Working Files\335 - Ag Team Data Support\Waves\Tanzania NPS\Tanzania NPS Wave 5\Raw DTA Files\TZA_2019_NPD-SDD_v06_M_STATA12\CM_SEC_A", clear
ren id_01 region
ren id_02 district
ren id_03 ward
ren id_04 village
ren id_05 ea 
duplicates drop $hhgeovars, force
merge 1:m $hhgeovars using "${Tanzania_NPS_W5_created_data}/Tanzania_NPS_W5_hhids.dta", nogen keep(3) 
ren cm_gps__Latitude LAT
ren cm_gps__Longitude LON
keep ward village ea LAT LON
gen year = "2018-19"
save "${shiny_dir}/Tanzania_w5_div8_coords.dta", replace
//preserve
/*collapse (mean) LAT LON, by(ward) */ //HS 2.17.23 Commented out because lat/lon is unavailable
//save "${shiny_dir}/Tanzania_w5_div7_coords.dta", replace
//restore
/*collapse (mean) LAT LON, by(ward) */ //HS 2.17.23 Commented out because lat/lon is unavailable
save "${shiny_dir}/Tanzania_w5_div5_coords.dta", replace
