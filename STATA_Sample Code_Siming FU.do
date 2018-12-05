/*
	Author: Siming Fu
	Sample Project Code
*/  


/* Project Description

Objective: 
	In this sample project, I use publicly available datasets on international trade to identify factors that influence a country's total trade value. We use data from Romania & Tanzania for this analysis.
	This is achieved by first constructing and modifying raw datasets, analyzing summary statistics followed by a regression analysis (both with baseline regression and sensitivity test model) using fixed effects.  
	As one can imagine, a similar set of techniques can be use to analyze product sales and uptake data.  

Data Sources:
	- Import and export data of Romania and Tanzania during 2011 - 2013.
	- GDP data
	- Geographic and demographic data 
All downloadable via Github: https://github.com/siming-fu/World-Trade-Data

Glossary:
	- HS Code: HS Codes (or HTS Codes) are a standardized international system to classify globally traded products.
	- ISO Code: ISO country codes are internationally recognized  two- or three- letter codes that designate every country or state. 
	
Output:
	- Summary statistics of annual import/export activities for focus group countries (Romania and Tanzania) [see output file: importsumstats.xlsx and statisticspoint5.xlsx];
	- Results of a regression analysis of factors that influence a country's total trade value [see output file: regressionsresultb.xml and regressionsresultfe.xml].
*/

/*  PRELIMINARIES  */

**  Global Stata Settings**

	clear
	clear matrix
	set more off

**  Path information  **
	cd "H:\test1"  /* Set your filepath with basefiles here. */
	
**************************
** I. Master Data Build **
**************************

/* 
Step 1. Construct a dataset including all exports and imports for Romania and Tanzania in 2011-2013 using .csv files whose names include a country 3-digit ISO code (KEN or ROM) and year. 
	Further,
	- Drop all observations whose partner country (for exports or imports) is the World (WLD). 
	- Transform the ‘productcode’ variable into a string variable with 6-digits for each observation.
*/

/* Importing and modifying raw datasets */

	forvalues x=2011/2013 {
		clear
		import delimited ROM_`x'.csv, varnames(1) stringcols(1 2 3 4 5 6 8) numericcols(7 9) 
			drop if partneriso3=="WLD"
			gen lngth = length(productcode)
			tab lngth
			replace productcode = "0" + productcode if lngth == 5
			drop lngth
		save ROM_`x'.dta, replace

		clear
		import delimited TZA_`x'.csv, varnames(1) stringcols(1 2 3 4 5 6 8) numericcols(7 9) 
			drop if partneriso3=="WLD"
			gen lngth = length(productcode)
			tab lngth
			replace productcode = "0" + productcode if lngth == 5
			drop lngth
		save TZA_`x'.dta, replace
		}

/* Appending Datasets */

	use ROM_2011.dta
	append using TZA_2011.dta

	forvalues x=2012/2013 {
		append using ROM_`x'.dta
		append using TZA_`x'.dta
		}

	sort reporteriso3 productcode partneriso3 year
	save dataset.dta, replace 		/* Saving Master Dataset */

*****************************
**II. Descriptive Analysis **
*****************************
/* 
Step 2. Descriptive analysis of broad trends in Tanzania's/Romania's import activities through summary statistics based on the data for imports. 
I calculate annual average, median, standard deviation for trade value and number of observations by reporting country (ROM and TZA).	
*/

/* Describe the variety of imported goods by calculating number of HS 6-digit product types */

	use dataset.dta, clear
		keep if tradeflowname=="Import"
		collapse (sum) tradevaluein1000usd, by(reporteriso3 productcode year)
		gen number_HS6=1
		collapse (count) number_HS6, by(reporteriso3 year)
	save dataset_numberHS6.dta, replace

/* Comments: 
From 2011 - 2013, we observe that the total number of import product types have declined slightly for both Romania and Tanzania. 
*/

/* Describe the basic statistics of import trading partners by calculating avg, median, std, count by country and year */

	use dataset.dta, clear
		keep if tradeflowname=="Import"
		merge n:1 reporteriso3 year using dataset_numberHS6.dta
		drop _merge
		rename tradevaluein1000usd import_mean

		gen import_median=import_mean
		gen import_stdev=import_mean
		gen number_obs=1
		collapse (mean) import_mean number_HS6 import_median (sd) import_stdev (count) number_obs, by (reporteriso3 year)
		order reporteriso3 year import_mean import_median import_stdev number_obs number_HS6 
		sort reporteriso3 year
	export excel importsumstats.xlsx, firstrow(variables) replace  

*******************************
** III. Exploratory Analysis **
*******************************

/* Step 3. 
After some basic descriptive analysis, I explore the data further for questions such as "who are the biggest import/export partners?',
and "what are the top import/export goods?". 
*/

/* Top 3 Import trade partners */

	use dataset.dta, clear
		keep if tradeflowname=="Import"
		collapse (sum) tradevaluein1000usd, by(reporteriso3 partneriso3 partnername year)
		bysort reporteriso3 year: egen rank=rank(-trade)
		keep if rank<=3
	
/* Comments: 
- For Romania, the biggest import partners from 2011-2013 have always been Germany(DEU), Italy(ITA) and Hungary (HUN);
- For Tanzania, the top 3 import partners are India(IND),Switzerland(CHE), China(CHN) and UAE(ARE).
*/

/* A follow-up, what is imported from the biggest import partners, say for Tanzania? */

	use dataset.dta, clear
		keep if tradeflowname=="Import" & reporteriso3=="TZA" & inlist(partneriso3 , "IND", "CHE", "CHN", "ARE")
		collapse (sum) tradevaluein1000usd, by(reporteriso3 partneriso3 partnername productcode year)
		bysort reporteriso3 year: egen rank=rank(-trade)
		keep if rank<=3

/* Comments:
- We observe that Tanzania mostly imports Petrol Oil (HS code=271011) & Light Oil & Prep (not crude) (HS code=271019) from its biggest trading partners. 
*/

/* Similarly for exports, I produce a table based on data for exports in 2011 to find out what are the largest 5 destination countries.
I further disaggregate these results for sector 07 (vegetables) and sector 63 (Textile) as sector of interest (as they are the common export industry for developing countries).
*/

	use dataset.dta, clear
		keep if tradeflowname=="Export" & year==2011
		gen hs2=substr(productcode,1,2)
		collapse (sum) tradevaluein1000usd, by(reporteriso3 partneriso3 partnername hs2)
		rename tradevaluein1000usd exports
		egen exports_top=rank(-exports), by(reporteriso3 hs2)
		egen exports_total=sum(exports), by(reporteriso3 hs2)
		gen export_share=exports/exports_total
		keep if exports_top<6
		keep if (hs2=="07" | hs2=="63")
		drop  exports_total
		order reporteriso3 hs2  
		sort reporteriso3 hs2 exports_top
		drop exports_top
	export excel statisticspoint5.xlsx, firstrow(variables) replace  

/*Comments: 
Several insights can be draw from these results. For example, we can see that for Tanzania, most of its vegetable export goes to India. 
Further, if reframed in the context of product analysis, a potential product opportunity could be "how we can facilitate agriculture export?" 
E.g. Can we allocate more resource to negotiate an export tariff treaty (high unpredictability but low cost: short term project), build better logistics infrastructure (high cost & long project timeline
but can bring fundamental improvement: long term project), or initiate marketing campaigns to promote Tanzania agri products (medium spending w. potentials for quantifiable marketing experimentation: mid-term project).
*/

****************************
** IV.Regression Analysis **
****************************

/* Step 4. I pull-in two additional external datasets to enrich the current datasets in preparation for regression analysis.
Specifically,
	- I combine the master dataset with information on GDP in the partner country using the STATA dataset “GDPdata.dta” and report the percentage of concordance across the datasets. 
	- I combine the master dataset with information on gravity variables for bilateral country pairs using the STATA dataset “dist_cepii_minimal.dta” and report the percentage of concordance across the datasets.
*/

	use "GDPdata.dta", clear 
		capture rename country partneriso3
	save "GDPdata.dta", replace 

	use "dist_cepii_minimal.dta", clear
		capture rename origin_country reporteriso3
		capture rename destination_country partneriso3
	save "dist_cepii_minimal.dta", replace
	 
	use dataset.dta, clear
		merge m:1 partneriso3 year using "GDPdata.dta"

		drop if _merge==2
		rename _merge _mergeGDP
		merge m:1 partneriso3 reporteriso3 using "dist_cepii_minimal.dta"

		drop if _merge==2
		rename _merge _mergedistance
	save dataset.dta, replace

/* Step 5. I constructed a new dataset at the level of the reporting country-partner country-HS 6-digit product-year  and aggregate it up to the level of  the reporting country-partner country-HS 2-digit sector-year.
Further after calculating the sum of trade value across all HS 6-digit products within an HS 2-digit sector (for that reporting country-partner country-year),  I then use this dataset to estimate separate regressions for imports and for exports.
*/ 

	use dataset.dta, clear
		gen hs2=substr(productcode,1,2)
		gen tradeflow=1 if tradeflowname=="Import"
		replace tradeflow=2 if tradeflowname=="Export"
		label define tradeflowl 1 "Import" 2 "Export"
		label values tradeflow tradeflowl
		gen number_HS6=1
		collapse (sum) tradevaluein1000usd (count) number_HS6 (mean) GDP dist comlang_off, by(reporteriso3 partneriso3 hs2 year tradeflow)
	save dataset_HS2.dta, replace 

	rename tradevaluein1000usd tradevalue
	foreach var of varlist tradevalue number_HS6  GDP dist {
		gen l`var'=ln(`var')
		}

/*
**** Base Line Regression: OLS Regression ****

Dependent variable:
- the logarithm of total trade value (1=import 2=export);

explanatory variables:
-	logarithm of GDP in the partner country;
-	logarithm of distance;
-	dummy variable for countries sharing a common language.
*/
	
	set more off
	forvalues x=1/2 {
			regress ltradevalue lGDP ldist comlang_off if tradeflow==`x', robust
			outreg2 lGDP ldist comlang_off using regressionsresultb, ctitle("`x', ltradevalue, baseline") excel append  
			}
		
	
/*
**** Sensitivity Analysis: Fixed Effects (least square dummy variable model) *****

I re-run the regressions above with 4 types of fixed effects:
-	fixed effects by reporting country; 
-	fixed effects by partner country;
-	fixed effects by HS 2-digit sector;
-	fixed effects by year.
*/

	set more off
	forvalues x=1/2 {
			xi: regress ltradevalue lGDP ldist comlang_off i.reporteriso3 i.partneriso3 i.hs2 i.year if tradeflow==`x', robust
			outreg2 lGDP ldist comlang_off using regressionsresultfe, ctitle("`x', ltradevalue, FE") excel append  
			}

/* Comments:
Reminder: Please open the XML file produced in this step in Excel for formatted regression outputs.
For presentation, I export all the results of base line regressions with coefficients, 
the standard errors in parentheses, the significance level(* 10%, ** 5%  *** 1%), 
the number of observations and the R-squared of the regressions. 

Methodological considerations:
	- Certain variables are logged in regression analysis to avoid distortion from outliers.
	- Using fixed effect (FE) is an attempt to control for unobserved heterogeneity, 
	i.e. when using FE I assume that something within the independent variable may bias the
	dependent variable therefore I need to control for this. For example, there might be some 
	characteristics that are intrinsic to a country itself (culture, political environment) that 
	also influence the outcome variable. FE removes the effect of those time-invariant characteristics 
	so I can assess the net effect of the predictors on the outcome variable. 

Comparing the baseline and sensitivity regression results:
	- The above model specification, both in baseline and sensitivity check regression, is mostly an over-simplified approach
	to measure the factors that influence a country's import and export activity, and likely to suffer from omitted-variable bias. 
	- However, the results between two models are still drastically different: after using fixed effects, the coefficient of the 
	variable that indicates share of common language (comlang_off) flipped sign, and the level of GDP of partner country (lGDP) 
	lost statistical significance. 
	- This simple practice serves as a good example to show that when using regression models, it is critical to check the robustness
	of model specification with sensitivity analysis, and a coefficient with statistical significance is not at all a reliable indicator 
	for real-life implications. 
