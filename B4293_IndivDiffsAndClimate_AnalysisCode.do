*** PROJECT: The role of individual differences in pro-environmental behaviour
*** STATA VERSION 18
*** Created 16/10/2023 by Dan Major-Smith (based on script by Ken Freminot)

** Set working directory
cd "X:\Studies\RSBB Team\Dan\B4293 - MSc Environmental Behaviors"

** Install any user-written packages
*ssc install missings, replace
*ssc install grc1leg, replace

** Create log file
log using "./Results/IndivDiffsAndClimate.log", replace

** Read in dataset
use "EnvironBehav_B4293.dta", clear


******************************************************************************
***** Data cleaning and processing

* Start by describing the data
describe

* Add value labels
numlabel, add

** Remove children not alive at 1 year of age and if withdrew consent
tab1 kz011b YPJ7500, m

drop if kz011b == 2 | kz011b == .a
drop if YPJ7500 == .b

drop kz011b


*** Go through each variable and clean ready for analysis

** Confounders

* Child sex
tab kz021, m

rename kz021 sex
tab sex, m

* Mother age at birth
tab mz028b, m

replace mz028b = . if mz028b < 0
rename mz028b ageAtBirth
tab ageAtBirth, m

* Mother home ownership status - Code some values together
tab a006, m

replace a006 = . if a006 < 0
recode a006 (0 1 = 1) (2 5 = 2) (3 4 = 3) (6 = 4)
label define home_lb 1 "Owned/Mortaged" 2 "Council/HA" 3 "Private rent" 4 "Other"
numlabel home_lb, add
label values a006 home_lb
rename a006 home
tab home, m 

* Mother highest education level
tab c645a, m

replace c645a = . if c645a < 0
rename c645a edu
tab edu, m

* IMD status in pregancy
tab jan1993imd2010q5_M, m

replace jan1993imd2010q5_M = . if jan1993imd2010q5_M < 0
rename jan1993imd2010q5_M imd
tab imd, m

* Child ethnicity
tab c804, m

replace c804 = . if c804 < 0
rename c804 ethnic
tab ethnic, m

* Offspring age at climate questionnaire
tab YPJ7500, m

replace YPJ7500 = . if YPJ7500 < 0
replace YPJ7500 = 29 if YPJ7500 == 28
rename YPJ7500 offspringAge
tab offspringAge, m


** Exposures

* Extraversion
sum fg7360

replace fg7360 = . if fg7360 < 0
rename fg7360 extra
sum extra

* Agreeableness
sum fg7361

replace fg7361 = . if fg7361 < 0
rename fg7361 agree
sum agree

* Conscientiousness
sum fg7362

replace fg7362 = . if fg7362 < 0
rename fg7362 consc
sum consc

* Emotional stability
sum fg7363

replace fg7363 = . if fg7363 < 0
rename fg7363 emoStab
sum emoStab

* Openness
sum fg7364

replace fg7364 = . if fg7364 < 0
rename fg7364 open
sum open

* Total IQ score
sum fh6280

replace fh6280 = . if fh6280 < 0
rename fh6280 iq
sum iq

drop fh6277 fh6278


** Outcomes

* Believes that the climate is changing - ordered categorical variable 
tab YPJ3000, m

replace YPJ3000 = . if YPJ3000 < 0
rename YPJ3000 climateChanging
tab climateChanging, m


* Degree to which YP is concerned about the impact of climate change (only answered if believe climate is changing) - ordered categorical variable
tab YPJ3001, m

replace YPJ3001 = . if YPJ3001 < 0
rename YPJ3001 climateConcern
tab climateConcern, m


* Believes that humans are to blame for climate change (only answered if believe climate is changing) - ordered categorical variable
tab YPJ3002, m

replace YPJ3002 = . if YPJ3002 < 0
rename YPJ3002 climateHumans
tab climateHumans, m


* Personal actions will make difference to long-term climate changes (only answered if believe climate is changing) - unordered categorical variable
tab YPJ3003, m

replace YPJ3003 = . if YPJ3003 < 0
rename YPJ3003 climateAction
tab climateAction, m


** Drop questions about which things will change due to climiate change
drop YPJ3010-YPJ3017


** Now go through the 'actions taken due to climate change' questions and recode as appropriate into 'not done this' vs 'done for non-climate reasons' vs 'done for climate reasons' vs 'done for both climate and non-climate reasons' (while excluding impossible combinations of values - e.g., 'not done and done')

* Travelling locally
tab1 YPJ3020 YPJ3021 YPJ3022, m

replace YPJ3020 = . if YPJ3020 < 0
replace YPJ3021 = . if YPJ3021 < 0
replace YPJ3022 = . if YPJ3022 < 0

egen travel = group(YPJ3020 YPJ3021 YPJ3022), label
tab travel, m

recode travel (1 4 6 8 =.) (2=0) (3=2) (5=1) (7=3)
label variable travel "Changed the way travel locally"
label define actions_lb 0 "No" 1 "Yes, for climate" 2 "Yes, for other" 3 "Yes, for climate and other"
numlabel actions_lb, add
label values travel actions_lb
tab travel, m

tab YPJ3020 YPJ3021

recode travel (0 2 = 0) (1 3 = 1), gen(travel_bin)
label variable travel_bin "Changed the way travel locally (binary)"
label define actions_bin_lb 0 "No" 1 "Yes"
numlabel actions_bin_lb, add
label values travel_bin actions_bin_lb
tab travel_bin, m

drop YPJ3020 YPJ3021 YPJ3022


* Reduced household waste
tab1 YPJ3023 YPJ3024 YPJ3025, m

replace YPJ3023 = . if YPJ3023 < 0
replace YPJ3024 = . if YPJ3024 < 0
replace YPJ3025 = . if YPJ3025 < 0

egen waste = group(YPJ3023 YPJ3024 YPJ3025), label
tab waste, m

recode waste (1 4 6 8 =.) (2=0) (3=2) (5=1) (7=3)
label variable waste "Reduced household waste"
label values waste actions_lb
tab waste, m

tab YPJ3023 YPJ3024

recode waste (0 2 = 0) (1 3 = 1), gen(waste_bin)
label variable waste_bin "Reduced household waste (binary)"
label values waste_bin actions_bin_lb
tab waste_bin, m

drop YPJ3023 YPJ3024 YPJ3025


* Reduced energy use
tab1 YPJ3026 YPJ3027 YPJ3028, m

replace YPJ3026 = . if YPJ3026 < 0
replace YPJ3027 = . if YPJ3027 < 0
replace YPJ3028 = . if YPJ3028 < 0

egen energy = group(YPJ3026 YPJ3027 YPJ3028), label
tab energy, m

recode energy (1 4 6 8 =.) (2=0) (3=2) (5=1) (7=3)
label variable energy "Reduced energy use"
label values energy actions_lb
tab energy, m

tab YPJ3026 YPJ3027

recode energy (0 2 = 0) (1 3 = 1), gen(energy_bin)
label variable energy_bin "Reduced energy use (binary)"
label values energy_bin actions_bin_lb
tab energy_bin, m

drop YPJ3026 YPJ3027 YPJ3028


* Changed what buy
tab1 YPJ3029 YPJ3030 YPJ3031, m

replace YPJ3029 = . if YPJ3029 < 0
replace YPJ3030 = . if YPJ3030 < 0
replace YPJ3031 = . if YPJ3031 < 0

egen buy = group(YPJ3029 YPJ3030 YPJ3031), label
tab buy, m

recode buy (1 4 6 8 =.) (2=0) (3=2) (5=1) (7=3)
label variable buy "Changed what buy"
label values buy actions_lb
tab buy, m

tab YPJ3029 YPJ3030

recode buy (0 2 = 0) (1 3 = 1), gen(buy_bin)
label variable buy_bin "Changed what buy (binary)"
label values buy_bin actions_bin_lb
tab buy_bin, m

drop YPJ3029 YPJ3030 YPJ3031


* Reduced air travel
tab1 YPJ3032 YPJ3033 YPJ3034, m

replace YPJ3032 = . if YPJ3032 < 0
replace YPJ3033 = . if YPJ3033 < 0
replace YPJ3034 = . if YPJ3034 < 0

egen airTravel = group(YPJ3032 YPJ3033 YPJ3034), label
tab airTravel, m

recode airTravel (1 4 6 8 =.) (2=0) (3=2) (5=1) (7=3)
label variable airTravel "Reduced air travel"
label values airTravel actions_lb
tab airTravel, m

tab YPJ3032 YPJ3033

recode airTravel (0 2 = 0) (1 3 = 1), gen(airTravel_bin)
label variable airTravel_bin "Reduced air travel (binary)"
label values airTravel_bin actions_bin_lb
tab airTravel_bin, m

drop YPJ3032 YPJ3033 YPJ3034


* Electric/hybrid car
tab1 YPJ3035 YPJ3036 YPJ3037, m

replace YPJ3035 = . if YPJ3035 < 0
replace YPJ3036 = . if YPJ3036 < 0
replace YPJ3037 = . if YPJ3037 < 0

egen elecCar = group(YPJ3035 YPJ3036 YPJ3037), label
tab elecCar, m

recode elecCar (1 4 6 8 =.) (2=0) (3=2) (5=1) (7=3)
label variable elecCar "Electric/hybrid car"
label values elecCar actions_lb
tab elecCar, m

tab YPJ3035 YPJ3036

recode elecCar (0 2 = 0) (1 3 = 1), gen(elecCar_bin)
label variable elecCar_bin "Electric/hybrid car (binary)"
label values elecCar_bin actions_bin_lb
tab elecCar_bin, m

drop YPJ3035 YPJ3036 YPJ3037


* Bought local food
tab1 YPJ3038 YPJ3039 YPJ3040, m

replace YPJ3038 = . if YPJ3038 < 0
replace YPJ3039 = . if YPJ3039 < 0
replace YPJ3040 = . if YPJ3040 < 0

egen localFood = group(YPJ3038 YPJ3039 YPJ3040), label
tab localFood, m

recode localFood (1 4 6 8 =.) (2=0) (3=2) (5=1) (7=3)
label variable localFood "Bought local food"
label values localFood actions_lb
tab localFood, m

tab YPJ3038 YPJ3039 

recode localFood (0 2 = 0) (1 3 = 1), gen(localFood_bin)
label variable localFood_bin "Bought local food (binary)"
label values localFood_bin actions_bin_lb
tab localFood_bin, m

drop YPJ3038 YPJ3039 YPJ3040


* Recycled more
tab1 YPJ3041 YPJ3042 YPJ3043, m

replace YPJ3041 = . if YPJ3041 < 0
replace YPJ3042 = . if YPJ3042 < 0
replace YPJ3043 = . if YPJ3043 < 0

egen recycle = group(YPJ3041 YPJ3042 YPJ3043), label
tab recycle, m

recode recycle (1 4 6 8 =.) (2=0) (3=2) (5=1) (7=3)
label variable recycle "Recycled more"
label values recycle actions_lb
tab recycle, m

tab YPJ3041 YPJ3042

recode recycle (0 2 = 0) (1 3 = 1), gen(recycle_bin)
label variable recycle_bin "Recycled more (binary)"
label values recycle_bin actions_bin_lb
tab recycle_bin, m

drop YPJ3041 YPJ3042 YPJ3043


* Reduced plastic use
tab1 YPJ3044 YPJ3045 YPJ3046, m

replace YPJ3044 = . if YPJ3044 < 0
replace YPJ3045 = . if YPJ3045 < 0
replace YPJ3046 = . if YPJ3046 < 0

egen plastic = group(YPJ3044 YPJ3045 YPJ3046), label
tab plastic, m

recode plastic (1 4 6 8 =.) (2=0) (3=2) (5=1) (7=3)
label variable plastic "Reduced plastic use"
label values plastic actions_lb
tab plastic, m

tab YPJ3044 YPJ3045

recode plastic (0 2 = 0) (1 3 = 1), gen(plastic_bin)
label variable plastic_bin "Reduced plastic use (binary)"
label values plastic_bin actions_bin_lb
tab plastic_bin, m

drop YPJ3044 YPJ3045 YPJ3046


* Chosen sustainably sourced items
tab1 YPJ3047 YPJ3048 YPJ3049, m

replace YPJ3047 = . if YPJ3047 < 0
replace YPJ3048 = . if YPJ3048 < 0
replace YPJ3049 = . if YPJ3049 < 0

egen sustainable = group(YPJ3047 YPJ3048 YPJ3049), label
tab sustainable, m

recode sustainable (1 4 6 8 =.) (2=0) (3=2) (5=1) (7=3)
label variable sustainable "Chosen sustainably sourced items"
label values sustainable actions_lb
tab sustainable, m

tab YPJ3047 YPJ3048

recode sustainable (0 2 = 0) (1 3 = 1), gen(sustainable_bin)
label variable sustainable_bin "Chosen sustainably sourced items (binary)"
label values sustainable_bin actions_bin_lb
tab sustainable_bin, m

drop YPJ3047 YPJ3048 YPJ3049


* Improved home insulation
tab1 YPJ3050 YPJ3051 YPJ3052, m

replace YPJ3050 = . if YPJ3050 < 0
replace YPJ3051 = . if YPJ3051 < 0
replace YPJ3052 = . if YPJ3052 < 0

egen insulation = group(YPJ3050 YPJ3051 YPJ3052), label
tab insulation, m

recode insulation (1 4 6 8 =.) (2=0) (3=2) (5=1) (7=3)
label variable insulation "Improved home insulation"
label values insulation actions_lb
tab insulation, m

tab YPJ3050 YPJ3051

recode insulation (0 2 = 0) (1 3 = 1), gen(insulation_bin)
label variable insulation_bin "Improved home insulation (binary)"
label values insulation_bin actions_bin_lb
tab insulation_bin, m

drop YPJ3050 YPJ3051 YPJ3052


* Installed solar panels
tab1 YPJ3053 YPJ3054 YPJ3055, m

replace YPJ3053 = . if YPJ3053 < 0
replace YPJ3054 = . if YPJ3054 < 0
replace YPJ3055 = . if YPJ3055 < 0

egen solar = group(YPJ3053 YPJ3054 YPJ3055), label
tab solar, m

recode solar (1 5 7 8 =.) (2=0) (3=2) (4=1) (6=3)
label variable solar "Installed solar panels"
label values solar actions_lb
tab solar, m

tab YPJ3053 YPJ3054

recode solar (0 2 = 0) (1 3 = 1), gen(solar_bin)
label variable solar_bin "Installed solar panels (binary)"
label values solar_bin actions_bin_lb
tab solar_bin, m

drop YPJ3053 YPJ3054 YPJ3055


* Started growing vegetables
tab1 YPJ3056 YPJ3057 YPJ3058, m

replace YPJ3056 = . if YPJ3056 < 0
replace YPJ3057 = . if YPJ3057 < 0
replace YPJ3058 = . if YPJ3058 < 0

egen veg = group(YPJ3056 YPJ3057 YPJ3058), label
tab veg, m

recode veg (1 4 6 8 =.) (2=0) (3=2) (5=1) (7=3)
label variable veg "Started growing vegetables"
label values veg actions_lb
tab veg, m

tab YPJ3056 YPJ3057

recode veg (0 2 = 0) (1 3 = 1), gen(veg_bin)
label variable veg_bin "Started growing vegetables (binary)"
label values veg_bin actions_bin_lb
tab veg_bin, m

drop YPJ3056 YPJ3057 YPJ3058


* Planted trees
tab1 YPJ3059 YPJ3060 YPJ3061, m

replace YPJ3059 = . if YPJ3059 < 0
replace YPJ3060 = . if YPJ3060 < 0
replace YPJ3061 = . if YPJ3061 < 0

egen trees = group(YPJ3059 YPJ3060 YPJ3061), label
tab trees, m

recode trees (1 5 7 8 =.) (2=0) (3=2) (4=1) (6=3)
label variable trees "Planted trees"
label values trees actions_lb
tab trees, m

tab YPJ3059 YPJ3060

recode trees (0 2 = 0) (1 3 = 1), gen(trees_bin)
label variable trees_bin "Planted trees (binary)"
label values trees_bin actions_bin_lb
tab trees_bin, m

drop YPJ3059 YPJ3060 YPJ3061


* Avoided fossil fuel organisations
tab1 YPJ3062 YPJ3063 YPJ3064, m

replace YPJ3062 = . if YPJ3062 < 0
replace YPJ3063 = . if YPJ3063 < 0
replace YPJ3064 = . if YPJ3064 < 0

egen avoidFossil = group(YPJ3062 YPJ3063 YPJ3064), label
tab avoidFossil, m

recode avoidFossil (1 4 6 8 =.) (2=0) (3=2) (5=1) (7=3)
label variable avoidFossil "Avoided fossil fuel organisations"
label values avoidFossil actions_lb
tab avoidFossil, m

tab YPJ3062 YPJ3063

recode avoidFossil (0 2 = 0) (1 3 = 1), gen(avoidFossil_bin)
label variable avoidFossil_bin "Avoided fossil fuel organisations (binary)"
label values avoidFossil_bin actions_bin_lb
tab avoidFossil_bin, m

drop YPJ3062 YPJ3063 YPJ3064


* Planned fewer children
tab1 YPJ3065 YPJ3066 YPJ3067, m

replace YPJ3065 = . if YPJ3065 < 0
replace YPJ3066 = . if YPJ3066 < 0
replace YPJ3067 = . if YPJ3067 < 0

egen children = group(YPJ3065 YPJ3066 YPJ3067), label
tab children, m

recode children (1 4 6 8 =.) (2=0) (3=2) (5=1) (7=3)
label variable children "Planned fewer children"
label values children actions_lb
tab children, m

tab YPJ3065 YPJ3066

recode children (0 2 = 0) (1 3 = 1), gen(children_bin)
label variable children_bin "Planned fewer children (binary)"
label values children_bin actions_bin_lb
tab children_bin, m

drop YPJ3065 YPJ3066 YPJ3067


* Taken other climate action
tab1 YPJ3068 YPJ3069 YPJ3070, m

replace YPJ3068 = . if YPJ3068 < 0
replace YPJ3069 = . if YPJ3069 < 0
replace YPJ3070 = . if YPJ3070 < 0

egen otherAction = group(YPJ3068 YPJ3069 YPJ3070), label
tab otherAction, m

recode otherAction (1 5 7 8 =.) (2=0) (3=2) (4=1) (6=3)
label variable otherAction "Taken other climate action"
label values otherAction actions_lb
tab otherAction, m

tab YPJ3068 YPJ3069

recode otherAction (0 2 = 0) (1 3 = 1), gen(otherAction_bin)
label variable otherAction_bin "Taken other climate action (binary)"
label values otherAction_bin actions_bin_lb
tab otherAction_bin, m

drop YPJ3068 YPJ3069 YPJ3070


* Reduced meat/dairy consumption
tab1 YPJ3071 YPJ3072 YPJ3073, m

replace YPJ3071 = . if YPJ3071 < 0
replace YPJ3072 = . if YPJ3072 < 0
replace YPJ3073 = . if YPJ3073 < 0

egen meatDairy = group(YPJ3071 YPJ3072 YPJ3073), label
tab meatDairy, m

recode meatDairy (1 4 7 8 =.) (2=0) (3=2) (5=1) (6=3)
label variable meatDairy "Reduced meat/dairy consumption"
label values meatDairy actions_lb
tab meatDairy, m

tab YPJ3071 YPJ3072

* Check answers if vegan/vegatarian - This is complicated, as some vegetarians/vegans answered this question, while others did not (vegetarians should also not be a separate category as they can still reduce dairy consumption, but hey ho). Will exclude answers from those who said 'always vegan' (as should not consume any meat or dairy products), but keep answers from those who said 'always vegetarian' 
tab1 YPJ3074 YPJ3075, m

tab meatDairy if YPJ3074 == 1 | YPJ3075 == 1
tab1 YPJ3074 YPJ3075 if meatDairy != .

replace meatDairy = . if YPJ3075 == 1
tab meatDairy, m
drop YPJ3074 YPJ3075

recode meatDairy (0 2 = 0) (1 3 = 1), gen(meatDairy_bin)
label variable meatDairy_bin "Reduced meat/dairy consumption (binary)"
label values meatDairy_bin actions_bin_lb
tab meatDairy_bin, m

drop YPJ3071 YPJ3072 YPJ3073


** Calculate total number of actions taken for climate reasons (will exclude 'other' though)
egen total_actions = rowtotal(travel_bin waste_bin energy_bin buy_bin airTravel_bin elecCar_bin localFood_bin recycle_bin plastic_bin sustainable_bin insulation_bin solar_bin veg_bin trees_bin avoidFossil_bin children_bin meatDairy_bin), missing

tab total_actions, m
sum total_actions

* Code as missing if any questions missing
egen total_miss = rowmiss(travel_bin waste_bin energy_bin buy_bin airTravel_bin elecCar_bin localFood_bin recycle_bin plastic_bin sustainable_bin insulation_bin solar_bin veg_bin trees_bin avoidFossil_bin children_bin meatDairy_bin)
tab total_miss, m

replace total_actions = . if total_miss > 0
tab total_actions, m
sum total_actions

drop total_miss


*****************************************************************************
***** Descriptive statistics

*** Compare descriptive stats in full sample vs sample with complete data (using just those who answered the first climate change question)
egen cca_marker = rowmiss(sex-climateChanging)
tab cca_marker

replace cca_marker = 1 if cca_marker > 1
recode cca_marker (1 = 0) (0 = 1)
tab cca_marker


** Confounders
missings report sex ageAtBirth home edu imd ethnic offspringAge, percent

* Child sex
tab sex
tab sex, m

tab sex if cca_marker == 1

* Mother age at birth
sum ageAtBirth

sum ageAtBirth if cca_marker == 1

* Mother home ownership status
tab home
tab home, m

tab home if cca_marker == 1

* Mother educational attainment
tab edu
tab edu, m

tab edu if cca_marker == 1

* IMD in pregnancy
tab imd
tab imd, m

tab imd if cca_marker == 1

* Child ethnicity
tab ethnic
tab ethnic, m

tab ethnic if cca_marker == 1

* Offspring age
tab offspringAge
tab offspringAge, m

tab offspringAge if cca_marker == 1

sum offspringAge
sum offspringAge if cca_marker == 1


** Exposures
missings report extra-iq, percent

foreach var of varlist extra-iq {
	sum `var'
	sum `var' if cca_marker == 1
}


** Outcomes
missings report climateChanging-meatDairy_bin, percent

egen cca_marker2 = rowmiss(sex-iq)
tab cca_marker2

replace cca_marker2 = 1 if cca_marker2 > 1
recode cca_marker2 (1 = 0) (0 = 1)
tab cca_marker2

* All categorical outcomes
foreach var of varlist climateChanging-meatDairy_bin {
	tab `var'
	tab `var', m
	
	tab `var' if cca_marker2 == 1 & `var' != .
}

* Number of climate items
missings report total_actions, percent

sum total_actions
sum total_actions if cca_marker == 1 & total_actions != .

tab total_actions
tab total_actions if cca_marker == 1 & total_actions != .

drop cca_marker2


** Make a complete-case marker for the confounder variables so the adjusted and unadjusted models have the same sample size
egen cca_confounders = rowmiss(sex-offspringAge)
tab cca_confounders

replace cca_confounders = 1 if cca_confounders > 1
recode cca_confounders (1 = 0) (0 = 1)
tab cca_confounders


** Save dataset to load in if needed
save "B4293_ReligionAndClimate_Processed.dta", replace


*** Trying to make some nice graphs

** First histogram of total number of climate actions
hist total_actions, freq discrete ///
	xlabel(0(3)18) xtitle("Total number of actions taken due to climate concern")
	
graph export ".\Results\totalClimateActions_hist.pdf", replace
	

** Next a bar chart for binary actions
keep aln qlet *_bin

order aln qlet recycle_bin plastic_bin waste_bin sustainable_bin buy_bin energy_bin meatDairy_bin localFood_bin travel_bin avoidFossil_bin airTravel_bin veg_bin trees_bin insulation_bin children_bin elecCar_bin otherAction_bin solar_bin

numlabel, remove

local i = 1
foreach var of varlist recycle_bin-solar_bin {
	rename `var' graph_`i'
	local i = `i' + 1
}

reshape long graph_, i(aln qlet) j(question)

label define q 1 "Recycled more" 2 "Reduced plastic use" 3 "Reduced household waste" ///
	4 "Chosen sustainable items" 5 "Changed what buy" 6 "Reduced energy use" ///
	7 "Reduced meat/dairy consumption" 8 "Bought local food" ///
	9 "Changed how travel locally" 10 "Avoided fossil fuel orgs." ///
	11 "Reduced air travel" 12 "Started growing vegetables" 13 "Planted trees" ///
	14 "Improved home insulation" 15 "Planned fewer children" ///
	16 "Used hybrid/electric car" 17 "Other action" 18 "Installed solar panels"
label values question q

graph hbar (count), over(graph_) over(question) stack asyvars percent ytitle("Percent")

graph export ".\Results\climateActions_binary.pdf", replace


** Next for categorical actions
use "B4293_ReligionAndClimate_Processed.dta", clear

keep aln qlet travel waste energy buy airTravel elecCar localFood recycle plastic sustainable insulation solar veg trees avoidFossil children meatDairy otherAction

order aln qlet recycle plastic waste sustainable buy energy meatDairy localFood travel avoidFossil airTravel veg trees insulation children elecCar otherAction solar

numlabel, remove

local i = 1
foreach var of varlist recycle-solar {
	rename `var' graph_`i'
	local i = `i' + 1
}

reshape long graph_, i(aln qlet) j(question)

label define q 1 "Recycled more" 2 "Reduced plastic use" 3 "Reduced household waste" ///
	4 "Chosen sustainable items" 5 "Changed what buy" 6 "Reduced energy use" ///
	7 "Reduced meat/dairy consumption" 8 "Bought local food" ///
	9 "Changed how travel locally" 10 "Avoided fossil fuel orgs." ///
	11 "Reduced air travel" 12 "Started growing vegetables" 13 "Planted trees" ///
	14 "Improved home insulation" 15 "Planned fewer children" ///
	16 "Used hybrid/electric car" 17 "Other action" 18 "Installed solar panels"
label values question q

graph hbar (count), over(graph_) over(question, label(labsize(small))) ///
	stack asyvars percent ///
	ytitle("Percent") ///
	legend(rows(1) position(6) size(small))
	
graph export ".\Results\climateActions.pdf", replace


*******************************************************************************
***** Data analysis

use "B4293_ReligionAndClimate_Processed.dta", clear


**** Believe the climate is changing

*** Extraversion 

* Ordinal model unadjusted
ologit climateChanging extra if cca_confounders == 1, or

* Check brant test - No violation
brant, detail

* Ordinal model adjusted
ologit climateChanging extra i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, or

* Predicted probabilities
sum extra if cca_confounder == 1 & climateChanging != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(extra = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
ologit climateChanging extra i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, or

margins, at(extra = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/5

clear 
set obs `n'
egen iq = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .
gen p4 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
	replace p4 = res[1,`n' + `n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 iq, col(black)) ///
	(line p1 iq, col(red)) ///
	(line p2 iq, col(blue)) ///
	(line p3 iq, col(green)) ///
	(line p4 iq, col(orange)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Extraversion", size(medium)) ///
	legend(order(1 "Definitely not" 2 "Probably not" 3 "Yes, maybe" ///
		4 "Yes, probably" 5 "Yes, definitely") rows(1) size(small) symxsize(*0.5)) ///
	name(extra_change, replace)

	
use "B4293_ReligionAndClimate_Processed.dta", clear


*** Agreeableness 

* Ordinal model unadjusted
ologit climateChanging agree if cca_confounders == 1, or

* Check brant test - No violation
brant, detail

* Ordinal model adjusted
ologit climateChanging agree i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, or

* Predicted probabilities
sum agree if cca_confounder == 1 & climateChanging != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(agree = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
ologit climateChanging agree i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, or

margins, at(agree = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/5

clear 
set obs `n'
egen agree = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .
gen p4 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
	replace p4 = res[1,`n' + `n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 agree, col(black)) ///
	(line p1 agree, col(red)) ///
	(line p2 agree, col(blue)) ///
	(line p3 agree, col(green)) ///
	(line p4 agree, col(orange)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Agreeableness", size(medium)) ///
	legend(order(1 "Definitely not" 2 "Probably not" 3 "Yes, maybe" ///
		4 "Yes, probably" 5 "Yes, definitely") rows(1) size(small) symxsize(*0.5)) ///
	name(agree_change, replace)

	
use "B4293_ReligionAndClimate_Processed.dta", clear


*** Conscientiousness

* Ordinal model unadjusted
ologit climateChanging consc if cca_confounders == 1, or

* Check brant test - Some violation
brant, detail

* Ordinal model adjusted
ologit climateChanging consc i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, or


** Proportional odds assumption appears violated, so will run multinomial model to check results
mlogit climateChanging consc if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit climateChanging if cca_confounders == 1 & consc != ., rrr baseoutcome(0)
est store base 

mlogit climateChanging consc if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit climateChanging i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if consc != ., rrr baseoutcome(0)
est store base 

mlogit climateChanging consc i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum consc if cca_confounder == 1 & climateChanging != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(consc = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
margins, at(consc = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/5

clear 
set obs `n'
egen consc = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .
gen p4 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
	replace p4 = res[1,`n' + `n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 consc, col(black)) ///
	(line p1 consc, col(red)) ///
	(line p2 consc, col(blue)) ///
	(line p3 consc, col(green)) ///
	(line p4 consc, col(orange)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Conscientiousness *", size(medium)) ///
	legend(order(1 "Definitely not" 2 "Probably not" 3 "Yes, maybe" ///
		4 "Yes, probably" 5 "Yes, definitely") rows(1) size(small) symxsize(*0.5)) ///
	name(consc_change_mult, replace)
	

use "B4293_ReligionAndClimate_Processed.dta", clear


*** Emotional stability

* Ordinal model unadjusted
ologit climateChanging emoStab if cca_confounders == 1, or

* Check brant test - No violation
brant, detail

* Ordinal model adjusted
ologit climateChanging emoStab i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, or

* Predicted probabilities
sum emoStab if cca_confounder == 1 & climateChanging != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(emoStab = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
ologit climateChanging emoStab i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, or

margins, at(emoStab = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/5

clear 
set obs `n'
egen emoStab = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .
gen p4 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
	replace p4 = res[1,`n' + `n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 emoStab, col(black)) ///
	(line p1 emoStab, col(red)) ///
	(line p2 emoStab, col(blue)) ///
	(line p3 emoStab, col(green)) ///
	(line p4 emoStab, col(orange)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Emotional Stability", size(medium)) ///
	legend(order(1 "Definitely not" 2 "Probably not" 3 "Yes, maybe" ///
		4 "Yes, probably" 5 "Yes, definitely") rows(1) size(small) symxsize(*0.5)) ///
	name(emoStab_change, replace)
	

use "B4293_ReligionAndClimate_Processed.dta", clear


*** Openness to experience

* Ordinal model unadjusted
ologit climateChanging open if cca_confounders == 1, or

* Check brant test - No violation
brant, detail

* Ordinal model adjusted
ologit climateChanging open i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, or

* Predicted probabilities
sum open if cca_confounder == 1 & climateChanging != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(open = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
ologit climateChanging open i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, or

margins, at(open = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/5

clear 
set obs `n'
egen open = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .
gen p4 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
	replace p4 = res[1,`n' + `n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 open, col(black)) ///
	(line p1 open, col(red)) ///
	(line p2 open, col(blue)) ///
	(line p3 open, col(green)) ///
	(line p4 open, col(orange)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Openness to Experience", size(medium)) ///
	legend(order(1 "Definitely not" 2 "Probably not" 3 "Yes, maybe" ///
		4 "Yes, probably" 5 "Yes, definitely") rows(1) size(small) symxsize(*0.5)) ///
	name(open_change, replace)
	
	
use "B4293_ReligionAndClimate_Processed.dta", clear


*** IQ

* Ordinal model unadjusted
ologit climateChanging iq if cca_confounders == 1, or

* Check brant test - Potential violation
brant, detail

* Ordinal model adjusted
ologit climateChanging iq i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, or

* Predicted probabilities
sum iq if cca_confounder == 1 & climateChanging != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(iq = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
ologit climateChanging iq i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, or

margins, at(iq = (70(1)130))

matrix res = r(table)
matrix list res

local n = colsof(res)/5

clear 
set obs `n'
egen iq = fill(70 71)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .
gen p4 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
	replace p4 = res[1,`n' + `n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 iq, col(black)) ///
	(line p1 iq, col(red)) ///
	(line p2 iq, col(blue)) ///
	(line p3 iq, col(green)) ///
	(line p4 iq, col(orange)), ///
	xscale(range(65 135)) xlabel(70(10)130, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("IQ scores", size(medium)) ///
	legend(order(1 "Definitely not" 2 "Probably not" 3 "Yes, maybe" ///
		4 "Yes, probably" 5 "Yes, definitely") rows(1) size(small) symxsize(*0.5)) ///
	name(iq_change, replace)
	

use "B4293_ReligionAndClimate_Processed.dta", clear


** As proportional odds assumption slightly violated, will check results in multinomial model
mlogit climateChanging iq if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit climateChanging if cca_confounders == 1 & iq != ., rrr baseoutcome(0)
est store base 

mlogit climateChanging iq if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit climateChanging i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if iq != ., rrr baseoutcome(0)
est store base 

mlogit climateChanging iq i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum iq if cca_confounder == 1 & climateChanging != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(iq = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
margins, at(iq = (70(1)130))

matrix res = r(table)
matrix list res

local n = colsof(res)/5

clear 
set obs `n'
egen iq = fill(70 71)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .
gen p4 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
	replace p4 = res[1,`n' + `n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 iq, col(black)) ///
	(line p1 iq, col(red)) ///
	(line p2 iq, col(blue)) ///
	(line p3 iq, col(green)) ///
	(line p4 iq, col(orange)), ///
	xscale(range(65 135)) xlabel(70(10)130, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("IQ scores *", size(medium)) ///
	legend(order(1 "Definitely not" 2 "Probably not" 3 "Yes, maybe" ///
		4 "Yes, probably" 5 "Yes, definitely") rows(1) size(small) symxsize(*0.5)) ///
	name(iq_change_mult, replace)
	

use "B4293_ReligionAndClimate_Processed.dta", clear



** Combine graphs together with a single legend using the 'grc1leg' user-written package
grc1leg extra_change agree_change consc_change_mult emoStab_change open_change iq_change_mult, rows(2) imargin(tiny) ycommon

graph export ".\Results\ClimateChanging_predProbs.pdf", replace

graph close _all


*** Creating various post-files to store results to (ordinal results and pseudo-r2, individual multinomial RRRs, and overall multinomial associations and r2)

capture postclose climateChanging_ordinal
postfile climateChanging_ordinal str30 exposure str30 outcome /// 
	n coef_uni lci_uni uci_uni double(p_uni) r2_uni propOdds ///
	coef_mult lci_mult uci_mult double(p_mult) r2_mult ///
	using ".\Results\climateChanging_ordinal.dta", replace

foreach var of varlist extra agree consc emoStab open iq {
	
	* Store exposure variable as a macro
	local exp = "`var'"
	
	* Store outcome as macro
	local out = "climateChanging"
	
	** Univariable analysis
	ologit climateChanging `var' if cca_confounders == 1, or
	
	* Store relevant statistics
	local n = e(N)
	local r2_uni = e(r2_p)
	
	matrix res = r(table)
	local coef_uni = res[1,1]
	local lci_uni = res[5,1]	
	local uci_uni = res[6,1]
	local p_uni = res[4,1]
	
	* Brant test
	brant
	local propOdds = r(p)
		
		
	** Multivariable analysis
	ologit climateChanging `var' i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, or
	
	* Store relevant statistics
	local r2_mult_main = e(r2_p)
	
	matrix res = r(table)
	local coef_mult = res[1,1]
	local lci_mult = res[5,1]	
	local uci_mult = res[6,1]
	local p_mult = res[4,1]
	
	* Improvement in pseudo-r2 value at inclusion of exposure
	ologit climateChanging i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if `var' != ., or
	
	local r2_mult = `r2_mult_main' - e(r2_p)
	
	** Post these results to the file
	post climateChanging_ordinal ("`exp'") ("`out'") (`n') (`coef_uni') (`lci_uni') ///
		(`uci_uni') (`p_uni') (`r2_uni') (`propOdds') (`coef_mult') (`lci_mult') ///
		(`uci_mult') (`p_mult') (`r2_mult')
	
}

postclose climateChanging_ordinal

* Check results
use ".\Results\climateChanging_ordinal.dta", clear

export delimited using ".\Results\climateChanging_ordinal.csv", replace


** Read back in original dataset and repeat for multinomial results
use "B4293_ReligionAndClimate_Processed.dta", clear


*** Multinomial models
capture postclose climateChanging_mult
postfile climateChanging_mult str30 exposure str30 outcome str30 out_level /// 
	n coef_uni lci_uni uci_uni double(p_uni) ///
	coef_mult lci_mult uci_mult double(p_mult) ///
	using ".\Results\climateChanging_mult.dta", replace
	
capture postclose climateChanging_mult_lr
postfile climateChanging_mult_lr str30 exposure str30 outcome /// 
	n double(p_uni) r2_uni double(p_mult) r2_mult ///
	using ".\Results\climateChanging_mult_lr.dta", replace

foreach var of varlist extra agree consc emoStab open iq {
	
	* Store exposure variable as a macro
	local exp = "`var'"
	
	* Store outcome as macro
	local out = "climateChanging"
	
	** Univariable analysis
	mlogit climateChanging `var' if cca_confounders == 1, baseoutcome(0) rrr
	
	* Store relevant summary statistics
	local n = e(N)
	local r2_uni = e(r2_p)
	local p_uni = e(p)
	
	* Store coefficients
	matrix res_uni = r(table)
	
	** Multivariable analysis - Repeat with and without exposure to get change in pseudo-R2 and LR test results
	mlogit climateChanging i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if `var' != ., baseoutcome(0) rrr
	local r2_mult_base = e(r2_p)
	est store base
	
	mlogit climateChanging `var' i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, baseoutcome(0) rrr
	local r2_mult = e(r2_p) - `r2_mult_base'
	est store main
	
	* Store coefficients
	matrix res_mult = r(table)
	
	* LR test
	lrtest base main
	local p_mult = r(p)
	
	** Post these summary results to the file
	post climateChanging_mult_lr ("`exp'") ("`out'") (`n') (`p_uni') (`r2_uni') ///
		(`p_mult') (`r2_mult')
		
	
	** Now focus on coefficient results
	
	* Outcome value = 1/Probably not
	local out_level = "Probably not"
	local coef_uni = res_uni[1,3]
	local lci_uni = res_uni[5,3]	
	local uci_uni = res_uni[6,3]
	local p_uni = res_uni[4,3]
	
	local coef_mult = res_mult[1,23]
	local lci_mult = res_mult[5,23]	
	local uci_mult = res_mult[6,23]
	local p_mult = res_mult[4,23]
	
	post climateChanging_mult ("`exp'") ("`out'") ("`out_level'") (`n') /// 
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`coef_mult') ///
		(`lci_mult') (`uci_mult') (`p_mult')
	
	* Outcome value = 2/Yes maybe
	local out_level = "Yes maybe"
	local coef_uni = res_uni[1,5]
	local lci_uni = res_uni[5,5]	
	local uci_uni = res_uni[6,5]
	local p_uni = res_uni[4,5]
	
	local coef_mult = res_mult[1,45]
	local lci_mult = res_mult[5,45]	
	local uci_mult = res_mult[6,45]
	local p_mult = res_mult[4,45]
	
	post climateChanging_mult ("`exp'") ("`out'") ("`out_level'") (`n') /// 
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`coef_mult') ///
		(`lci_mult') (`uci_mult') (`p_mult')
	
	* Outcome value = 3/Yes probably
	local out_level = "Yes probably"
	local coef_uni = res_uni[1,7]
	local lci_uni = res_uni[5,7]	
	local uci_uni = res_uni[6,7]
	local p_uni = res_uni[4,7]
	
	local coef_mult = res_mult[1,67]
	local lci_mult = res_mult[5,67]	
	local uci_mult = res_mult[6,67]
	local p_mult = res_mult[4,67]
	
	post climateChanging_mult ("`exp'") ("`out'") ("`out_level'") (`n') /// 
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`coef_mult') ///
		(`lci_mult') (`uci_mult') (`p_mult')
	
	* Outcome value = 4/Yes definitely
	local out_level = "Yes definitely"
	local coef_uni = res_uni[1,9]
	local lci_uni = res_uni[5,9]	
	local uci_uni = res_uni[6,9]
	local p_uni = res_uni[4,9]
	
	local coef_mult = res_mult[1,89]
	local lci_mult = res_mult[5,89]	
	local uci_mult = res_mult[6,89]
	local p_mult = res_mult[4,89]
	
	post climateChanging_mult ("`exp'") ("`out'") ("`out_level'") (`n') /// 
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`coef_mult') ///
		(`lci_mult') (`uci_mult') (`p_mult')
	
}

postclose climateChanging_mult
postclose climateChanging_mult_lr

* Check results
use ".\Results\climateChanging_mult_lr.dta", clear

export delimited using ".\Results\climateChanging_mult_lr.csv", replace


use ".\Results\climateChanging_mult.dta", clear

export delimited using ".\Results\climateChanging_mult.csv", replace


** Read back in original dataset
use "B4293_ReligionAndClimate_Processed.dta", clear


***************************************************************************************
**** Concenred about impact of climate change

*** Extraversion 

* Ordinal model unadjusted
ologit climateConcern extra if cca_confounders == 1, or

* Check brant test - No violation
brant, detail

* Ordinal model adjusted
ologit climateConcern extra i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, or

* Predicted probabilities
sum extra if cca_confounder == 1 & climateConcern != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(extra = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
ologit climateConcern extra i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, or

margins, at(extra = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen iq = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 iq, col(black)) ///
	(line p1 iq, col(red)) ///
	(line p2 iq, col(blue)) ///
	(line p3 iq, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Extraversion", size(medium)) ///
	legend(order(1 "Not at all concerned" 2 "Not very concerned" 3 "Somewhat concerned" ///
		4 "Very concerned") rows(1) size(small) symxsize(*0.5)) ///
	name(extra_concern, replace)

	
use "B4293_ReligionAndClimate_Processed.dta", clear


*** Agreeableness 

* Ordinal model unadjusted
ologit climateConcern agree if cca_confounders == 1, or

* Check brant test - No violation
brant, detail

* Ordinal model adjusted
ologit climateConcern agree i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, or

* Predicted probabilities
sum agree if cca_confounder == 1 & climateConcern != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(agree = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
ologit climateConcern agree i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, or

margins, at(agree = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen agree = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 agree, col(black)) ///
	(line p1 agree, col(red)) ///
	(line p2 agree, col(blue)) ///
	(line p3 agree, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Agreeableness", size(medium)) ///
	legend(order(1 "Not at all concerned" 2 "Not very concerned" 3 "Somewhat concerned" ///
		4 "Very concerned") rows(1) size(small) symxsize(*0.5)) ///
	name(agree_concern, replace)

	
use "B4293_ReligionAndClimate_Processed.dta", clear


*** Conscientiousness

* Ordinal model unadjusted
ologit climateConcern consc if cca_confounders == 1, or

* Check brant test - Some violation
brant, detail

* Ordinal model adjusted
ologit climateConcern consc i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, or


** Proportional odds assumption appears violated, so will run multinomial model to check results
mlogit climateConcern consc if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit climateConcern if cca_confounders == 1 & consc != ., rrr baseoutcome(0)
est store base 

mlogit climateConcern consc if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit climateConcern i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if consc != ., rrr baseoutcome(0)
est store base 

mlogit climateConcern consc i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum consc if cca_confounder == 1 & climateConcern != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(consc = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
margins, at(consc = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen consc = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 consc, col(black)) ///
	(line p1 consc, col(red)) ///
	(line p2 consc, col(blue)) ///
	(line p3 consc, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Conscientiousness *", size(medium)) ///
	legend(order(1 "Not at all concerned" 2 "Not very concerned" 3 "Somewhat concerned" ///
		4 "Very concerned") rows(1) size(small) symxsize(*0.5)) ///
	name(consc_concern_mult, replace)
	

use "B4293_ReligionAndClimate_Processed.dta", clear


*** Emotional stability

* Ordinal model unadjusted
ologit climateConcern emoStab if cca_confounders == 1, or

* Check brant test - No violation
brant, detail

* Ordinal model adjusted
ologit climateConcern emoStab i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, or

* Predicted probabilities
sum emoStab if cca_confounder == 1 & climateConcern != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(emoStab = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
ologit climateConcern emoStab i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, or

margins, at(emoStab = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen emoStab = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 emoStab, col(black)) ///
	(line p1 emoStab, col(red)) ///
	(line p2 emoStab, col(blue)) ///
	(line p3 emoStab, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Emotional Stability", size(medium)) ///
	legend(order(1 "Not at all concerned" 2 "Not very concerned" 3 "Somewhat concerned" ///
		4 "Very concerned") rows(1) size(small) symxsize(*0.5)) ///
	name(emoStab_concern, replace)
	

use "B4293_ReligionAndClimate_Processed.dta", clear


*** Openness to experience

* Ordinal model unadjusted
ologit climateConcern open if cca_confounders == 1, or

* Check brant test - No violation
brant, detail

* Ordinal model adjusted
ologit climateConcern open i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, or

* Predicted probabilities
sum open if cca_confounder == 1 & climateConcern != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(open = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
ologit climateConcern open i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, or

margins, at(open = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen open = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 open, col(black)) ///
	(line p1 open, col(red)) ///
	(line p2 open, col(blue)) ///
	(line p3 open, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Openness to Experience", size(medium)) ///
	legend(order(1 "Not at all concerned" 2 "Not very concerned" 3 "Somewhat concerned" ///
		4 "Very concerned") rows(1) size(small) symxsize(*0.5)) ///
	name(open_concern, replace)
	
	
use "B4293_ReligionAndClimate_Processed.dta", clear


*** IQ

* Ordinal model unadjusted
ologit climateConcern iq if cca_confounders == 1, or

* Check brant test - No violation
brant, detail

* Ordinal model adjusted
ologit climateConcern iq i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, or

* Predicted probabilities
sum iq if cca_confounder == 1 & climateConcern != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(iq = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
ologit climateConcern iq i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, or

margins, at(iq = (70(1)130))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen iq = fill(70 71)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 iq, col(black)) ///
	(line p1 iq, col(red)) ///
	(line p2 iq, col(blue)) ///
	(line p3 iq, col(green)), ///
	xscale(range(65 135)) xlabel(70(10)130, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("IQ scores", size(medium)) ///
	legend(order(1 "Not at all concerned" 2 "Not very concerned" 3 "Somewhat concerned" ///
		4 "Very concerned") rows(1) size(small) symxsize(*0.5)) ///
	name(iq_concern, replace)
	

use "B4293_ReligionAndClimate_Processed.dta", clear



** Combine graphs together with a single legend using the 'grc1leg' user-written package
grc1leg extra_concern agree_concern consc_concern_mult emoStab_concern open_concern iq_concern, rows(2) imargin(tiny) ycommon

graph export ".\Results\ClimateConcern_predProbs.pdf", replace

graph close _all


*** Creating various post-files to store results to (ordinal results and pseudo-r2, individual multinomial RRRs, and overall multinomial associations and r2)

capture postclose climateConcern_ordinal
postfile climateConcern_ordinal str30 exposure str30 outcome /// 
	n coef_uni lci_uni uci_uni double(p_uni) r2_uni propOdds ///
	coef_mult lci_mult uci_mult double(p_mult) r2_mult ///
	using ".\Results\climateConcern_ordinal.dta", replace

foreach var of varlist extra agree consc emoStab open iq {
	
	* Store exposure variable as a macro
	local exp = "`var'"
	
	* Store outcome as macro
	local out = "climateConcern"
	
	** Univariable analysis
	ologit climateConcern `var' if cca_confounders == 1, or
	
	* Store relevant statistics
	local n = e(N)
	local r2_uni = e(r2_p)
	
	matrix res = r(table)
	local coef_uni = res[1,1]
	local lci_uni = res[5,1]	
	local uci_uni = res[6,1]
	local p_uni = res[4,1]
	
	* Brant test
	brant
	local propOdds = r(p)
		
		
	** Multivariable analysis
	ologit climateConcern `var' i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, or
	
	* Store relevant statistics
	local r2_mult_main = e(r2_p)
	
	matrix res = r(table)
	local coef_mult = res[1,1]
	local lci_mult = res[5,1]	
	local uci_mult = res[6,1]
	local p_mult = res[4,1]
	
	* Improvement in pseudo-r2 value at inclusion of exposure
	ologit climateConcern i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if `var' != ., or
	
	local r2_mult = `r2_mult_main' - e(r2_p)
	
	** Post these results to the file
	post climateConcern_ordinal ("`exp'") ("`out'") (`n') (`coef_uni') (`lci_uni') ///
		(`uci_uni') (`p_uni') (`r2_uni') (`propOdds') (`coef_mult') (`lci_mult') ///
		(`uci_mult') (`p_mult') (`r2_mult')
	
}

postclose climateConcern_ordinal

* Check results
use ".\Results\climateConcern_ordinal.dta", clear

export delimited using ".\Results\climateConcern_ordinal.csv", replace


** Read back in original dataset and repeat for multinomial results
use "B4293_ReligionAndClimate_Processed.dta", clear


*** Multinomial models
capture postclose climateConcern_mult
postfile climateConcern_mult str30 exposure str30 outcome str30 out_level /// 
	n coef_uni lci_uni uci_uni double(p_uni) ///
	coef_mult lci_mult uci_mult double(p_mult) ///
	using ".\Results\climateConcern_mult.dta", replace
	
capture postclose climateConcern_mult_lr
postfile climateConcern_mult_lr str30 exposure str30 outcome /// 
	n double(p_uni) r2_uni double(p_mult) r2_mult ///
	using ".\Results\climateConcern_mult_lr.dta", replace

foreach var of varlist extra agree consc emoStab open iq {
	
	* Store exposure variable as a macro
	local exp = "`var'"
	
	* Store outcome as macro
	local out = "climateConcern"
	
	** Univariable analysis
	mlogit climateConcern `var' if cca_confounders == 1, baseoutcome(0) rrr
	
	* Store relevant summary statistics
	local n = e(N)
	local r2_uni = e(r2_p)
	local p_uni = e(p)
	
	* Store coefficients
	matrix res_uni = r(table)
	
	** Multivariable analysis - Repeat with and without exposure to get change in pseudo-R2 and LR test results
	mlogit climateConcern i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if `var' != ., baseoutcome(0) rrr
	local r2_mult_base = e(r2_p)
	est store base
	
	mlogit climateConcern `var' i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, baseoutcome(0) rrr
	local r2_mult = e(r2_p) - `r2_mult_base'
	est store main
	
	* Store coefficients
	matrix res_mult = r(table)
	
	* LR test
	lrtest base main
	local p_mult = r(p)
	
	** Post these summary results to the file
	post climateConcern_mult_lr ("`exp'") ("`out'") (`n') (`p_uni') (`r2_uni') ///
		(`p_mult') (`r2_mult')
		
	
	** Now focus on coefficient results
	
	* Outcome value = 1/Not very
	local out_level = "Not very"
	local coef_uni = res_uni[1,3]
	local lci_uni = res_uni[5,3]	
	local uci_uni = res_uni[6,3]
	local p_uni = res_uni[4,3]
	
	local coef_mult = res_mult[1,23]
	local lci_mult = res_mult[5,23]	
	local uci_mult = res_mult[6,23]
	local p_mult = res_mult[4,23]
	
	post climateConcern_mult ("`exp'") ("`out'") ("`out_level'") (`n') /// 
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`coef_mult') ///
		(`lci_mult') (`uci_mult') (`p_mult')
	
	* Outcome value = 2/Somewhat
	local out_level = "Somewhat"
	local coef_uni = res_uni[1,5]
	local lci_uni = res_uni[5,5]	
	local uci_uni = res_uni[6,5]
	local p_uni = res_uni[4,5]
	
	local coef_mult = res_mult[1,45]
	local lci_mult = res_mult[5,45]	
	local uci_mult = res_mult[6,45]
	local p_mult = res_mult[4,45]
	
	post climateConcern_mult ("`exp'") ("`out'") ("`out_level'") (`n') /// 
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`coef_mult') ///
		(`lci_mult') (`uci_mult') (`p_mult')
	
	* Outcome value = 3/Very
	local out_level = "Very"
	local coef_uni = res_uni[1,7]
	local lci_uni = res_uni[5,7]	
	local uci_uni = res_uni[6,7]
	local p_uni = res_uni[4,7]
	
	local coef_mult = res_mult[1,67]
	local lci_mult = res_mult[5,67]	
	local uci_mult = res_mult[6,67]
	local p_mult = res_mult[4,67]
	
	post climateConcern_mult ("`exp'") ("`out'") ("`out_level'") (`n') /// 
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`coef_mult') ///
		(`lci_mult') (`uci_mult') (`p_mult')
	
}

postclose climateConcern_mult
postclose climateConcern_mult_lr

* Check results
use ".\Results\climateConcern_mult_lr.dta", clear

export delimited using ".\Results\climateConcern_mult_lr.csv", replace


use ".\Results\climateConcern_mult.dta", clear

export delimited using ".\Results\climateConcern_mult.csv", replace


** Read back in original dataset
use "B4293_ReligionAndClimate_Processed.dta", clear



********************************************************************************
**** Believes humans are to blame for climate change

*** Extraversion 

* Ordinal model unadjusted
ologit climateHumans extra if cca_confounders == 1, or

* Check brant test - No violation
brant, detail

* Ordinal model adjusted
ologit climateHumans extra i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, or

* Predicted probabilities
sum extra if cca_confounder == 1 & climateHumans != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(extra = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
ologit climateHumans extra i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, or

margins, at(extra = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen iq = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 iq, col(black)) ///
	(line p1 iq, col(red)) ///
	(line p2 iq, col(blue)) ///
	(line p3 iq, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Extraversion", size(medium)) ///
	legend(order(1 "Not at all" 2 "For some of it" 3 "For most of it" ///
		4 "For all of it") rows(1) size(small) symxsize(*0.5)) ///
	name(extra_humans, replace)

	
use "B4293_ReligionAndClimate_Processed.dta", clear


*** Agreeableness 

* Ordinal model unadjusted
ologit climateHumans agree if cca_confounders == 1, or

* Check brant test - No violation
brant, detail

* Ordinal model adjusted
ologit climateHumans agree i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, or

* Predicted probabilities
sum agree if cca_confounder == 1 & climateConcern != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(agree = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
ologit climateHumans agree i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, or

margins, at(agree = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen agree = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 agree, col(black)) ///
	(line p1 agree, col(red)) ///
	(line p2 agree, col(blue)) ///
	(line p3 agree, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Agreeableness", size(medium)) ///
	legend(order(1 "Not at all" 2 "For some of it" 3 "For most of it" ///
		4 "For all of it") rows(1) size(small) symxsize(*0.5)) ///
	name(agree_humans, replace)

	
use "B4293_ReligionAndClimate_Processed.dta", clear


*** Conscientiousness

* Ordinal model unadjusted
ologit climateHumans consc if cca_confounders == 1, or

* Check brant test - No violation
brant, detail

* Ordinal model adjusted
ologit climateHumans consc i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, or

* Predicted probabilities
sum consc if cca_confounder == 1 & climateHumans != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(consc = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
margins, at(consc = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen consc = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 consc, col(black)) ///
	(line p1 consc, col(red)) ///
	(line p2 consc, col(blue)) ///
	(line p3 consc, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Conscientiousness", size(medium)) ///
	legend(order(1 "Not at all" 2 "For some of it" 3 "For most of it" ///
		4 "For all of it") rows(1) size(small) symxsize(*0.5)) ///
	name(consc_humans, replace)
	

use "B4293_ReligionAndClimate_Processed.dta", clear


*** Emotional stability

* Ordinal model unadjusted
ologit climateHumans emoStab if cca_confounders == 1, or

* Check brant test - No violation
brant, detail

* Ordinal model adjusted
ologit climateHumans emoStab i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, or

* Predicted probabilities
sum emoStab if cca_confounder == 1 & climateHumans != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(emoStab = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
ologit climateHumans emoStab i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, or

margins, at(emoStab = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen emoStab = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 emoStab, col(black)) ///
	(line p1 emoStab, col(red)) ///
	(line p2 emoStab, col(blue)) ///
	(line p3 emoStab, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Emotional Stability", size(medium)) ///
	legend(order(1 "Not at all" 2 "For some of it" 3 "For most of it" ///
		4 "For all of it") rows(1) size(small) symxsize(*0.5)) ///
	name(emoStab_humans, replace)
	

use "B4293_ReligionAndClimate_Processed.dta", clear


*** Openness to experience

* Ordinal model unadjusted
ologit climateConcern open if climateHumans == 1, or

* Check brant test - No violation
brant, detail

* Ordinal model adjusted
ologit climateHumans open i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, or

* Predicted probabilities
sum open if cca_confounder == 1 & climateHumans != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(open = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
ologit climateHumans open i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, or

margins, at(open = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen open = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 open, col(black)) ///
	(line p1 open, col(red)) ///
	(line p2 open, col(blue)) ///
	(line p3 open, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Openness to Experience", size(medium)) ///
	legend(order(1 "Not at all" 2 "For some of it" 3 "For most of it" ///
		4 "For all of it") rows(1) size(small) symxsize(*0.5)) ///
	name(open_humans, replace)
	
	
use "B4293_ReligionAndClimate_Processed.dta", clear


*** IQ

* Ordinal model unadjusted
ologit climateHumans iq if cca_confounders == 1, or

* Check brant test - Is violation
brant, detail

* Ordinal model adjusted
ologit climateHumans iq i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, or

* Predicted probabilities
sum iq if cca_confounder == 1 & climateHumans != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(iq = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
ologit climateHumans iq i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, or

margins, at(iq = (70(1)130))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen iq = fill(70 71)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 iq, col(black)) ///
	(line p1 iq, col(red)) ///
	(line p2 iq, col(blue)) ///
	(line p3 iq, col(green)), ///
	xscale(range(65 135)) xlabel(70(10)130, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("IQ scores", size(medium)) ///
	legend(order(1 "Not at all" 2 "For some of it" 3 "For most of it" ///
		4 "For all of it") rows(1) size(small) symxsize(*0.5)) ///
	name(iq_humans, replace)
	

use "B4293_ReligionAndClimate_Processed.dta", clear


** As proportional odds assumption violated, will check results in multinomial model
mlogit climateHumans iq if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit climateHumans if cca_confounders == 1 & iq != ., rrr baseoutcome(0)
est store base 

mlogit climateHumans iq if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit climateHumans i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if iq != ., rrr baseoutcome(0)
est store base 

mlogit climateHumans iq i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum iq if cca_confounder == 1 & climateHumans != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(iq = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
margins, at(iq = (70(1)130))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen iq = fill(70 71)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 iq, col(black)) ///
	(line p1 iq, col(red)) ///
	(line p2 iq, col(blue)) ///
	(line p3 iq, col(green)), ///
	xscale(range(65 135)) xlabel(70(10)130, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("IQ scores *", size(medium)) ///
	legend(order(1 "Not at all" 2 "For some of it" 3 "For most of it" ///
		4 "For all of it") rows(1) size(small) symxsize(*0.5)) ///
	name(iq_humans_mult, replace)
	

use "B4293_ReligionAndClimate_Processed.dta", clear



** Combine graphs together with a single legend using the 'grc1leg' user-written package
grc1leg extra_humans agree_humans consc_humans emoStab_humans open_humans iq_humans_mult, rows(2) imargin(tiny) ycommon

graph export ".\Results\ClimateHumanBlame_predProbs.pdf", replace

graph close _all


*** Creating various post-files to store results to (ordinal results and pseudo-r2, individual multinomial RRRs, and overall multinomial associations and r2)

capture postclose climateHumans_ordinal
postfile climateHumans_ordinal str30 exposure str30 outcome /// 
	n coef_uni lci_uni uci_uni double(p_uni) r2_uni propOdds ///
	coef_mult lci_mult uci_mult double(p_mult) r2_mult ///
	using ".\Results\climateHumans_ordinal.dta", replace

foreach var of varlist extra agree consc emoStab open iq {
	
	* Store exposure variable as a macro
	local exp = "`var'"
	
	* Store outcome as macro
	local out = "climateHumans"
	
	** Univariable analysis
	ologit climateHumans `var' if cca_confounders == 1, or
	
	* Store relevant statistics
	local n = e(N)
	local r2_uni = e(r2_p)
	
	matrix res = r(table)
	local coef_uni = res[1,1]
	local lci_uni = res[5,1]	
	local uci_uni = res[6,1]
	local p_uni = res[4,1]
	
	* Brant test
	brant
	local propOdds = r(p)
		
		
	** Multivariable analysis
	ologit climateHumans `var' i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, or
	
	* Store relevant statistics
	local r2_mult_main = e(r2_p)
	
	matrix res = r(table)
	local coef_mult = res[1,1]
	local lci_mult = res[5,1]	
	local uci_mult = res[6,1]
	local p_mult = res[4,1]
	
	* Improvement in pseudo-r2 value at inclusion of exposure
	ologit climateHumans i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if `var' != ., or
	
	local r2_mult = `r2_mult_main' - e(r2_p)
	
	** Post these results to the file
	post climateHumans_ordinal ("`exp'") ("`out'") (`n') (`coef_uni') (`lci_uni') ///
		(`uci_uni') (`p_uni') (`r2_uni') (`propOdds') (`coef_mult') (`lci_mult') ///
		(`uci_mult') (`p_mult') (`r2_mult')
	
}

postclose climateHumans_ordinal

* Check results
use ".\Results\climateHumans_ordinal.dta", clear

export delimited using ".\Results\climateHumans_ordinal.csv", replace


** Read back in original dataset and repeat for multinomial results
use "B4293_ReligionAndClimate_Processed.dta", clear


*** Multinomial models
capture postclose climateHumans_mult
postfile climateHumans_mult str30 exposure str30 outcome str30 out_level /// 
	n coef_uni lci_uni uci_uni double(p_uni) ///
	coef_mult lci_mult uci_mult double(p_mult) ///
	using ".\Results\climateHumans_mult.dta", replace
	
capture postclose climateHumans_mult_lr
postfile climateHumans_mult_lr str30 exposure str30 outcome /// 
	n double(p_uni) r2_uni double(p_mult) r2_mult ///
	using ".\Results\climateHumans_mult_lr.dta", replace

foreach var of varlist extra agree consc emoStab open iq {
	
	* Store exposure variable as a macro
	local exp = "`var'"
	
	* Store outcome as macro
	local out = "climateHumans"
	
	** Univariable analysis
	mlogit climateHumans `var' if cca_confounders == 1, baseoutcome(0) rrr
	
	* Store relevant summary statistics
	local n = e(N)
	local r2_uni = e(r2_p)
	local p_uni = e(p)
	
	* Store coefficients
	matrix res_uni = r(table)
	
	** Multivariable analysis - Repeat with and without exposure to get change in pseudo-R2 and LR test results
	mlogit climateHumans i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if `var' != ., baseoutcome(0) rrr
	local r2_mult_base = e(r2_p)
	est store base
	
	mlogit climateHumans `var' i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, baseoutcome(0) rrr
	local r2_mult = e(r2_p) - `r2_mult_base'
	est store main
	
	* Store coefficients
	matrix res_mult = r(table)
	
	* LR test
	lrtest base main
	local p_mult = r(p)
	
	** Post these summary results to the file
	post climateHumans_mult_lr ("`exp'") ("`out'") (`n') (`p_uni') (`r2_uni') ///
		(`p_mult') (`r2_mult')
		
	
	** Now focus on coefficient results
	
	* Outcome value = 1/Some of it
	local out_level = "Some of it"
	local coef_uni = res_uni[1,3]
	local lci_uni = res_uni[5,3]	
	local uci_uni = res_uni[6,3]
	local p_uni = res_uni[4,3]
	
	local coef_mult = res_mult[1,23]
	local lci_mult = res_mult[5,23]	
	local uci_mult = res_mult[6,23]
	local p_mult = res_mult[4,23]
	
	post climateHumans_mult ("`exp'") ("`out'") ("`out_level'") (`n') /// 
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`coef_mult') ///
		(`lci_mult') (`uci_mult') (`p_mult')
	
	* Outcome value = 2/Most of it
	local out_level = "Most of it"
	local coef_uni = res_uni[1,5]
	local lci_uni = res_uni[5,5]	
	local uci_uni = res_uni[6,5]
	local p_uni = res_uni[4,5]
	
	local coef_mult = res_mult[1,45]
	local lci_mult = res_mult[5,45]	
	local uci_mult = res_mult[6,45]
	local p_mult = res_mult[4,45]
	
	post climateHumans_mult ("`exp'") ("`out'") ("`out_level'") (`n') /// 
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`coef_mult') ///
		(`lci_mult') (`uci_mult') (`p_mult')
	
	* Outcome value = 3/All of it
	local out_level = "All of it"
	local coef_uni = res_uni[1,7]
	local lci_uni = res_uni[5,7]	
	local uci_uni = res_uni[6,7]
	local p_uni = res_uni[4,7]
	
	local coef_mult = res_mult[1,67]
	local lci_mult = res_mult[5,67]	
	local uci_mult = res_mult[6,67]
	local p_mult = res_mult[4,67]
	
	post climateHumans_mult ("`exp'") ("`out'") ("`out_level'") (`n') /// 
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`coef_mult') ///
		(`lci_mult') (`uci_mult') (`p_mult')
	
}

postclose climateHumans_mult
postclose climateHumans_mult_lr

* Check results
use ".\Results\climateHumans_mult_lr.dta", clear

export delimited using ".\Results\climateHumans_mult_lr.csv", replace


use ".\Results\climateHumans_mult.dta", clear

export delimited using ".\Results\climateHumans_mult.csv", replace


** Read back in original dataset
use "B4293_ReligionAndClimate_Processed.dta", clear



****************************************************************************
**** Believes personal actions can mitigate climate change

*** Extraversion 

* Multinomial model unadjusted
mlogit climateAction extra if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit climateAction if cca_confounders == 1 & extra != ., rrr baseoutcome(0)
est store base 

mlogit climateAction extra if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit climateAction i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if extra != ., rrr baseoutcome(0)
est store base 

mlogit climateAction extra i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum extra if cca_confounder == 1 & climateAction != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(extra = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit climateAction extra i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(extra = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/3

clear 
set obs `n'
egen extra = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 extra, col(black)) ///
	(line p1 extra, col(red)) ///
	(line p2 extra, col(blue)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Extraversion", size(medium)) ///
	legend(order(1 "No" 2 "Yes" 3 "Not sure") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(extra_actions, replace)

	
use "B4293_ReligionAndClimate_Processed.dta", clear


*** Agreeableness 

* Multinomial model unadjusted
mlogit climateAction agree if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit climateAction if cca_confounders == 1 & agree != ., rrr baseoutcome(0)
est store base 

mlogit climateAction agree if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit climateAction i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if agree != ., rrr baseoutcome(0)
est store base 

mlogit climateAction agree i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum agree if cca_confounder == 1 & climateAction != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(agree = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit climateAction agree i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(agree = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/3

clear 
set obs `n'
egen agree = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 agree, col(black)) ///
	(line p1 agree, col(red)) ///
	(line p2 agree, col(blue)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Agreeableness", size(medium)) ///
	legend(order(1 "No" 2 "Yes" 3 "Not sure") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(agree_actions, replace)

	
use "B4293_ReligionAndClimate_Processed.dta", clear


*** Conscientiousness

* Multinomial model unadjusted
mlogit climateAction consc if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit climateAction if cca_confounders == 1 & consc != ., rrr baseoutcome(0)
est store base 

mlogit climateAction consc if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit climateAction i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if consc != ., rrr baseoutcome(0)
est store base 

mlogit climateAction consc i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum consc if cca_confounder == 1 & climateAction != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(consc = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit climateAction consc i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(consc = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/3

clear 
set obs `n'
egen consc = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 consc, col(black)) ///
	(line p1 consc, col(red)) ///
	(line p2 consc, col(blue)), ////
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Conscientiousness", size(medium)) ///
	legend(order(1 "No" 2 "Yes" 3 "Not sure") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(consc_actions, replace)
	

use "B4293_ReligionAndClimate_Processed.dta", clear


*** Emotional stability

* Multinomial model unadjusted
mlogit climateAction emoStab if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit climateAction if cca_confounders == 1 & emoStab != ., rrr baseoutcome(0)
est store base 

mlogit climateAction emoStab if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit climateAction i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if emoStab != ., rrr baseoutcome(0)
est store base 

mlogit climateAction emoStab i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum emoStab if cca_confounder == 1 & climateAction != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(emoStab = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit climateAction emoStab i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(emoStab = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/3

clear 
set obs `n'
egen emoStab = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 emoStab, col(black)) ///
	(line p1 emoStab, col(red)) ///
	(line p2 emoStab, col(blue)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Emotional Stability", size(medium)) ///
	legend(order(1 "No" 2 "Yes" 3 "Not sure") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(emoStab_actions, replace)
	

use "B4293_ReligionAndClimate_Processed.dta", clear


*** Openness to experience

* Multinomial model unadjusted
mlogit climateAction open if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit climateAction if cca_confounders == 1 & open != ., rrr baseoutcome(0)
est store base 

mlogit climateAction open if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit climateAction i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if open != ., rrr baseoutcome(0)
est store base 

mlogit climateAction open i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum open if cca_confounder == 1 & climateAction != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(open = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit climateAction open i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(open = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/3

clear 
set obs `n'
egen open = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 open, col(black)) ///
	(line p1 open, col(red)) ///
	(line p2 open, col(blue)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Openness to Experience", size(medium)) ///
	legend(order(1 "No" 2 "Yes" 3 "Not sure") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(open_actions, replace)
	
	
use "B4293_ReligionAndClimate_Processed.dta", clear


*** IQ

* Multinomial model unadjusted
mlogit climateAction iq if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit climateAction if cca_confounders == 1 & iq != ., rrr baseoutcome(0)
est store base 

mlogit climateAction iq if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit climateAction i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if iq != ., rrr baseoutcome(0)
est store base 

mlogit climateAction iq i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum iq if cca_confounder == 1 & climateAction != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(iq = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit climateAction iq i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(iq = (70(1)130))

matrix res = r(table)
matrix list res

local n = colsof(res)/3

clear 
set obs `n'
egen iq = fill(70 71)
gen p0 = .
gen p1 = .
gen p2 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 iq, col(black)) ///
	(line p1 iq, col(red)) ///
	(line p2 iq, col(blue)), ///
	xscale(range(65 135)) xlabel(70(10)130, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("IQ scores", size(medium)) ///
	legend(order(1 "No" 2 "Yes" 3 "Not sure") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(iq_actions, replace)
	

use "B4293_ReligionAndClimate_Processed.dta", clear



** Combine graphs together with a single legend using the 'grc1leg' user-written package
grc1leg extra_actions agree_actions consc_actions emoStab_actions open_actions iq_actions, rows(2) imargin(tiny) ycommon

graph export ".\Results\ClimateActions_predProbs.pdf", replace

graph close _all


*** Creating various post-files to store results to (individual multinomial RRRs and overall multinomial associations and r2)
capture postclose climateAction_mult
postfile climateAction_mult str30 exposure str30 outcome str30 out_level /// 
	n coef_uni lci_uni uci_uni double(p_uni) ///
	coef_mult lci_mult uci_mult double(p_mult) ///
	using ".\Results\climateAction_mult.dta", replace
	
capture postclose climateAction_mult_lr
postfile climateAction_mult_lr str30 exposure str30 outcome /// 
	n double(p_uni) r2_uni double(p_mult) r2_mult ///
	using ".\Results\climateAction_mult_lr.dta", replace

foreach var of varlist extra agree consc emoStab open iq {
	
	* Store exposure variable as a macro
	local exp = "`var'"
	
	* Store outcome as macro
	local out = "climateAction"
	
	** Univariable analysis
	mlogit climateAction `var' if cca_confounders == 1, baseoutcome(0) rrr
	
	* Store relevant summary statistics
	local n = e(N)
	local r2_uni = e(r2_p)
	local p_uni = e(p)
	
	* Store coefficients
	matrix res_uni = r(table)
	
	** Multivariable analysis - Repeat with and without exposure to get change in pseudo-R2 and LR test results
	mlogit climateAction i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if `var' != ., baseoutcome(0) rrr
	local r2_mult_base = e(r2_p)
	est store base
	
	mlogit climateAction `var' i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, baseoutcome(0) rrr
	local r2_mult = e(r2_p) - `r2_mult_base'
	est store main
	
	* Store coefficients
	matrix res_mult = r(table)
	
	* LR test
	lrtest base main
	local p_mult = r(p)
	
	** Post these summary results to the file
	post climateAction_mult_lr ("`exp'") ("`out'") (`n') (`p_uni') (`r2_uni') ///
		(`p_mult') (`r2_mult')
		
	
	** Now focus on coefficient results
	
	* Outcome value = 1/Yes
	local out_level = "Yes"
	local coef_uni = res_uni[1,3]
	local lci_uni = res_uni[5,3]	
	local uci_uni = res_uni[6,3]
	local p_uni = res_uni[4,3]
	
	local coef_mult = res_mult[1,23]
	local lci_mult = res_mult[5,23]	
	local uci_mult = res_mult[6,23]
	local p_mult = res_mult[4,23]
	
	post climateAction_mult ("`exp'") ("`out'") ("`out_level'") (`n') /// 
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`coef_mult') ///
		(`lci_mult') (`uci_mult') (`p_mult')
	
	* Outcome value = 9/Not sure
	local out_level = "Not sure"
	local coef_uni = res_uni[1,5]
	local lci_uni = res_uni[5,5]	
	local uci_uni = res_uni[6,5]
	local p_uni = res_uni[4,5]
	
	local coef_mult = res_mult[1,45]
	local lci_mult = res_mult[5,45]	
	local uci_mult = res_mult[6,45]
	local p_mult = res_mult[4,45]
	
	post climateAction_mult ("`exp'") ("`out'") ("`out_level'") (`n') /// 
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`coef_mult') ///
		(`lci_mult') (`uci_mult') (`p_mult')
}

postclose climateAction_mult
postclose climateAction_mult_lr

* Check results
use ".\Results\climateAction_mult_lr.dta", clear

export delimited using ".\Results\climateAction_mult_lr.csv", replace


use ".\Results\climateAction_mult.dta", clear

export delimited using ".\Results\climateAction_mult.csv", replace


** Read back in original dataset
use "B4293_ReligionAndClimate_Processed.dta", clear



*****************************************************************************************
*****************************************************************************************
***** Climate action questions


*****************************************************************************************
**** Changed the way travel locally

*** Extraversion 

* Multinomial model unadjusted
mlogit travel extra if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit travel if cca_confounders == 1 & extra != ., rrr baseoutcome(0)
est store base 

mlogit travel extra if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit travel i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if extra != ., rrr baseoutcome(0)
est store base 

mlogit travel extra i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum extra if cca_confounder == 1 & travel != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(extra = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit travel extra i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(extra = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen extra = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 extra, col(black)) ///
	(line p1 extra, col(red)) ///
	(line p2 extra, col(blue)) ///
	(line p3 extra, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Extraversion", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(extra_travel, replace)

	
use "B4293_ReligionAndClimate_Processed.dta", clear


*** Agreeableness 

* Multinomial model unadjusted
mlogit travel agree if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit travel if cca_confounders == 1 & agree != ., rrr baseoutcome(0)
est store base 

mlogit travel agree if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit travel i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if agree != ., rrr baseoutcome(0)
est store base 

mlogit travel agree i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum agree if cca_confounder == 1 & travel != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(agree = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit travel agree i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(agree = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen agree = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 agree, col(black)) ///
	(line p1 agree, col(red)) ///
	(line p2 agree, col(blue)) ///
	(line p3 agree, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Agreeableness", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(agree_travel, replace)

	
use "B4293_ReligionAndClimate_Processed.dta", clear


*** Conscientiousness

* Multinomial model unadjusted
mlogit travel consc if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit travel if cca_confounders == 1 & consc != ., rrr baseoutcome(0)
est store base 

mlogit travel consc if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit travel i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if consc != ., rrr baseoutcome(0)
est store base 

mlogit travel consc i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum consc if cca_confounder == 1 & travel != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(consc = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit travel consc i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(consc = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen consc = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 consc, col(black)) ///
	(line p1 consc, col(red)) ///
	(line p2 consc, col(blue)) ///
	(line p3 consc, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Conscientiousness", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(consc_travel, replace)
	

use "B4293_ReligionAndClimate_Processed.dta", clear


*** Emotional stability

* Multinomial model unadjusted
mlogit travel emoStab if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit travel if cca_confounders == 1 & emoStab != ., rrr baseoutcome(0)
est store base 

mlogit travel emoStab if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit travel i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if emoStab != ., rrr baseoutcome(0)
est store base 

mlogit travel emoStab i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum emoStab if cca_confounder == 1 & travel != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(emoStab = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit travel emoStab i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(emoStab = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen emoStab = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 emoStab, col(black)) ///
	(line p1 emoStab, col(red)) ///
	(line p2 emoStab, col(blue)) ///
	(line p3 emoStab, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Emotional stability", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(emoStab_travel, replace)
	

use "B4293_ReligionAndClimate_Processed.dta", clear


*** Openness to experience

* Multinomial model unadjusted
mlogit travel open if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit travel if cca_confounders == 1 & open != ., rrr baseoutcome(0)
est store base 

mlogit travel open if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit travel i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if open != ., rrr baseoutcome(0)
est store base 

mlogit travel open i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum open if cca_confounder == 1 & travel != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(open = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit travel open i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(open = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen open = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 open, col(black)) ///
	(line p1 open, col(red)) ///
	(line p2 open, col(blue)) ///
	(line p3 open, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Openness to experience", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(open_travel, replace)
	
	
use "B4293_ReligionAndClimate_Processed.dta", clear


*** IQ

* Multinomial model unadjusted
mlogit travel iq if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit travel if cca_confounders == 1 & iq != ., rrr baseoutcome(0)
est store base 

mlogit travel iq if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit travel i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if iq != ., rrr baseoutcome(0)
est store base 

mlogit travel iq i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum iq if cca_confounder == 1 & travel != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(iq = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit travel iq i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(iq = (70(1)130))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen iq = fill(70 71)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 iq, col(black)) ///
	(line p1 iq, col(red)) ///
	(line p2 iq, col(blue)) ///
	(line p3 iq, col(green)), ///
	xscale(range(65 135)) xlabel(70(10)130, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("IQ scores", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(iq_travel, replace)
	

use "B4293_ReligionAndClimate_Processed.dta", clear



** Combine graphs together with a single legend using the 'grc1leg' user-written package
grc1leg extra_travel agree_travel consc_travel emoStab_travel open_travel iq_travel, rows(2) imargin(tiny) ycommon

graph export ".\Results\travelLocally_predProbs.pdf", replace

graph close _all


*** Creating various post-files to store results to (individual multinomial RRRs and overall multinomial associations and r2)
capture postclose travel_mult
postfile travel_mult str30 exposure str30 outcome str30 out_level /// 
	n coef_uni lci_uni uci_uni double(p_uni) ///
	coef_mult lci_mult uci_mult double(p_mult) ///
	using ".\Results\travel_mult.dta", replace
	
capture postclose travel_mult_lr
postfile travel_mult_lr str30 exposure str30 outcome /// 
	n double(p_uni) r2_uni double(p_mult) r2_mult ///
	using ".\Results\travel_mult_lr.dta", replace

foreach var of varlist extra agree consc emoStab open iq {
	
	* Store exposure variable as a macro
	local exp = "`var'"
	
	* Store outcome as macro
	local out = "travel"
	
	** Univariable analysis
	mlogit travel `var' if cca_confounders == 1, baseoutcome(0) rrr
	
	* Store relevant summary statistics
	local n = e(N)
	local r2_uni = e(r2_p)
	local p_uni = e(p)
	
	* Store coefficients
	matrix res_uni = r(table)
	
	** Multivariable analysis - Repeat with and without exposure to get change in pseudo-R2 and LR test results
	mlogit travel i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if `var' != ., baseoutcome(0) rrr
	local r2_mult_base = e(r2_p)
	est store base
	
	mlogit travel `var' i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, baseoutcome(0) rrr
	local r2_mult = e(r2_p) - `r2_mult_base'
	est store main
	
	* Store coefficients
	matrix res_mult = r(table)
	
	* LR test
	lrtest base main
	local p_mult = r(p)
	
	** Post these summary results to the file
	post travel_mult_lr ("`exp'") ("`out'") (`n') (`p_uni') (`r2_uni') ///
		(`p_mult') (`r2_mult')
		
	
	** Now focus on coefficient results
	
	* Outcome value = 1/Yes for climate
	local out_level = "for climate"
	local coef_uni = res_uni[1,3]
	local lci_uni = res_uni[5,3]	
	local uci_uni = res_uni[6,3]
	local p_uni = res_uni[4,3]
	
	local coef_mult = res_mult[1,23]
	local lci_mult = res_mult[5,23]	
	local uci_mult = res_mult[6,23]
	local p_mult = res_mult[4,23]
	
	post travel_mult ("`exp'") ("`out'") ("`out_level'") (`n') /// 
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`coef_mult') ///
		(`lci_mult') (`uci_mult') (`p_mult')
	
	* Outcome value = 2/Yes, for non-climate
	local out_level = "for non-climate"
	local coef_uni = res_uni[1,5]
	local lci_uni = res_uni[5,5]	
	local uci_uni = res_uni[6,5]
	local p_uni = res_uni[4,5]
	
	local coef_mult = res_mult[1,45]
	local lci_mult = res_mult[5,45]	
	local uci_mult = res_mult[6,45]
	local p_mult = res_mult[4,45]
	
	post travel_mult ("`exp'") ("`out'") ("`out_level'") (`n') /// 
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`coef_mult') ///
		(`lci_mult') (`uci_mult') (`p_mult')
		
	* Outcome value = 3/Yes, for climate and non-climate
	local out_level = "for climate and non-climate"
	local coef_uni = res_uni[1,7]
	local lci_uni = res_uni[5,7]	
	local uci_uni = res_uni[6,7]
	local p_uni = res_uni[4,7]
	
	local coef_mult = res_mult[1,67]
	local lci_mult = res_mult[5,67]	
	local uci_mult = res_mult[6,67]
	local p_mult = res_mult[4,67]
	
	post travel_mult ("`exp'") ("`out'") ("`out_level'") (`n') /// 
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`coef_mult') ///
		(`lci_mult') (`uci_mult') (`p_mult')
		
}

postclose travel_mult
postclose travel_mult_lr

* Check results
use ".\Results\travel_mult_lr.dta", clear

export delimited using ".\Results\travel_mult_lr.csv", replace


use ".\Results\travel_mult.dta", clear

export delimited using ".\Results\travel_mult.csv", replace


** Read back in original dataset
use "B4293_ReligionAndClimate_Processed.dta", clear


**** Changed the way travel locally (binary)

*** Extraversion 

* Logistic model unadjusted
logistic travel_bin extra if cca_confounders == 1

* And for adjusted model
logistic travel_bin extra i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum extra if cca_confounder == 1 & travel_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(extra = (`lower_sd', `mean', `upper_sd'))


*** Agreeableness 

* Logistic model unadjusted
logistic travel_bin agree if cca_confounders == 1

* And for adjusted model
logistic travel_bin agree i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum agree if cca_confounder == 1 & travel_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(agree = (`lower_sd', `mean', `upper_sd'))


*** Conscientiousness

* Logistic model unadjusted
logistic travel_bin consc if cca_confounders == 1

* And for adjusted model
logistic travel_bin consc i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum consc if cca_confounder == 1 & travel_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(consc = (`lower_sd', `mean', `upper_sd'))


*** Emotional stability

* Logistic model unadjusted
logistic travel_bin emoStab if cca_confounders == 1

* And for adjusted model
logistic travel_bin emoStab i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum emoStab if cca_confounder == 1 & travel_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(emoStab = (`lower_sd', `mean', `upper_sd'))


*** Openness to experience

* Logistic model unadjusted
logistic travel_bin open if cca_confounders == 1

* And for adjusted model
logistic travel_bin open i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum open if cca_confounder == 1 & travel_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(open = (`lower_sd', `mean', `upper_sd'))


*** IQ

* Logistic model unadjusted
logistic travel_bin iq if cca_confounders == 1

* And for adjusted model
logistic travel_bin iq i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum iq if cca_confounder == 1 & travel_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(iq = (`lower_sd', `mean', `upper_sd'))


*** Creating post-file to store logistic results to
capture postclose travel_bin
postfile travel_bin str30 exposure str30 outcome /// 
	n coef_uni lci_uni uci_uni double(p_uni) r2_uni ///
	coef_mult lci_mult uci_mult double(p_mult) r2_mult ///
	using ".\Results\travel_bin.dta", replace

foreach var of varlist extra agree consc emoStab open iq {
	
	* Store exposure variable as a macro
	local exp = "`var'"
	
	* Store outcome as macro
	local out = "travel_bin"
	
	** Univariable analysis
	logistic travel_bin `var' if cca_confounders == 1
	
	* Store relevant summary statistics
	local n = e(N)
	local r2_uni = e(r2_p)
	local p_uni = e(p)
	
	* Store coefficients
	matrix res = r(table)
	
	local coef_uni = res[1,1]
	local lci_uni = res[5,1]	
	local uci_uni = res[6,1]
	local p_uni = res[4,1]
	
	** Multivariable analysis - Repeat with and without exposure to get change in pseudo-R2 results
	logistic travel i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if `var' != .
	local r2_mult_base = e(r2_p)
	
	logistic travel `var' i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge
	local r2_mult = e(r2_p) - `r2_mult_base'
	
	* Store coefficients
	matrix res = r(table)
	
	local coef_mult = res[1,1]
	local lci_mult = res[5,1]	
	local uci_mult = res[6,1]
	local p_mult = res[4,1]
	
	** Post these summary results to the file
	post travel_bin ("`exp'") ("`out'") (`n') ///
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`r2_uni') ///
		(`coef_mult') (`lci_mult') (`uci_mult') ///
		(`p_mult') (`r2_mult')
		
}

postclose travel_bin

* Check results
use ".\Results\travel_bin.dta", clear

export delimited using ".\Results\travel_bin.csv", replace


** Read back in original dataset
use "B4293_ReligionAndClimate_Processed.dta", clear



*****************************************************************************************
**** Reduced household waste

*** Extraversion 

* Multinomial model unadjusted
mlogit waste extra if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit waste if cca_confounders == 1 & extra != ., rrr baseoutcome(0)
est store base 

mlogit waste extra if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit waste i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if extra != ., rrr baseoutcome(0)
est store base 

mlogit waste extra i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum extra if cca_confounder == 1 & waste != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(extra = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit waste extra i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(extra = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen extra = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 extra, col(black)) ///
	(line p1 extra, col(red)) ///
	(line p2 extra, col(blue)) ///
	(line p3 extra, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Extraversion", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(extra_waste, replace)

	
use "B4293_ReligionAndClimate_Processed.dta", clear


*** Agreeableness 

* Multinomial model unadjusted
mlogit waste agree if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit waste if cca_confounders == 1 & agree != ., rrr baseoutcome(0)
est store base 

mlogit waste agree if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit waste i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if agree != ., rrr baseoutcome(0)
est store base 

mlogit waste agree i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum agree if cca_confounder == 1 & waste != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(agree = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit waste agree i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(agree = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen agree = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 agree, col(black)) ///
	(line p1 agree, col(red)) ///
	(line p2 agree, col(blue)) ///
	(line p3 agree, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Agreeableness", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(agree_waste, replace)

	
use "B4293_ReligionAndClimate_Processed.dta", clear


*** Conscientiousness

* Multinomial model unadjusted
mlogit waste consc if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit waste if cca_confounders == 1 & consc != ., rrr baseoutcome(0)
est store base 

mlogit waste consc if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit waste i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if consc != ., rrr baseoutcome(0)
est store base 

mlogit waste consc i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum consc if cca_confounder == 1 & waste != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(consc = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit waste consc i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(consc = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen consc = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 consc, col(black)) ///
	(line p1 consc, col(red)) ///
	(line p2 consc, col(blue)) ///
	(line p3 consc, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Conscientiousness", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(consc_waste, replace)
	

use "B4293_ReligionAndClimate_Processed.dta", clear


*** Emotional stability

* Multinomial model unadjusted
mlogit waste emoStab if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit waste if cca_confounders == 1 & emoStab != ., rrr baseoutcome(0)
est store base 

mlogit waste emoStab if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit waste i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if emoStab != ., rrr baseoutcome(0)
est store base 

mlogit waste emoStab i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum emoStab if cca_confounder == 1 & waste != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(emoStab = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit waste emoStab i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(emoStab = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen emoStab = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 emoStab, col(black)) ///
	(line p1 emoStab, col(red)) ///
	(line p2 emoStab, col(blue)) ///
	(line p3 emoStab, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Emotional stability", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(emoStab_waste, replace)
	

use "B4293_ReligionAndClimate_Processed.dta", clear


*** Openness to experience

* Multinomial model unadjusted
mlogit waste open if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit waste if cca_confounders == 1 & open != ., rrr baseoutcome(0)
est store base 

mlogit waste open if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit waste i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if open != ., rrr baseoutcome(0)
est store base 

mlogit waste open i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum open if cca_confounder == 1 & waste != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(open = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit waste open i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(open = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen open = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 open, col(black)) ///
	(line p1 open, col(red)) ///
	(line p2 open, col(blue)) ///
	(line p3 open, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Openness to experience", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(open_waste, replace)
	
	
use "B4293_ReligionAndClimate_Processed.dta", clear


*** IQ

* Multinomial model unadjusted
mlogit waste iq if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit waste if cca_confounders == 1 & iq != ., rrr baseoutcome(0)
est store base 

mlogit waste iq if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit waste i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if iq != ., rrr baseoutcome(0)
est store base 

mlogit waste iq i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum iq if cca_confounder == 1 & waste != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(iq = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit waste iq i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(iq = (70(1)130))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen iq = fill(70 71)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 iq, col(black)) ///
	(line p1 iq, col(red)) ///
	(line p2 iq, col(blue)) ///
	(line p3 iq, col(green)), ///
	xscale(range(65 135)) xlabel(70(10)130, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("IQ scores", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(iq_waste, replace)
	

use "B4293_ReligionAndClimate_Processed.dta", clear



** Combine graphs together with a single legend using the 'grc1leg' user-written package
grc1leg extra_waste agree_waste consc_waste emoStab_waste open_waste iq_waste, rows(2) imargin(tiny) ycommon

graph export ".\Results\waste_predProbs.pdf", replace

graph close _all


*** Creating various post-files to store results to (individual multinomial RRRs and overall multinomial associations and r2)
capture postclose waste_mult
postfile waste_mult str30 exposure str30 outcome str30 out_level /// 
	n coef_uni lci_uni uci_uni double(p_uni) ///
	coef_mult lci_mult uci_mult double(p_mult) ///
	using ".\Results\waste_mult.dta", replace
	
capture postclose waste_mult_lr
postfile waste_mult_lr str30 exposure str30 outcome /// 
	n double(p_uni) r2_uni double(p_mult) r2_mult ///
	using ".\Results\waste_mult_lr.dta", replace

foreach var of varlist extra agree consc emoStab open iq {
	
	* Store exposure variable as a macro
	local exp = "`var'"
	
	* Store outcome as macro
	local out = "waste"
	
	** Univariable analysis
	mlogit waste `var' if cca_confounders == 1, baseoutcome(0) rrr
	
	* Store relevant summary statistics
	local n = e(N)
	local r2_uni = e(r2_p)
	local p_uni = e(p)
	
	* Store coefficients
	matrix res_uni = r(table)
	
	** Multivariable analysis - Repeat with and without exposure to get change in pseudo-R2 and LR test results
	mlogit waste i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if `var' != ., baseoutcome(0) rrr
	local r2_mult_base = e(r2_p)
	est store base
	
	mlogit waste `var' i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, baseoutcome(0) rrr
	local r2_mult = e(r2_p) - `r2_mult_base'
	est store main
	
	* Store coefficients
	matrix res_mult = r(table)
	
	* LR test
	lrtest base main
	local p_mult = r(p)
	
	** Post these summary results to the file
	post waste_mult_lr ("`exp'") ("`out'") (`n') (`p_uni') (`r2_uni') ///
		(`p_mult') (`r2_mult')
		
	
	** Now focus on coefficient results
	
	* Outcome value = 1/Yes for climate
	local out_level = "for climate"
	local coef_uni = res_uni[1,3]
	local lci_uni = res_uni[5,3]	
	local uci_uni = res_uni[6,3]
	local p_uni = res_uni[4,3]
	
	local coef_mult = res_mult[1,23]
	local lci_mult = res_mult[5,23]	
	local uci_mult = res_mult[6,23]
	local p_mult = res_mult[4,23]
	
	post waste_mult ("`exp'") ("`out'") ("`out_level'") (`n') /// 
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`coef_mult') ///
		(`lci_mult') (`uci_mult') (`p_mult')
	
	* Outcome value = 2/Yes, for non-climate
	local out_level = "for non-climate"
	local coef_uni = res_uni[1,5]
	local lci_uni = res_uni[5,5]	
	local uci_uni = res_uni[6,5]
	local p_uni = res_uni[4,5]
	
	local coef_mult = res_mult[1,45]
	local lci_mult = res_mult[5,45]	
	local uci_mult = res_mult[6,45]
	local p_mult = res_mult[4,45]
	
	post waste_mult ("`exp'") ("`out'") ("`out_level'") (`n') /// 
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`coef_mult') ///
		(`lci_mult') (`uci_mult') (`p_mult')
		
	* Outcome value = 3/Yes, for climate and non-climate
	local out_level = "for climate and non-climate"
	local coef_uni = res_uni[1,7]
	local lci_uni = res_uni[5,7]	
	local uci_uni = res_uni[6,7]
	local p_uni = res_uni[4,7]
	
	local coef_mult = res_mult[1,67]
	local lci_mult = res_mult[5,67]	
	local uci_mult = res_mult[6,67]
	local p_mult = res_mult[4,67]
	
	post waste_mult ("`exp'") ("`out'") ("`out_level'") (`n') /// 
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`coef_mult') ///
		(`lci_mult') (`uci_mult') (`p_mult')
		
}

postclose waste_mult
postclose waste_mult_lr

* Check results
use ".\Results\waste_mult_lr.dta", clear

export delimited using ".\Results\waste_mult_lr.csv", replace


use ".\Results\waste_mult.dta", clear

export delimited using ".\Results\waste_mult.csv", replace


** Read back in original dataset
use "B4293_ReligionAndClimate_Processed.dta", clear


**** Reduced household waste (binary)

*** Extraversion 

* Logistic model unadjusted
logistic waste_bin extra if cca_confounders == 1

* And for adjusted model
logistic waste_bin extra i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum extra if cca_confounder == 1 & waste_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(extra = (`lower_sd', `mean', `upper_sd'))


*** Agreeableness 

* Logistic model unadjusted
logistic waste_bin agree if cca_confounders == 1

* And for adjusted model
logistic waste_bin agree i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum agree if cca_confounder == 1 & waste_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(agree = (`lower_sd', `mean', `upper_sd'))


*** Conscientiousness

* Logistic model unadjusted
logistic waste_bin consc if cca_confounders == 1

* And for adjusted model
logistic waste_bin consc i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum consc if cca_confounder == 1 & waste_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(consc = (`lower_sd', `mean', `upper_sd'))


*** Emotional stability

* Logistic model unadjusted
logistic waste_bin emoStab if cca_confounders == 1

* And for adjusted model
logistic waste_bin emoStab i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum emoStab if cca_confounder == 1 & waste_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(emoStab = (`lower_sd', `mean', `upper_sd'))


*** Openness to experience

* Logistic model unadjusted
logistic waste_bin open if cca_confounders == 1

* And for adjusted model
logistic waste_bin open i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum open if cca_confounder == 1 & waste_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(open = (`lower_sd', `mean', `upper_sd'))


*** IQ

* Logistic model unadjusted
logistic waste_bin iq if cca_confounders == 1

* And for adjusted model
logistic waste_bin iq i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum iq if cca_confounder == 1 & waste_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(iq = (`lower_sd', `mean', `upper_sd'))


*** Creating post-file to store logistic results to
capture postclose waste_bin
postfile waste_bin str30 exposure str30 outcome /// 
	n coef_uni lci_uni uci_uni double(p_uni) r2_uni ///
	coef_mult lci_mult uci_mult double(p_mult) r2_mult ///
	using ".\Results\waste_bin.dta", replace

foreach var of varlist extra agree consc emoStab open iq {
	
	* Store exposure variable as a macro
	local exp = "`var'"
	
	* Store outcome as macro
	local out = "waste_bin"
	
	** Univariable analysis
	logistic waste_bin `var' if cca_confounders == 1
	
	* Store relevant summary statistics
	local n = e(N)
	local r2_uni = e(r2_p)
	local p_uni = e(p)
	
	* Store coefficients
	matrix res = r(table)
	
	local coef_uni = res[1,1]
	local lci_uni = res[5,1]	
	local uci_uni = res[6,1]
	local p_uni = res[4,1]
	
	** Multivariable analysis - Repeat with and without exposure to get change in pseudo-R2 results
	logistic waste i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if `var' != .
	local r2_mult_base = e(r2_p)
	
	logistic waste `var' i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge
	local r2_mult = e(r2_p) - `r2_mult_base'
	
	* Store coefficients
	matrix res = r(table)
	
	local coef_mult = res[1,1]
	local lci_mult = res[5,1]	
	local uci_mult = res[6,1]
	local p_mult = res[4,1]
	
	** Post these summary results to the file
	post waste_bin ("`exp'") ("`out'") (`n') ///
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`r2_uni') ///
		(`coef_mult') (`lci_mult') (`uci_mult') ///
		(`p_mult') (`r2_mult')
		
}

postclose waste_bin

* Check results
use ".\Results\waste_bin.dta", clear

export delimited using ".\Results\waste_bin.csv", replace


** Read back in original dataset
use "B4293_ReligionAndClimate_Processed.dta", clear



*****************************************************************************************
**** Reduced energy use

*** Extraversion 

* Multinomial model unadjusted
mlogit energy extra if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit energy if cca_confounders == 1 & extra != ., rrr baseoutcome(0)
est store base 

mlogit energy extra if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit energy i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if extra != ., rrr baseoutcome(0)
est store base 

mlogit energy extra i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum extra if cca_confounder == 1 & energy != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(extra = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit energy extra i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(extra = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen extra = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 extra, col(black)) ///
	(line p1 extra, col(red)) ///
	(line p2 extra, col(blue)) ///
	(line p3 extra, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Extraversion", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(extra_energy, replace)

	
use "B4293_ReligionAndClimate_Processed.dta", clear


*** Agreeableness 

* Multinomial model unadjusted
mlogit energy agree if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit energy if cca_confounders == 1 & agree != ., rrr baseoutcome(0)
est store base 

mlogit energy agree if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit energy i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if agree != ., rrr baseoutcome(0)
est store base 

mlogit energy agree i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum agree if cca_confounder == 1 & energy != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(agree = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit energy agree i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(agree = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen agree = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 agree, col(black)) ///
	(line p1 agree, col(red)) ///
	(line p2 agree, col(blue)) ///
	(line p3 agree, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Agreeableness", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(agree_energy, replace)

	
use "B4293_ReligionAndClimate_Processed.dta", clear


*** Conscientiousness

* Multinomial model unadjusted
mlogit energy consc if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit energy if cca_confounders == 1 & consc != ., rrr baseoutcome(0)
est store base 

mlogit energy consc if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit energy i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if consc != ., rrr baseoutcome(0)
est store base 

mlogit energy consc i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum consc if cca_confounder == 1 & energy != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(consc = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit energy consc i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(consc = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen consc = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 consc, col(black)) ///
	(line p1 consc, col(red)) ///
	(line p2 consc, col(blue)) ///
	(line p3 consc, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Conscientiousness", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(consc_energy, replace)
	

use "B4293_ReligionAndClimate_Processed.dta", clear


*** Emotional stability

* Multinomial model unadjusted
mlogit energy emoStab if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit energy if cca_confounders == 1 & emoStab != ., rrr baseoutcome(0)
est store base 

mlogit energy emoStab if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit energy i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if emoStab != ., rrr baseoutcome(0)
est store base 

mlogit energy emoStab i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum emoStab if cca_confounder == 1 & energy != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(emoStab = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit energy emoStab i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(emoStab = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen emoStab = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 emoStab, col(black)) ///
	(line p1 emoStab, col(red)) ///
	(line p2 emoStab, col(blue)) ///
	(line p3 emoStab, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Emotional stability", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(emoStab_energy, replace)
	

use "B4293_ReligionAndClimate_Processed.dta", clear


*** Openness to experience

* Multinomial model unadjusted
mlogit energy open if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit energy if cca_confounders == 1 & open != ., rrr baseoutcome(0)
est store base 

mlogit energy open if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit energy i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if open != ., rrr baseoutcome(0)
est store base 

mlogit energy open i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum open if cca_confounder == 1 & energy != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(open = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit energy open i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(open = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen open = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 open, col(black)) ///
	(line p1 open, col(red)) ///
	(line p2 open, col(blue)) ///
	(line p3 open, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Openness to experience", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(open_energy, replace)
	
	
use "B4293_ReligionAndClimate_Processed.dta", clear


*** IQ

* Multinomial model unadjusted
mlogit energy iq if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit energy if cca_confounders == 1 & iq != ., rrr baseoutcome(0)
est store base 

mlogit energy iq if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit energy i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if iq != ., rrr baseoutcome(0)
est store base 

mlogit energy iq i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum iq if cca_confounder == 1 & energy != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(iq = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit energy iq i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(iq = (70(1)130))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen iq = fill(70 71)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 iq, col(black)) ///
	(line p1 iq, col(red)) ///
	(line p2 iq, col(blue)) ///
	(line p3 iq, col(green)), ///
	xscale(range(65 135)) xlabel(70(10)130, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("IQ scores", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(iq_energy, replace)
	

use "B4293_ReligionAndClimate_Processed.dta", clear



** Combine graphs together with a single legend using the 'grc1leg' user-written package
grc1leg extra_energy agree_energy consc_energy emoStab_energy open_energy iq_energy, rows(2) imargin(tiny) ycommon

graph export ".\Results\energy_predProbs.pdf", replace

graph close _all


*** Creating various post-files to store results to (individual multinomial RRRs and overall multinomial associations and r2)
capture postclose energy_mult
postfile energy_mult str30 exposure str30 outcome str30 out_level /// 
	n coef_uni lci_uni uci_uni double(p_uni) ///
	coef_mult lci_mult uci_mult double(p_mult) ///
	using ".\Results\energy_mult.dta", replace
	
capture postclose energy_mult_lr
postfile energy_mult_lr str30 exposure str30 outcome /// 
	n double(p_uni) r2_uni double(p_mult) r2_mult ///
	using ".\Results\energy_mult_lr.dta", replace

foreach var of varlist extra agree consc emoStab open iq {
	
	* Store exposure variable as a macro
	local exp = "`var'"
	
	* Store outcome as macro
	local out = "energy"
	
	** Univariable analysis
	mlogit energy `var' if cca_confounders == 1, baseoutcome(0) rrr
	
	* Store relevant summary statistics
	local n = e(N)
	local r2_uni = e(r2_p)
	local p_uni = e(p)
	
	* Store coefficients
	matrix res_uni = r(table)
	
	** Multivariable analysis - Repeat with and without exposure to get change in pseudo-R2 and LR test results
	mlogit energy i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if `var' != ., baseoutcome(0) rrr
	local r2_mult_base = e(r2_p)
	est store base
	
	mlogit energy `var' i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, baseoutcome(0) rrr
	local r2_mult = e(r2_p) - `r2_mult_base'
	est store main
	
	* Store coefficients
	matrix res_mult = r(table)
	
	* LR test
	lrtest base main
	local p_mult = r(p)
	
	** Post these summary results to the file
	post energy_mult_lr ("`exp'") ("`out'") (`n') (`p_uni') (`r2_uni') ///
		(`p_mult') (`r2_mult')
		
	
	** Now focus on coefficient results
	
	* Outcome value = 1/Yes for climate
	local out_level = "for climate"
	local coef_uni = res_uni[1,3]
	local lci_uni = res_uni[5,3]	
	local uci_uni = res_uni[6,3]
	local p_uni = res_uni[4,3]
	
	local coef_mult = res_mult[1,23]
	local lci_mult = res_mult[5,23]	
	local uci_mult = res_mult[6,23]
	local p_mult = res_mult[4,23]
	
	post energy_mult ("`exp'") ("`out'") ("`out_level'") (`n') /// 
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`coef_mult') ///
		(`lci_mult') (`uci_mult') (`p_mult')
	
	* Outcome value = 2/Yes, for non-climate
	local out_level = "for non-climate"
	local coef_uni = res_uni[1,5]
	local lci_uni = res_uni[5,5]	
	local uci_uni = res_uni[6,5]
	local p_uni = res_uni[4,5]
	
	local coef_mult = res_mult[1,45]
	local lci_mult = res_mult[5,45]	
	local uci_mult = res_mult[6,45]
	local p_mult = res_mult[4,45]
	
	post energy_mult ("`exp'") ("`out'") ("`out_level'") (`n') /// 
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`coef_mult') ///
		(`lci_mult') (`uci_mult') (`p_mult')
		
	* Outcome value = 3/Yes, for climate and non-climate
	local out_level = "for climate and non-climate"
	local coef_uni = res_uni[1,7]
	local lci_uni = res_uni[5,7]	
	local uci_uni = res_uni[6,7]
	local p_uni = res_uni[4,7]
	
	local coef_mult = res_mult[1,67]
	local lci_mult = res_mult[5,67]	
	local uci_mult = res_mult[6,67]
	local p_mult = res_mult[4,67]
	
	post energy_mult ("`exp'") ("`out'") ("`out_level'") (`n') /// 
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`coef_mult') ///
		(`lci_mult') (`uci_mult') (`p_mult')
		
}

postclose energy_mult
postclose energy_mult_lr

* Check results
use ".\Results\energy_mult_lr.dta", clear

export delimited using ".\Results\energy_mult_lr.csv", replace


use ".\Results\energy_mult.dta", clear

export delimited using ".\Results\energy_mult.csv", replace


** Read back in original dataset
use "B4293_ReligionAndClimate_Processed.dta", clear


**** Reduced energy use (binary)

*** Extraversion 

* Logistic model unadjusted
logistic energy_bin extra if cca_confounders == 1

* And for adjusted model
logistic energy_bin extra i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum extra if cca_confounder == 1 & energy_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(extra = (`lower_sd', `mean', `upper_sd'))


*** Agreeableness 

* Logistic model unadjusted
logistic energy_bin agree if cca_confounders == 1

* And for adjusted model
logistic energy_bin agree i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum agree if cca_confounder == 1 & energy_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(agree = (`lower_sd', `mean', `upper_sd'))


*** Conscientiousness

* Logistic model unadjusted
logistic energy_bin consc if cca_confounders == 1

* And for adjusted model
logistic energy_bin consc i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum consc if cca_confounder == 1 & energy_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(consc = (`lower_sd', `mean', `upper_sd'))


*** Emotional stability

* Logistic model unadjusted
logistic energy_bin emoStab if cca_confounders == 1

* And for adjusted model
logistic energy_bin emoStab i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum emoStab if cca_confounder == 1 & energy_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(emoStab = (`lower_sd', `mean', `upper_sd'))


*** Openness to experience

* Logistic model unadjusted
logistic energy_bin open if cca_confounders == 1

* And for adjusted model
logistic energy_bin open i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum open if cca_confounder == 1 & energy_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(open = (`lower_sd', `mean', `upper_sd'))


*** IQ

* Logistic model unadjusted
logistic energy_bin iq if cca_confounders == 1

* And for adjusted model
logistic energy_bin iq i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum iq if cca_confounder == 1 & energy_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(iq = (`lower_sd', `mean', `upper_sd'))


*** Creating post-file to store logistic results to
capture postclose energy_bin
postfile energy_bin str30 exposure str30 outcome /// 
	n coef_uni lci_uni uci_uni double(p_uni) r2_uni ///
	coef_mult lci_mult uci_mult double(p_mult) r2_mult ///
	using ".\Results\energy_bin.dta", replace

foreach var of varlist extra agree consc emoStab open iq {
	
	* Store exposure variable as a macro
	local exp = "`var'"
	
	* Store outcome as macro
	local out = "energy_bin"
	
	** Univariable analysis
	logistic energy_bin `var' if cca_confounders == 1
	
	* Store relevant summary statistics
	local n = e(N)
	local r2_uni = e(r2_p)
	local p_uni = e(p)
	
	* Store coefficients
	matrix res = r(table)
	
	local coef_uni = res[1,1]
	local lci_uni = res[5,1]	
	local uci_uni = res[6,1]
	local p_uni = res[4,1]
	
	** Multivariable analysis - Repeat with and without exposure to get change in pseudo-R2 results
	logistic energy i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if `var' != .
	local r2_mult_base = e(r2_p)
	
	logistic energy `var' i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge
	local r2_mult = e(r2_p) - `r2_mult_base'
	
	* Store coefficients
	matrix res = r(table)
	
	local coef_mult = res[1,1]
	local lci_mult = res[5,1]	
	local uci_mult = res[6,1]
	local p_mult = res[4,1]
	
	** Post these summary results to the file
	post energy_bin ("`exp'") ("`out'") (`n') ///
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`r2_uni') ///
		(`coef_mult') (`lci_mult') (`uci_mult') ///
		(`p_mult') (`r2_mult')
		
}

postclose energy_bin

* Check results
use ".\Results\energy_bin.dta", clear

export delimited using ".\Results\energy_bin.csv", replace


** Read back in original dataset
use "B4293_ReligionAndClimate_Processed.dta", clear



*****************************************************************************************
**** Changed whay buy

*** Extraversion 

* Multinomial model unadjusted
mlogit buy extra if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit buy if cca_confounders == 1 & extra != ., rrr baseoutcome(0)
est store base 

mlogit buy extra if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit buy i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if extra != ., rrr baseoutcome(0)
est store base 

mlogit buy extra i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum extra if cca_confounder == 1 & buy != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(extra = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit buy extra i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(extra = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen extra = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 extra, col(black)) ///
	(line p1 extra, col(red)) ///
	(line p2 extra, col(blue)) ///
	(line p3 extra, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Extraversion", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(extra_buy, replace)

	
use "B4293_ReligionAndClimate_Processed.dta", clear


*** Agreeableness 

* Multinomial model unadjusted
mlogit buy agree if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit buy if cca_confounders == 1 & agree != ., rrr baseoutcome(0)
est store base 

mlogit buy agree if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit buy i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if agree != ., rrr baseoutcome(0)
est store base 

mlogit buy agree i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum agree if cca_confounder == 1 & buy != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(agree = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit buy agree i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(agree = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen agree = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 agree, col(black)) ///
	(line p1 agree, col(red)) ///
	(line p2 agree, col(blue)) ///
	(line p3 agree, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Agreeableness", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(agree_buy, replace)

	
use "B4293_ReligionAndClimate_Processed.dta", clear


*** Conscientiousness

* Multinomial model unadjusted
mlogit buy consc if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit buy if cca_confounders == 1 & consc != ., rrr baseoutcome(0)
est store base 

mlogit buy consc if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit buy i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if consc != ., rrr baseoutcome(0)
est store base 

mlogit buy consc i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum consc if cca_confounder == 1 & buy != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(consc = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit buy consc i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(consc = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen consc = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 consc, col(black)) ///
	(line p1 consc, col(red)) ///
	(line p2 consc, col(blue)) ///
	(line p3 consc, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Conscientiousness", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(consc_buy, replace)
	

use "B4293_ReligionAndClimate_Processed.dta", clear


*** Emotional stability

* Multinomial model unadjusted
mlogit buy emoStab if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit buy if cca_confounders == 1 & emoStab != ., rrr baseoutcome(0)
est store base 

mlogit buy emoStab if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit buy i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if emoStab != ., rrr baseoutcome(0)
est store base 

mlogit buy emoStab i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum emoStab if cca_confounder == 1 & buy != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(emoStab = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit buy emoStab i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(emoStab = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen emoStab = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 emoStab, col(black)) ///
	(line p1 emoStab, col(red)) ///
	(line p2 emoStab, col(blue)) ///
	(line p3 emoStab, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Emotional stability", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(emoStab_buy, replace)
	

use "B4293_ReligionAndClimate_Processed.dta", clear


*** Openness to experience

* Multinomial model unadjusted
mlogit buy open if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit buy if cca_confounders == 1 & open != ., rrr baseoutcome(0)
est store base 

mlogit buy open if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit buy i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if open != ., rrr baseoutcome(0)
est store base 

mlogit buy open i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum open if cca_confounder == 1 & buy != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(open = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit buy open i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(open = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen open = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 open, col(black)) ///
	(line p1 open, col(red)) ///
	(line p2 open, col(blue)) ///
	(line p3 open, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Openness to experience", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(open_buy, replace)
	
	
use "B4293_ReligionAndClimate_Processed.dta", clear


*** IQ

* Multinomial model unadjusted
mlogit buy iq if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit buy if cca_confounders == 1 & iq != ., rrr baseoutcome(0)
est store base 

mlogit buy iq if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit buy i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if iq != ., rrr baseoutcome(0)
est store base 

mlogit buy iq i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum iq if cca_confounder == 1 & buy != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(iq = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit buy iq i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(iq = (70(1)130))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen iq = fill(70 71)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 iq, col(black)) ///
	(line p1 iq, col(red)) ///
	(line p2 iq, col(blue)) ///
	(line p3 iq, col(green)), ///
	xscale(range(65 135)) xlabel(70(10)130, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("IQ scores", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(iq_buy, replace)
	

use "B4293_ReligionAndClimate_Processed.dta", clear



** Combine graphs together with a single legend using the 'grc1leg' user-written package
grc1leg extra_buy agree_buy consc_buy emoStab_buy open_buy iq_buy, rows(2) imargin(tiny) ycommon

graph export ".\Results\buy_predProbs.pdf", replace

graph close _all


*** Creating various post-files to store results to (individual multinomial RRRs and overall multinomial associations and r2)
capture postclose buy_mult
postfile buy_mult str30 exposure str30 outcome str30 out_level /// 
	n coef_uni lci_uni uci_uni double(p_uni) ///
	coef_mult lci_mult uci_mult double(p_mult) ///
	using ".\Results\buy_mult.dta", replace
	
capture postclose buy_mult_lr
postfile buy_mult_lr str30 exposure str30 outcome /// 
	n double(p_uni) r2_uni double(p_mult) r2_mult ///
	using ".\Results\buy_mult_lr.dta", replace

foreach var of varlist extra agree consc emoStab open iq {
	
	* Store exposure variable as a macro
	local exp = "`var'"
	
	* Store outcome as macro
	local out = "buy"
	
	** Univariable analysis
	mlogit buy `var' if cca_confounders == 1, baseoutcome(0) rrr
	
	* Store relevant summary statistics
	local n = e(N)
	local r2_uni = e(r2_p)
	local p_uni = e(p)
	
	* Store coefficients
	matrix res_uni = r(table)
	
	** Multivariable analysis - Repeat with and without exposure to get change in pseudo-R2 and LR test results
	mlogit buy i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if `var' != ., baseoutcome(0) rrr
	local r2_mult_base = e(r2_p)
	est store base
	
	mlogit buy `var' i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, baseoutcome(0) rrr
	local r2_mult = e(r2_p) - `r2_mult_base'
	est store main
	
	* Store coefficients
	matrix res_mult = r(table)
	
	* LR test
	lrtest base main
	local p_mult = r(p)
	
	** Post these summary results to the file
	post buy_mult_lr ("`exp'") ("`out'") (`n') (`p_uni') (`r2_uni') ///
		(`p_mult') (`r2_mult')
		
	
	** Now focus on coefficient results
	
	* Outcome value = 1/Yes for climate
	local out_level = "for climate"
	local coef_uni = res_uni[1,3]
	local lci_uni = res_uni[5,3]	
	local uci_uni = res_uni[6,3]
	local p_uni = res_uni[4,3]
	
	local coef_mult = res_mult[1,23]
	local lci_mult = res_mult[5,23]	
	local uci_mult = res_mult[6,23]
	local p_mult = res_mult[4,23]
	
	post buy_mult ("`exp'") ("`out'") ("`out_level'") (`n') /// 
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`coef_mult') ///
		(`lci_mult') (`uci_mult') (`p_mult')
	
	* Outcome value = 2/Yes, for non-climate
	local out_level = "for non-climate"
	local coef_uni = res_uni[1,5]
	local lci_uni = res_uni[5,5]	
	local uci_uni = res_uni[6,5]
	local p_uni = res_uni[4,5]
	
	local coef_mult = res_mult[1,45]
	local lci_mult = res_mult[5,45]	
	local uci_mult = res_mult[6,45]
	local p_mult = res_mult[4,45]
	
	post buy_mult ("`exp'") ("`out'") ("`out_level'") (`n') /// 
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`coef_mult') ///
		(`lci_mult') (`uci_mult') (`p_mult')
		
	* Outcome value = 3/Yes, for climate and non-climate
	local out_level = "for climate and non-climate"
	local coef_uni = res_uni[1,7]
	local lci_uni = res_uni[5,7]	
	local uci_uni = res_uni[6,7]
	local p_uni = res_uni[4,7]
	
	local coef_mult = res_mult[1,67]
	local lci_mult = res_mult[5,67]	
	local uci_mult = res_mult[6,67]
	local p_mult = res_mult[4,67]
	
	post buy_mult ("`exp'") ("`out'") ("`out_level'") (`n') /// 
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`coef_mult') ///
		(`lci_mult') (`uci_mult') (`p_mult')
		
}

postclose buy_mult
postclose buy_mult_lr

* Check results
use ".\Results\buy_mult_lr.dta", clear

export delimited using ".\Results\buy_mult_lr.csv", replace


use ".\Results\buy_mult.dta", clear

export delimited using ".\Results\buy_mult.csv", replace


** Read back in original dataset
use "B4293_ReligionAndClimate_Processed.dta", clear


**** Changed what buy (binary)

*** Extraversion 

* Logistic model unadjusted
logistic buy_bin extra if cca_confounders == 1

* And for adjusted model
logistic buy_bin extra i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum extra if cca_confounder == 1 & buy_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(extra = (`lower_sd', `mean', `upper_sd'))


*** Agreeableness 

* Logistic model unadjusted
logistic buy_bin agree if cca_confounders == 1

* And for adjusted model
logistic buy_bin agree i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum agree if cca_confounder == 1 & buy_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(agree = (`lower_sd', `mean', `upper_sd'))


*** Conscientiousness

* Logistic model unadjusted
logistic buy_bin consc if cca_confounders == 1

* And for adjusted model
logistic buy_bin consc i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum consc if cca_confounder == 1 & buy_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(consc = (`lower_sd', `mean', `upper_sd'))


*** Emotional stability

* Logistic model unadjusted
logistic buy_bin emoStab if cca_confounders == 1

* And for adjusted model
logistic buy_bin emoStab i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum emoStab if cca_confounder == 1 & buy_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(emoStab = (`lower_sd', `mean', `upper_sd'))


*** Openness to experience

* Logistic model unadjusted
logistic buy_bin open if cca_confounders == 1

* And for adjusted model
logistic buy_bin open i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum open if cca_confounder == 1 & buy_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(open = (`lower_sd', `mean', `upper_sd'))


*** IQ

* Logistic model unadjusted
logistic buy_bin iq if cca_confounders == 1

* And for adjusted model
logistic buy_bin iq i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum iq if cca_confounder == 1 & buy_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(iq = (`lower_sd', `mean', `upper_sd'))


*** Creating post-file to store logistic results to
capture postclose buy_bin
postfile buy_bin str30 exposure str30 outcome /// 
	n coef_uni lci_uni uci_uni double(p_uni) r2_uni ///
	coef_mult lci_mult uci_mult double(p_mult) r2_mult ///
	using ".\Results\buy_bin.dta", replace

foreach var of varlist extra agree consc emoStab open iq {
	
	* Store exposure variable as a macro
	local exp = "`var'"
	
	* Store outcome as macro
	local out = "buy_bin"
	
	** Univariable analysis
	logistic buy_bin `var' if cca_confounders == 1
	
	* Store relevant summary statistics
	local n = e(N)
	local r2_uni = e(r2_p)
	local p_uni = e(p)
	
	* Store coefficients
	matrix res = r(table)
	
	local coef_uni = res[1,1]
	local lci_uni = res[5,1]	
	local uci_uni = res[6,1]
	local p_uni = res[4,1]
	
	** Multivariable analysis - Repeat with and without exposure to get change in pseudo-R2 results
	logistic buy i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if `var' != .
	local r2_mult_base = e(r2_p)
	
	logistic buy `var' i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge
	local r2_mult = e(r2_p) - `r2_mult_base'
	
	* Store coefficients
	matrix res = r(table)
	
	local coef_mult = res[1,1]
	local lci_mult = res[5,1]	
	local uci_mult = res[6,1]
	local p_mult = res[4,1]
	
	** Post these summary results to the file
	post buy_bin ("`exp'") ("`out'") (`n') ///
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`r2_uni') ///
		(`coef_mult') (`lci_mult') (`uci_mult') ///
		(`p_mult') (`r2_mult')
		
}

postclose buy_bin

* Check results
use ".\Results\buy_bin.dta", clear

export delimited using ".\Results\buy_bin.csv", replace


** Read back in original dataset
use "B4293_ReligionAndClimate_Processed.dta", clear



*****************************************************************************************
**** Reduced air travel

*** Extraversion 

* Multinomial model unadjusted
mlogit airTravel extra if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit airTravel if cca_confounders == 1 & extra != ., rrr baseoutcome(0)
est store base 

mlogit airTravel extra if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit airTravel i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if extra != ., rrr baseoutcome(0)
est store base 

mlogit airTravel extra i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum extra if cca_confounder == 1 & airTravel != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(extra = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit airTravel extra i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(extra = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen extra = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 extra, col(black)) ///
	(line p1 extra, col(red)) ///
	(line p2 extra, col(blue)) ///
	(line p3 extra, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Extraversion", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(extra_airTravel, replace)

	
use "B4293_ReligionAndClimate_Processed.dta", clear


*** Agreeableness 

* Multinomial model unadjusted
mlogit airTravel agree if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit airTravel if cca_confounders == 1 & agree != ., rrr baseoutcome(0)
est store base 

mlogit airTravel agree if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit airTravel i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if agree != ., rrr baseoutcome(0)
est store base 

mlogit airTravel agree i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum agree if cca_confounder == 1 & airTravel != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(agree = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit airTravel agree i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(agree = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen agree = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 agree, col(black)) ///
	(line p1 agree, col(red)) ///
	(line p2 agree, col(blue)) ///
	(line p3 agree, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Agreeableness", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(agree_airTravel, replace)

	
use "B4293_ReligionAndClimate_Processed.dta", clear


*** Conscientiousness

* Multinomial model unadjusted
mlogit airTravel consc if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit airTravel if cca_confounders == 1 & consc != ., rrr baseoutcome(0)
est store base 

mlogit airTravel consc if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit airTravel i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if consc != ., rrr baseoutcome(0)
est store base 

mlogit airTravel consc i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum consc if cca_confounder == 1 & airTravel != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(consc = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit airTravel consc i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(consc = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen consc = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 consc, col(black)) ///
	(line p1 consc, col(red)) ///
	(line p2 consc, col(blue)) ///
	(line p3 consc, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Conscientiousness", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(consc_airTravel, replace)
	

use "B4293_ReligionAndClimate_Processed.dta", clear


*** Emotional stability

* Multinomial model unadjusted
mlogit airTravel emoStab if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit airTravel if cca_confounders == 1 & emoStab != ., rrr baseoutcome(0)
est store base 

mlogit airTravel emoStab if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit airTravel i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if emoStab != ., rrr baseoutcome(0)
est store base 

mlogit airTravel emoStab i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum emoStab if cca_confounder == 1 & airTravel != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(emoStab = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit airTravel emoStab i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(emoStab = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen emoStab = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 emoStab, col(black)) ///
	(line p1 emoStab, col(red)) ///
	(line p2 emoStab, col(blue)) ///
	(line p3 emoStab, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Emotional stability", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(emoStab_airTravel, replace)
	

use "B4293_ReligionAndClimate_Processed.dta", clear


*** Openness to experience

* Multinomial model unadjusted
mlogit airTravel open if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit airTravel if cca_confounders == 1 & open != ., rrr baseoutcome(0)
est store base 

mlogit airTravel open if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit airTravel i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if open != ., rrr baseoutcome(0)
est store base 

mlogit airTravel open i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum open if cca_confounder == 1 & airTravel != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(open = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit airTravel open i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(open = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen open = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 open, col(black)) ///
	(line p1 open, col(red)) ///
	(line p2 open, col(blue)) ///
	(line p3 open, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Openness to experience", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(open_airTravel, replace)
	
	
use "B4293_ReligionAndClimate_Processed.dta", clear


*** IQ

* Multinomial model unadjusted
mlogit airTravel iq if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit airTravel if cca_confounders == 1 & iq != ., rrr baseoutcome(0)
est store base 

mlogit airTravel iq if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit airTravel i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if iq != ., rrr baseoutcome(0)
est store base 

mlogit airTravel iq i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum iq if cca_confounder == 1 & airTravel != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(iq = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit airTravel iq i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(iq = (70(1)130))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen iq = fill(70 71)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 iq, col(black)) ///
	(line p1 iq, col(red)) ///
	(line p2 iq, col(blue)) ///
	(line p3 iq, col(green)), ///
	xscale(range(65 135)) xlabel(70(10)130, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("IQ scores", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(iq_airTravel, replace)
	

use "B4293_ReligionAndClimate_Processed.dta", clear



** Combine graphs together with a single legend using the 'grc1leg' user-written package
grc1leg extra_airTravel agree_airTravel consc_airTravel emoStab_airTravel open_airTravel iq_airTravel, rows(2) imargin(tiny) ycommon

graph export ".\Results\airTravel_predProbs.pdf", replace

graph close _all


*** Creating various post-files to store results to (individual multinomial RRRs and overall multinomial associations and r2)
capture postclose airTravel_mult
postfile airTravel_mult str30 exposure str30 outcome str30 out_level /// 
	n coef_uni lci_uni uci_uni double(p_uni) ///
	coef_mult lci_mult uci_mult double(p_mult) ///
	using ".\Results\airTravel_mult.dta", replace
	
capture postclose airTravel_mult_lr
postfile airTravel_mult_lr str30 exposure str30 outcome /// 
	n double(p_uni) r2_uni double(p_mult) r2_mult ///
	using ".\Results\airTravel_mult_lr.dta", replace

foreach var of varlist extra agree consc emoStab open iq {
	
	* Store exposure variable as a macro
	local exp = "`var'"
	
	* Store outcome as macro
	local out = "airTravel"
	
	** Univariable analysis
	mlogit airTravel `var' if cca_confounders == 1, baseoutcome(0) rrr
	
	* Store relevant summary statistics
	local n = e(N)
	local r2_uni = e(r2_p)
	local p_uni = e(p)
	
	* Store coefficients
	matrix res_uni = r(table)
	
	** Multivariable analysis - Repeat with and without exposure to get change in pseudo-R2 and LR test results
	mlogit airTravel i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if `var' != ., baseoutcome(0) rrr
	local r2_mult_base = e(r2_p)
	est store base
	
	mlogit airTravel `var' i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, baseoutcome(0) rrr
	local r2_mult = e(r2_p) - `r2_mult_base'
	est store main
	
	* Store coefficients
	matrix res_mult = r(table)
	
	* LR test
	lrtest base main
	local p_mult = r(p)
	
	** Post these summary results to the file
	post airTravel_mult_lr ("`exp'") ("`out'") (`n') (`p_uni') (`r2_uni') ///
		(`p_mult') (`r2_mult')
		
	
	** Now focus on coefficient results
	
	* Outcome value = 1/Yes for climate
	local out_level = "for climate"
	local coef_uni = res_uni[1,3]
	local lci_uni = res_uni[5,3]	
	local uci_uni = res_uni[6,3]
	local p_uni = res_uni[4,3]
	
	local coef_mult = res_mult[1,23]
	local lci_mult = res_mult[5,23]	
	local uci_mult = res_mult[6,23]
	local p_mult = res_mult[4,23]
	
	post airTravel_mult ("`exp'") ("`out'") ("`out_level'") (`n') /// 
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`coef_mult') ///
		(`lci_mult') (`uci_mult') (`p_mult')
	
	* Outcome value = 2/Yes, for non-climate
	local out_level = "for non-climate"
	local coef_uni = res_uni[1,5]
	local lci_uni = res_uni[5,5]	
	local uci_uni = res_uni[6,5]
	local p_uni = res_uni[4,5]
	
	local coef_mult = res_mult[1,45]
	local lci_mult = res_mult[5,45]	
	local uci_mult = res_mult[6,45]
	local p_mult = res_mult[4,45]
	
	post airTravel_mult ("`exp'") ("`out'") ("`out_level'") (`n') /// 
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`coef_mult') ///
		(`lci_mult') (`uci_mult') (`p_mult')
		
	* Outcome value = 3/Yes, for climate and non-climate
	local out_level = "for climate and non-climate"
	local coef_uni = res_uni[1,7]
	local lci_uni = res_uni[5,7]	
	local uci_uni = res_uni[6,7]
	local p_uni = res_uni[4,7]
	
	local coef_mult = res_mult[1,67]
	local lci_mult = res_mult[5,67]	
	local uci_mult = res_mult[6,67]
	local p_mult = res_mult[4,67]
	
	post airTravel_mult ("`exp'") ("`out'") ("`out_level'") (`n') /// 
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`coef_mult') ///
		(`lci_mult') (`uci_mult') (`p_mult')
		
}

postclose airTravel_mult
postclose airTravel_mult_lr

* Check results
use ".\Results\airTravel_mult_lr.dta", clear

export delimited using ".\Results\airTravel_mult_lr.csv", replace


use ".\Results\airTravel_mult.dta", clear

export delimited using ".\Results\airTravel_mult.csv", replace


** Read back in original dataset
use "B4293_ReligionAndClimate_Processed.dta", clear


**** Reduced air travel (binary)

*** Extraversion 

* Logistic model unadjusted
logistic airTravel_bin extra if cca_confounders == 1

* And for adjusted model
logistic airTravel_bin extra i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum extra if cca_confounder == 1 & airTravel_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(extra = (`lower_sd', `mean', `upper_sd'))


*** Agreeableness 

* Logistic model unadjusted
logistic airTravel_bin agree if cca_confounders == 1

* And for adjusted model
logistic airTravel_bin agree i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum agree if cca_confounder == 1 & airTravel_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(agree = (`lower_sd', `mean', `upper_sd'))


*** Conscientiousness

* Logistic model unadjusted
logistic airTravel_bin consc if cca_confounders == 1

* And for adjusted model
logistic airTravel_bin consc i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum consc if cca_confounder == 1 & airTravel_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(consc = (`lower_sd', `mean', `upper_sd'))


*** Emotional stability

* Logistic model unadjusted
logistic airTravel_bin emoStab if cca_confounders == 1

* And for adjusted model
logistic airTravel_bin emoStab i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum emoStab if cca_confounder == 1 & airTravel_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(emoStab = (`lower_sd', `mean', `upper_sd'))


*** Openness to experience

* Logistic model unadjusted
logistic airTravel_bin open if cca_confounders == 1

* And for adjusted model
logistic airTravel_bin open i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum open if cca_confounder == 1 & airTravel_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(open = (`lower_sd', `mean', `upper_sd'))


*** IQ

* Logistic model unadjusted
logistic airTravel_bin iq if cca_confounders == 1

* And for adjusted model
logistic airTravel_bin iq i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum iq if cca_confounder == 1 & airTravel_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(iq = (`lower_sd', `mean', `upper_sd'))


*** Creating post-file to store logistic results to
capture postclose airTravel_bin
postfile airTravel_bin str30 exposure str30 outcome /// 
	n coef_uni lci_uni uci_uni double(p_uni) r2_uni ///
	coef_mult lci_mult uci_mult double(p_mult) r2_mult ///
	using ".\Results\airTravel_bin.dta", replace

foreach var of varlist extra agree consc emoStab open iq {
	
	* Store exposure variable as a macro
	local exp = "`var'"
	
	* Store outcome as macro
	local out = "airTravel_bin"
	
	** Univariable analysis
	logistic airTravel_bin `var' if cca_confounders == 1
	
	* Store relevant summary statistics
	local n = e(N)
	local r2_uni = e(r2_p)
	local p_uni = e(p)
	
	* Store coefficients
	matrix res = r(table)
	
	local coef_uni = res[1,1]
	local lci_uni = res[5,1]	
	local uci_uni = res[6,1]
	local p_uni = res[4,1]
	
	** Multivariable analysis - Repeat with and without exposure to get change in pseudo-R2 results
	logistic airTravel i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if `var' != .
	local r2_mult_base = e(r2_p)
	
	logistic airTravel `var' i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge
	local r2_mult = e(r2_p) - `r2_mult_base'
	
	* Store coefficients
	matrix res = r(table)
	
	local coef_mult = res[1,1]
	local lci_mult = res[5,1]	
	local uci_mult = res[6,1]
	local p_mult = res[4,1]
	
	** Post these summary results to the file
	post airTravel_bin ("`exp'") ("`out'") (`n') ///
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`r2_uni') ///
		(`coef_mult') (`lci_mult') (`uci_mult') ///
		(`p_mult') (`r2_mult')
		
}

postclose airTravel_bin

* Check results
use ".\Results\airTravel_bin.dta", clear

export delimited using ".\Results\airTravel_bin.csv", replace


** Read back in original dataset
use "B4293_ReligionAndClimate_Processed.dta", clear



*****************************************************************************************
**** Used electric car

*** Extraversion 

* Multinomial model unadjusted
mlogit elecCar extra if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit elecCar if cca_confounders == 1 & extra != ., rrr baseoutcome(0)
est store base 

mlogit elecCar extra if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit elecCar i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if extra != ., rrr baseoutcome(0)
est store base 

mlogit elecCar extra i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum extra if cca_confounder == 1 & elecCar != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(extra = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit elecCar extra i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(extra = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen extra = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 extra, col(black)) ///
	(line p1 extra, col(red)) ///
	(line p2 extra, col(blue)) ///
	(line p3 extra, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Extraversion", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(extra_elecCar, replace)

	
use "B4293_ReligionAndClimate_Processed.dta", clear


*** Agreeableness 

* Multinomial model unadjusted
mlogit elecCar agree if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit elecCar if cca_confounders == 1 & agree != ., rrr baseoutcome(0)
est store base 

mlogit elecCar agree if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit elecCar i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if agree != ., rrr baseoutcome(0)
est store base 

mlogit elecCar agree i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum agree if cca_confounder == 1 & elecCar != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(agree = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit elecCar agree i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(agree = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen agree = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 agree, col(black)) ///
	(line p1 agree, col(red)) ///
	(line p2 agree, col(blue)) ///
	(line p3 agree, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Agreeableness", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(agree_elecCar, replace)

	
use "B4293_ReligionAndClimate_Processed.dta", clear


*** Conscientiousness

* Multinomial model unadjusted
mlogit elecCar consc if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit elecCar if cca_confounders == 1 & consc != ., rrr baseoutcome(0)
est store base 

mlogit elecCar consc if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit elecCar i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if consc != ., rrr baseoutcome(0)
est store base 

mlogit elecCar consc i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum consc if cca_confounder == 1 & elecCar != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(consc = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit elecCar consc i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(consc = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen consc = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 consc, col(black)) ///
	(line p1 consc, col(red)) ///
	(line p2 consc, col(blue)) ///
	(line p3 consc, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Conscientiousness", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(consc_elecCar, replace)
	

use "B4293_ReligionAndClimate_Processed.dta", clear


*** Emotional stability

* Multinomial model unadjusted
mlogit elecCar emoStab if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit elecCar if cca_confounders == 1 & emoStab != ., rrr baseoutcome(0)
est store base 

mlogit elecCar emoStab if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit elecCar i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if emoStab != ., rrr baseoutcome(0)
est store base 

mlogit elecCar emoStab i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum emoStab if cca_confounder == 1 & elecCar != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(emoStab = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit elecCar emoStab i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(emoStab = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen emoStab = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 emoStab, col(black)) ///
	(line p1 emoStab, col(red)) ///
	(line p2 emoStab, col(blue)) ///
	(line p3 emoStab, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Emotional stability", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(emoStab_elecCar, replace)
	

use "B4293_ReligionAndClimate_Processed.dta", clear


*** Openness to experience

* Multinomial model unadjusted
mlogit elecCar open if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit elecCar if cca_confounders == 1 & open != ., rrr baseoutcome(0)
est store base 

mlogit elecCar open if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit elecCar i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if open != ., rrr baseoutcome(0)
est store base 

mlogit elecCar open i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum open if cca_confounder == 1 & elecCar != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(open = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit elecCar open i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(open = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen open = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 open, col(black)) ///
	(line p1 open, col(red)) ///
	(line p2 open, col(blue)) ///
	(line p3 open, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Openness to experience", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(open_elecCar, replace)
	
	
use "B4293_ReligionAndClimate_Processed.dta", clear


*** IQ

* Multinomial model unadjusted
mlogit elecCar iq if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit elecCar if cca_confounders == 1 & iq != ., rrr baseoutcome(0)
est store base 

mlogit elecCar iq if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit elecCar i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if iq != ., rrr baseoutcome(0)
est store base 

mlogit elecCar iq i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum iq if cca_confounder == 1 & elecCar != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(iq = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit elecCar iq i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(iq = (70(1)130))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen iq = fill(70 71)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 iq, col(black)) ///
	(line p1 iq, col(red)) ///
	(line p2 iq, col(blue)) ///
	(line p3 iq, col(green)), ///
	xscale(range(65 135)) xlabel(70(10)130, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("IQ scores", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(iq_elecCar, replace)
	

use "B4293_ReligionAndClimate_Processed.dta", clear



** Combine graphs together with a single legend using the 'grc1leg' user-written package
grc1leg extra_elecCar agree_elecCar consc_elecCar emoStab_elecCar open_elecCar iq_elecCar, rows(2) imargin(tiny) ycommon

graph export ".\Results\elecCar_predProbs.pdf", replace

graph close _all


*** Creating various post-files to store results to (individual multinomial RRRs and overall multinomial associations and r2)
capture postclose elecCar_mult
postfile elecCar_mult str30 exposure str30 outcome str30 out_level /// 
	n coef_uni lci_uni uci_uni double(p_uni) ///
	coef_mult lci_mult uci_mult double(p_mult) ///
	using ".\Results\elecCar_mult.dta", replace
	
capture postclose elecCar_mult_lr
postfile elecCar_mult_lr str30 exposure str30 outcome /// 
	n double(p_uni) r2_uni double(p_mult) r2_mult ///
	using ".\Results\elecCar_mult_lr.dta", replace

foreach var of varlist extra agree consc emoStab open iq {
	
	* Store exposure variable as a macro
	local exp = "`var'"
	
	* Store outcome as macro
	local out = "elecCar"
	
	** Univariable analysis
	mlogit elecCar `var' if cca_confounders == 1, baseoutcome(0) rrr
	
	* Store relevant summary statistics
	local n = e(N)
	local r2_uni = e(r2_p)
	local p_uni = e(p)
	
	* Store coefficients
	matrix res_uni = r(table)
	
	** Multivariable analysis - Repeat with and without exposure to get change in pseudo-R2 and LR test results
	mlogit elecCar i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if `var' != ., baseoutcome(0) rrr
	local r2_mult_base = e(r2_p)
	est store base
	
	mlogit elecCar `var' i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, baseoutcome(0) rrr
	local r2_mult = e(r2_p) - `r2_mult_base'
	est store main
	
	* Store coefficients
	matrix res_mult = r(table)
	
	* LR test
	lrtest base main
	local p_mult = r(p)
	
	** Post these summary results to the file
	post elecCar_mult_lr ("`exp'") ("`out'") (`n') (`p_uni') (`r2_uni') ///
		(`p_mult') (`r2_mult')
		
	
	** Now focus on coefficient results
	
	* Outcome value = 1/Yes for climate
	local out_level = "for climate"
	local coef_uni = res_uni[1,3]
	local lci_uni = res_uni[5,3]	
	local uci_uni = res_uni[6,3]
	local p_uni = res_uni[4,3]
	
	local coef_mult = res_mult[1,23]
	local lci_mult = res_mult[5,23]	
	local uci_mult = res_mult[6,23]
	local p_mult = res_mult[4,23]
	
	post elecCar_mult ("`exp'") ("`out'") ("`out_level'") (`n') /// 
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`coef_mult') ///
		(`lci_mult') (`uci_mult') (`p_mult')
	
	* Outcome value = 2/Yes, for non-climate
	local out_level = "for non-climate"
	local coef_uni = res_uni[1,5]
	local lci_uni = res_uni[5,5]	
	local uci_uni = res_uni[6,5]
	local p_uni = res_uni[4,5]
	
	local coef_mult = res_mult[1,45]
	local lci_mult = res_mult[5,45]	
	local uci_mult = res_mult[6,45]
	local p_mult = res_mult[4,45]
	
	post elecCar_mult ("`exp'") ("`out'") ("`out_level'") (`n') /// 
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`coef_mult') ///
		(`lci_mult') (`uci_mult') (`p_mult')
		
	* Outcome value = 3/Yes, for climate and non-climate
	local out_level = "for climate and non-climate"
	local coef_uni = res_uni[1,7]
	local lci_uni = res_uni[5,7]	
	local uci_uni = res_uni[6,7]
	local p_uni = res_uni[4,7]
	
	local coef_mult = res_mult[1,67]
	local lci_mult = res_mult[5,67]	
	local uci_mult = res_mult[6,67]
	local p_mult = res_mult[4,67]
	
	post elecCar_mult ("`exp'") ("`out'") ("`out_level'") (`n') /// 
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`coef_mult') ///
		(`lci_mult') (`uci_mult') (`p_mult')
		
}

postclose elecCar_mult
postclose elecCar_mult_lr

* Check results
use ".\Results\elecCar_mult_lr.dta", clear

export delimited using ".\Results\elecCar_mult_lr.csv", replace


use ".\Results\elecCar_mult.dta", clear

export delimited using ".\Results\elecCar_mult.csv", replace


** Read back in original dataset
use "B4293_ReligionAndClimate_Processed.dta", clear


**** Used electric car (binary)

*** Extraversion 

* Logistic model unadjusted
logistic elecCar_bin extra if cca_confounders == 1

* And for adjusted model
logistic elecCar_bin extra i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum extra if cca_confounder == 1 & elecCar_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(extra = (`lower_sd', `mean', `upper_sd'))


*** Agreeableness 

* Logistic model unadjusted
logistic elecCar_bin agree if cca_confounders == 1

* And for adjusted model
logistic elecCar_bin agree i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum agree if cca_confounder == 1 & elecCar_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(agree = (`lower_sd', `mean', `upper_sd'))


*** Conscientiousness

* Logistic model unadjusted
logistic elecCar_bin consc if cca_confounders == 1

* And for adjusted model
logistic elecCar_bin consc i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum consc if cca_confounder == 1 & elecCar_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(consc = (`lower_sd', `mean', `upper_sd'))


*** Emotional stability

* Logistic model unadjusted
logistic elecCar_bin emoStab if cca_confounders == 1

* And for adjusted model
logistic elecCar_bin emoStab i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum emoStab if cca_confounder == 1 & elecCar_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(emoStab = (`lower_sd', `mean', `upper_sd'))


*** Openness to experience

* Logistic model unadjusted
logistic elecCar_bin open if cca_confounders == 1

* And for adjusted model
logistic elecCar_bin open i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum open if cca_confounder == 1 & elecCar_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(open = (`lower_sd', `mean', `upper_sd'))


*** IQ

* Logistic model unadjusted
logistic elecCar_bin iq if cca_confounders == 1

* And for adjusted model
logistic elecCar_bin iq i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum iq if cca_confounder == 1 & elecCar_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(iq = (`lower_sd', `mean', `upper_sd'))


*** Creating post-file to store logistic results to
capture postclose elecCar_bin
postfile elecCar_bin str30 exposure str30 outcome /// 
	n coef_uni lci_uni uci_uni double(p_uni) r2_uni ///
	coef_mult lci_mult uci_mult double(p_mult) r2_mult ///
	using ".\Results\elecCar_bin.dta", replace

foreach var of varlist extra agree consc emoStab open iq {
	
	* Store exposure variable as a macro
	local exp = "`var'"
	
	* Store outcome as macro
	local out = "elecCar_bin"
	
	** Univariable analysis
	logistic elecCar_bin `var' if cca_confounders == 1
	
	* Store relevant summary statistics
	local n = e(N)
	local r2_uni = e(r2_p)
	local p_uni = e(p)
	
	* Store coefficients
	matrix res = r(table)
	
	local coef_uni = res[1,1]
	local lci_uni = res[5,1]	
	local uci_uni = res[6,1]
	local p_uni = res[4,1]
	
	** Multivariable analysis - Repeat with and without exposure to get change in pseudo-R2 results
	logistic elecCar i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if `var' != .
	local r2_mult_base = e(r2_p)
	
	logistic elecCar `var' i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge
	local r2_mult = e(r2_p) - `r2_mult_base'
	
	* Store coefficients
	matrix res = r(table)
	
	local coef_mult = res[1,1]
	local lci_mult = res[5,1]	
	local uci_mult = res[6,1]
	local p_mult = res[4,1]
	
	** Post these summary results to the file
	post elecCar_bin ("`exp'") ("`out'") (`n') ///
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`r2_uni') ///
		(`coef_mult') (`lci_mult') (`uci_mult') ///
		(`p_mult') (`r2_mult')
		
}

postclose elecCar_bin

* Check results
use ".\Results\elecCar_bin.dta", clear

export delimited using ".\Results\elecCar_bin.csv", replace


** Read back in original dataset
use "B4293_ReligionAndClimate_Processed.dta", clear



*****************************************************************************************
**** Purchased local food

*** Extraversion 

* Multinomial model unadjusted
mlogit localFood extra if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit localFood if cca_confounders == 1 & extra != ., rrr baseoutcome(0)
est store base 

mlogit localFood extra if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit localFood i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if extra != ., rrr baseoutcome(0)
est store base 

mlogit localFood extra i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum extra if cca_confounder == 1 & localFood != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(extra = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit localFood extra i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(extra = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen extra = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 extra, col(black)) ///
	(line p1 extra, col(red)) ///
	(line p2 extra, col(blue)) ///
	(line p3 extra, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Extraversion", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(extra_localFood, replace)

	
use "B4293_ReligionAndClimate_Processed.dta", clear


*** Agreeableness 

* Multinomial model unadjusted
mlogit localFood agree if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit localFood if cca_confounders == 1 & agree != ., rrr baseoutcome(0)
est store base 

mlogit localFood agree if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit localFood i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if agree != ., rrr baseoutcome(0)
est store base 

mlogit localFood agree i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum agree if cca_confounder == 1 & localFood != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(agree = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit localFood agree i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(agree = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen agree = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 agree, col(black)) ///
	(line p1 agree, col(red)) ///
	(line p2 agree, col(blue)) ///
	(line p3 agree, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Agreeableness", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(agree_localFood, replace)

	
use "B4293_ReligionAndClimate_Processed.dta", clear


*** Conscientiousness

* Multinomial model unadjusted
mlogit localFood consc if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit localFood if cca_confounders == 1 & consc != ., rrr baseoutcome(0)
est store base 

mlogit localFood consc if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit localFood i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if consc != ., rrr baseoutcome(0)
est store base 

mlogit localFood consc i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum consc if cca_confounder == 1 & localFood != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(consc = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit localFood consc i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(consc = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen consc = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 consc, col(black)) ///
	(line p1 consc, col(red)) ///
	(line p2 consc, col(blue)) ///
	(line p3 consc, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Conscientiousness", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(consc_localFood, replace)
	

use "B4293_ReligionAndClimate_Processed.dta", clear


*** Emotional stability

* Multinomial model unadjusted
mlogit localFood emoStab if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit localFood if cca_confounders == 1 & emoStab != ., rrr baseoutcome(0)
est store base 

mlogit localFood emoStab if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit localFood i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if emoStab != ., rrr baseoutcome(0)
est store base 

mlogit localFood emoStab i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum emoStab if cca_confounder == 1 & localFood != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(emoStab = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit localFood emoStab i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(emoStab = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen emoStab = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 emoStab, col(black)) ///
	(line p1 emoStab, col(red)) ///
	(line p2 emoStab, col(blue)) ///
	(line p3 emoStab, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Emotional stability", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(emoStab_localFood, replace)
	

use "B4293_ReligionAndClimate_Processed.dta", clear


*** Openness to experience

* Multinomial model unadjusted
mlogit localFood open if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit localFood if cca_confounders == 1 & open != ., rrr baseoutcome(0)
est store base 

mlogit localFood open if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit localFood i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if open != ., rrr baseoutcome(0)
est store base 

mlogit localFood open i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum open if cca_confounder == 1 & localFood != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(open = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit localFood open i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(open = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen open = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 open, col(black)) ///
	(line p1 open, col(red)) ///
	(line p2 open, col(blue)) ///
	(line p3 open, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Openness to experience", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(open_localFood, replace)
	
	
use "B4293_ReligionAndClimate_Processed.dta", clear


*** IQ

* Multinomial model unadjusted
mlogit localFood iq if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit localFood if cca_confounders == 1 & iq != ., rrr baseoutcome(0)
est store base 

mlogit localFood iq if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit localFood i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if iq != ., rrr baseoutcome(0)
est store base 

mlogit localFood iq i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum iq if cca_confounder == 1 & localFood != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(iq = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit localFood iq i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(iq = (70(1)130))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen iq = fill(70 71)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 iq, col(black)) ///
	(line p1 iq, col(red)) ///
	(line p2 iq, col(blue)) ///
	(line p3 iq, col(green)), ///
	xscale(range(65 135)) xlabel(70(10)130, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("IQ scores", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(iq_localFood, replace)
	

use "B4293_ReligionAndClimate_Processed.dta", clear



** Combine graphs together with a single legend using the 'grc1leg' user-written package
grc1leg extra_localFood agree_localFood consc_localFood emoStab_localFood open_localFood iq_localFood, rows(2) imargin(tiny) ycommon

graph export ".\Results\localFood_predProbs.pdf", replace

graph close _all


*** Creating various post-files to store results to (individual multinomial RRRs and overall multinomial associations and r2)
capture postclose localFood_mult
postfile localFood_mult str30 exposure str30 outcome str30 out_level /// 
	n coef_uni lci_uni uci_uni double(p_uni) ///
	coef_mult lci_mult uci_mult double(p_mult) ///
	using ".\Results\localFood_mult.dta", replace
	
capture postclose localFood_mult_lr
postfile localFood_mult_lr str30 exposure str30 outcome /// 
	n double(p_uni) r2_uni double(p_mult) r2_mult ///
	using ".\Results\localFood_mult_lr.dta", replace

foreach var of varlist extra agree consc emoStab open iq {
	
	* Store exposure variable as a macro
	local exp = "`var'"
	
	* Store outcome as macro
	local out = "localFood"
	
	** Univariable analysis
	mlogit localFood `var' if cca_confounders == 1, baseoutcome(0) rrr
	
	* Store relevant summary statistics
	local n = e(N)
	local r2_uni = e(r2_p)
	local p_uni = e(p)
	
	* Store coefficients
	matrix res_uni = r(table)
	
	** Multivariable analysis - Repeat with and without exposure to get change in pseudo-R2 and LR test results
	mlogit localFood i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if `var' != ., baseoutcome(0) rrr
	local r2_mult_base = e(r2_p)
	est store base
	
	mlogit localFood `var' i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, baseoutcome(0) rrr
	local r2_mult = e(r2_p) - `r2_mult_base'
	est store main
	
	* Store coefficients
	matrix res_mult = r(table)
	
	* LR test
	lrtest base main
	local p_mult = r(p)
	
	** Post these summary results to the file
	post localFood_mult_lr ("`exp'") ("`out'") (`n') (`p_uni') (`r2_uni') ///
		(`p_mult') (`r2_mult')
		
	
	** Now focus on coefficient results
	
	* Outcome value = 1/Yes for climate
	local out_level = "for climate"
	local coef_uni = res_uni[1,3]
	local lci_uni = res_uni[5,3]	
	local uci_uni = res_uni[6,3]
	local p_uni = res_uni[4,3]
	
	local coef_mult = res_mult[1,23]
	local lci_mult = res_mult[5,23]	
	local uci_mult = res_mult[6,23]
	local p_mult = res_mult[4,23]
	
	post localFood_mult ("`exp'") ("`out'") ("`out_level'") (`n') /// 
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`coef_mult') ///
		(`lci_mult') (`uci_mult') (`p_mult')
	
	* Outcome value = 2/Yes, for non-climate
	local out_level = "for non-climate"
	local coef_uni = res_uni[1,5]
	local lci_uni = res_uni[5,5]	
	local uci_uni = res_uni[6,5]
	local p_uni = res_uni[4,5]
	
	local coef_mult = res_mult[1,45]
	local lci_mult = res_mult[5,45]	
	local uci_mult = res_mult[6,45]
	local p_mult = res_mult[4,45]
	
	post localFood_mult ("`exp'") ("`out'") ("`out_level'") (`n') /// 
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`coef_mult') ///
		(`lci_mult') (`uci_mult') (`p_mult')
		
	* Outcome value = 3/Yes, for climate and non-climate
	local out_level = "for climate and non-climate"
	local coef_uni = res_uni[1,7]
	local lci_uni = res_uni[5,7]	
	local uci_uni = res_uni[6,7]
	local p_uni = res_uni[4,7]
	
	local coef_mult = res_mult[1,67]
	local lci_mult = res_mult[5,67]	
	local uci_mult = res_mult[6,67]
	local p_mult = res_mult[4,67]
	
	post localFood_mult ("`exp'") ("`out'") ("`out_level'") (`n') /// 
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`coef_mult') ///
		(`lci_mult') (`uci_mult') (`p_mult')
		
}

postclose localFood_mult
postclose localFood_mult_lr

* Check results
use ".\Results\localFood_mult_lr.dta", clear

export delimited using ".\Results\localFood_mult_lr.csv", replace


use ".\Results\localFood_mult.dta", clear

export delimited using ".\Results\localFood_mult.csv", replace


** Read back in original dataset
use "B4293_ReligionAndClimate_Processed.dta", clear


**** Purchased local food (binary)

*** Extraversion 

* Logistic model unadjusted
logistic localFood_bin extra if cca_confounders == 1

* And for adjusted model
logistic localFood_bin extra i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum extra if cca_confounder == 1 & localFood_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(extra = (`lower_sd', `mean', `upper_sd'))


*** Agreeableness 

* Logistic model unadjusted
logistic localFood_bin agree if cca_confounders == 1

* And for adjusted model
logistic localFood_bin agree i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum agree if cca_confounder == 1 & localFood_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(agree = (`lower_sd', `mean', `upper_sd'))


*** Conscientiousness

* Logistic model unadjusted
logistic localFood_bin consc if cca_confounders == 1

* And for adjusted model
logistic localFood_bin consc i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum consc if cca_confounder == 1 & localFood_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(consc = (`lower_sd', `mean', `upper_sd'))


*** Emotional stability

* Logistic model unadjusted
logistic localFood_bin emoStab if cca_confounders == 1

* And for adjusted model
logistic localFood_bin emoStab i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum emoStab if cca_confounder == 1 & localFood_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(emoStab = (`lower_sd', `mean', `upper_sd'))


*** Openness to experience

* Logistic model unadjusted
logistic localFood_bin open if cca_confounders == 1

* And for adjusted model
logistic localFood_bin open i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum open if cca_confounder == 1 & localFood_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(open = (`lower_sd', `mean', `upper_sd'))


*** IQ

* Logistic model unadjusted
logistic localFood_bin iq if cca_confounders == 1

* And for adjusted model
logistic localFood_bin iq i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum iq if cca_confounder == 1 & localFood_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(iq = (`lower_sd', `mean', `upper_sd'))


*** Creating post-file to store logistic results to
capture postclose localFood_bin
postfile localFood_bin str30 exposure str30 outcome /// 
	n coef_uni lci_uni uci_uni double(p_uni) r2_uni ///
	coef_mult lci_mult uci_mult double(p_mult) r2_mult ///
	using ".\Results\localFood_bin.dta", replace

foreach var of varlist extra agree consc emoStab open iq {
	
	* Store exposure variable as a macro
	local exp = "`var'"
	
	* Store outcome as macro
	local out = "localFood_bin"
	
	** Univariable analysis
	logistic localFood_bin `var' if cca_confounders == 1
	
	* Store relevant summary statistics
	local n = e(N)
	local r2_uni = e(r2_p)
	local p_uni = e(p)
	
	* Store coefficients
	matrix res = r(table)
	
	local coef_uni = res[1,1]
	local lci_uni = res[5,1]	
	local uci_uni = res[6,1]
	local p_uni = res[4,1]
	
	** Multivariable analysis - Repeat with and without exposure to get change in pseudo-R2 results
	logistic localFood i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if `var' != .
	local r2_mult_base = e(r2_p)
	
	logistic localFood `var' i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge
	local r2_mult = e(r2_p) - `r2_mult_base'
	
	* Store coefficients
	matrix res = r(table)
	
	local coef_mult = res[1,1]
	local lci_mult = res[5,1]	
	local uci_mult = res[6,1]
	local p_mult = res[4,1]
	
	** Post these summary results to the file
	post localFood_bin ("`exp'") ("`out'") (`n') ///
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`r2_uni') ///
		(`coef_mult') (`lci_mult') (`uci_mult') ///
		(`p_mult') (`r2_mult')
		
}

postclose localFood_bin

* Check results
use ".\Results\localFood_bin.dta", clear

export delimited using ".\Results\localFood_bin.csv", replace


** Read back in original dataset
use "B4293_ReligionAndClimate_Processed.dta", clear



*****************************************************************************************
**** Recycled more

*** Extraversion 

* Multinomial model unadjusted
mlogit recycle extra if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit recycle if cca_confounders == 1 & extra != ., rrr baseoutcome(0)
est store base 

mlogit recycle extra if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit recycle i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if extra != ., rrr baseoutcome(0)
est store base 

mlogit recycle extra i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum extra if cca_confounder == 1 & recycle != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(extra = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit recycle extra i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(extra = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen extra = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 extra, col(black)) ///
	(line p1 extra, col(red)) ///
	(line p2 extra, col(blue)) ///
	(line p3 extra, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Extraversion", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(extra_recycle, replace)

	
use "B4293_ReligionAndClimate_Processed.dta", clear


*** Agreeableness 

* Multinomial model unadjusted
mlogit recycle agree if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit recycle if cca_confounders == 1 & agree != ., rrr baseoutcome(0)
est store base 

mlogit recycle agree if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit recycle i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if agree != ., rrr baseoutcome(0)
est store base 

mlogit recycle agree i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum agree if cca_confounder == 1 & recycle != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(agree = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit recycle agree i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(agree = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen agree = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 agree, col(black)) ///
	(line p1 agree, col(red)) ///
	(line p2 agree, col(blue)) ///
	(line p3 agree, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Agreeableness", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(agree_recycle, replace)

	
use "B4293_ReligionAndClimate_Processed.dta", clear


*** Conscientiousness

* Multinomial model unadjusted
mlogit recycle consc if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit recycle if cca_confounders == 1 & consc != ., rrr baseoutcome(0)
est store base 

mlogit recycle consc if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit recycle i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if consc != ., rrr baseoutcome(0)
est store base 

mlogit recycle consc i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum consc if cca_confounder == 1 & recycle != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(consc = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit recycle consc i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(consc = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen consc = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 consc, col(black)) ///
	(line p1 consc, col(red)) ///
	(line p2 consc, col(blue)) ///
	(line p3 consc, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Conscientiousness", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(consc_recycle, replace)
	

use "B4293_ReligionAndClimate_Processed.dta", clear


*** Emotional stability

* Multinomial model unadjusted
mlogit recycle emoStab if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit recycle if cca_confounders == 1 & emoStab != ., rrr baseoutcome(0)
est store base 

mlogit recycle emoStab if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit recycle i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if emoStab != ., rrr baseoutcome(0)
est store base 

mlogit recycle emoStab i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum emoStab if cca_confounder == 1 & recycle != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(emoStab = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit recycle emoStab i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(emoStab = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen emoStab = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 emoStab, col(black)) ///
	(line p1 emoStab, col(red)) ///
	(line p2 emoStab, col(blue)) ///
	(line p3 emoStab, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Emotional stability", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(emoStab_recycle, replace)
	

use "B4293_ReligionAndClimate_Processed.dta", clear


*** Openness to experience

* Multinomial model unadjusted
mlogit recycle open if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit recycle if cca_confounders == 1 & open != ., rrr baseoutcome(0)
est store base 

mlogit recycle open if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit recycle i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if open != ., rrr baseoutcome(0)
est store base 

mlogit recycle open i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum open if cca_confounder == 1 & recycle != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(open = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit recycle open i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(open = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen open = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 open, col(black)) ///
	(line p1 open, col(red)) ///
	(line p2 open, col(blue)) ///
	(line p3 open, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Openness to experience", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(open_recycle, replace)
	
	
use "B4293_ReligionAndClimate_Processed.dta", clear


*** IQ

* Multinomial model unadjusted
mlogit recycle iq if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit recycle if cca_confounders == 1 & iq != ., rrr baseoutcome(0)
est store base 

mlogit recycle iq if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit recycle i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if iq != ., rrr baseoutcome(0)
est store base 

mlogit recycle iq i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum iq if cca_confounder == 1 & recycle != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(iq = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit recycle iq i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(iq = (70(1)130))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen iq = fill(70 71)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 iq, col(black)) ///
	(line p1 iq, col(red)) ///
	(line p2 iq, col(blue)) ///
	(line p3 iq, col(green)), ///
	xscale(range(65 135)) xlabel(70(10)130, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("IQ scores", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(iq_recycle, replace)
	

use "B4293_ReligionAndClimate_Processed.dta", clear



** Combine graphs together with a single legend using the 'grc1leg' user-written package
grc1leg extra_recycle agree_recycle consc_recycle emoStab_recycle open_recycle iq_recycle, rows(2) imargin(tiny) ycommon

graph export ".\Results\recycle_predProbs.pdf", replace

graph close _all


*** Creating various post-files to store results to (individual multinomial RRRs and overall multinomial associations and r2)
capture postclose recycle_mult
postfile recycle_mult str30 exposure str30 outcome str30 out_level /// 
	n coef_uni lci_uni uci_uni double(p_uni) ///
	coef_mult lci_mult uci_mult double(p_mult) ///
	using ".\Results\recycle_mult.dta", replace
	
capture postclose recycle_mult_lr
postfile recycle_mult_lr str30 exposure str30 outcome /// 
	n double(p_uni) r2_uni double(p_mult) r2_mult ///
	using ".\Results\recycle_mult_lr.dta", replace

foreach var of varlist extra agree consc emoStab open iq {
	
	* Store exposure variable as a macro
	local exp = "`var'"
	
	* Store outcome as macro
	local out = "recycle"
	
	** Univariable analysis
	mlogit recycle `var' if cca_confounders == 1, baseoutcome(0) rrr
	
	* Store relevant summary statistics
	local n = e(N)
	local r2_uni = e(r2_p)
	local p_uni = e(p)
	
	* Store coefficients
	matrix res_uni = r(table)
	
	** Multivariable analysis - Repeat with and without exposure to get change in pseudo-R2 and LR test results
	mlogit recycle i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if `var' != ., baseoutcome(0) rrr
	local r2_mult_base = e(r2_p)
	est store base
	
	mlogit recycle `var' i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, baseoutcome(0) rrr
	local r2_mult = e(r2_p) - `r2_mult_base'
	est store main
	
	* Store coefficients
	matrix res_mult = r(table)
	
	* LR test
	lrtest base main
	local p_mult = r(p)
	
	** Post these summary results to the file
	post recycle_mult_lr ("`exp'") ("`out'") (`n') (`p_uni') (`r2_uni') ///
		(`p_mult') (`r2_mult')
		
	
	** Now focus on coefficient results
	
	* Outcome value = 1/Yes for climate
	local out_level = "for climate"
	local coef_uni = res_uni[1,3]
	local lci_uni = res_uni[5,3]	
	local uci_uni = res_uni[6,3]
	local p_uni = res_uni[4,3]
	
	local coef_mult = res_mult[1,23]
	local lci_mult = res_mult[5,23]	
	local uci_mult = res_mult[6,23]
	local p_mult = res_mult[4,23]
	
	post recycle_mult ("`exp'") ("`out'") ("`out_level'") (`n') /// 
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`coef_mult') ///
		(`lci_mult') (`uci_mult') (`p_mult')
	
	* Outcome value = 2/Yes, for non-climate
	local out_level = "for non-climate"
	local coef_uni = res_uni[1,5]
	local lci_uni = res_uni[5,5]	
	local uci_uni = res_uni[6,5]
	local p_uni = res_uni[4,5]
	
	local coef_mult = res_mult[1,45]
	local lci_mult = res_mult[5,45]	
	local uci_mult = res_mult[6,45]
	local p_mult = res_mult[4,45]
	
	post recycle_mult ("`exp'") ("`out'") ("`out_level'") (`n') /// 
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`coef_mult') ///
		(`lci_mult') (`uci_mult') (`p_mult')
		
	* Outcome value = 3/Yes, for climate and non-climate
	local out_level = "for climate and non-climate"
	local coef_uni = res_uni[1,7]
	local lci_uni = res_uni[5,7]	
	local uci_uni = res_uni[6,7]
	local p_uni = res_uni[4,7]
	
	local coef_mult = res_mult[1,67]
	local lci_mult = res_mult[5,67]	
	local uci_mult = res_mult[6,67]
	local p_mult = res_mult[4,67]
	
	post recycle_mult ("`exp'") ("`out'") ("`out_level'") (`n') /// 
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`coef_mult') ///
		(`lci_mult') (`uci_mult') (`p_mult')
		
}

postclose recycle_mult
postclose recycle_mult_lr

* Check results
use ".\Results\recycle_mult_lr.dta", clear

export delimited using ".\Results\recycle_mult_lr.csv", replace


use ".\Results\recycle_mult.dta", clear

export delimited using ".\Results\recycle_mult.csv", replace


** Read back in original dataset
use "B4293_ReligionAndClimate_Processed.dta", clear


**** Recycled more (binary)

*** Extraversion 

* Logistic model unadjusted
logistic recycle_bin extra if cca_confounders == 1

* And for adjusted model
logistic recycle_bin extra i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum extra if cca_confounder == 1 & recycle_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(extra = (`lower_sd', `mean', `upper_sd'))


*** Agreeableness 

* Logistic model unadjusted
logistic recycle_bin agree if cca_confounders == 1

* And for adjusted model
logistic recycle_bin agree i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum agree if cca_confounder == 1 & recycle_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(agree = (`lower_sd', `mean', `upper_sd'))


*** Conscientiousness

* Logistic model unadjusted
logistic recycle_bin consc if cca_confounders == 1

* And for adjusted model
logistic recycle_bin consc i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum consc if cca_confounder == 1 & recycle_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(consc = (`lower_sd', `mean', `upper_sd'))


*** Emotional stability

* Logistic model unadjusted
logistic recycle_bin emoStab if cca_confounders == 1

* And for adjusted model
logistic recycle_bin emoStab i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum emoStab if cca_confounder == 1 & recycle_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(emoStab = (`lower_sd', `mean', `upper_sd'))


*** Openness to experience

* Logistic model unadjusted
logistic recycle_bin open if cca_confounders == 1

* And for adjusted model
logistic recycle_bin open i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum open if cca_confounder == 1 & recycle_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(open = (`lower_sd', `mean', `upper_sd'))


*** IQ

* Logistic model unadjusted
logistic recycle_bin iq if cca_confounders == 1

* And for adjusted model
logistic recycle_bin iq i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum iq if cca_confounder == 1 & recycle_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(iq = (`lower_sd', `mean', `upper_sd'))


*** Creating post-file to store logistic results to
capture postclose recycle_bin
postfile recycle_bin str30 exposure str30 outcome /// 
	n coef_uni lci_uni uci_uni double(p_uni) r2_uni ///
	coef_mult lci_mult uci_mult double(p_mult) r2_mult ///
	using ".\Results\recycle_bin.dta", replace

foreach var of varlist extra agree consc emoStab open iq {
	
	* Store exposure variable as a macro
	local exp = "`var'"
	
	* Store outcome as macro
	local out = "recycle_bin"
	
	** Univariable analysis
	logistic recycle_bin `var' if cca_confounders == 1
	
	* Store relevant summary statistics
	local n = e(N)
	local r2_uni = e(r2_p)
	local p_uni = e(p)
	
	* Store coefficients
	matrix res = r(table)
	
	local coef_uni = res[1,1]
	local lci_uni = res[5,1]	
	local uci_uni = res[6,1]
	local p_uni = res[4,1]
	
	** Multivariable analysis - Repeat with and without exposure to get change in pseudo-R2 results
	logistic recycle i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if `var' != .
	local r2_mult_base = e(r2_p)
	
	logistic recycle `var' i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge
	local r2_mult = e(r2_p) - `r2_mult_base'
	
	* Store coefficients
	matrix res = r(table)
	
	local coef_mult = res[1,1]
	local lci_mult = res[5,1]	
	local uci_mult = res[6,1]
	local p_mult = res[4,1]
	
	** Post these summary results to the file
	post recycle_bin ("`exp'") ("`out'") (`n') ///
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`r2_uni') ///
		(`coef_mult') (`lci_mult') (`uci_mult') ///
		(`p_mult') (`r2_mult')
		
}

postclose recycle_bin

* Check results
use ".\Results\recycle_bin.dta", clear

export delimited using ".\Results\recycle_bin.csv", replace


** Read back in original dataset
use "B4293_ReligionAndClimate_Processed.dta", clear



*****************************************************************************************
**** Reduced plastic use

*** Extraversion 

* Multinomial model unadjusted
mlogit plastic extra if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit plastic if cca_confounders == 1 & extra != ., rrr baseoutcome(0)
est store base 

mlogit plastic extra if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit plastic i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if extra != ., rrr baseoutcome(0)
est store base 

mlogit plastic extra i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum extra if cca_confounder == 1 & plastic != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(extra = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit plastic extra i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(extra = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen extra = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 extra, col(black)) ///
	(line p1 extra, col(red)) ///
	(line p2 extra, col(blue)) ///
	(line p3 extra, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Extraversion", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(extra_plastic, replace)

	
use "B4293_ReligionAndClimate_Processed.dta", clear


*** Agreeableness 

* Multinomial model unadjusted
mlogit plastic agree if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit plastic if cca_confounders == 1 & agree != ., rrr baseoutcome(0)
est store base 

mlogit plastic agree if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit plastic i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if agree != ., rrr baseoutcome(0)
est store base 

mlogit plastic agree i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum agree if cca_confounder == 1 & plastic != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(agree = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit plastic agree i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(agree = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen agree = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 agree, col(black)) ///
	(line p1 agree, col(red)) ///
	(line p2 agree, col(blue)) ///
	(line p3 agree, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Agreeableness", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(agree_plastic, replace)

	
use "B4293_ReligionAndClimate_Processed.dta", clear


*** Conscientiousness

* Multinomial model unadjusted
mlogit plastic consc if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit plastic if cca_confounders == 1 & consc != ., rrr baseoutcome(0)
est store base 

mlogit plastic consc if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit plastic i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if consc != ., rrr baseoutcome(0)
est store base 

mlogit plastic consc i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum consc if cca_confounder == 1 & plastic != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(consc = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit plastic consc i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(consc = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen consc = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 consc, col(black)) ///
	(line p1 consc, col(red)) ///
	(line p2 consc, col(blue)) ///
	(line p3 consc, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Conscientiousness", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(consc_plastic, replace)
	

use "B4293_ReligionAndClimate_Processed.dta", clear


*** Emotional stability

* Multinomial model unadjusted
mlogit plastic emoStab if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit plastic if cca_confounders == 1 & emoStab != ., rrr baseoutcome(0)
est store base 

mlogit plastic emoStab if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit plastic i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if emoStab != ., rrr baseoutcome(0)
est store base 

mlogit plastic emoStab i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum emoStab if cca_confounder == 1 & plastic != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(emoStab = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit plastic emoStab i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(emoStab = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen emoStab = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 emoStab, col(black)) ///
	(line p1 emoStab, col(red)) ///
	(line p2 emoStab, col(blue)) ///
	(line p3 emoStab, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Emotional stability", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(emoStab_plastic, replace)
	

use "B4293_ReligionAndClimate_Processed.dta", clear


*** Openness to experience

* Multinomial model unadjusted
mlogit plastic open if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit plastic if cca_confounders == 1 & open != ., rrr baseoutcome(0)
est store base 

mlogit plastic open if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit plastic i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if open != ., rrr baseoutcome(0)
est store base 

mlogit plastic open i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum open if cca_confounder == 1 & plastic != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(open = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit plastic open i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(open = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen open = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 open, col(black)) ///
	(line p1 open, col(red)) ///
	(line p2 open, col(blue)) ///
	(line p3 open, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Openness to experience", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(open_plastic, replace)
	
	
use "B4293_ReligionAndClimate_Processed.dta", clear


*** IQ

* Multinomial model unadjusted
mlogit plastic iq if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit plastic if cca_confounders == 1 & iq != ., rrr baseoutcome(0)
est store base 

mlogit plastic iq if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit plastic i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if iq != ., rrr baseoutcome(0)
est store base 

mlogit plastic iq i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum iq if cca_confounder == 1 & plastic != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(iq = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit plastic iq i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(iq = (70(1)130))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen iq = fill(70 71)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 iq, col(black)) ///
	(line p1 iq, col(red)) ///
	(line p2 iq, col(blue)) ///
	(line p3 iq, col(green)), ///
	xscale(range(65 135)) xlabel(70(10)130, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("IQ scores", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(iq_plastic, replace)
	

use "B4293_ReligionAndClimate_Processed.dta", clear



** Combine graphs together with a single legend using the 'grc1leg' user-written package
grc1leg extra_plastic agree_plastic consc_plastic emoStab_plastic open_plastic iq_plastic, rows(2) imargin(tiny) ycommon

graph export ".\Results\plastic_predProbs.pdf", replace

graph close _all


*** Creating various post-files to store results to (individual multinomial RRRs and overall multinomial associations and r2)
capture postclose plastic_mult
postfile plastic_mult str30 exposure str30 outcome str30 out_level /// 
	n coef_uni lci_uni uci_uni double(p_uni) ///
	coef_mult lci_mult uci_mult double(p_mult) ///
	using ".\Results\plastic_mult.dta", replace
	
capture postclose plastic_mult_lr
postfile plastic_mult_lr str30 exposure str30 outcome /// 
	n double(p_uni) r2_uni double(p_mult) r2_mult ///
	using ".\Results\plastic_mult_lr.dta", replace

foreach var of varlist extra agree consc emoStab open iq {
	
	* Store exposure variable as a macro
	local exp = "`var'"
	
	* Store outcome as macro
	local out = "plastic"
	
	** Univariable analysis
	mlogit plastic `var' if cca_confounders == 1, baseoutcome(0) rrr
	
	* Store relevant summary statistics
	local n = e(N)
	local r2_uni = e(r2_p)
	local p_uni = e(p)
	
	* Store coefficients
	matrix res_uni = r(table)
	
	** Multivariable analysis - Repeat with and without exposure to get change in pseudo-R2 and LR test results
	mlogit plastic i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if `var' != ., baseoutcome(0) rrr
	local r2_mult_base = e(r2_p)
	est store base
	
	mlogit plastic `var' i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, baseoutcome(0) rrr
	local r2_mult = e(r2_p) - `r2_mult_base'
	est store main
	
	* Store coefficients
	matrix res_mult = r(table)
	
	* LR test
	lrtest base main
	local p_mult = r(p)
	
	** Post these summary results to the file
	post plastic_mult_lr ("`exp'") ("`out'") (`n') (`p_uni') (`r2_uni') ///
		(`p_mult') (`r2_mult')
		
	
	** Now focus on coefficient results
	
	* Outcome value = 1/Yes for climate
	local out_level = "for climate"
	local coef_uni = res_uni[1,3]
	local lci_uni = res_uni[5,3]	
	local uci_uni = res_uni[6,3]
	local p_uni = res_uni[4,3]
	
	local coef_mult = res_mult[1,23]
	local lci_mult = res_mult[5,23]	
	local uci_mult = res_mult[6,23]
	local p_mult = res_mult[4,23]
	
	post plastic_mult ("`exp'") ("`out'") ("`out_level'") (`n') /// 
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`coef_mult') ///
		(`lci_mult') (`uci_mult') (`p_mult')
	
	* Outcome value = 2/Yes, for non-climate
	local out_level = "for non-climate"
	local coef_uni = res_uni[1,5]
	local lci_uni = res_uni[5,5]	
	local uci_uni = res_uni[6,5]
	local p_uni = res_uni[4,5]
	
	local coef_mult = res_mult[1,45]
	local lci_mult = res_mult[5,45]	
	local uci_mult = res_mult[6,45]
	local p_mult = res_mult[4,45]
	
	post plastic_mult ("`exp'") ("`out'") ("`out_level'") (`n') /// 
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`coef_mult') ///
		(`lci_mult') (`uci_mult') (`p_mult')
		
	* Outcome value = 3/Yes, for climate and non-climate
	local out_level = "for climate and non-climate"
	local coef_uni = res_uni[1,7]
	local lci_uni = res_uni[5,7]	
	local uci_uni = res_uni[6,7]
	local p_uni = res_uni[4,7]
	
	local coef_mult = res_mult[1,67]
	local lci_mult = res_mult[5,67]	
	local uci_mult = res_mult[6,67]
	local p_mult = res_mult[4,67]
	
	post plastic_mult ("`exp'") ("`out'") ("`out_level'") (`n') /// 
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`coef_mult') ///
		(`lci_mult') (`uci_mult') (`p_mult')
		
}

postclose plastic_mult
postclose plastic_mult_lr

* Check results
use ".\Results\plastic_mult_lr.dta", clear

export delimited using ".\Results\plastic_mult_lr.csv", replace


use ".\Results\plastic_mult.dta", clear

export delimited using ".\Results\plastic_mult.csv", replace


** Read back in original dataset
use "B4293_ReligionAndClimate_Processed.dta", clear


**** Reduced plastic use (binary)

*** Extraversion 

* Logistic model unadjusted
logistic plastic_bin extra if cca_confounders == 1

* And for adjusted model
logistic plastic_bin extra i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum extra if cca_confounder == 1 & plastic_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(extra = (`lower_sd', `mean', `upper_sd'))


*** Agreeableness 

* Logistic model unadjusted
logistic plastic_bin agree if cca_confounders == 1

* And for adjusted model
logistic plastic_bin agree i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum agree if cca_confounder == 1 & plastic_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(agree = (`lower_sd', `mean', `upper_sd'))


*** Conscientiousness

* Logistic model unadjusted
logistic plastic_bin consc if cca_confounders == 1

* And for adjusted model
logistic plastic_bin consc i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum consc if cca_confounder == 1 & plastic_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(consc = (`lower_sd', `mean', `upper_sd'))


*** Emotional stability

* Logistic model unadjusted
logistic plastic_bin emoStab if cca_confounders == 1

* And for adjusted model
logistic plastic_bin emoStab i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum emoStab if cca_confounder == 1 & plastic_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(emoStab = (`lower_sd', `mean', `upper_sd'))


*** Openness to experience

* Logistic model unadjusted
logistic plastic_bin open if cca_confounders == 1

* And for adjusted model
logistic plastic_bin open i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum open if cca_confounder == 1 & plastic_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(open = (`lower_sd', `mean', `upper_sd'))


*** IQ

* Logistic model unadjusted
logistic plastic_bin iq if cca_confounders == 1

* And for adjusted model
logistic plastic_bin iq i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum iq if cca_confounder == 1 & plastic_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(iq = (`lower_sd', `mean', `upper_sd'))


*** Creating post-file to store logistic results to
capture postclose plastic_bin
postfile plastic_bin str30 exposure str30 outcome /// 
	n coef_uni lci_uni uci_uni double(p_uni) r2_uni ///
	coef_mult lci_mult uci_mult double(p_mult) r2_mult ///
	using ".\Results\plastic_bin.dta", replace

foreach var of varlist extra agree consc emoStab open iq {
	
	* Store exposure variable as a macro
	local exp = "`var'"
	
	* Store outcome as macro
	local out = "plastic_bin"
	
	** Univariable analysis
	logistic plastic_bin `var' if cca_confounders == 1
	
	* Store relevant summary statistics
	local n = e(N)
	local r2_uni = e(r2_p)
	local p_uni = e(p)
	
	* Store coefficients
	matrix res = r(table)
	
	local coef_uni = res[1,1]
	local lci_uni = res[5,1]	
	local uci_uni = res[6,1]
	local p_uni = res[4,1]
	
	** Multivariable analysis - Repeat with and without exposure to get change in pseudo-R2 results
	logistic plastic i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if `var' != .
	local r2_mult_base = e(r2_p)
	
	logistic plastic `var' i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge
	local r2_mult = e(r2_p) - `r2_mult_base'
	
	* Store coefficients
	matrix res = r(table)
	
	local coef_mult = res[1,1]
	local lci_mult = res[5,1]	
	local uci_mult = res[6,1]
	local p_mult = res[4,1]
	
	** Post these summary results to the file
	post plastic_bin ("`exp'") ("`out'") (`n') ///
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`r2_uni') ///
		(`coef_mult') (`lci_mult') (`uci_mult') ///
		(`p_mult') (`r2_mult')
		
}

postclose plastic_bin

* Check results
use ".\Results\plastic_bin.dta", clear

export delimited using ".\Results\plastic_bin.csv", replace


** Read back in original dataset
use "B4293_ReligionAndClimate_Processed.dta", clear



*****************************************************************************************
**** Used sustainable products

*** Extraversion 

* Multinomial model unadjusted
mlogit sustainable extra if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit sustainable if cca_confounders == 1 & extra != ., rrr baseoutcome(0)
est store base 

mlogit sustainable extra if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit sustainable i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if extra != ., rrr baseoutcome(0)
est store base 

mlogit sustainable extra i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum extra if cca_confounder == 1 & sustainable != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(extra = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit sustainable extra i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(extra = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen extra = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 extra, col(black)) ///
	(line p1 extra, col(red)) ///
	(line p2 extra, col(blue)) ///
	(line p3 extra, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Extraversion", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(extra_sustainable, replace)

	
use "B4293_ReligionAndClimate_Processed.dta", clear


*** Agreeableness 

* Multinomial model unadjusted
mlogit sustainable agree if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit sustainable if cca_confounders == 1 & agree != ., rrr baseoutcome(0)
est store base 

mlogit sustainable agree if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit sustainable i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if agree != ., rrr baseoutcome(0)
est store base 

mlogit sustainable agree i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum agree if cca_confounder == 1 & sustainable != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(agree = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit sustainable agree i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(agree = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen agree = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 agree, col(black)) ///
	(line p1 agree, col(red)) ///
	(line p2 agree, col(blue)) ///
	(line p3 agree, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Agreeableness", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(agree_sustainable, replace)

	
use "B4293_ReligionAndClimate_Processed.dta", clear


*** Conscientiousness

* Multinomial model unadjusted
mlogit sustainable consc if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit sustainable if cca_confounders == 1 & consc != ., rrr baseoutcome(0)
est store base 

mlogit sustainable consc if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit sustainable i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if consc != ., rrr baseoutcome(0)
est store base 

mlogit sustainable consc i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum consc if cca_confounder == 1 & sustainable != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(consc = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit sustainable consc i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(consc = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen consc = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 consc, col(black)) ///
	(line p1 consc, col(red)) ///
	(line p2 consc, col(blue)) ///
	(line p3 consc, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Conscientiousness", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(consc_sustainable, replace)
	

use "B4293_ReligionAndClimate_Processed.dta", clear


*** Emotional stability

* Multinomial model unadjusted
mlogit sustainable emoStab if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit sustainable if cca_confounders == 1 & emoStab != ., rrr baseoutcome(0)
est store base 

mlogit sustainable emoStab if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit sustainable i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if emoStab != ., rrr baseoutcome(0)
est store base 

mlogit sustainable emoStab i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum emoStab if cca_confounder == 1 & sustainable != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(emoStab = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit sustainable emoStab i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(emoStab = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen emoStab = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 emoStab, col(black)) ///
	(line p1 emoStab, col(red)) ///
	(line p2 emoStab, col(blue)) ///
	(line p3 emoStab, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Emotional stability", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(emoStab_sustainable, replace)
	

use "B4293_ReligionAndClimate_Processed.dta", clear


*** Openness to experience

* Multinomial model unadjusted
mlogit sustainable open if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit sustainable if cca_confounders == 1 & open != ., rrr baseoutcome(0)
est store base 

mlogit sustainable open if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit sustainable i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if open != ., rrr baseoutcome(0)
est store base 

mlogit sustainable open i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum open if cca_confounder == 1 & sustainable != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(open = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit sustainable open i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(open = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen open = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 open, col(black)) ///
	(line p1 open, col(red)) ///
	(line p2 open, col(blue)) ///
	(line p3 open, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Openness to experience", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(open_sustainable, replace)
	
	
use "B4293_ReligionAndClimate_Processed.dta", clear


*** IQ

* Multinomial model unadjusted
mlogit sustainable iq if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit sustainable if cca_confounders == 1 & iq != ., rrr baseoutcome(0)
est store base 

mlogit sustainable iq if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit sustainable i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if iq != ., rrr baseoutcome(0)
est store base 

mlogit sustainable iq i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum iq if cca_confounder == 1 & sustainable != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(iq = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit sustainable iq i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(iq = (70(1)130))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen iq = fill(70 71)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 iq, col(black)) ///
	(line p1 iq, col(red)) ///
	(line p2 iq, col(blue)) ///
	(line p3 iq, col(green)), ///
	xscale(range(65 135)) xlabel(70(10)130, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("IQ scores", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(iq_sustainable, replace)
	

use "B4293_ReligionAndClimate_Processed.dta", clear



** Combine graphs together with a single legend using the 'grc1leg' user-written package
grc1leg extra_sustainable agree_sustainable consc_sustainable emoStab_sustainable open_sustainable iq_sustainable, rows(2) imargin(tiny) ycommon

graph export ".\Results\sustainable_predProbs.pdf", replace

graph close _all


*** Creating various post-files to store results to (individual multinomial RRRs and overall multinomial associations and r2)
capture postclose sustainable_mult
postfile sustainable_mult str30 exposure str30 outcome str30 out_level /// 
	n coef_uni lci_uni uci_uni double(p_uni) ///
	coef_mult lci_mult uci_mult double(p_mult) ///
	using ".\Results\sustainable_mult.dta", replace
	
capture postclose sustainable_mult_lr
postfile sustainable_mult_lr str30 exposure str30 outcome /// 
	n double(p_uni) r2_uni double(p_mult) r2_mult ///
	using ".\Results\sustainable_mult_lr.dta", replace

foreach var of varlist extra agree consc emoStab open iq {
	
	* Store exposure variable as a macro
	local exp = "`var'"
	
	* Store outcome as macro
	local out = "sustainable"
	
	** Univariable analysis
	mlogit sustainable `var' if cca_confounders == 1, baseoutcome(0) rrr
	
	* Store relevant summary statistics
	local n = e(N)
	local r2_uni = e(r2_p)
	local p_uni = e(p)
	
	* Store coefficients
	matrix res_uni = r(table)
	
	** Multivariable analysis - Repeat with and without exposure to get change in pseudo-R2 and LR test results
	mlogit sustainable i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if `var' != ., baseoutcome(0) rrr
	local r2_mult_base = e(r2_p)
	est store base
	
	mlogit sustainable `var' i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, baseoutcome(0) rrr
	local r2_mult = e(r2_p) - `r2_mult_base'
	est store main
	
	* Store coefficients
	matrix res_mult = r(table)
	
	* LR test
	lrtest base main
	local p_mult = r(p)
	
	** Post these summary results to the file
	post sustainable_mult_lr ("`exp'") ("`out'") (`n') (`p_uni') (`r2_uni') ///
		(`p_mult') (`r2_mult')
		
	
	** Now focus on coefficient results
	
	* Outcome value = 1/Yes for climate
	local out_level = "for climate"
	local coef_uni = res_uni[1,3]
	local lci_uni = res_uni[5,3]	
	local uci_uni = res_uni[6,3]
	local p_uni = res_uni[4,3]
	
	local coef_mult = res_mult[1,23]
	local lci_mult = res_mult[5,23]	
	local uci_mult = res_mult[6,23]
	local p_mult = res_mult[4,23]
	
	post sustainable_mult ("`exp'") ("`out'") ("`out_level'") (`n') /// 
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`coef_mult') ///
		(`lci_mult') (`uci_mult') (`p_mult')
	
	* Outcome value = 2/Yes, for non-climate
	local out_level = "for non-climate"
	local coef_uni = res_uni[1,5]
	local lci_uni = res_uni[5,5]	
	local uci_uni = res_uni[6,5]
	local p_uni = res_uni[4,5]
	
	local coef_mult = res_mult[1,45]
	local lci_mult = res_mult[5,45]	
	local uci_mult = res_mult[6,45]
	local p_mult = res_mult[4,45]
	
	post sustainable_mult ("`exp'") ("`out'") ("`out_level'") (`n') /// 
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`coef_mult') ///
		(`lci_mult') (`uci_mult') (`p_mult')
		
	* Outcome value = 3/Yes, for climate and non-climate
	local out_level = "for climate and non-climate"
	local coef_uni = res_uni[1,7]
	local lci_uni = res_uni[5,7]	
	local uci_uni = res_uni[6,7]
	local p_uni = res_uni[4,7]
	
	local coef_mult = res_mult[1,67]
	local lci_mult = res_mult[5,67]	
	local uci_mult = res_mult[6,67]
	local p_mult = res_mult[4,67]
	
	post sustainable_mult ("`exp'") ("`out'") ("`out_level'") (`n') /// 
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`coef_mult') ///
		(`lci_mult') (`uci_mult') (`p_mult')
		
}

postclose sustainable_mult
postclose sustainable_mult_lr

* Check results
use ".\Results\sustainable_mult_lr.dta", clear

export delimited using ".\Results\sustainable_mult_lr.csv", replace


use ".\Results\sustainable_mult.dta", clear

export delimited using ".\Results\sustainable_mult.csv", replace


** Read back in original dataset
use "B4293_ReligionAndClimate_Processed.dta", clear


**** Used sustainable products (binary)

*** Extraversion 

* Logistic model unadjusted
logistic sustainable_bin extra if cca_confounders == 1

* And for adjusted model
logistic sustainable_bin extra i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum extra if cca_confounder == 1 & sustainable_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(extra = (`lower_sd', `mean', `upper_sd'))


*** Agreeableness 

* Logistic model unadjusted
logistic sustainable_bin agree if cca_confounders == 1

* And for adjusted model
logistic sustainable_bin agree i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum agree if cca_confounder == 1 & sustainable_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(agree = (`lower_sd', `mean', `upper_sd'))


*** Conscientiousness

* Logistic model unadjusted
logistic sustainable_bin consc if cca_confounders == 1

* And for adjusted model
logistic sustainable_bin consc i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum consc if cca_confounder == 1 & sustainable_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(consc = (`lower_sd', `mean', `upper_sd'))


*** Emotional stability

* Logistic model unadjusted
logistic sustainable_bin emoStab if cca_confounders == 1

* And for adjusted model
logistic sustainable_bin emoStab i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum emoStab if cca_confounder == 1 & sustainable_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(emoStab = (`lower_sd', `mean', `upper_sd'))


*** Openness to experience

* Logistic model unadjusted
logistic sustainable_bin open if cca_confounders == 1

* And for adjusted model
logistic sustainable_bin open i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum open if cca_confounder == 1 & sustainable_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(open = (`lower_sd', `mean', `upper_sd'))


*** IQ

* Logistic model unadjusted
logistic sustainable_bin iq if cca_confounders == 1

* And for adjusted model
logistic sustainable_bin iq i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum iq if cca_confounder == 1 & sustainable_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(iq = (`lower_sd', `mean', `upper_sd'))


*** Creating post-file to store logistic results to
capture postclose sustainable_bin
postfile sustainable_bin str30 exposure str30 outcome /// 
	n coef_uni lci_uni uci_uni double(p_uni) r2_uni ///
	coef_mult lci_mult uci_mult double(p_mult) r2_mult ///
	using ".\Results\sustainable_bin.dta", replace

foreach var of varlist extra agree consc emoStab open iq {
	
	* Store exposure variable as a macro
	local exp = "`var'"
	
	* Store outcome as macro
	local out = "sustainable_bin"
	
	** Univariable analysis
	logistic sustainable_bin `var' if cca_confounders == 1
	
	* Store relevant summary statistics
	local n = e(N)
	local r2_uni = e(r2_p)
	local p_uni = e(p)
	
	* Store coefficients
	matrix res = r(table)
	
	local coef_uni = res[1,1]
	local lci_uni = res[5,1]	
	local uci_uni = res[6,1]
	local p_uni = res[4,1]
	
	** Multivariable analysis - Repeat with and without exposure to get change in pseudo-R2 results
	logistic sustainable i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if `var' != .
	local r2_mult_base = e(r2_p)
	
	logistic sustainable `var' i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge
	local r2_mult = e(r2_p) - `r2_mult_base'
	
	* Store coefficients
	matrix res = r(table)
	
	local coef_mult = res[1,1]
	local lci_mult = res[5,1]	
	local uci_mult = res[6,1]
	local p_mult = res[4,1]
	
	** Post these summary results to the file
	post sustainable_bin ("`exp'") ("`out'") (`n') ///
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`r2_uni') ///
		(`coef_mult') (`lci_mult') (`uci_mult') ///
		(`p_mult') (`r2_mult')
		
}

postclose sustainable_bin

* Check results
use ".\Results\sustainable_bin.dta", clear

export delimited using ".\Results\sustainable_bin.csv", replace


** Read back in original dataset
use "B4293_ReligionAndClimate_Processed.dta", clear



*****************************************************************************************
**** Insulated home

*** Extraversion 

* Multinomial model unadjusted
mlogit insulation extra if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit insulation if cca_confounders == 1 & extra != ., rrr baseoutcome(0)
est store base 

mlogit insulation extra if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit insulation i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if extra != ., rrr baseoutcome(0)
est store base 

mlogit insulation extra i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum extra if cca_confounder == 1 & insulation != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(extra = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit insulation extra i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(extra = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen extra = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 extra, col(black)) ///
	(line p1 extra, col(red)) ///
	(line p2 extra, col(blue)) ///
	(line p3 extra, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Extraversion", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(extra_insulation, replace)

	
use "B4293_ReligionAndClimate_Processed.dta", clear


*** Agreeableness 

* Multinomial model unadjusted
mlogit insulation agree if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit insulation if cca_confounders == 1 & agree != ., rrr baseoutcome(0)
est store base 

mlogit insulation agree if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit insulation i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if agree != ., rrr baseoutcome(0)
est store base 

mlogit insulation agree i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum agree if cca_confounder == 1 & insulation != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(agree = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit insulation agree i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(agree = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen agree = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 agree, col(black)) ///
	(line p1 agree, col(red)) ///
	(line p2 agree, col(blue)) ///
	(line p3 agree, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Agreeableness", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(agree_insulation, replace)

	
use "B4293_ReligionAndClimate_Processed.dta", clear


*** Conscientiousness

* Multinomial model unadjusted
mlogit insulation consc if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit insulation if cca_confounders == 1 & consc != ., rrr baseoutcome(0)
est store base 

mlogit insulation consc if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit insulation i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if consc != ., rrr baseoutcome(0)
est store base 

mlogit insulation consc i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum consc if cca_confounder == 1 & insulation != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(consc = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit insulation consc i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(consc = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen consc = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 consc, col(black)) ///
	(line p1 consc, col(red)) ///
	(line p2 consc, col(blue)) ///
	(line p3 consc, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Conscientiousness", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(consc_insulation, replace)
	

use "B4293_ReligionAndClimate_Processed.dta", clear


*** Emotional stability

* Multinomial model unadjusted
mlogit insulation emoStab if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit insulation if cca_confounders == 1 & emoStab != ., rrr baseoutcome(0)
est store base 

mlogit insulation emoStab if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit insulation i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if emoStab != ., rrr baseoutcome(0)
est store base 

mlogit insulation emoStab i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum emoStab if cca_confounder == 1 & insulation != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(emoStab = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit insulation emoStab i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(emoStab = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen emoStab = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 emoStab, col(black)) ///
	(line p1 emoStab, col(red)) ///
	(line p2 emoStab, col(blue)) ///
	(line p3 emoStab, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Emotional stability", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(emoStab_insulation, replace)
	

use "B4293_ReligionAndClimate_Processed.dta", clear


*** Openness to experience

* Multinomial model unadjusted
mlogit insulation open if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit insulation if cca_confounders == 1 & open != ., rrr baseoutcome(0)
est store base 

mlogit insulation open if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit insulation i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if open != ., rrr baseoutcome(0)
est store base 

mlogit insulation open i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum open if cca_confounder == 1 & insulation != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(open = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit insulation open i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(open = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen open = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 open, col(black)) ///
	(line p1 open, col(red)) ///
	(line p2 open, col(blue)) ///
	(line p3 open, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Openness to experience", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(open_insulation, replace)
	
	
use "B4293_ReligionAndClimate_Processed.dta", clear


*** IQ

* Multinomial model unadjusted
mlogit insulation iq if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit insulation if cca_confounders == 1 & iq != ., rrr baseoutcome(0)
est store base 

mlogit insulation iq if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit insulation i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if iq != ., rrr baseoutcome(0)
est store base 

mlogit insulation iq i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum iq if cca_confounder == 1 & insulation != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(iq = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit insulation iq i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(iq = (70(1)130))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen iq = fill(70 71)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 iq, col(black)) ///
	(line p1 iq, col(red)) ///
	(line p2 iq, col(blue)) ///
	(line p3 iq, col(green)), ///
	xscale(range(65 135)) xlabel(70(10)130, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("IQ scores", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(iq_insulation, replace)
	

use "B4293_ReligionAndClimate_Processed.dta", clear



** Combine graphs together with a single legend using the 'grc1leg' user-written package
grc1leg extra_insulation agree_insulation consc_insulation emoStab_insulation open_insulation iq_insulation, rows(2) imargin(tiny) ycommon

graph export ".\Results\insulation_predProbs.pdf", replace

graph close _all


*** Creating various post-files to store results to (individual multinomial RRRs and overall multinomial associations and r2)
capture postclose insulation_mult
postfile insulation_mult str30 exposure str30 outcome str30 out_level /// 
	n coef_uni lci_uni uci_uni double(p_uni) ///
	coef_mult lci_mult uci_mult double(p_mult) ///
	using ".\Results\insulation_mult.dta", replace
	
capture postclose insulation_mult_lr
postfile insulation_mult_lr str30 exposure str30 outcome /// 
	n double(p_uni) r2_uni double(p_mult) r2_mult ///
	using ".\Results\insulation_mult_lr.dta", replace

foreach var of varlist extra agree consc emoStab open iq {
	
	* Store exposure variable as a macro
	local exp = "`var'"
	
	* Store outcome as macro
	local out = "insulation"
	
	** Univariable analysis
	mlogit insulation `var' if cca_confounders == 1, baseoutcome(0) rrr
	
	* Store relevant summary statistics
	local n = e(N)
	local r2_uni = e(r2_p)
	local p_uni = e(p)
	
	* Store coefficients
	matrix res_uni = r(table)
	
	** Multivariable analysis - Repeat with and without exposure to get change in pseudo-R2 and LR test results
	mlogit insulation i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if `var' != ., baseoutcome(0) rrr
	local r2_mult_base = e(r2_p)
	est store base
	
	mlogit insulation `var' i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, baseoutcome(0) rrr
	local r2_mult = e(r2_p) - `r2_mult_base'
	est store main
	
	* Store coefficients
	matrix res_mult = r(table)
	
	* LR test
	lrtest base main
	local p_mult = r(p)
	
	** Post these summary results to the file
	post insulation_mult_lr ("`exp'") ("`out'") (`n') (`p_uni') (`r2_uni') ///
		(`p_mult') (`r2_mult')
		
	
	** Now focus on coefficient results
	
	* Outcome value = 1/Yes for climate
	local out_level = "for climate"
	local coef_uni = res_uni[1,3]
	local lci_uni = res_uni[5,3]	
	local uci_uni = res_uni[6,3]
	local p_uni = res_uni[4,3]
	
	local coef_mult = res_mult[1,23]
	local lci_mult = res_mult[5,23]	
	local uci_mult = res_mult[6,23]
	local p_mult = res_mult[4,23]
	
	post insulation_mult ("`exp'") ("`out'") ("`out_level'") (`n') /// 
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`coef_mult') ///
		(`lci_mult') (`uci_mult') (`p_mult')
	
	* Outcome value = 2/Yes, for non-climate
	local out_level = "for non-climate"
	local coef_uni = res_uni[1,5]
	local lci_uni = res_uni[5,5]	
	local uci_uni = res_uni[6,5]
	local p_uni = res_uni[4,5]
	
	local coef_mult = res_mult[1,45]
	local lci_mult = res_mult[5,45]	
	local uci_mult = res_mult[6,45]
	local p_mult = res_mult[4,45]
	
	post insulation_mult ("`exp'") ("`out'") ("`out_level'") (`n') /// 
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`coef_mult') ///
		(`lci_mult') (`uci_mult') (`p_mult')
		
	* Outcome value = 3/Yes, for climate and non-climate
	local out_level = "for climate and non-climate"
	local coef_uni = res_uni[1,7]
	local lci_uni = res_uni[5,7]	
	local uci_uni = res_uni[6,7]
	local p_uni = res_uni[4,7]
	
	local coef_mult = res_mult[1,67]
	local lci_mult = res_mult[5,67]	
	local uci_mult = res_mult[6,67]
	local p_mult = res_mult[4,67]
	
	post insulation_mult ("`exp'") ("`out'") ("`out_level'") (`n') /// 
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`coef_mult') ///
		(`lci_mult') (`uci_mult') (`p_mult')
		
}

postclose insulation_mult
postclose insulation_mult_lr

* Check results
use ".\Results\insulation_mult_lr.dta", clear

export delimited using ".\Results\insulation_mult_lr.csv", replace


use ".\Results\insulation_mult.dta", clear

export delimited using ".\Results\insulation_mult.csv", replace


** Read back in original dataset
use "B4293_ReligionAndClimate_Processed.dta", clear


**** Insulated home (binary)

*** Extraversion 

* Logistic model unadjusted
logistic insulation_bin extra if cca_confounders == 1

* And for adjusted model
logistic insulation_bin extra i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum extra if cca_confounder == 1 & insulation_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(extra = (`lower_sd', `mean', `upper_sd'))


*** Agreeableness 

* Logistic model unadjusted
logistic insulation_bin agree if cca_confounders == 1

* And for adjusted model
logistic insulation_bin agree i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum agree if cca_confounder == 1 & insulation_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(agree = (`lower_sd', `mean', `upper_sd'))


*** Conscientiousness

* Logistic model unadjusted
logistic insulation_bin consc if cca_confounders == 1

* And for adjusted model
logistic insulation_bin consc i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum consc if cca_confounder == 1 & insulation_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(consc = (`lower_sd', `mean', `upper_sd'))


*** Emotional stability

* Logistic model unadjusted
logistic insulation_bin emoStab if cca_confounders == 1

* And for adjusted model
logistic insulation_bin emoStab i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum emoStab if cca_confounder == 1 & insulation_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(emoStab = (`lower_sd', `mean', `upper_sd'))


*** Openness to experience

* Logistic model unadjusted
logistic insulation_bin open if cca_confounders == 1

* And for adjusted model
logistic insulation_bin open i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum open if cca_confounder == 1 & insulation_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(open = (`lower_sd', `mean', `upper_sd'))


*** IQ

* Logistic model unadjusted
logistic insulation_bin iq if cca_confounders == 1

* And for adjusted model
logistic insulation_bin iq i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum iq if cca_confounder == 1 & insulation_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(iq = (`lower_sd', `mean', `upper_sd'))


*** Creating post-file to store logistic results to
capture postclose insulation_bin
postfile insulation_bin str30 exposure str30 outcome /// 
	n coef_uni lci_uni uci_uni double(p_uni) r2_uni ///
	coef_mult lci_mult uci_mult double(p_mult) r2_mult ///
	using ".\Results\insulation_bin.dta", replace

foreach var of varlist extra agree consc emoStab open iq {
	
	* Store exposure variable as a macro
	local exp = "`var'"
	
	* Store outcome as macro
	local out = "insulation_bin"
	
	** Univariable analysis
	logistic insulation_bin `var' if cca_confounders == 1
	
	* Store relevant summary statistics
	local n = e(N)
	local r2_uni = e(r2_p)
	local p_uni = e(p)
	
	* Store coefficients
	matrix res = r(table)
	
	local coef_uni = res[1,1]
	local lci_uni = res[5,1]	
	local uci_uni = res[6,1]
	local p_uni = res[4,1]
	
	** Multivariable analysis - Repeat with and without exposure to get change in pseudo-R2 results
	logistic insulation i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if `var' != .
	local r2_mult_base = e(r2_p)
	
	logistic insulation `var' i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge
	local r2_mult = e(r2_p) - `r2_mult_base'
	
	* Store coefficients
	matrix res = r(table)
	
	local coef_mult = res[1,1]
	local lci_mult = res[5,1]	
	local uci_mult = res[6,1]
	local p_mult = res[4,1]
	
	** Post these summary results to the file
	post insulation_bin ("`exp'") ("`out'") (`n') ///
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`r2_uni') ///
		(`coef_mult') (`lci_mult') (`uci_mult') ///
		(`p_mult') (`r2_mult')
		
}

postclose insulation_bin

* Check results
use ".\Results\insulation_bin.dta", clear

export delimited using ".\Results\insulation_bin.csv", replace


** Read back in original dataset
use "B4293_ReligionAndClimate_Processed.dta", clear



*****************************************************************************************
**** Solar panels

*** Extraversion 

* Multinomial model unadjusted
mlogit solar extra if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit solar if cca_confounders == 1 & extra != ., rrr baseoutcome(0)
est store base 

mlogit solar extra if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit solar i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if extra != ., rrr baseoutcome(0)
est store base 

mlogit solar extra i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum extra if cca_confounder == 1 & solar != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(extra = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit solar extra i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(extra = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen extra = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 extra, col(black)) ///
	(line p1 extra, col(red)) ///
	(line p2 extra, col(blue)) ///
	(line p3 extra, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Extraversion", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(extra_solar, replace)

	
use "B4293_ReligionAndClimate_Processed.dta", clear


*** Agreeableness 

* Multinomial model unadjusted
mlogit solar agree if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit solar if cca_confounders == 1 & agree != ., rrr baseoutcome(0)
est store base 

mlogit solar agree if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit solar i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if agree != ., rrr baseoutcome(0)
est store base 

mlogit solar agree i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum agree if cca_confounder == 1 & solar != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(agree = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit solar agree i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(agree = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen agree = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 agree, col(black)) ///
	(line p1 agree, col(red)) ///
	(line p2 agree, col(blue)) ///
	(line p3 agree, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Agreeableness", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(agree_solar, replace)

	
use "B4293_ReligionAndClimate_Processed.dta", clear


*** Conscientiousness

* Multinomial model unadjusted
mlogit solar consc if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit solar if cca_confounders == 1 & consc != ., rrr baseoutcome(0)
est store base 

mlogit solar consc if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit solar i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if consc != ., rrr baseoutcome(0)
est store base 

mlogit solar consc i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum consc if cca_confounder == 1 & solar != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(consc = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit solar consc i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(consc = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen consc = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 consc, col(black)) ///
	(line p1 consc, col(red)) ///
	(line p2 consc, col(blue)) ///
	(line p3 consc, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Conscientiousness", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(consc_solar, replace)
	

use "B4293_ReligionAndClimate_Processed.dta", clear


*** Emotional stability

* Multinomial model unadjusted
mlogit solar emoStab if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit solar if cca_confounders == 1 & emoStab != ., rrr baseoutcome(0)
est store base 

mlogit solar emoStab if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit solar i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if emoStab != ., rrr baseoutcome(0)
est store base 

mlogit solar emoStab i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum emoStab if cca_confounder == 1 & solar != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(emoStab = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit solar emoStab i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(emoStab = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen emoStab = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 emoStab, col(black)) ///
	(line p1 emoStab, col(red)) ///
	(line p2 emoStab, col(blue)) ///
	(line p3 emoStab, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Emotional stability", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(emoStab_solar, replace)
	

use "B4293_ReligionAndClimate_Processed.dta", clear


*** Openness to experience

* Multinomial model unadjusted
mlogit solar open if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit solar if cca_confounders == 1 & open != ., rrr baseoutcome(0)
est store base 

mlogit solar open if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit solar i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if open != ., rrr baseoutcome(0)
est store base 

mlogit solar open i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum open if cca_confounder == 1 & solar != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(open = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit solar open i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(open = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen open = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 open, col(black)) ///
	(line p1 open, col(red)) ///
	(line p2 open, col(blue)) ///
	(line p3 open, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Openness to experience", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(open_solar, replace)
	
	
use "B4293_ReligionAndClimate_Processed.dta", clear


*** IQ

* Multinomial model unadjusted
mlogit solar iq if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit solar if cca_confounders == 1 & iq != ., rrr baseoutcome(0)
est store base 

mlogit solar iq if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit solar i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if iq != ., rrr baseoutcome(0)
est store base 

mlogit solar iq i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum iq if cca_confounder == 1 & solar != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(iq = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit solar iq i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(iq = (70(1)130))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen iq = fill(70 71)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 iq, col(black)) ///
	(line p1 iq, col(red)) ///
	(line p2 iq, col(blue)) ///
	(line p3 iq, col(green)), ///
	xscale(range(65 135)) xlabel(70(10)130, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("IQ scores", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(iq_solar, replace)
	

use "B4293_ReligionAndClimate_Processed.dta", clear



** Combine graphs together with a single legend using the 'grc1leg' user-written package
grc1leg extra_solar agree_solar consc_solar emoStab_solar open_solar iq_solar, rows(2) imargin(tiny) ycommon

graph export ".\Results\solar_predProbs.pdf", replace

graph close _all


*** Creating various post-files to store results to (individual multinomial RRRs and overall multinomial associations and r2)
capture postclose solar_mult
postfile solar_mult str30 exposure str30 outcome str30 out_level /// 
	n coef_uni lci_uni uci_uni double(p_uni) ///
	coef_mult lci_mult uci_mult double(p_mult) ///
	using ".\Results\solar_mult.dta", replace
	
capture postclose solar_mult_lr
postfile solar_mult_lr str30 exposure str30 outcome /// 
	n double(p_uni) r2_uni double(p_mult) r2_mult ///
	using ".\Results\solar_mult_lr.dta", replace

foreach var of varlist extra agree consc emoStab open iq {
	
	* Store exposure variable as a macro
	local exp = "`var'"
	
	* Store outcome as macro
	local out = "solar"
	
	** Univariable analysis
	mlogit solar `var' if cca_confounders == 1, baseoutcome(0) rrr
	
	* Store relevant summary statistics
	local n = e(N)
	local r2_uni = e(r2_p)
	local p_uni = e(p)
	
	* Store coefficients
	matrix res_uni = r(table)
	
	** Multivariable analysis - Repeat with and without exposure to get change in pseudo-R2 and LR test results
	mlogit solar i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if `var' != ., baseoutcome(0) rrr
	local r2_mult_base = e(r2_p)
	est store base
	
	mlogit solar `var' i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, baseoutcome(0) rrr
	local r2_mult = e(r2_p) - `r2_mult_base'
	est store main
	
	* Store coefficients
	matrix res_mult = r(table)
	
	* LR test
	lrtest base main
	local p_mult = r(p)
	
	** Post these summary results to the file
	post solar_mult_lr ("`exp'") ("`out'") (`n') (`p_uni') (`r2_uni') ///
		(`p_mult') (`r2_mult')
		
	
	** Now focus on coefficient results
	
	* Outcome value = 1/Yes for climate
	local out_level = "for climate"
	local coef_uni = res_uni[1,3]
	local lci_uni = res_uni[5,3]	
	local uci_uni = res_uni[6,3]
	local p_uni = res_uni[4,3]
	
	local coef_mult = res_mult[1,23]
	local lci_mult = res_mult[5,23]	
	local uci_mult = res_mult[6,23]
	local p_mult = res_mult[4,23]
	
	post solar_mult ("`exp'") ("`out'") ("`out_level'") (`n') /// 
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`coef_mult') ///
		(`lci_mult') (`uci_mult') (`p_mult')
	
	* Outcome value = 2/Yes, for non-climate
	local out_level = "for non-climate"
	local coef_uni = res_uni[1,5]
	local lci_uni = res_uni[5,5]	
	local uci_uni = res_uni[6,5]
	local p_uni = res_uni[4,5]
	
	local coef_mult = res_mult[1,45]
	local lci_mult = res_mult[5,45]	
	local uci_mult = res_mult[6,45]
	local p_mult = res_mult[4,45]
	
	post solar_mult ("`exp'") ("`out'") ("`out_level'") (`n') /// 
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`coef_mult') ///
		(`lci_mult') (`uci_mult') (`p_mult')
		
	* Outcome value = 3/Yes, for climate and non-climate
	local out_level = "for climate and non-climate"
	local coef_uni = res_uni[1,7]
	local lci_uni = res_uni[5,7]	
	local uci_uni = res_uni[6,7]
	local p_uni = res_uni[4,7]
	
	local coef_mult = res_mult[1,67]
	local lci_mult = res_mult[5,67]	
	local uci_mult = res_mult[6,67]
	local p_mult = res_mult[4,67]
	
	post solar_mult ("`exp'") ("`out'") ("`out_level'") (`n') /// 
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`coef_mult') ///
		(`lci_mult') (`uci_mult') (`p_mult')
		
}

postclose solar_mult
postclose solar_mult_lr

* Check results
use ".\Results\solar_mult_lr.dta", clear

export delimited using ".\Results\solar_mult_lr.csv", replace


use ".\Results\solar_mult.dta", clear

export delimited using ".\Results\solar_mult.csv", replace


** Read back in original dataset
use "B4293_ReligionAndClimate_Processed.dta", clear


**** Solar panels (binary)

*** Extraversion 

* Logistic model unadjusted
logistic solar_bin extra if cca_confounders == 1

* And for adjusted model
logistic solar_bin extra i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum extra if cca_confounder == 1 & solar_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(extra = (`lower_sd', `mean', `upper_sd'))


*** Agreeableness 

* Logistic model unadjusted
logistic solar_bin agree if cca_confounders == 1

* And for adjusted model
logistic solar_bin agree i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum agree if cca_confounder == 1 & solar_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(agree = (`lower_sd', `mean', `upper_sd'))


*** Conscientiousness

* Logistic model unadjusted
logistic solar_bin consc if cca_confounders == 1

* And for adjusted model
logistic solar_bin consc i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum consc if cca_confounder == 1 & solar_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(consc = (`lower_sd', `mean', `upper_sd'))


*** Emotional stability

* Logistic model unadjusted
logistic solar_bin emoStab if cca_confounders == 1

* And for adjusted model
logistic solar_bin emoStab i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum emoStab if cca_confounder == 1 & solar_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(emoStab = (`lower_sd', `mean', `upper_sd'))


*** Openness to experience

* Logistic model unadjusted
logistic solar_bin open if cca_confounders == 1

* And for adjusted model
logistic solar_bin open i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum open if cca_confounder == 1 & solar_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(open = (`lower_sd', `mean', `upper_sd'))


*** IQ

* Logistic model unadjusted
logistic solar_bin iq if cca_confounders == 1

* And for adjusted model
logistic solar_bin iq i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum iq if cca_confounder == 1 & solar_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(iq = (`lower_sd', `mean', `upper_sd'))


*** Creating post-file to store logistic results to
capture postclose solar_bin
postfile solar_bin str30 exposure str30 outcome /// 
	n coef_uni lci_uni uci_uni double(p_uni) r2_uni ///
	coef_mult lci_mult uci_mult double(p_mult) r2_mult ///
	using ".\Results\solar_bin.dta", replace

foreach var of varlist extra agree consc emoStab open iq {
	
	* Store exposure variable as a macro
	local exp = "`var'"
	
	* Store outcome as macro
	local out = "solar_bin"
	
	** Univariable analysis
	logistic solar_bin `var' if cca_confounders == 1
	
	* Store relevant summary statistics
	local n = e(N)
	local r2_uni = e(r2_p)
	local p_uni = e(p)
	
	* Store coefficients
	matrix res = r(table)
	
	local coef_uni = res[1,1]
	local lci_uni = res[5,1]	
	local uci_uni = res[6,1]
	local p_uni = res[4,1]
	
	** Multivariable analysis - Repeat with and without exposure to get change in pseudo-R2 results
	logistic solar i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if `var' != .
	local r2_mult_base = e(r2_p)
	
	logistic solar `var' i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge
	local r2_mult = e(r2_p) - `r2_mult_base'
	
	* Store coefficients
	matrix res = r(table)
	
	local coef_mult = res[1,1]
	local lci_mult = res[5,1]	
	local uci_mult = res[6,1]
	local p_mult = res[4,1]
	
	** Post these summary results to the file
	post solar_bin ("`exp'") ("`out'") (`n') ///
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`r2_uni') ///
		(`coef_mult') (`lci_mult') (`uci_mult') ///
		(`p_mult') (`r2_mult')
		
}

postclose solar_bin

* Check results
use ".\Results\solar_bin.dta", clear

export delimited using ".\Results\solar_bin.csv", replace


** Read back in original dataset
use "B4293_ReligionAndClimate_Processed.dta", clear



*****************************************************************************************
**** Grew veg

*** Extraversion 

* Multinomial model unadjusted
mlogit veg extra if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit veg if cca_confounders == 1 & extra != ., rrr baseoutcome(0)
est store base 

mlogit veg extra if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit veg i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if extra != ., rrr baseoutcome(0)
est store base 

mlogit veg extra i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum extra if cca_confounder == 1 & veg != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(extra = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit veg extra i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(extra = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen extra = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 extra, col(black)) ///
	(line p1 extra, col(red)) ///
	(line p2 extra, col(blue)) ///
	(line p3 extra, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Extraversion", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(extra_veg, replace)

	
use "B4293_ReligionAndClimate_Processed.dta", clear


*** Agreeableness 

* Multinomial model unadjusted
mlogit veg agree if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit veg if cca_confounders == 1 & agree != ., rrr baseoutcome(0)
est store base 

mlogit veg agree if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit veg i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if agree != ., rrr baseoutcome(0)
est store base 

mlogit veg agree i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum agree if cca_confounder == 1 & veg != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(agree = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit veg agree i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(agree = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen agree = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 agree, col(black)) ///
	(line p1 agree, col(red)) ///
	(line p2 agree, col(blue)) ///
	(line p3 agree, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Agreeableness", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(agree_veg, replace)

	
use "B4293_ReligionAndClimate_Processed.dta", clear


*** Conscientiousness

* Multinomial model unadjusted
mlogit veg consc if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit veg if cca_confounders == 1 & consc != ., rrr baseoutcome(0)
est store base 

mlogit veg consc if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit veg i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if consc != ., rrr baseoutcome(0)
est store base 

mlogit veg consc i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum consc if cca_confounder == 1 & veg != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(consc = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit veg consc i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(consc = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen consc = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 consc, col(black)) ///
	(line p1 consc, col(red)) ///
	(line p2 consc, col(blue)) ///
	(line p3 consc, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Conscientiousness", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(consc_veg, replace)
	

use "B4293_ReligionAndClimate_Processed.dta", clear


*** Emotional stability

* Multinomial model unadjusted
mlogit veg emoStab if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit veg if cca_confounders == 1 & emoStab != ., rrr baseoutcome(0)
est store base 

mlogit veg emoStab if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit veg i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if emoStab != ., rrr baseoutcome(0)
est store base 

mlogit veg emoStab i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum emoStab if cca_confounder == 1 & veg != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(emoStab = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit veg emoStab i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(emoStab = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen emoStab = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 emoStab, col(black)) ///
	(line p1 emoStab, col(red)) ///
	(line p2 emoStab, col(blue)) ///
	(line p3 emoStab, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Emotional stability", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(emoStab_veg, replace)
	

use "B4293_ReligionAndClimate_Processed.dta", clear


*** Openness to experience

* Multinomial model unadjusted
mlogit veg open if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit veg if cca_confounders == 1 & open != ., rrr baseoutcome(0)
est store base 

mlogit veg open if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit veg i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if open != ., rrr baseoutcome(0)
est store base 

mlogit veg open i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum open if cca_confounder == 1 & veg != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(open = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit veg open i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(open = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen open = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 open, col(black)) ///
	(line p1 open, col(red)) ///
	(line p2 open, col(blue)) ///
	(line p3 open, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Openness to experience", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(open_veg, replace)
	
	
use "B4293_ReligionAndClimate_Processed.dta", clear


*** IQ

* Multinomial model unadjusted
mlogit veg iq if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit veg if cca_confounders == 1 & iq != ., rrr baseoutcome(0)
est store base 

mlogit veg iq if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit veg i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if iq != ., rrr baseoutcome(0)
est store base 

mlogit veg iq i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum iq if cca_confounder == 1 & veg != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(iq = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit veg iq i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(iq = (70(1)130))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen iq = fill(70 71)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 iq, col(black)) ///
	(line p1 iq, col(red)) ///
	(line p2 iq, col(blue)) ///
	(line p3 iq, col(green)), ///
	xscale(range(65 135)) xlabel(70(10)130, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("IQ scores", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(iq_veg, replace)
	

use "B4293_ReligionAndClimate_Processed.dta", clear



** Combine graphs together with a single legend using the 'grc1leg' user-written package
grc1leg extra_veg agree_veg consc_veg emoStab_veg open_veg iq_veg, rows(2) imargin(tiny) ycommon

graph export ".\Results\veg_predProbs.pdf", replace

graph close _all


*** Creating various post-files to store results to (individual multinomial RRRs and overall multinomial associations and r2)
capture postclose veg_mult
postfile veg_mult str30 exposure str30 outcome str30 out_level /// 
	n coef_uni lci_uni uci_uni double(p_uni) ///
	coef_mult lci_mult uci_mult double(p_mult) ///
	using ".\Results\veg_mult.dta", replace
	
capture postclose veg_mult_lr
postfile veg_mult_lr str30 exposure str30 outcome /// 
	n double(p_uni) r2_uni double(p_mult) r2_mult ///
	using ".\Results\veg_mult_lr.dta", replace

foreach var of varlist extra agree consc emoStab open iq {
	
	* Store exposure variable as a macro
	local exp = "`var'"
	
	* Store outcome as macro
	local out = "veg"
	
	** Univariable analysis
	mlogit veg `var' if cca_confounders == 1, baseoutcome(0) rrr
	
	* Store relevant summary statistics
	local n = e(N)
	local r2_uni = e(r2_p)
	local p_uni = e(p)
	
	* Store coefficients
	matrix res_uni = r(table)
	
	** Multivariable analysis - Repeat with and without exposure to get change in pseudo-R2 and LR test results
	mlogit veg i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if `var' != ., baseoutcome(0) rrr
	local r2_mult_base = e(r2_p)
	est store base
	
	mlogit veg `var' i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, baseoutcome(0) rrr
	local r2_mult = e(r2_p) - `r2_mult_base'
	est store main
	
	* Store coefficients
	matrix res_mult = r(table)
	
	* LR test
	lrtest base main
	local p_mult = r(p)
	
	** Post these summary results to the file
	post veg_mult_lr ("`exp'") ("`out'") (`n') (`p_uni') (`r2_uni') ///
		(`p_mult') (`r2_mult')
		
	
	** Now focus on coefficient results
	
	* Outcome value = 1/Yes for climate
	local out_level = "for climate"
	local coef_uni = res_uni[1,3]
	local lci_uni = res_uni[5,3]	
	local uci_uni = res_uni[6,3]
	local p_uni = res_uni[4,3]
	
	local coef_mult = res_mult[1,23]
	local lci_mult = res_mult[5,23]	
	local uci_mult = res_mult[6,23]
	local p_mult = res_mult[4,23]
	
	post veg_mult ("`exp'") ("`out'") ("`out_level'") (`n') /// 
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`coef_mult') ///
		(`lci_mult') (`uci_mult') (`p_mult')
	
	* Outcome value = 2/Yes, for non-climate
	local out_level = "for non-climate"
	local coef_uni = res_uni[1,5]
	local lci_uni = res_uni[5,5]	
	local uci_uni = res_uni[6,5]
	local p_uni = res_uni[4,5]
	
	local coef_mult = res_mult[1,45]
	local lci_mult = res_mult[5,45]	
	local uci_mult = res_mult[6,45]
	local p_mult = res_mult[4,45]
	
	post veg_mult ("`exp'") ("`out'") ("`out_level'") (`n') /// 
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`coef_mult') ///
		(`lci_mult') (`uci_mult') (`p_mult')
		
	* Outcome value = 3/Yes, for climate and non-climate
	local out_level = "for climate and non-climate"
	local coef_uni = res_uni[1,7]
	local lci_uni = res_uni[5,7]	
	local uci_uni = res_uni[6,7]
	local p_uni = res_uni[4,7]
	
	local coef_mult = res_mult[1,67]
	local lci_mult = res_mult[5,67]	
	local uci_mult = res_mult[6,67]
	local p_mult = res_mult[4,67]
	
	post veg_mult ("`exp'") ("`out'") ("`out_level'") (`n') /// 
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`coef_mult') ///
		(`lci_mult') (`uci_mult') (`p_mult')
		
}

postclose veg_mult
postclose veg_mult_lr

* Check results
use ".\Results\veg_mult_lr.dta", clear

export delimited using ".\Results\veg_mult_lr.csv", replace


use ".\Results\veg_mult.dta", clear

export delimited using ".\Results\veg_mult.csv", replace


** Read back in original dataset
use "B4293_ReligionAndClimate_Processed.dta", clear


**** Grew veg (binary)

*** Extraversion 

* Logistic model unadjusted
logistic veg_bin extra if cca_confounders == 1

* And for adjusted model
logistic veg_bin extra i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum extra if cca_confounder == 1 & veg_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(extra = (`lower_sd', `mean', `upper_sd'))


*** Agreeableness 

* Logistic model unadjusted
logistic veg_bin agree if cca_confounders == 1

* And for adjusted model
logistic veg_bin agree i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum agree if cca_confounder == 1 & veg_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(agree = (`lower_sd', `mean', `upper_sd'))


*** Conscientiousness

* Logistic model unadjusted
logistic veg_bin consc if cca_confounders == 1

* And for adjusted model
logistic veg_bin consc i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum consc if cca_confounder == 1 & veg_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(consc = (`lower_sd', `mean', `upper_sd'))


*** Emotional stability

* Logistic model unadjusted
logistic veg_bin emoStab if cca_confounders == 1

* And for adjusted model
logistic veg_bin emoStab i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum emoStab if cca_confounder == 1 & veg_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(emoStab = (`lower_sd', `mean', `upper_sd'))


*** Openness to experience

* Logistic model unadjusted
logistic veg_bin open if cca_confounders == 1

* And for adjusted model
logistic veg_bin open i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum open if cca_confounder == 1 & veg_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(open = (`lower_sd', `mean', `upper_sd'))


*** IQ

* Logistic model unadjusted
logistic veg_bin iq if cca_confounders == 1

* And for adjusted model
logistic veg_bin iq i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum iq if cca_confounder == 1 & veg_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(iq = (`lower_sd', `mean', `upper_sd'))


*** Creating post-file to store logistic results to
capture postclose veg_bin
postfile veg_bin str30 exposure str30 outcome /// 
	n coef_uni lci_uni uci_uni double(p_uni) r2_uni ///
	coef_mult lci_mult uci_mult double(p_mult) r2_mult ///
	using ".\Results\veg_bin.dta", replace

foreach var of varlist extra agree consc emoStab open iq {
	
	* Store exposure variable as a macro
	local exp = "`var'"
	
	* Store outcome as macro
	local out = "veg_bin"
	
	** Univariable analysis
	logistic veg_bin `var' if cca_confounders == 1
	
	* Store relevant summary statistics
	local n = e(N)
	local r2_uni = e(r2_p)
	local p_uni = e(p)
	
	* Store coefficients
	matrix res = r(table)
	
	local coef_uni = res[1,1]
	local lci_uni = res[5,1]	
	local uci_uni = res[6,1]
	local p_uni = res[4,1]
	
	** Multivariable analysis - Repeat with and without exposure to get change in pseudo-R2 results
	logistic veg i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if `var' != .
	local r2_mult_base = e(r2_p)
	
	logistic veg `var' i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge
	local r2_mult = e(r2_p) - `r2_mult_base'
	
	* Store coefficients
	matrix res = r(table)
	
	local coef_mult = res[1,1]
	local lci_mult = res[5,1]	
	local uci_mult = res[6,1]
	local p_mult = res[4,1]
	
	** Post these summary results to the file
	post veg_bin ("`exp'") ("`out'") (`n') ///
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`r2_uni') ///
		(`coef_mult') (`lci_mult') (`uci_mult') ///
		(`p_mult') (`r2_mult')
		
}

postclose veg_bin

* Check results
use ".\Results\veg_bin.dta", clear

export delimited using ".\Results\veg_bin.csv", replace


** Read back in original dataset
use "B4293_ReligionAndClimate_Processed.dta", clear



*****************************************************************************************
**** Planted trees

*** Extraversion 

* Multinomial model unadjusted
mlogit trees extra if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit trees if cca_confounders == 1 & extra != ., rrr baseoutcome(0)
est store base 

mlogit trees extra if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit trees i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if extra != ., rrr baseoutcome(0)
est store base 

mlogit trees extra i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum extra if cca_confounder == 1 & trees != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(extra = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit trees extra i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(extra = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen extra = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 extra, col(black)) ///
	(line p1 extra, col(red)) ///
	(line p2 extra, col(blue)) ///
	(line p3 extra, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Extraversion", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(extra_trees, replace)

	
use "B4293_ReligionAndClimate_Processed.dta", clear


*** Agreeableness 

* Multinomial model unadjusted
mlogit trees agree if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit trees if cca_confounders == 1 & agree != ., rrr baseoutcome(0)
est store base 

mlogit trees agree if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit trees i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if agree != ., rrr baseoutcome(0)
est store base 

mlogit trees agree i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum agree if cca_confounder == 1 & trees != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(agree = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit trees agree i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(agree = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen agree = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 agree, col(black)) ///
	(line p1 agree, col(red)) ///
	(line p2 agree, col(blue)) ///
	(line p3 agree, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Agreeableness", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(agree_trees, replace)

	
use "B4293_ReligionAndClimate_Processed.dta", clear


*** Conscientiousness

* Multinomial model unadjusted
mlogit trees consc if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit trees if cca_confounders == 1 & consc != ., rrr baseoutcome(0)
est store base 

mlogit trees consc if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit trees i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if consc != ., rrr baseoutcome(0)
est store base 

mlogit trees consc i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum consc if cca_confounder == 1 & trees != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(consc = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit trees consc i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(consc = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen consc = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 consc, col(black)) ///
	(line p1 consc, col(red)) ///
	(line p2 consc, col(blue)) ///
	(line p3 consc, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Conscientiousness", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(consc_trees, replace)
	

use "B4293_ReligionAndClimate_Processed.dta", clear


*** Emotional stability

* Multinomial model unadjusted
mlogit trees emoStab if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit trees if cca_confounders == 1 & emoStab != ., rrr baseoutcome(0)
est store base 

mlogit trees emoStab if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit trees i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if emoStab != ., rrr baseoutcome(0)
est store base 

mlogit trees emoStab i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum emoStab if cca_confounder == 1 & trees != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(emoStab = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit trees emoStab i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(emoStab = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen emoStab = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 emoStab, col(black)) ///
	(line p1 emoStab, col(red)) ///
	(line p2 emoStab, col(blue)) ///
	(line p3 emoStab, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Emotional stability", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(emoStab_trees, replace)
	

use "B4293_ReligionAndClimate_Processed.dta", clear


*** Openness to experience

* Multinomial model unadjusted
mlogit trees open if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit trees if cca_confounders == 1 & open != ., rrr baseoutcome(0)
est store base 

mlogit trees open if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit trees i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if open != ., rrr baseoutcome(0)
est store base 

mlogit trees open i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum open if cca_confounder == 1 & trees != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(open = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit trees open i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(open = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen open = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 open, col(black)) ///
	(line p1 open, col(red)) ///
	(line p2 open, col(blue)) ///
	(line p3 open, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Openness to experience", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(open_trees, replace)
	
	
use "B4293_ReligionAndClimate_Processed.dta", clear


*** IQ

* Multinomial model unadjusted
mlogit trees iq if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit trees if cca_confounders == 1 & iq != ., rrr baseoutcome(0)
est store base 

mlogit trees iq if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit trees i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if iq != ., rrr baseoutcome(0)
est store base 

mlogit trees iq i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum iq if cca_confounder == 1 & trees != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(iq = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit trees iq i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(iq = (70(1)130))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen iq = fill(70 71)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 iq, col(black)) ///
	(line p1 iq, col(red)) ///
	(line p2 iq, col(blue)) ///
	(line p3 iq, col(green)), ///
	xscale(range(65 135)) xlabel(70(10)130, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("IQ scores", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(iq_trees, replace)
	

use "B4293_ReligionAndClimate_Processed.dta", clear



** Combine graphs together with a single legend using the 'grc1leg' user-written package
grc1leg extra_trees agree_trees consc_trees emoStab_trees open_trees iq_trees, rows(2) imargin(tiny) ycommon

graph export ".\Results\trees_predProbs.pdf", replace

graph close _all


*** Creating various post-files to store results to (individual multinomial RRRs and overall multinomial associations and r2)
capture postclose trees_mult
postfile trees_mult str30 exposure str30 outcome str30 out_level /// 
	n coef_uni lci_uni uci_uni double(p_uni) ///
	coef_mult lci_mult uci_mult double(p_mult) ///
	using ".\Results\trees_mult.dta", replace
	
capture postclose trees_mult_lr
postfile trees_mult_lr str30 exposure str30 outcome /// 
	n double(p_uni) r2_uni double(p_mult) r2_mult ///
	using ".\Results\trees_mult_lr.dta", replace

foreach var of varlist extra agree consc emoStab open iq {
	
	* Store exposure variable as a macro
	local exp = "`var'"
	
	* Store outcome as macro
	local out = "trees"
	
	** Univariable analysis
	mlogit trees `var' if cca_confounders == 1, baseoutcome(0) rrr
	
	* Store relevant summary statistics
	local n = e(N)
	local r2_uni = e(r2_p)
	local p_uni = e(p)
	
	* Store coefficients
	matrix res_uni = r(table)
	
	** Multivariable analysis - Repeat with and without exposure to get change in pseudo-R2 and LR test results
	mlogit trees i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if `var' != ., baseoutcome(0) rrr
	local r2_mult_base = e(r2_p)
	est store base
	
	mlogit trees `var' i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, baseoutcome(0) rrr
	local r2_mult = e(r2_p) - `r2_mult_base'
	est store main
	
	* Store coefficients
	matrix res_mult = r(table)
	
	* LR test
	lrtest base main
	local p_mult = r(p)
	
	** Post these summary results to the file
	post trees_mult_lr ("`exp'") ("`out'") (`n') (`p_uni') (`r2_uni') ///
		(`p_mult') (`r2_mult')
		
	
	** Now focus on coefficient results
	
	* Outcome value = 1/Yes for climate
	local out_level = "for climate"
	local coef_uni = res_uni[1,3]
	local lci_uni = res_uni[5,3]	
	local uci_uni = res_uni[6,3]
	local p_uni = res_uni[4,3]
	
	local coef_mult = res_mult[1,23]
	local lci_mult = res_mult[5,23]	
	local uci_mult = res_mult[6,23]
	local p_mult = res_mult[4,23]
	
	post trees_mult ("`exp'") ("`out'") ("`out_level'") (`n') /// 
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`coef_mult') ///
		(`lci_mult') (`uci_mult') (`p_mult')
	
	* Outcome value = 2/Yes, for non-climate
	local out_level = "for non-climate"
	local coef_uni = res_uni[1,5]
	local lci_uni = res_uni[5,5]	
	local uci_uni = res_uni[6,5]
	local p_uni = res_uni[4,5]
	
	local coef_mult = res_mult[1,45]
	local lci_mult = res_mult[5,45]	
	local uci_mult = res_mult[6,45]
	local p_mult = res_mult[4,45]
	
	post trees_mult ("`exp'") ("`out'") ("`out_level'") (`n') /// 
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`coef_mult') ///
		(`lci_mult') (`uci_mult') (`p_mult')
		
	* Outcome value = 3/Yes, for climate and non-climate
	local out_level = "for climate and non-climate"
	local coef_uni = res_uni[1,7]
	local lci_uni = res_uni[5,7]	
	local uci_uni = res_uni[6,7]
	local p_uni = res_uni[4,7]
	
	local coef_mult = res_mult[1,67]
	local lci_mult = res_mult[5,67]	
	local uci_mult = res_mult[6,67]
	local p_mult = res_mult[4,67]
	
	post trees_mult ("`exp'") ("`out'") ("`out_level'") (`n') /// 
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`coef_mult') ///
		(`lci_mult') (`uci_mult') (`p_mult')
		
}

postclose trees_mult
postclose trees_mult_lr

* Check results
use ".\Results\trees_mult_lr.dta", clear

export delimited using ".\Results\trees_mult_lr.csv", replace


use ".\Results\trees_mult.dta", clear

export delimited using ".\Results\trees_mult.csv", replace


** Read back in original dataset
use "B4293_ReligionAndClimate_Processed.dta", clear


**** Planted trees (binary)

*** Extraversion 

* Logistic model unadjusted
logistic trees_bin extra if cca_confounders == 1

* And for adjusted model
logistic trees_bin extra i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum extra if cca_confounder == 1 & trees_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(extra = (`lower_sd', `mean', `upper_sd'))


*** Agreeableness 

* Logistic model unadjusted
logistic trees_bin agree if cca_confounders == 1

* And for adjusted model
logistic trees_bin agree i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum agree if cca_confounder == 1 & trees_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(agree = (`lower_sd', `mean', `upper_sd'))


*** Conscientiousness

* Logistic model unadjusted
logistic trees_bin consc if cca_confounders == 1

* And for adjusted model
logistic trees_bin consc i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum consc if cca_confounder == 1 & trees_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(consc = (`lower_sd', `mean', `upper_sd'))


*** Emotional stability

* Logistic model unadjusted
logistic trees_bin emoStab if cca_confounders == 1

* And for adjusted model
logistic trees_bin emoStab i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum emoStab if cca_confounder == 1 & trees_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(emoStab = (`lower_sd', `mean', `upper_sd'))


*** Openness to experience

* Logistic model unadjusted
logistic trees_bin open if cca_confounders == 1

* And for adjusted model
logistic trees_bin open i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum open if cca_confounder == 1 & trees_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(open = (`lower_sd', `mean', `upper_sd'))


*** IQ

* Logistic model unadjusted
logistic trees_bin iq if cca_confounders == 1

* And for adjusted model
logistic trees_bin iq i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum iq if cca_confounder == 1 & trees_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(iq = (`lower_sd', `mean', `upper_sd'))


*** Creating post-file to store logistic results to
capture postclose trees_bin
postfile trees_bin str30 exposure str30 outcome /// 
	n coef_uni lci_uni uci_uni double(p_uni) r2_uni ///
	coef_mult lci_mult uci_mult double(p_mult) r2_mult ///
	using ".\Results\trees_bin.dta", replace

foreach var of varlist extra agree consc emoStab open iq {
	
	* Store exposure variable as a macro
	local exp = "`var'"
	
	* Store outcome as macro
	local out = "trees_bin"
	
	** Univariable analysis
	logistic trees_bin `var' if cca_confounders == 1
	
	* Store relevant summary statistics
	local n = e(N)
	local r2_uni = e(r2_p)
	local p_uni = e(p)
	
	* Store coefficients
	matrix res = r(table)
	
	local coef_uni = res[1,1]
	local lci_uni = res[5,1]	
	local uci_uni = res[6,1]
	local p_uni = res[4,1]
	
	** Multivariable analysis - Repeat with and without exposure to get change in pseudo-R2 results
	logistic trees i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if `var' != .
	local r2_mult_base = e(r2_p)
	
	logistic trees `var' i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge
	local r2_mult = e(r2_p) - `r2_mult_base'
	
	* Store coefficients
	matrix res = r(table)
	
	local coef_mult = res[1,1]
	local lci_mult = res[5,1]	
	local uci_mult = res[6,1]
	local p_mult = res[4,1]
	
	** Post these summary results to the file
	post trees_bin ("`exp'") ("`out'") (`n') ///
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`r2_uni') ///
		(`coef_mult') (`lci_mult') (`uci_mult') ///
		(`p_mult') (`r2_mult')
		
}

postclose trees_bin

* Check results
use ".\Results\trees_bin.dta", clear

export delimited using ".\Results\trees_bin.csv", replace


** Read back in original dataset
use "B4293_ReligionAndClimate_Processed.dta", clear



*****************************************************************************************
**** Avoided fossil fuel companies

*** Extraversion 

* Multinomial model unadjusted
mlogit avoidFossil extra if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit avoidFossil if cca_confounders == 1 & extra != ., rrr baseoutcome(0)
est store base 

mlogit avoidFossil extra if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit avoidFossil i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if extra != ., rrr baseoutcome(0)
est store base 

mlogit avoidFossil extra i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum extra if cca_confounder == 1 & avoidFossil != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(extra = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit avoidFossil extra i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(extra = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen extra = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 extra, col(black)) ///
	(line p1 extra, col(red)) ///
	(line p2 extra, col(blue)) ///
	(line p3 extra, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Extraversion", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(extra_avoidFossil, replace)

	
use "B4293_ReligionAndClimate_Processed.dta", clear


*** Agreeableness 

* Multinomial model unadjusted
mlogit avoidFossil agree if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit avoidFossil if cca_confounders == 1 & agree != ., rrr baseoutcome(0)
est store base 

mlogit avoidFossil agree if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit avoidFossil i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if agree != ., rrr baseoutcome(0)
est store base 

mlogit avoidFossil agree i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum agree if cca_confounder == 1 & avoidFossil != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(agree = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit avoidFossil agree i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(agree = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen agree = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 agree, col(black)) ///
	(line p1 agree, col(red)) ///
	(line p2 agree, col(blue)) ///
	(line p3 agree, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Agreeableness", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(agree_avoidFossil, replace)

	
use "B4293_ReligionAndClimate_Processed.dta", clear


*** Conscientiousness

* Multinomial model unadjusted
mlogit avoidFossil consc if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit avoidFossil if cca_confounders == 1 & consc != ., rrr baseoutcome(0)
est store base 

mlogit avoidFossil consc if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit avoidFossil i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if consc != ., rrr baseoutcome(0)
est store base 

mlogit avoidFossil consc i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum consc if cca_confounder == 1 & avoidFossil != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(consc = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit avoidFossil consc i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(consc = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen consc = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 consc, col(black)) ///
	(line p1 consc, col(red)) ///
	(line p2 consc, col(blue)) ///
	(line p3 consc, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Conscientiousness", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(consc_avoidFossil, replace)
	

use "B4293_ReligionAndClimate_Processed.dta", clear


*** Emotional stability

* Multinomial model unadjusted
mlogit avoidFossil emoStab if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit avoidFossil if cca_confounders == 1 & emoStab != ., rrr baseoutcome(0)
est store base 

mlogit avoidFossil emoStab if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit avoidFossil i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if emoStab != ., rrr baseoutcome(0)
est store base 

mlogit avoidFossil emoStab i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum emoStab if cca_confounder == 1 & avoidFossil != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(emoStab = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit avoidFossil emoStab i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(emoStab = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen emoStab = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 emoStab, col(black)) ///
	(line p1 emoStab, col(red)) ///
	(line p2 emoStab, col(blue)) ///
	(line p3 emoStab, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Emotional stability", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(emoStab_avoidFossil, replace)
	

use "B4293_ReligionAndClimate_Processed.dta", clear


*** Openness to experience

* Multinomial model unadjusted
mlogit avoidFossil open if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit avoidFossil if cca_confounders == 1 & open != ., rrr baseoutcome(0)
est store base 

mlogit avoidFossil open if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit avoidFossil i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if open != ., rrr baseoutcome(0)
est store base 

mlogit avoidFossil open i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum open if cca_confounder == 1 & avoidFossil != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(open = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit avoidFossil open i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(open = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen open = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 open, col(black)) ///
	(line p1 open, col(red)) ///
	(line p2 open, col(blue)) ///
	(line p3 open, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Openness to experience", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(open_avoidFossil, replace)
	
	
use "B4293_ReligionAndClimate_Processed.dta", clear


*** IQ

* Multinomial model unadjusted
mlogit avoidFossil iq if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit avoidFossil if cca_confounders == 1 & iq != ., rrr baseoutcome(0)
est store base 

mlogit avoidFossil iq if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit avoidFossil i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if iq != ., rrr baseoutcome(0)
est store base 

mlogit avoidFossil iq i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum iq if cca_confounder == 1 & avoidFossil != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(iq = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit avoidFossil iq i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(iq = (70(1)130))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen iq = fill(70 71)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 iq, col(black)) ///
	(line p1 iq, col(red)) ///
	(line p2 iq, col(blue)) ///
	(line p3 iq, col(green)), ///
	xscale(range(65 135)) xlabel(70(10)130, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("IQ scores", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(iq_avoidFossil, replace)
	

use "B4293_ReligionAndClimate_Processed.dta", clear



** Combine graphs together with a single legend using the 'grc1leg' user-written package
grc1leg extra_avoidFossil agree_avoidFossil consc_avoidFossil emoStab_avoidFossil open_avoidFossil iq_avoidFossil, rows(2) imargin(tiny) ycommon

graph export ".\Results\avoidFossil_predProbs.pdf", replace

graph close _all


*** Creating various post-files to store results to (individual multinomial RRRs and overall multinomial associations and r2)
capture postclose avoidFossil_mult
postfile avoidFossil_mult str30 exposure str30 outcome str30 out_level /// 
	n coef_uni lci_uni uci_uni double(p_uni) ///
	coef_mult lci_mult uci_mult double(p_mult) ///
	using ".\Results\avoidFossil_mult.dta", replace
	
capture postclose avoidFossil_mult_lr
postfile avoidFossil_mult_lr str30 exposure str30 outcome /// 
	n double(p_uni) r2_uni double(p_mult) r2_mult ///
	using ".\Results\avoidFossil_mult_lr.dta", replace

foreach var of varlist extra agree consc emoStab open iq {
	
	* Store exposure variable as a macro
	local exp = "`var'"
	
	* Store outcome as macro
	local out = "avoidFossil"
	
	** Univariable analysis
	mlogit avoidFossil `var' if cca_confounders == 1, baseoutcome(0) rrr
	
	* Store relevant summary statistics
	local n = e(N)
	local r2_uni = e(r2_p)
	local p_uni = e(p)
	
	* Store coefficients
	matrix res_uni = r(table)
	
	** Multivariable analysis - Repeat with and without exposure to get change in pseudo-R2 and LR test results
	mlogit avoidFossil i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if `var' != ., baseoutcome(0) rrr
	local r2_mult_base = e(r2_p)
	est store base
	
	mlogit avoidFossil `var' i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, baseoutcome(0) rrr
	local r2_mult = e(r2_p) - `r2_mult_base'
	est store main
	
	* Store coefficients
	matrix res_mult = r(table)
	
	* LR test
	lrtest base main
	local p_mult = r(p)
	
	** Post these summary results to the file
	post avoidFossil_mult_lr ("`exp'") ("`out'") (`n') (`p_uni') (`r2_uni') ///
		(`p_mult') (`r2_mult')
		
	
	** Now focus on coefficient results
	
	* Outcome value = 1/Yes for climate
	local out_level = "for climate"
	local coef_uni = res_uni[1,3]
	local lci_uni = res_uni[5,3]	
	local uci_uni = res_uni[6,3]
	local p_uni = res_uni[4,3]
	
	local coef_mult = res_mult[1,23]
	local lci_mult = res_mult[5,23]	
	local uci_mult = res_mult[6,23]
	local p_mult = res_mult[4,23]
	
	post avoidFossil_mult ("`exp'") ("`out'") ("`out_level'") (`n') /// 
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`coef_mult') ///
		(`lci_mult') (`uci_mult') (`p_mult')
	
	* Outcome value = 2/Yes, for non-climate
	local out_level = "for non-climate"
	local coef_uni = res_uni[1,5]
	local lci_uni = res_uni[5,5]	
	local uci_uni = res_uni[6,5]
	local p_uni = res_uni[4,5]
	
	local coef_mult = res_mult[1,45]
	local lci_mult = res_mult[5,45]	
	local uci_mult = res_mult[6,45]
	local p_mult = res_mult[4,45]
	
	post avoidFossil_mult ("`exp'") ("`out'") ("`out_level'") (`n') /// 
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`coef_mult') ///
		(`lci_mult') (`uci_mult') (`p_mult')
		
	* Outcome value = 3/Yes, for climate and non-climate
	local out_level = "for climate and non-climate"
	local coef_uni = res_uni[1,7]
	local lci_uni = res_uni[5,7]	
	local uci_uni = res_uni[6,7]
	local p_uni = res_uni[4,7]
	
	local coef_mult = res_mult[1,67]
	local lci_mult = res_mult[5,67]	
	local uci_mult = res_mult[6,67]
	local p_mult = res_mult[4,67]
	
	post avoidFossil_mult ("`exp'") ("`out'") ("`out_level'") (`n') /// 
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`coef_mult') ///
		(`lci_mult') (`uci_mult') (`p_mult')
		
}

postclose avoidFossil_mult
postclose avoidFossil_mult_lr

* Check results
use ".\Results\avoidFossil_mult_lr.dta", clear

export delimited using ".\Results\avoidFossil_mult_lr.csv", replace


use ".\Results\avoidFossil_mult.dta", clear

export delimited using ".\Results\avoidFossil_mult.csv", replace


** Read back in original dataset
use "B4293_ReligionAndClimate_Processed.dta", clear


**** Avoided fossil fuel companies (binary)

*** Extraversion 

* Logistic model unadjusted
logistic avoidFossil_bin extra if cca_confounders == 1

* And for adjusted model
logistic avoidFossil_bin extra i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum extra if cca_confounder == 1 & avoidFossil_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(extra = (`lower_sd', `mean', `upper_sd'))


*** Agreeableness 

* Logistic model unadjusted
logistic avoidFossil_bin agree if cca_confounders == 1

* And for adjusted model
logistic avoidFossil_bin agree i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum agree if cca_confounder == 1 & avoidFossil_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(agree = (`lower_sd', `mean', `upper_sd'))


*** Conscientiousness

* Logistic model unadjusted
logistic avoidFossil_bin consc if cca_confounders == 1

* And for adjusted model
logistic avoidFossil_bin consc i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum consc if cca_confounder == 1 & avoidFossil_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(consc = (`lower_sd', `mean', `upper_sd'))


*** Emotional stability

* Logistic model unadjusted
logistic avoidFossil_bin emoStab if cca_confounders == 1

* And for adjusted model
logistic avoidFossil_bin emoStab i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum emoStab if cca_confounder == 1 & avoidFossil_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(emoStab = (`lower_sd', `mean', `upper_sd'))


*** Openness to experience

* Logistic model unadjusted
logistic avoidFossil_bin open if cca_confounders == 1

* And for adjusted model
logistic avoidFossil_bin open i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum open if cca_confounder == 1 & avoidFossil_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(open = (`lower_sd', `mean', `upper_sd'))


*** IQ

* Logistic model unadjusted
logistic avoidFossil_bin iq if cca_confounders == 1

* And for adjusted model
logistic avoidFossil_bin iq i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum iq if cca_confounder == 1 & avoidFossil_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(iq = (`lower_sd', `mean', `upper_sd'))


*** Creating post-file to store logistic results to
capture postclose avoidFossil_bin
postfile avoidFossil_bin str30 exposure str30 outcome /// 
	n coef_uni lci_uni uci_uni double(p_uni) r2_uni ///
	coef_mult lci_mult uci_mult double(p_mult) r2_mult ///
	using ".\Results\avoidFossil_bin.dta", replace

foreach var of varlist extra agree consc emoStab open iq {
	
	* Store exposure variable as a macro
	local exp = "`var'"
	
	* Store outcome as macro
	local out = "avoidFossil_bin"
	
	** Univariable analysis
	logistic avoidFossil_bin `var' if cca_confounders == 1
	
	* Store relevant summary statistics
	local n = e(N)
	local r2_uni = e(r2_p)
	local p_uni = e(p)
	
	* Store coefficients
	matrix res = r(table)
	
	local coef_uni = res[1,1]
	local lci_uni = res[5,1]	
	local uci_uni = res[6,1]
	local p_uni = res[4,1]
	
	** Multivariable analysis - Repeat with and without exposure to get change in pseudo-R2 results
	logistic avoidFossil i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if `var' != .
	local r2_mult_base = e(r2_p)
	
	logistic avoidFossil `var' i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge
	local r2_mult = e(r2_p) - `r2_mult_base'
	
	* Store coefficients
	matrix res = r(table)
	
	local coef_mult = res[1,1]
	local lci_mult = res[5,1]	
	local uci_mult = res[6,1]
	local p_mult = res[4,1]
	
	** Post these summary results to the file
	post avoidFossil_bin ("`exp'") ("`out'") (`n') ///
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`r2_uni') ///
		(`coef_mult') (`lci_mult') (`uci_mult') ///
		(`p_mult') (`r2_mult')
		
}

postclose avoidFossil_bin

* Check results
use ".\Results\avoidFossil_bin.dta", clear

export delimited using ".\Results\avoidFossil_bin.csv", replace


** Read back in original dataset
use "B4293_ReligionAndClimate_Processed.dta", clear



*****************************************************************************************
**** Planned fewer children

*** Extraversion 

* Multinomial model unadjusted
mlogit children extra if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit children if cca_confounders == 1 & extra != ., rrr baseoutcome(0)
est store base 

mlogit children extra if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit children i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if extra != ., rrr baseoutcome(0)
est store base 

mlogit children extra i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum extra if cca_confounder == 1 & children != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(extra = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit children extra i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(extra = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen extra = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 extra, col(black)) ///
	(line p1 extra, col(red)) ///
	(line p2 extra, col(blue)) ///
	(line p3 extra, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Extraversion", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(extra_children, replace)

	
use "B4293_ReligionAndClimate_Processed.dta", clear


*** Agreeableness 

* Multinomial model unadjusted
mlogit children agree if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit children if cca_confounders == 1 & agree != ., rrr baseoutcome(0)
est store base 

mlogit children agree if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit children i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if agree != ., rrr baseoutcome(0)
est store base 

mlogit children agree i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum agree if cca_confounder == 1 & children != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(agree = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit children agree i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(agree = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen agree = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 agree, col(black)) ///
	(line p1 agree, col(red)) ///
	(line p2 agree, col(blue)) ///
	(line p3 agree, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Agreeableness", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(agree_children, replace)

	
use "B4293_ReligionAndClimate_Processed.dta", clear


*** Conscientiousness

* Multinomial model unadjusted
mlogit children consc if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit children if cca_confounders == 1 & consc != ., rrr baseoutcome(0)
est store base 

mlogit children consc if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit children i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if consc != ., rrr baseoutcome(0)
est store base 

mlogit children consc i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum consc if cca_confounder == 1 & children != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(consc = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit children consc i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(consc = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen consc = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 consc, col(black)) ///
	(line p1 consc, col(red)) ///
	(line p2 consc, col(blue)) ///
	(line p3 consc, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Conscientiousness", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(consc_children, replace)
	

use "B4293_ReligionAndClimate_Processed.dta", clear


*** Emotional stability

* Multinomial model unadjusted
mlogit children emoStab if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit children if cca_confounders == 1 & emoStab != ., rrr baseoutcome(0)
est store base 

mlogit children emoStab if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit children i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if emoStab != ., rrr baseoutcome(0)
est store base 

mlogit children emoStab i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum emoStab if cca_confounder == 1 & children != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(emoStab = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit children emoStab i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(emoStab = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen emoStab = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 emoStab, col(black)) ///
	(line p1 emoStab, col(red)) ///
	(line p2 emoStab, col(blue)) ///
	(line p3 emoStab, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Emotional stability", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(emoStab_children, replace)
	

use "B4293_ReligionAndClimate_Processed.dta", clear


*** Openness to experience

* Multinomial model unadjusted
mlogit children open if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit children if cca_confounders == 1 & open != ., rrr baseoutcome(0)
est store base 

mlogit children open if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit children i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if open != ., rrr baseoutcome(0)
est store base 

mlogit children open i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum open if cca_confounder == 1 & children != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(open = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit children open i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(open = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen open = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 open, col(black)) ///
	(line p1 open, col(red)) ///
	(line p2 open, col(blue)) ///
	(line p3 open, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Openness to experience", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(open_children, replace)
	
	
use "B4293_ReligionAndClimate_Processed.dta", clear


*** IQ

* Multinomial model unadjusted
mlogit children iq if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit children if cca_confounders == 1 & iq != ., rrr baseoutcome(0)
est store base 

mlogit children iq if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit children i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if iq != ., rrr baseoutcome(0)
est store base 

mlogit children iq i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum iq if cca_confounder == 1 & children != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(iq = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit children iq i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(iq = (70(1)130))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen iq = fill(70 71)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 iq, col(black)) ///
	(line p1 iq, col(red)) ///
	(line p2 iq, col(blue)) ///
	(line p3 iq, col(green)), ///
	xscale(range(65 135)) xlabel(70(10)130, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("IQ scores", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(iq_children, replace)
	

use "B4293_ReligionAndClimate_Processed.dta", clear



** Combine graphs together with a single legend using the 'grc1leg' user-written package
grc1leg extra_children agree_children consc_children emoStab_children open_children iq_children, rows(2) imargin(tiny) ycommon

graph export ".\Results\children_predProbs.pdf", replace

graph close _all


*** Creating various post-files to store results to (individual multinomial RRRs and overall multinomial associations and r2)
capture postclose children_mult
postfile children_mult str30 exposure str30 outcome str30 out_level /// 
	n coef_uni lci_uni uci_uni double(p_uni) ///
	coef_mult lci_mult uci_mult double(p_mult) ///
	using ".\Results\children_mult.dta", replace
	
capture postclose children_mult_lr
postfile children_mult_lr str30 exposure str30 outcome /// 
	n double(p_uni) r2_uni double(p_mult) r2_mult ///
	using ".\Results\children_mult_lr.dta", replace

foreach var of varlist extra agree consc emoStab open iq {
	
	* Store exposure variable as a macro
	local exp = "`var'"
	
	* Store outcome as macro
	local out = "children"
	
	** Univariable analysis
	mlogit children `var' if cca_confounders == 1, baseoutcome(0) rrr
	
	* Store relevant summary statistics
	local n = e(N)
	local r2_uni = e(r2_p)
	local p_uni = e(p)
	
	* Store coefficients
	matrix res_uni = r(table)
	
	** Multivariable analysis - Repeat with and without exposure to get change in pseudo-R2 and LR test results
	mlogit children i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if `var' != ., baseoutcome(0) rrr
	local r2_mult_base = e(r2_p)
	est store base
	
	mlogit children `var' i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, baseoutcome(0) rrr
	local r2_mult = e(r2_p) - `r2_mult_base'
	est store main
	
	* Store coefficients
	matrix res_mult = r(table)
	
	* LR test
	lrtest base main
	local p_mult = r(p)
	
	** Post these summary results to the file
	post children_mult_lr ("`exp'") ("`out'") (`n') (`p_uni') (`r2_uni') ///
		(`p_mult') (`r2_mult')
		
	
	** Now focus on coefficient results
	
	* Outcome value = 1/Yes for climate
	local out_level = "for climate"
	local coef_uni = res_uni[1,3]
	local lci_uni = res_uni[5,3]	
	local uci_uni = res_uni[6,3]
	local p_uni = res_uni[4,3]
	
	local coef_mult = res_mult[1,23]
	local lci_mult = res_mult[5,23]	
	local uci_mult = res_mult[6,23]
	local p_mult = res_mult[4,23]
	
	post children_mult ("`exp'") ("`out'") ("`out_level'") (`n') /// 
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`coef_mult') ///
		(`lci_mult') (`uci_mult') (`p_mult')
	
	* Outcome value = 2/Yes, for non-climate
	local out_level = "for non-climate"
	local coef_uni = res_uni[1,5]
	local lci_uni = res_uni[5,5]	
	local uci_uni = res_uni[6,5]
	local p_uni = res_uni[4,5]
	
	local coef_mult = res_mult[1,45]
	local lci_mult = res_mult[5,45]	
	local uci_mult = res_mult[6,45]
	local p_mult = res_mult[4,45]
	
	post children_mult ("`exp'") ("`out'") ("`out_level'") (`n') /// 
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`coef_mult') ///
		(`lci_mult') (`uci_mult') (`p_mult')
		
	* Outcome value = 3/Yes, for climate and non-climate
	local out_level = "for climate and non-climate"
	local coef_uni = res_uni[1,7]
	local lci_uni = res_uni[5,7]	
	local uci_uni = res_uni[6,7]
	local p_uni = res_uni[4,7]
	
	local coef_mult = res_mult[1,67]
	local lci_mult = res_mult[5,67]	
	local uci_mult = res_mult[6,67]
	local p_mult = res_mult[4,67]
	
	post children_mult ("`exp'") ("`out'") ("`out_level'") (`n') /// 
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`coef_mult') ///
		(`lci_mult') (`uci_mult') (`p_mult')
		
}

postclose children_mult
postclose children_mult_lr

* Check results
use ".\Results\children_mult_lr.dta", clear

export delimited using ".\Results\children_mult_lr.csv", replace


use ".\Results\children_mult.dta", clear

export delimited using ".\Results\children_mult.csv", replace


** Read back in original dataset
use "B4293_ReligionAndClimate_Processed.dta", clear


**** Planned fewer children (binary)

*** Extraversion 

* Logistic model unadjusted
logistic children_bin extra if cca_confounders == 1

* And for adjusted model
logistic children_bin extra i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum extra if cca_confounder == 1 & children_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(extra = (`lower_sd', `mean', `upper_sd'))


*** Agreeableness 

* Logistic model unadjusted
logistic children_bin agree if cca_confounders == 1

* And for adjusted model
logistic children_bin agree i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum agree if cca_confounder == 1 & children_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(agree = (`lower_sd', `mean', `upper_sd'))


*** Conscientiousness

* Logistic model unadjusted
logistic children_bin consc if cca_confounders == 1

* And for adjusted model
logistic children_bin consc i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum consc if cca_confounder == 1 & children_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(consc = (`lower_sd', `mean', `upper_sd'))


*** Emotional stability

* Logistic model unadjusted
logistic children_bin emoStab if cca_confounders == 1

* And for adjusted model
logistic children_bin emoStab i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum emoStab if cca_confounder == 1 & children_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(emoStab = (`lower_sd', `mean', `upper_sd'))


*** Openness to experience

* Logistic model unadjusted
logistic children_bin open if cca_confounders == 1

* And for adjusted model
logistic children_bin open i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum open if cca_confounder == 1 & children_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(open = (`lower_sd', `mean', `upper_sd'))


*** IQ

* Logistic model unadjusted
logistic children_bin iq if cca_confounders == 1

* And for adjusted model
logistic children_bin iq i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum iq if cca_confounder == 1 & children_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(iq = (`lower_sd', `mean', `upper_sd'))


*** Creating post-file to store logistic results to
capture postclose children_bin
postfile children_bin str30 exposure str30 outcome /// 
	n coef_uni lci_uni uci_uni double(p_uni) r2_uni ///
	coef_mult lci_mult uci_mult double(p_mult) r2_mult ///
	using ".\Results\children_bin.dta", replace

foreach var of varlist extra agree consc emoStab open iq {
	
	* Store exposure variable as a macro
	local exp = "`var'"
	
	* Store outcome as macro
	local out = "children_bin"
	
	** Univariable analysis
	logistic children_bin `var' if cca_confounders == 1
	
	* Store relevant summary statistics
	local n = e(N)
	local r2_uni = e(r2_p)
	local p_uni = e(p)
	
	* Store coefficients
	matrix res = r(table)
	
	local coef_uni = res[1,1]
	local lci_uni = res[5,1]	
	local uci_uni = res[6,1]
	local p_uni = res[4,1]
	
	** Multivariable analysis - Repeat with and without exposure to get change in pseudo-R2 results
	logistic children i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if `var' != .
	local r2_mult_base = e(r2_p)
	
	logistic children `var' i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge
	local r2_mult = e(r2_p) - `r2_mult_base'
	
	* Store coefficients
	matrix res = r(table)
	
	local coef_mult = res[1,1]
	local lci_mult = res[5,1]	
	local uci_mult = res[6,1]
	local p_mult = res[4,1]
	
	** Post these summary results to the file
	post children_bin ("`exp'") ("`out'") (`n') ///
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`r2_uni') ///
		(`coef_mult') (`lci_mult') (`uci_mult') ///
		(`p_mult') (`r2_mult')
		
}

postclose children_bin

* Check results
use ".\Results\children_bin.dta", clear

export delimited using ".\Results\children_bin.csv", replace


** Read back in original dataset
use "B4293_ReligionAndClimate_Processed.dta", clear



*****************************************************************************************
**** Other environmental action

*** Extraversion 

* Multinomial model unadjusted
mlogit otherAction extra if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit otherAction if cca_confounders == 1 & extra != ., rrr baseoutcome(0)
est store base 

mlogit otherAction extra if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit otherAction i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if extra != ., rrr baseoutcome(0)
est store base 

mlogit otherAction extra i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum extra if cca_confounder == 1 & otherAction != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(extra = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit otherAction extra i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(extra = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen extra = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 extra, col(black)) ///
	(line p1 extra, col(red)) ///
	(line p2 extra, col(blue)) ///
	(line p3 extra, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Extraversion", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(extra_otherAction, replace)

	
use "B4293_ReligionAndClimate_Processed.dta", clear


*** Agreeableness 

* Multinomial model unadjusted
mlogit otherAction agree if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit otherAction if cca_confounders == 1 & agree != ., rrr baseoutcome(0)
est store base 

mlogit otherAction agree if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit otherAction i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if agree != ., rrr baseoutcome(0)
est store base 

mlogit otherAction agree i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum agree if cca_confounder == 1 & otherAction != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(agree = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit otherAction agree i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(agree = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen agree = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 agree, col(black)) ///
	(line p1 agree, col(red)) ///
	(line p2 agree, col(blue)) ///
	(line p3 agree, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Agreeableness", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(agree_otherAction, replace)

	
use "B4293_ReligionAndClimate_Processed.dta", clear


*** Conscientiousness

* Multinomial model unadjusted
mlogit otherAction consc if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit otherAction if cca_confounders == 1 & consc != ., rrr baseoutcome(0)
est store base 

mlogit otherAction consc if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit otherAction i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if consc != ., rrr baseoutcome(0)
est store base 

mlogit otherAction consc i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum consc if cca_confounder == 1 & otherAction != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(consc = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit otherAction consc i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(consc = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen consc = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 consc, col(black)) ///
	(line p1 consc, col(red)) ///
	(line p2 consc, col(blue)) ///
	(line p3 consc, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Conscientiousness", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(consc_otherAction, replace)
	

use "B4293_ReligionAndClimate_Processed.dta", clear


*** Emotional stability

* Multinomial model unadjusted
mlogit otherAction emoStab if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit otherAction if cca_confounders == 1 & emoStab != ., rrr baseoutcome(0)
est store base 

mlogit otherAction emoStab if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit otherAction i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if emoStab != ., rrr baseoutcome(0)
est store base 

mlogit otherAction emoStab i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum emoStab if cca_confounder == 1 & otherAction != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(emoStab = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit otherAction emoStab i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(emoStab = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen emoStab = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 emoStab, col(black)) ///
	(line p1 emoStab, col(red)) ///
	(line p2 emoStab, col(blue)) ///
	(line p3 emoStab, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Emotional stability", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(emoStab_otherAction, replace)
	

use "B4293_ReligionAndClimate_Processed.dta", clear


*** Openness to experience

* Multinomial model unadjusted
mlogit otherAction open if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit otherAction if cca_confounders == 1 & open != ., rrr baseoutcome(0)
est store base 

mlogit otherAction open if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit otherAction i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if open != ., rrr baseoutcome(0)
est store base 

mlogit otherAction open i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum open if cca_confounder == 1 & otherAction != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(open = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit otherAction open i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(open = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen open = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 open, col(black)) ///
	(line p1 open, col(red)) ///
	(line p2 open, col(blue)) ///
	(line p3 open, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Openness to experience", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(open_otherAction, replace)
	
	
use "B4293_ReligionAndClimate_Processed.dta", clear


*** IQ

* Multinomial model unadjusted
mlogit otherAction iq if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit otherAction if cca_confounders == 1 & iq != ., rrr baseoutcome(0)
est store base 

mlogit otherAction iq if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit otherAction i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if iq != ., rrr baseoutcome(0)
est store base 

mlogit otherAction iq i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum iq if cca_confounder == 1 & otherAction != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(iq = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit otherAction iq i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(iq = (70(1)130))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen iq = fill(70 71)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 iq, col(black)) ///
	(line p1 iq, col(red)) ///
	(line p2 iq, col(blue)) ///
	(line p3 iq, col(green)), ///
	xscale(range(65 135)) xlabel(70(10)130, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("IQ scores", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(iq_otherAction, replace)
	

use "B4293_ReligionAndClimate_Processed.dta", clear



** Combine graphs together with a single legend using the 'grc1leg' user-written package
grc1leg extra_otherAction agree_otherAction consc_otherAction emoStab_otherAction open_otherAction iq_otherAction, rows(2) imargin(tiny) ycommon

graph export ".\Results\otherAction_predProbs.pdf", replace

graph close _all


*** Creating various post-files to store results to (individual multinomial RRRs and overall multinomial associations and r2)
capture postclose otherAction_mult
postfile otherAction_mult str30 exposure str30 outcome str30 out_level /// 
	n coef_uni lci_uni uci_uni double(p_uni) ///
	coef_mult lci_mult uci_mult double(p_mult) ///
	using ".\Results\otherAction_mult.dta", replace
	
capture postclose otherAction_mult_lr
postfile otherAction_mult_lr str30 exposure str30 outcome /// 
	n double(p_uni) r2_uni double(p_mult) r2_mult ///
	using ".\Results\otherAction_mult_lr.dta", replace

foreach var of varlist extra agree consc emoStab open iq {
	
	* Store exposure variable as a macro
	local exp = "`var'"
	
	* Store outcome as macro
	local out = "otherAction"
	
	** Univariable analysis
	mlogit otherAction `var' if cca_confounders == 1, baseoutcome(0) rrr
	
	* Store relevant summary statistics
	local n = e(N)
	local r2_uni = e(r2_p)
	local p_uni = e(p)
	
	* Store coefficients
	matrix res_uni = r(table)
	
	** Multivariable analysis - Repeat with and without exposure to get change in pseudo-R2 and LR test results
	mlogit otherAction i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if `var' != ., baseoutcome(0) rrr
	local r2_mult_base = e(r2_p)
	est store base
	
	mlogit otherAction `var' i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, baseoutcome(0) rrr
	local r2_mult = e(r2_p) - `r2_mult_base'
	est store main
	
	* Store coefficients
	matrix res_mult = r(table)
	
	* LR test
	lrtest base main
	local p_mult = r(p)
	
	** Post these summary results to the file
	post otherAction_mult_lr ("`exp'") ("`out'") (`n') (`p_uni') (`r2_uni') ///
		(`p_mult') (`r2_mult')
		
	
	** Now focus on coefficient results
	
	* Outcome value = 1/Yes for climate
	local out_level = "for climate"
	local coef_uni = res_uni[1,3]
	local lci_uni = res_uni[5,3]	
	local uci_uni = res_uni[6,3]
	local p_uni = res_uni[4,3]
	
	local coef_mult = res_mult[1,23]
	local lci_mult = res_mult[5,23]	
	local uci_mult = res_mult[6,23]
	local p_mult = res_mult[4,23]
	
	post otherAction_mult ("`exp'") ("`out'") ("`out_level'") (`n') /// 
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`coef_mult') ///
		(`lci_mult') (`uci_mult') (`p_mult')
	
	* Outcome value = 2/Yes, for non-climate
	local out_level = "for non-climate"
	local coef_uni = res_uni[1,5]
	local lci_uni = res_uni[5,5]	
	local uci_uni = res_uni[6,5]
	local p_uni = res_uni[4,5]
	
	local coef_mult = res_mult[1,45]
	local lci_mult = res_mult[5,45]	
	local uci_mult = res_mult[6,45]
	local p_mult = res_mult[4,45]
	
	post otherAction_mult ("`exp'") ("`out'") ("`out_level'") (`n') /// 
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`coef_mult') ///
		(`lci_mult') (`uci_mult') (`p_mult')
		
	* Outcome value = 3/Yes, for climate and non-climate
	local out_level = "for climate and non-climate"
	local coef_uni = res_uni[1,7]
	local lci_uni = res_uni[5,7]	
	local uci_uni = res_uni[6,7]
	local p_uni = res_uni[4,7]
	
	local coef_mult = res_mult[1,67]
	local lci_mult = res_mult[5,67]	
	local uci_mult = res_mult[6,67]
	local p_mult = res_mult[4,67]
	
	post otherAction_mult ("`exp'") ("`out'") ("`out_level'") (`n') /// 
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`coef_mult') ///
		(`lci_mult') (`uci_mult') (`p_mult')
		
}

postclose otherAction_mult
postclose otherAction_mult_lr

* Check results
use ".\Results\otherAction_mult_lr.dta", clear

export delimited using ".\Results\otherAction_mult_lr.csv", replace


use ".\Results\otherAction_mult.dta", clear

export delimited using ".\Results\otherAction_mult.csv", replace


** Read back in original dataset
use "B4293_ReligionAndClimate_Processed.dta", clear


**** Other environmental actions (binary)

*** Extraversion 

* Logistic model unadjusted
logistic otherAction_bin extra if cca_confounders == 1

* And for adjusted model
logistic otherAction_bin extra i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum extra if cca_confounder == 1 & otherAction_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(extra = (`lower_sd', `mean', `upper_sd'))


*** Agreeableness 

* Logistic model unadjusted
logistic otherAction_bin agree if cca_confounders == 1

* And for adjusted model
logistic otherAction_bin agree i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum agree if cca_confounder == 1 & otherAction_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(agree = (`lower_sd', `mean', `upper_sd'))


*** Conscientiousness

* Logistic model unadjusted
logistic otherAction_bin consc if cca_confounders == 1

* And for adjusted model
logistic otherAction_bin consc i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum consc if cca_confounder == 1 & otherAction_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(consc = (`lower_sd', `mean', `upper_sd'))


*** Emotional stability

* Logistic model unadjusted
logistic otherAction_bin emoStab if cca_confounders == 1

* And for adjusted model
logistic otherAction_bin emoStab i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum emoStab if cca_confounder == 1 & otherAction_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(emoStab = (`lower_sd', `mean', `upper_sd'))


*** Openness to experience

* Logistic model unadjusted
logistic otherAction_bin open if cca_confounders == 1

* And for adjusted model
logistic otherAction_bin open i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum open if cca_confounder == 1 & otherAction_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(open = (`lower_sd', `mean', `upper_sd'))


*** IQ

* Logistic model unadjusted
logistic otherAction_bin iq if cca_confounders == 1

* And for adjusted model
logistic otherAction_bin iq i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum iq if cca_confounder == 1 & otherAction_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(iq = (`lower_sd', `mean', `upper_sd'))


*** Creating post-file to store logistic results to
capture postclose otherAction_bin
postfile otherAction_bin str30 exposure str30 outcome /// 
	n coef_uni lci_uni uci_uni double(p_uni) r2_uni ///
	coef_mult lci_mult uci_mult double(p_mult) r2_mult ///
	using ".\Results\otherAction_bin.dta", replace

foreach var of varlist extra agree consc emoStab open iq {
	
	* Store exposure variable as a macro
	local exp = "`var'"
	
	* Store outcome as macro
	local out = "otherAction_bin"
	
	** Univariable analysis
	logistic otherAction_bin `var' if cca_confounders == 1
	
	* Store relevant summary statistics
	local n = e(N)
	local r2_uni = e(r2_p)
	local p_uni = e(p)
	
	* Store coefficients
	matrix res = r(table)
	
	local coef_uni = res[1,1]
	local lci_uni = res[5,1]	
	local uci_uni = res[6,1]
	local p_uni = res[4,1]
	
	** Multivariable analysis - Repeat with and without exposure to get change in pseudo-R2 results
	logistic otherAction i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if `var' != .
	local r2_mult_base = e(r2_p)
	
	logistic otherAction `var' i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge
	local r2_mult = e(r2_p) - `r2_mult_base'
	
	* Store coefficients
	matrix res = r(table)
	
	local coef_mult = res[1,1]
	local lci_mult = res[5,1]	
	local uci_mult = res[6,1]
	local p_mult = res[4,1]
	
	** Post these summary results to the file
	post otherAction_bin ("`exp'") ("`out'") (`n') ///
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`r2_uni') ///
		(`coef_mult') (`lci_mult') (`uci_mult') ///
		(`p_mult') (`r2_mult')
		
}

postclose otherAction_bin

* Check results
use ".\Results\otherAction_bin.dta", clear

export delimited using ".\Results\otherAction_bin.csv", replace


** Read back in original dataset
use "B4293_ReligionAndClimate_Processed.dta", clear



*****************************************************************************************
**** Reduced meat/dairy

*** Extraversion 

* Multinomial model unadjusted
mlogit meatDairy extra if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit meatDairy if cca_confounders == 1 & extra != ., rrr baseoutcome(0)
est store base 

mlogit meatDairy extra if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit meatDairy i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if extra != ., rrr baseoutcome(0)
est store base 

mlogit meatDairy extra i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum extra if cca_confounder == 1 & meatDairy != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(extra = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit meatDairy extra i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(extra = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen extra = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 extra, col(black)) ///
	(line p1 extra, col(red)) ///
	(line p2 extra, col(blue)) ///
	(line p3 extra, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Extraversion", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(extra_meatDairy, replace)

	
use "B4293_ReligionAndClimate_Processed.dta", clear


*** Agreeableness 

* Multinomial model unadjusted
mlogit meatDairy agree if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit meatDairy if cca_confounders == 1 & agree != ., rrr baseoutcome(0)
est store base 

mlogit meatDairy agree if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit meatDairy i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if agree != ., rrr baseoutcome(0)
est store base 

mlogit meatDairy agree i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum agree if cca_confounder == 1 & meatDairy != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(agree = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit meatDairy agree i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(agree = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen agree = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 agree, col(black)) ///
	(line p1 agree, col(red)) ///
	(line p2 agree, col(blue)) ///
	(line p3 agree, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Agreeableness", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(agree_meatDairy, replace)

	
use "B4293_ReligionAndClimate_Processed.dta", clear


*** Conscientiousness

* Multinomial model unadjusted
mlogit meatDairy consc if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit meatDairy if cca_confounders == 1 & consc != ., rrr baseoutcome(0)
est store base 

mlogit meatDairy consc if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit meatDairy i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if consc != ., rrr baseoutcome(0)
est store base 

mlogit meatDairy consc i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum consc if cca_confounder == 1 & meatDairy != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(consc = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit meatDairy consc i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(consc = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen consc = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 consc, col(black)) ///
	(line p1 consc, col(red)) ///
	(line p2 consc, col(blue)) ///
	(line p3 consc, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Conscientiousness", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(consc_meatDairy, replace)
	

use "B4293_ReligionAndClimate_Processed.dta", clear


*** Emotional stability

* Multinomial model unadjusted
mlogit meatDairy emoStab if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit meatDairy if cca_confounders == 1 & emoStab != ., rrr baseoutcome(0)
est store base 

mlogit meatDairy emoStab if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit meatDairy i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if emoStab != ., rrr baseoutcome(0)
est store base 

mlogit meatDairy emoStab i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum emoStab if cca_confounder == 1 & meatDairy != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(emoStab = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit meatDairy emoStab i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(emoStab = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen emoStab = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 emoStab, col(black)) ///
	(line p1 emoStab, col(red)) ///
	(line p2 emoStab, col(blue)) ///
	(line p3 emoStab, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Emotional stability", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(emoStab_meatDairy, replace)
	

use "B4293_ReligionAndClimate_Processed.dta", clear


*** Openness to experience

* Multinomial model unadjusted
mlogit meatDairy open if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit meatDairy if cca_confounders == 1 & open != ., rrr baseoutcome(0)
est store base 

mlogit meatDairy open if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit meatDairy i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if open != ., rrr baseoutcome(0)
est store base 

mlogit meatDairy open i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum open if cca_confounder == 1 & meatDairy != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(open = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit meatDairy open i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(open = (20(1)50))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen open = fill(20 21)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 open, col(black)) ///
	(line p1 open, col(red)) ///
	(line p2 open, col(blue)) ///
	(line p3 open, col(green)), ///
	xscale(range(17 53)) xlabel(20(5)50, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("Openness to experience", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(open_meatDairy, replace)
	
	
use "B4293_ReligionAndClimate_Processed.dta", clear


*** IQ

* Multinomial model unadjusted
mlogit meatDairy iq if cca_confounders == 1, rrr baseoutcome(0)

* Does inclusion of exposure improve model fit via likelihood ratio test
mlogit meatDairy if cca_confounders == 1 & iq != ., rrr baseoutcome(0)
est store base 

mlogit meatDairy iq if cca_confounders == 1, rrr baseoutcome(0)
est store main

lrtest base main

* And for adjusted model
mlogit meatDairy i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if iq != ., rrr baseoutcome(0)
est store base 

mlogit meatDairy iq i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)
est store main

lrtest base main

* Predicted probabilities
sum iq if cca_confounder == 1 & meatDairy != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(iq = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted probabilities
mlogit meatDairy iq i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, rrr baseoutcome(0)

margins, at(iq = (70(1)130))

matrix res = r(table)
matrix list res

local n = colsof(res)/4

clear 
set obs `n'
egen iq = fill(70 71)
gen p0 = .
gen p1 = .
gen p2 = .
gen p3 = .

forvalues i = 1(1)`n' {
	replace p0 = res[1,`i'] if _n == `i'
	replace p1 = res[1,`n' + `i'] if _n == `i'
	replace p2 = res[1,`n' + `n' + `i'] if _n == `i'
	replace p3 = res[1,`n' + `n' + `n' + `i'] if _n == `i'
}

list, clean

twoway (line p0 iq, col(black)) ///
	(line p1 iq, col(red)) ///
	(line p2 iq, col(blue)) ///
	(line p3 iq, col(green)), ///
	xscale(range(65 135)) xlabel(70(10)130, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted probability") yscale(titlegap(2)) ///
	title("IQ scores", size(medium)) ///
	legend(order(1 "No" 2 "Yes, climate reasons" 3 "Yes, non-climate reasons" ///
		4 "Yes, climate and non-climate reasons") ///
		rows(1) size(small) symxsize(*0.5)) ///
	name(iq_meatDairy, replace)
	

use "B4293_ReligionAndClimate_Processed.dta", clear



** Combine graphs together with a single legend using the 'grc1leg' user-written package
grc1leg extra_meatDairy agree_meatDairy consc_meatDairy emoStab_meatDairy open_meatDairy iq_meatDairy, rows(2) imargin(tiny) ycommon

graph export ".\Results\meatDairy_predProbs.pdf", replace

graph close _all


*** Creating various post-files to store results to (individual multinomial RRRs and overall multinomial associations and r2)
capture postclose meatDairy_mult
postfile meatDairy_mult str30 exposure str30 outcome str30 out_level /// 
	n coef_uni lci_uni uci_uni double(p_uni) ///
	coef_mult lci_mult uci_mult double(p_mult) ///
	using ".\Results\meatDairy_mult.dta", replace
	
capture postclose meatDairy_mult_lr
postfile meatDairy_mult_lr str30 exposure str30 outcome /// 
	n double(p_uni) r2_uni double(p_mult) r2_mult ///
	using ".\Results\meatDairy_mult_lr.dta", replace

foreach var of varlist extra agree consc emoStab open iq {
	
	* Store exposure variable as a macro
	local exp = "`var'"
	
	* Store outcome as macro
	local out = "meatDairy"
	
	** Univariable analysis
	mlogit meatDairy `var' if cca_confounders == 1, baseoutcome(0) rrr
	
	* Store relevant summary statistics
	local n = e(N)
	local r2_uni = e(r2_p)
	local p_uni = e(p)
	
	* Store coefficients
	matrix res_uni = r(table)
	
	** Multivariable analysis - Repeat with and without exposure to get change in pseudo-R2 and LR test results
	mlogit meatDairy i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if `var' != ., baseoutcome(0) rrr
	local r2_mult_base = e(r2_p)
	est store base
	
	mlogit meatDairy `var' i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, baseoutcome(0) rrr
	local r2_mult = e(r2_p) - `r2_mult_base'
	est store main
	
	* Store coefficients
	matrix res_mult = r(table)
	
	* LR test
	lrtest base main
	local p_mult = r(p)
	
	** Post these summary results to the file
	post meatDairy_mult_lr ("`exp'") ("`out'") (`n') (`p_uni') (`r2_uni') ///
		(`p_mult') (`r2_mult')
		
	
	** Now focus on coefficient results
	
	* Outcome value = 1/Yes for climate
	local out_level = "for climate"
	local coef_uni = res_uni[1,3]
	local lci_uni = res_uni[5,3]	
	local uci_uni = res_uni[6,3]
	local p_uni = res_uni[4,3]
	
	local coef_mult = res_mult[1,23]
	local lci_mult = res_mult[5,23]	
	local uci_mult = res_mult[6,23]
	local p_mult = res_mult[4,23]
	
	post meatDairy_mult ("`exp'") ("`out'") ("`out_level'") (`n') /// 
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`coef_mult') ///
		(`lci_mult') (`uci_mult') (`p_mult')
	
	* Outcome value = 2/Yes, for non-climate
	local out_level = "for non-climate"
	local coef_uni = res_uni[1,5]
	local lci_uni = res_uni[5,5]	
	local uci_uni = res_uni[6,5]
	local p_uni = res_uni[4,5]
	
	local coef_mult = res_mult[1,45]
	local lci_mult = res_mult[5,45]	
	local uci_mult = res_mult[6,45]
	local p_mult = res_mult[4,45]
	
	post meatDairy_mult ("`exp'") ("`out'") ("`out_level'") (`n') /// 
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`coef_mult') ///
		(`lci_mult') (`uci_mult') (`p_mult')
		
	* Outcome value = 3/Yes, for climate and non-climate
	local out_level = "for climate and non-climate"
	local coef_uni = res_uni[1,7]
	local lci_uni = res_uni[5,7]	
	local uci_uni = res_uni[6,7]
	local p_uni = res_uni[4,7]
	
	local coef_mult = res_mult[1,67]
	local lci_mult = res_mult[5,67]	
	local uci_mult = res_mult[6,67]
	local p_mult = res_mult[4,67]
	
	post meatDairy_mult ("`exp'") ("`out'") ("`out_level'") (`n') /// 
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`coef_mult') ///
		(`lci_mult') (`uci_mult') (`p_mult')
		
}

postclose meatDairy_mult
postclose meatDairy_mult_lr

* Check results
use ".\Results\meatDairy_mult_lr.dta", clear

export delimited using ".\Results\meatDairy_mult_lr.csv", replace


use ".\Results\meatDairy_mult.dta", clear

export delimited using ".\Results\meatDairy_mult.csv", replace


** Read back in original dataset
use "B4293_ReligionAndClimate_Processed.dta", clear


**** Reduced meat/dairy (binary)

*** Extraversion 

* Logistic model unadjusted
logistic meatDairy_bin extra if cca_confounders == 1

* And for adjusted model
logistic meatDairy_bin extra i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum extra if cca_confounder == 1 & meatDairy_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(extra = (`lower_sd', `mean', `upper_sd'))


*** Agreeableness 

* Logistic model unadjusted
logistic meatDairy_bin agree if cca_confounders == 1

* And for adjusted model
logistic meatDairy_bin agree i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum agree if cca_confounder == 1 & meatDairy_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(agree = (`lower_sd', `mean', `upper_sd'))


*** Conscientiousness

* Logistic model unadjusted
logistic meatDairy_bin consc if cca_confounders == 1

* And for adjusted model
logistic meatDairy_bin consc i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum consc if cca_confounder == 1 & meatDairy_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(consc = (`lower_sd', `mean', `upper_sd'))


*** Emotional stability

* Logistic model unadjusted
logistic meatDairy_bin emoStab if cca_confounders == 1

* And for adjusted model
logistic meatDairy_bin emoStab i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum emoStab if cca_confounder == 1 & meatDairy_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(emoStab = (`lower_sd', `mean', `upper_sd'))


*** Openness to experience

* Logistic model unadjusted
logistic meatDairy_bin open if cca_confounders == 1

* And for adjusted model
logistic meatDairy_bin open i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum open if cca_confounder == 1 & meatDairy_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(open = (`lower_sd', `mean', `upper_sd'))


*** IQ

* Logistic model unadjusted
logistic meatDairy_bin iq if cca_confounders == 1

* And for adjusted model
logistic meatDairy_bin iq i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted probabilities
sum iq if cca_confounder == 1 & meatDairy_bin != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(iq = (`lower_sd', `mean', `upper_sd'))


*** Creating post-file to store logistic results to
capture postclose meatDairy_bin
postfile meatDairy_bin str30 exposure str30 outcome /// 
	n coef_uni lci_uni uci_uni double(p_uni) r2_uni ///
	coef_mult lci_mult uci_mult double(p_mult) r2_mult ///
	using ".\Results\meatDairy_bin.dta", replace

foreach var of varlist extra agree consc emoStab open iq {
	
	* Store exposure variable as a macro
	local exp = "`var'"
	
	* Store outcome as macro
	local out = "meatDairy_bin"
	
	** Univariable analysis
	logistic meatDairy_bin `var' if cca_confounders == 1
	
	* Store relevant summary statistics
	local n = e(N)
	local r2_uni = e(r2_p)
	local p_uni = e(p)
	
	* Store coefficients
	matrix res = r(table)
	
	local coef_uni = res[1,1]
	local lci_uni = res[5,1]	
	local uci_uni = res[6,1]
	local p_uni = res[4,1]
	
	** Multivariable analysis - Repeat with and without exposure to get change in pseudo-R2 results
	logistic meatDairy i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if `var' != .
	local r2_mult_base = e(r2_p)
	
	logistic meatDairy `var' i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge
	local r2_mult = e(r2_p) - `r2_mult_base'
	
	* Store coefficients
	matrix res = r(table)
	
	local coef_mult = res[1,1]
	local lci_mult = res[5,1]	
	local uci_mult = res[6,1]
	local p_mult = res[4,1]
	
	** Post these summary results to the file
	post meatDairy_bin ("`exp'") ("`out'") (`n') ///
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`r2_uni') ///
		(`coef_mult') (`lci_mult') (`uci_mult') ///
		(`p_mult') (`r2_mult')
		
}

postclose meatDairy_bin

* Check results
use ".\Results\meatDairy_bin.dta", clear

export delimited using ".\Results\meatDairy_bin.csv", replace


** Read back in original dataset
use "B4293_ReligionAndClimate_Processed.dta", clear



*****************************************************************************************
**** Total number of pro-environmental actions

** Descriptive stats and histogram
sum total_actions

hist total_actions, discrete freq

*** Is a very non-normal distribution, with a spike at '0', and then a relatively normal distribution for all other values. Will compare results of linear model, poisson model, and zero-inflated poisson model, to make sure that results are comparable, regardless of the distributional assumptions made.


**** Linear models

*** Extraversion 

* Linear model unadjusted
regress total_actions extra if cca_confounders == 1

* And for adjusted model
regress total_actions extra i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted outcomes
sum extra if cca_confounder == 1 & total_actions != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(extra = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted outcomes
capture drop p1
predict p1 if total_actions != .
sum p1

twoway (scatter p1 extra, col(black) msym(D) msize(vsmall) ///
	jitter(2) jitterseed(1234)) ///
	(lfitci p1 extra, ciplot(rline) lwidth(medthick)), ///
	xlabel(, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted number of actions") ///
	title("Extraversion", size(medium)) ///
	legend(off) ///
	name(extra_actions_lm, replace)


*** Agreeableness 

* Linear model unadjusted
regress total_actions agree if cca_confounders == 1

* And for adjusted model
regress total_actions agree i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted outcomes
sum agree if cca_confounder == 1 & total_actions != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(agree = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted outcomes
capture drop p1
predict p1 if total_actions != .
sum p1

twoway (scatter p1 agree, col(black) msym(D) msize(vsmall) ///
	jitter(2) jitterseed(1234)) ///
	(lfitci p1 agree, ciplot(rline) lwidth(medthick)), ///
	xlabel(, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted number of actions") ///
	title("Agreeableness", size(medium)) ///
	legend(off) ///
	name(agree_actions_lm, replace)
	
	

*** Conscientiousness

* Linear model unadjusted
regress total_actions consc if cca_confounders == 1

* And for adjusted model
regress total_actions consc i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted outcomes
sum consc if cca_confounder == 1 & total_actions != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(consc = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted outcomes
capture drop p1
predict p1 if total_actions != .
sum p1

twoway (scatter p1 consc, col(black) msym(D) msize(vsmall) ///
	jitter(2) jitterseed(1234)) ///
	(lfitci p1 consc, ciplot(rline) lwidth(medthick)), ///
	xlabel(, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted number of actions") ///
	title("Conscientiousness", size(medium)) ///
	legend(off) ///
	name(consc_actions_lm, replace)

	
	
*** Emotional stability

* Linear model unadjusted
regress total_actions emoStab if cca_confounders == 1

* And for adjusted model
regress total_actions emoStab i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted outcomes
sum emoStab if cca_confounder == 1 & total_actions != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(emoStab = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted outcomes
capture drop p1
predict p1 if total_actions != .
sum p1

twoway (scatter p1 emoStab, col(black) msym(D) msize(vsmall) ///
	jitter(2) jitterseed(1234)) ///
	(lfitci p1 emoStab, ciplot(rline) lwidth(medthick)), ///
	xlabel(, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted number of actions") ///
	title("Emotional Stability", size(medium)) ///
	legend(off) ///
	name(emoStab_actions_lm, replace)
	


*** Openness to experience

* Linear model unadjusted
regress total_actions open if cca_confounders == 1

* And for adjusted model
regress total_actions open i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted outcomes
sum open if cca_confounder == 1 & total_actions != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(open = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted outcomes
capture drop p1
predict p1 if total_actions != .
sum p1

twoway (scatter p1 open, col(black) msym(D) msize(vsmall) ///
	jitter(2) jitterseed(1234)) ///
	(lfitci p1 open, ciplot(rline) lwidth(medthick)), ///
	xlabel(, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted number of actions") ///
	title("Openness to experience", size(medium)) ///
	legend(off) ///
	name(open_actions_lm, replace)
	


*** IQ

* Linear model unadjusted
regress total_actions iq if cca_confounders == 1

* And for adjusted model
regress total_actions iq i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge

* Predicted outcomes
sum iq if cca_confounder == 1 & total_actions != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(iq = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted outcomes
capture drop p1
predict p1 if total_actions != .
sum p1

twoway (scatter p1 iq, col(black) msym(D) msize(vsmall) ///
	jitter(2) jitterseed(1234)) ///
	(lfitci p1 iq, ciplot(rline) lwidth(medthick)), ///
	xlabel(, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted number of actions") ///
	title("IQ scores", size(medium)) ///
	legend(off) ///
	name(iq_actions_lm, replace)
	

** Combine graphs together
graph combine extra_actions_lm agree_actions_lm consc_actions_lm emoStab_actions_lm open_actions_lm iq_actions_lm, rows(2) imargin(tiny) ycommon

graph export ".\Results\totalActions_lm_predOutcomes.pdf", replace

graph close _all


*** Creating post-file to store results to
capture postclose totalActions_lm
postfile totalActions_lm str30 exposure str30 outcome /// 
	n coef_uni lci_uni uci_uni double(p_uni) r2_uni ///
	coef_mult lci_mult uci_mult double(p_mult) r2_mult ///
	using ".\Results\totalActions_lm.dta", replace

foreach var of varlist extra agree consc emoStab open iq {
	
	* Store exposure variable as a macro
	local exp = "`var'"
	
	* Store outcome as macro
	local out = "total_actions"
	
	** Univariable analysis
	regress total_actions `var' if cca_confounders == 1
	
	* Store relevant summary statistics
	local n = e(N)
	local r2_uni = e(r2)
	
	* Store coefficients
	matrix res = r(table)
	
	local coef_uni = res[1,1]
	local lci_uni = res[5,1]	
	local uci_uni = res[6,1]
	local p_uni = res[4,1]
	
	
	** Multivariable analysis - Repeat with and without exposure to get change in pseudo-R2 and LR test results
	regress total_actions i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if `var' != .
	local r2_mult_base = e(r2)
	
	regress total_actions `var' i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge
	local r2_mult = e(r2) - `r2_mult_base'
	
	* Store coefficients
	matrix res = r(table)
	
	local coef_mult = res[1,1]
	local lci_mult = res[5,1]	
	local uci_mult = res[6,1]
	local p_mult = res[4,1]
	
	** Post these summary results to the file
	post totalActions_lm ("`exp'") ("`out'") (`n') /// 
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`r2_uni') (`coef_mult') ///
		(`lci_mult') (`uci_mult') (`p_mult') (`r2_mult')
				
}

postclose totalActions_lm

* Check results
use ".\Results\totalActions_lm.dta", clear

export delimited using ".\Results\totalActions_lm.csv", replace


** Read back in original dataset
use "B4293_ReligionAndClimate_Processed.dta", clear



**** Poisson models

*** Extraversion 

* Poisson model unadjusted
poisson total_actions extra if cca_confounders == 1, irr

* And for adjusted model
poisson total_actions extra i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, irr

* Predicted outcomes
sum extra if cca_confounder == 1 & total_actions != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(extra = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted outcomes
capture drop p1
predict p1 if total_actions != .
sum p1

twoway (scatter p1 extra, col(black) msym(D) msize(vsmall) ///
	jitter(2) jitterseed(1234)) ///
	(lfitci p1 extra, ciplot(rline) lwidth(medthick)), ///
	xlabel(, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted number of actions") ///
	title("Extraversion", size(medium)) ///
	legend(off) ///
	name(extra_actions_p, replace)


*** Agreeableness 

* Poisson model unadjusted
poisson total_actions agree if cca_confounders == 1, irr

* And for adjusted model
poisson total_actions agree i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, irr

* Predicted outcomes
sum agree if cca_confounder == 1 & total_actions != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(agree = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted outcomes
capture drop p1
predict p1 if total_actions != .
sum p1

twoway (scatter p1 agree, col(black) msym(D) msize(vsmall) ///
	jitter(2) jitterseed(1234)) ///
	(lfitci p1 agree, ciplot(rline) lwidth(medthick)), ///
	xlabel(, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted number of actions") ///
	title("Agreeableness", size(medium)) ///
	legend(off) ///
	name(agree_actions_p, replace)
	
	

*** Conscientiousness

* Linear model unadjusted
poisson total_actions consc if cca_confounders == 1, irr

* And for adjusted model
poisson total_actions consc i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, irr

* Predicted outcomes
sum consc if cca_confounder == 1 & total_actions != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(consc = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted outcomes
capture drop p1
predict p1 if total_actions != .
sum p1

twoway (scatter p1 consc, col(black) msym(D) msize(vsmall) ///
	jitter(2) jitterseed(1234)) ///
	(lfitci p1 consc, ciplot(rline) lwidth(medthick)), ///
	xlabel(, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted number of actions") ///
	title("Conscientiousness", size(medium)) ///
	legend(off) ///
	name(consc_actions_p, replace)

	
	
*** Emotional stability

* Linear model unadjusted
poisson total_actions emoStab if cca_confounders == 1, irr

* And for adjusted model
poisson total_actions emoStab i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, irr

* Predicted outcomes
sum emoStab if cca_confounder == 1 & total_actions != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(emoStab = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted outcomes
capture drop p1
predict p1 if total_actions != .
sum p1

twoway (scatter p1 emoStab, col(black) msym(D) msize(vsmall) ///
	jitter(2) jitterseed(1234)) ///
	(lfitci p1 emoStab, ciplot(rline) lwidth(medthick)), ///
	xlabel(, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted number of actions") ///
	title("Emotional Stability", size(medium)) ///
	legend(off) ///
	name(emoStab_actions_p, replace)
	


*** Openness to experience

* Linear model unadjusted
poisson total_actions open if cca_confounders == 1, irr

* And for adjusted model
poisson total_actions open i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, irr

* Predicted outcomes
sum open if cca_confounder == 1 & total_actions != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(open = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted outcomes
capture drop p1
predict p1 if total_actions != .
sum p1

twoway (scatter p1 open, col(black) msym(D) msize(vsmall) ///
	jitter(2) jitterseed(1234)) ///
	(lfitci p1 open, ciplot(rline) lwidth(medthick)), ///
	xlabel(, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted number of actions") ///
	title("Openness to experience", size(medium)) ///
	legend(off) ///
	name(open_actions_p, replace)
	


*** IQ

* Linear model unadjusted
poisson total_actions iq if cca_confounders == 1, irr

* And for adjusted model
poisson total_actions iq i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, irr

* Predicted outcomes
sum iq if cca_confounder == 1 & total_actions != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(iq = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted outcomes
capture drop p1
predict p1 if total_actions != .
sum p1

twoway (scatter p1 iq, col(black) msym(D) msize(vsmall) ///
	jitter(2) jitterseed(1234)) ///
	(lfitci p1 iq, ciplot(rline) lwidth(medthick)), ///
	xlabel(, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted number of actions") ///
	title("IQ scores", size(medium)) ///
	legend(off) ///
	name(iq_actions_p, replace)
	

** Combine graphs together
graph combine extra_actions_p agree_actions_p consc_actions_p emoStab_actions_p open_actions_p iq_actions_p, rows(2) imargin(tiny) ycommon

graph export ".\Results\totalActions_poisson_predOutcomes.pdf", replace

graph close _all


*** Creating post-file to store results to
capture postclose totalActions_p
postfile totalActions_p str30 exposure str30 outcome /// 
	n coef_uni lci_uni uci_uni double(p_uni) r2_uni ///
	coef_mult lci_mult uci_mult double(p_mult) r2_mult ///
	using ".\Results\totalActions_p.dta", replace

foreach var of varlist extra agree consc emoStab open iq {
	
	* Store exposure variable as a macro
	local exp = "`var'"
	
	* Store outcome as macro
	local out = "total_actions"
	
	** Univariable analysis
	poisson total_actions `var' if cca_confounders == 1, irr
	
	* Store relevant summary statistics
	local n = e(N)
	local r2_uni = e(r2_p)
	
	* Store coefficients
	matrix res = r(table)
	
	local coef_uni = res[1,1]
	local lci_uni = res[5,1]	
	local uci_uni = res[6,1]
	local p_uni = res[4,1]
	
	
	** Multivariable analysis - Repeat with and without exposure to get change in pseudo-R2 and LR test results
	poisson total_actions i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge if `var' != ., irr
	local r2_mult_base = e(r2_p)
	
	poisson total_actions `var' i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, irr
	local r2_mult = e(r2_p) - `r2_mult_base'
	
	* Store coefficients
	matrix res = r(table)
	
	local coef_mult = res[1,1]
	local lci_mult = res[5,1]	
	local uci_mult = res[6,1]
	local p_mult = res[4,1]
	
	** Post these summary results to the file
	post totalActions_p ("`exp'") ("`out'") (`n') /// 
		(`coef_uni') (`lci_uni') (`uci_uni') (`p_uni') (`r2_uni') (`coef_mult') ///
		(`lci_mult') (`uci_mult') (`p_mult') (`r2_mult')
				
}

postclose totalActions_p

* Check results
use ".\Results\totalActions_p.dta", clear

export delimited using ".\Results\totalActions_p.csv", replace


** Read back in original dataset
use "B4293_ReligionAndClimate_Processed.dta", clear



**** Zero-inflated Poisson models

*** Extraversion 

* Zero-inflated Poisson model unadjusted
zip total_actions extra if cca_confounders == 1, inflate(extra) irr

* And for adjusted model
zip total_actions extra i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, inflate(extra i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge) irr

* Predicted outcomes
sum extra if cca_confounder == 1 & total_actions != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(extra = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted outcomes
capture drop p1
predict p1 if total_actions != .
sum p1

twoway (scatter p1 extra, col(black) msym(D) msize(vsmall) ///
	jitter(2) jitterseed(1234)) ///
	(lfitci p1 extra, ciplot(rline) lwidth(medthick)), ///
	xlabel(, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted number of actions") ///
	title("Extraversion", size(medium)) ///
	legend(off) ///
	name(extra_actions_zip, replace)


*** Agreeableness 

* Zero-inflated Poisson model unadjusted
zip total_actions agree if cca_confounders == 1, inflate(agree) irr

* And for adjusted model
zip total_actions agree i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, inflate(agree i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge) irr

* Predicted outcomes
sum agree if cca_confounder == 1 & total_actions != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(agree = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted outcomes
capture drop p1
predict p1 if total_actions != .
sum p1

twoway (scatter p1 agree, col(black) msym(D) msize(vsmall) ///
	jitter(2) jitterseed(1234)) ///
	(lfitci p1 agree, ciplot(rline) lwidth(medthick)), ///
	xlabel(, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted number of actions") ///
	title("Agreeableness", size(medium)) ///
	legend(off) ///
	name(agree_actions_zip, replace)
	
	

*** Conscientiousness

* Zero-inflated Linear model unadjusted
zip total_actions consc if cca_confounders == 1, inflate(consc) irr

* And for adjusted model
zip total_actions consc i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, inflate(consc i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge) irr

* Predicted outcomes
sum consc if cca_confounder == 1 & total_actions != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(consc = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted outcomes
capture drop p1
predict p1 if total_actions != .
sum p1

twoway (scatter p1 consc, col(black) msym(D) msize(vsmall) ///
	jitter(2) jitterseed(1234)) ///
	(lfitci p1 consc, ciplot(rline) lwidth(medthick)), ///
	xlabel(, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted number of actions") ///
	title("Conscientiousness", size(medium)) ///
	legend(off) ///
	name(consc_actions_zip, replace)

	
	
*** Emotional stability

* Zero-inflated Linear model unadjusted
zip total_actions emoStab if cca_confounders == 1, inflate(emoStab) irr

* And for adjusted model
zip total_actions emoStab i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, inflate(emoStab i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge) irr

* Predicted outcomes
sum emoStab if cca_confounder == 1 & total_actions != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(emoStab = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted outcomes
capture drop p1
predict p1 if total_actions != .
sum p1

twoway (scatter p1 emoStab, col(black) msym(D) msize(vsmall) ///
	jitter(2) jitterseed(1234)) ///
	(lfitci p1 emoStab, ciplot(rline) lwidth(medthick)), ///
	xlabel(, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted number of actions") ///
	title("Emotional Stability", size(medium)) ///
	legend(off) ///
	name(emoStab_actions_zip, replace)
	


*** Openness to experience

* Zero-inflated Linear model unadjusted
zip total_actions open if cca_confounders == 1, inflate(open) irr

* And for adjusted model
zip total_actions open i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, inflate(open i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge) irr

* Predicted outcomes
sum open if cca_confounder == 1 & total_actions != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(open = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted outcomes
capture drop p1
predict p1 if total_actions != .
sum p1

twoway (scatter p1 open, col(black) msym(D) msize(vsmall) ///
	jitter(2) jitterseed(1234)) ///
	(lfitci p1 open, ciplot(rline) lwidth(medthick)), ///
	xlabel(, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted number of actions") ///
	title("Openness to experience", size(medium)) ///
	legend(off) ///
	name(open_actions_zip, replace)
	


*** IQ

* Zero-inflated Linear model unadjusted
zip total_actions iq if cca_confounders == 1, inflate(iq) irr

* And for adjusted model
zip total_actions iq i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, inflate(iq i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge) irr

* Predicted outcomes
sum iq if cca_confounder == 1 & total_actions != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(iq = (`lower_sd', `mean', `upper_sd'))

* Plot of predicted outcomes
capture drop p1
predict p1 if total_actions != .
sum p1

twoway (scatter p1 iq, col(black) msym(D) msize(vsmall) ///
	jitter(2) jitterseed(1234)) ///
	(lfitci p1 iq, ciplot(rline) lwidth(medthick)), ///
	xlabel(, labsize(small)) ylabel(, labsize(small)) ///
	xtitle("") ytitle("Predicted number of actions") ///
	title("IQ scores", size(medium)) ///
	legend(off) ///
	name(iq_actions_zip, replace)
	

** Combine graphs together
graph combine extra_actions_zip agree_actions_zip consc_actions_zip emoStab_actions_zip open_actions_zip iq_actions_zip, rows(2) imargin(tiny) ycommon

graph export ".\Results\totalActions_zip_predOutcomes.pdf", replace

graph close _all


*** Creating post-file to store results to - Note that peusdo-R2 stats not available for zero-inflated poisson models, nore can do LR test
capture postclose totalActions_zip
postfile totalActions_zip str30 exposure str30 outcome /// 
	n coef_uni_pois lci_uni_pois uci_uni_pois double(p_uni_pois) ///
	coef_uni_zi lci_uni_zi uci_uni_zi double(p_uni_zi) ///
	coef_mult_pois lci_mult_pois uci_mult_pois double(p_mult_pois) ///
	coef_mult_zi lci_mult_zi uci_mult_zi double(p_mult_zi) ///
	using ".\Results\totalActions_zip.dta", replace

foreach var of varlist extra agree consc emoStab open iq {
	
	* Store exposure variable as a macro
	local exp = "`var'"
	
	* Store outcome as macro
	local out = "total_actions"
	
	** Univariable analysis
	zip total_actions `var' if cca_confounders == 1, inflate(`var') irr
	
	* Store relevant summary statistics
	local n = e(N)
	
	* Store coefficients
	matrix res = r(table)
	
	local coef_uni_pois = res[1,1]
	local lci_uni_pois = res[5,1]	
	local uci_uni_pois = res[6,1]
	local p_uni_pois = res[4,1]
	
	local coef_uni_zi = exp(res[1,3])
	local lci_uni_zi = exp(res[5,3])	
	local uci_uni_zi = exp(res[6,3])
	local p_uni_zi = res[4,3]
	
	
	** Multivariable analysis	
	zip total_actions `var' i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge, inflate(`var' i.sex ageAtBirth i.home i.edu i.imd i.ethnic offspringAge) irr
	
	* Store coefficients
	matrix res = r(table)
	
	local coef_mult_pois = res[1,1]
	local lci_mult_pois = res[5,1]	
	local uci_mult_pois = res[6,1]
	local p_mult_pois = res[4,1]
	
	local coef_mult_zi = exp(res[1,23])
	local lci_mult_zi = exp(res[5,23])	
	local uci_mult_zi = exp(res[6,23])
	local p_mult_zi = res[4,23]
	
	** Post these summary results to the file
	post totalActions_zip ("`exp'") ("`out'") (`n') /// 
		(`coef_uni_pois') (`lci_uni_pois') (`uci_uni_pois') (`p_uni_pois') ///
		(`coef_uni_zi') (`lci_uni_zi') (`uci_uni_zi') (`p_uni_zi') ///
		(`coef_mult_pois') (`lci_mult_pois') (`uci_mult_pois') (`p_mult_pois') ///
		(`coef_mult_zi') (`lci_mult_zi') (`uci_mult_zi') (`p_mult_zi')
				
}

postclose totalActions_zip

* Check results
use ".\Results\totalActions_zip.dta", clear

export delimited using ".\Results\totalActions_zip.csv", replace


******* Clear data and close log file
clear

log close

