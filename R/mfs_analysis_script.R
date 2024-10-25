###############################################################################
## IMPACT Market Functionality Score (MFS)
## Author: Sirimon Thomas - sirimon.thomas@impact-initiatives.org
## Date: 01/05/2023
## Adopted by: Getu GARI - getu.gari@reach-initiatives.org

rm(list=ls())
library(tidyverse)
library(here)

#import clean data
data <- read.csv('inputs/REACH_ETH2001_JMMI_OCT_cleaned_data.csv', na.strings = '')

# tool
loc_tool <- "inputs/REACH_ETH2001_JMMI_OCT_2024_tool.xlsx"
df_survey <- readxl::read_excel(loc_tool, sheet = "survey")
df_choices <- readxl::read_excel(loc_tool, sheet = "choices")

###### USER INPUT - change the lines below to match the administrative levels in the country of interest
###################################################
data <- data %>%  rename('adm1' = 'adm1_region.name',
                         'adm2' = 'adm2_zone.name',
                         'adm3' = 'adm3_woreda.name')
###################################################

get.all.items <- function(){
        return((df_choices %>% filter(list_name == "all_items", 
                                      name != "water"))$name)
}
all.items<-get.all.items()

#### ACCESSIBILITY #### -----------------------------------------

#AC.1
#calculate '% of vendors selecting an option other than "Hazardous, damaged, or unsafe buildings in the marketplace," 
#"Hazards or damage on roads leading to the marketplace," "No issues," "Don't know," or "Prefer not to answer"'

mfs_access_physical <- data %>%
    mutate(physical_access_true = if_else(rowSums(select(.,contains('physical_access.') 
                                                         & -contains('no_issues') 
                                                         & -contains('dont_know')
                                                         & -contains('hazardous_buildings')
                                                         & -contains('hazardous_roads')), na.rm = T) > 0,1,0)) %>% 
    group_by(adm1,adm2,adm3) %>% 
    summarise(physical_access = round(sum(physical_access_true)/n()*100,1)) %>% 
    mutate(physical_access_score = case_when(physical_access<5 ~ 8,
                                             physical_access>=5 & physical_access<10 ~ 6,
                                             physical_access>=10 & physical_access<25 ~ 4,
                                             physical_access>=25 & physical_access<50 ~ 2,
                                             physical_access>=50 ~ 0))

#AC.2
#calculate '% of vendors selecting "Hazards or damage on roads leading to the marketplace"'

mfs_access_roads <- data %>%  
    group_by(adm1,adm2,adm3) %>% 
    summarise(physical_roads_access = round(sum(physical_access.hazardous_roads, na.rm = T)/n()*100,1)) %>% 
    mutate(physical_roads_access_score = case_when(physical_roads_access<5 ~ 4,
                                                   physical_roads_access>=5 & physical_roads_access<10 ~ 3,
                                                   physical_roads_access>=10 & physical_roads_access<25 ~ 2,
                                                   physical_roads_access>=25 & physical_roads_access<50 ~ 1,
                                                   physical_roads_access>=50 ~ 0))

#AC.3
#if any vendor responds "Yes," the market is coded as a "Yes"

mfs_access_social <- data %>% 
    mutate(accessibility_social_access = if_else(accessibility_social_access == 'yes',1,0)) %>% 
    group_by(adm1,adm2,adm3) %>% 
    summarise(social_access_score = if_else(sum(accessibility_social_access)>0,0,2))

#AC.4
#calculate % of vendors selecting an option other than "No issues" or "Prefer not to answer"

mfs_access_safety <- data %>%  
    mutate(safety_access_true = if_else(rowSums(select(.,contains('accessibility_safety.') 
                                                       & -contains('no_issues') 
                                                       & -contains('dont_know')),na.rm = T) > 0,1,0)) %>% 
    group_by(adm1,adm2,adm3) %>% 
    summarise(safety_access = round(sum(safety_access_true)/n()*100,1)) %>% 
    mutate(safety_access_score = case_when(safety_access<5 ~ 3,
                                           safety_access>=5 & safety_access<10 ~ 2,
                                           safety_access>=10 & safety_access<20 ~ 1,
                                           safety_access>=20 ~ 0))

#### AVAILABILITY #### -------------------------------------

#AV.1
#Mode of vendor responses for each item

# function to take most common answer (statistical mode). This favours available over limited over unavailable i.e. if one trader says 'available' and one says 'limited', the adm3 will be given 'available'
mode_fun <- function(x) {
    #remove NAs and sort alphabetically - this priotises available over limited over unavailable
    x <- x[!is.na(x)] %>% sort()
    
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}

mfs_available <- data %>% 
    select(adm1, adm2, adm3, starts_with('availability_food_') & !contains(c('summary','wholesale', 'etb_available'))) %>%  #NB excludes currencies
    group_by(adm1, adm2, adm3) %>% 
    summarise(across(starts_with('availability_food_'), mode_fun)) %>%  #use mode_fun to extract mode value
    mutate(across(starts_with('availability_food_'), ~ case_when( #convert to scores
        . == 'fully_available' ~ 3,
        . == 'limited' ~ 2,
        . == 'unavailable' ~ 0))) %>% 
    ungroup() %>% 
    mutate(availability_score = rowSums(across(starts_with('availability_food_')), na.rm = T)) %>% #sum rows to give overall score - max score is 105 (35 items x 3)
    select(adm1, adm2, adm3, availability_score)

#### AFORDABILITY #### -------------------------------------

#AF.1
#Median of vendor responses for each item

# function to score prices based on national median price
price_score_fun <- function(item_median){
    item <- deparse(substitute(item_median))
    result = case_when(
        {{item_median}} > median_national[[item]] * 1.5 ~ -2,
        {{item_median}} > median_national[[item]] * 1.25 ~ -1.5,
        {{item_median}} > median_national[[item]] * 1.1 ~ -1,
        {{item_median}} > median_national[[item]] * 0.90 ~ 0,
        {{item_median}} > median_national[[item]] * 0.75 ~ 1,
        {{item_median}} > median_national[[item]] * 0.5 ~ 1.5,
        {{item_median}} <= median_national[[item]] * 0.5 ~ 2,
        TRUE ~ NA_real_
    )
    return (result)
}

#select concerned columns
selection_price <- data%>%
    select(adm1,adm2,adm3,paste0(all.items,"_price"))

#remove those having no measurements
selection_price <- selection_price[, !sapply(selection_price, function(col) all(is.na(col)))]

#create national median dataset
median_national <- selection_price %>%  # NB does not include currency prices
    lapply(median, na.rm=T) %>%  #ignore warning messages- these are from trying to calculate the median on the adm1, adm2 and adm3 columns
    as.data.frame() %>% 
    mutate(across(c(adm1, adm2, adm3), as.character))

n_items <- ncol(median_national) - 3 #also used later in final pillar score calculations

afford_scale <- 12 # set the max score for the affordability price levels - the scores will be scaled based on this value

#calculate scores
mfs_afford_price <- data %>% 
    select(adm1,adm2,adm3,paste0(all.items,"_price"),-contains('wholesale'),
                                                      -contains('bleach_price'),
                                                      -contains('camel_meat_price'),
                                                      -contains('mutton_price'),
                                                      -contains('carrots_price'),
                                                      -contains('coffee_price'),
                                                      -contains('sugar_price'),
                                                      -contains('water_price'),
                                                      -contains('water_5km_price'),
                                                     -contains('water_10km_price'))%>% 
    group_by(adm1, adm2, adm3) %>% 
    summarise(across(everything(), ~median(., na.rm = TRUE)))%>% 
    ungroup()%>% 
    mutate(across(contains('_price'), price_score_fun), #apply function to score each item
           afford_price_sum = rowSums(across(contains('_price') | contains('_price')), na.rm = T), # row sums to give total score - +/- 2x number of items)
           afford_price_score = ((afford_price_sum - (-2*n_items)) * (afford_scale-0)) / ((2*n_items) - (-2*n_items) + 0) # use scaling formula to scale values to 0-12 - optional
    )%>%  
    select(adm1, adm2, adm3, afford_price_sum, afford_price_score)

#AF.2
#% of vendors selecting an option other than "No issues," "Don't know," or "Prefer not to answer"

mfs_afford_finance <- data %>%  
    mutate(afford_finance_true = if_else(rowSums(select(.,contains('finacial_access_challenges.') # simon changed 'affordability_financial'
                                                                                                ##to 'finacial_access_challenges'
                                                        & -contains('no_issues') 
                                                        & -contains('dont_know')
                                                        & -contains('prefer_not_answer')), na.rm = T) > 0,1,0)) %>% 
    group_by(adm1,adm2,adm3) %>% 
    summarise(afford_finance = round(sum(afford_finance_true)/n()*100,1)) %>% 
    mutate(afford_finance_score = case_when(afford_finance<10 ~ 9,
                                            afford_finance>=10 & afford_finance<25 ~ 6,
                                            afford_finance>=25 & afford_finance<50 ~ 3,
                                            afford_finance>=50 ~ 0))

#AF.3
#% of vendors selecting "Yes"

mfs_afford_price_vol <- data %>% 
    mutate(affordability_price_volatility = case_when(estimate_price_food == 'no' | estimate_price_nfi == 'no' ~ 1,
                                                      is.na(estimate_price_food) & is.na(estimate_price_nfi) ~ NA_integer_,
                                                      TRUE ~ 0)) %>% #NOTE GG: This indicators was contextualized accordingly
    group_by(adm1,adm2,adm3) %>% 
    summarise(afford_price_vol = round(sum(affordability_price_volatility)/n()*100,1)) %>% 
    mutate(afford_price_vol_score = case_when(afford_price_vol<10 ~ 6,
                                              afford_price_vol>=10 & afford_price_vol<25 ~ 4,
                                              afford_price_vol>=25 & afford_price_vol<50 ~ 2,
                                              afford_price_vol>=50 ~ 0))

#### RESILIENCE #### -------------------------------------

#RE.1
# For each vendor, subtract # restocking days from # days of remaining stock for each item or category; aggregate by taking the median of these vendor-level calculations

# get names of items with stock data
stock_items <- data %>% 
    select(ends_with('_stock_days')) %>% colnames() %>% str_replace_all('_stock_days','')

#create empty dataset
mfs_resil_restock <- data %>%  select(adm1,adm2,adm3)

#for loop for calculating restock days
for (item in stock_items) {
    #select the stock ('_stock_current) and restock duration ('_duration') columns for each item
    item_stock <- data %>% select(contains(paste0(item,'_')) & ends_with('_stock_days'))
    #& -contains('wholesale')) %>% select(1) #edit these parameters to ensure only 1 item is selected for each iteration
    item_restock <- data %>% select(contains(paste0(item,'_')) & ends_with('_resupply_days'))
    #& -contains('wholesale')) %>% select(1) #edit these parameters to ensure only 1 item is selected for each iteration
    item_resilience <- item_stock - item_restock #calculate resilience days
    item_resilience <- ifelse(item_resilience > 3,3,
                              ifelse(item_resilience > 0,2,
                                     ifelse(item_resilience == 0,1,0))) #calculate scores
    
    colnames(item_resilience) <- paste0(item,'_resupply_days')
    
    mfs_resil_restock <- cbind(mfs_resil_restock, item_resilience)
}

mfs_resil_restock <- mfs_resil_restock %>% 
    group_by(adm1, adm2, adm3) %>%  
    summarise(across(everything(), ~median(., na.rm = TRUE))) %>%  #aggregate by adm3
    mutate(resil_restock_score = rowSums(across(ends_with('_resupply_days')), na.rm = T)) %>% 
    select(adm1,adm2,adm3,resil_restock_score)

#RE.2
#### see above

#RE.3
#% of vendors selecting "Yes"

mfs_resil_supply_diverse <- data %>% 
    select(adm1, adm2, adm3, ends_with('_single')) %>% 
    mutate(across(ends_with('_single'), ~if_else(. == 'yes',1,0))) %>% 
    group_by(adm1,adm2,adm3) %>% 
    summarise(across(ends_with('_single'), ~round(sum(., na.rm=T)/sum(!is.na(.))*100,1))) %>% 
    ungroup() %>% 
    mutate(food_supplier_local_single = case_when(food_supplier_local_single > 75 ~ 0,
                                                  food_supplier_local_single > 50 ~ 1,
                                                  food_supplier_local_single > 25 ~ 2,
                                                  food_supplier_local_single <= 25 ~ 3),
           #food_supplier_imported_single = case_when(food_supplier_imported_single  > 75 ~ 0,
           #food_supplier_imported_single  > 50 ~ 1,
           #food_supplier_imported_single  > 25 ~ 2,
           #food_supplier_imported_single  <= 25 ~ 3),
           nfi_supplier_single = case_when(nfi_supplier_single > 75 ~ 0,
                                           nfi_supplier_single > 50 ~ 1,
                                           nfi_supplier_single > 25 ~ 2,
                                           nfi_supplier_single <= 25 ~ 3),
           
           resil_supply_diverse_score = rowSums(across(ends_with('_single')), na.rm = T)) #sum rows to give final score - in ETB max score is 9 (3 x 3 categories)

#RE.4
#% of vendors selecting an option other than "No difficulties" or "Prefer not to answer"

mfs_resil_supply <- data %>%  
    mutate(resilience_supply_true = if_else(rowSums(select(.,contains('resilience_supply_chain.') 
                                                           & -contains('no_difficulties') 
                                                           & -contains('dont_know')
                                                           & -contains('prefer_not_answer')), na.rm = T) > 0,1,0)) %>% 
    group_by(adm1,adm2,adm3) %>% 
    summarise(resil_supply = round(sum(resilience_supply_true)/n()*100,1)) %>% 
    mutate(resil_supply_score = case_when(resil_supply<5 ~ 12,
                                          resil_supply>=5 & resil_supply<10 ~ 9,
                                          resil_supply>=10 & resil_supply<25 ~ 6,
                                          resil_supply>=25 & resil_supply<50 ~ 3,
                                          resil_supply>=50 ~ 0))

#### INFRASTRUCTURE #### -------------------------------------

#IN.1
#% of vendors selecting "Hazardous, damaged, or unsafe buildings in the marketplace"

mfs_infra_facilities <- data %>%  
    group_by(adm1, adm2, adm3) %>% 
    summarise(infra_facilities = round(sum(physical_access.hazardous_buildings, na.rm = T)/n()*100,1)) %>% 
    mutate(infra_facilities_score = case_when(infra_facilities<5 ~ 4,
                                              infra_facilities>=5 & infra_facilities<10 ~ 3,
                                              infra_facilities>=10 & infra_facilities<25 ~ 2,
                                              infra_facilities>=25 & infra_facilities<50 ~ 1,
                                              infra_facilities>=50 ~ 0))

#IN.2
#% of vendors selecting an option other than "Yes, within my own business facilities" or "Yes, elsewhere within the marketplace"

mfs_infra_storage <- data %>%  
    mutate(infrastructure_storage = if_else(infrastructure_storage == 'no_store_facility_outside' |
                                                infrastructure_storage == 'no_store_at_home' |
                                                infrastructure_storage == 'other', 1,0)) %>%
    group_by(adm1, adm2, adm3) %>% 
    summarise(infra_storage = round(sum(infrastructure_storage)/n()*100,1)) %>% 
    mutate(infra_storage_score = case_when(infra_storage<10 ~ 3,
                                           infra_storage>=10 & infra_storage<25 ~ 2,
                                           infra_storage>=25 & infra_storage<50 ~ 1,
                                           infra_storage>=50 ~ 0))

#IN.3
#% of vendors selecting an option other than "Cash (local currency)", "Cash (foreign currencies)", or "Prefer not to answer"

mfs_infra_payment <- data %>%  
    mutate(infra_payment_true = if_else(rowSums(select(.,contains('payment_modalities.') 
                                                       & -contains('cash') 
                                                       & -contains('dont_know')
                                                       & -contains('prefer_not_answer')),na.rm = T) > 0,1,0)) %>% 
    group_by(adm1, adm2, adm3) %>% 
    summarise(infra_payment = round(sum(infra_payment_true, na.rm = T)/n()*100,1)) %>% 
    mutate(infra_payment_score = case_when(infra_payment>75 ~ 3,
                                           infra_payment>50 ~ 2,
                                           infra_payment>25 ~ 1,
                                           infra_payment<=25 ~ 0))

#======================================== CALCULATE MFS =============================================

mfs_data_list <- list(mfs_access_physical,mfs_access_roads,mfs_access_social,mfs_access_safety, #accessibility pillar - max 17
                      mfs_available,                                                            #availability pillar - max depends on number of items or categories
                      mfs_afford_price,mfs_afford_finance,mfs_afford_price_vol,                 #affordability pillar - max depends on number of items
                      mfs_resil_restock,mfs_resil_supply_diverse,mfs_resil_supply,              #resilience pillar - max depends on number of items or categories
                      mfs_infra_facilities,mfs_infra_storage,mfs_infra_payment)                 #infrastructure pillar - max 10

#create full dataset
mfs <- mfs_data_list %>% reduce(full_join, by=c('adm1','adm2','adm3'))

#calculate final mfs
mfs <- mfs %>% 
    select(adm1, adm2, adm3, contains('_score'))%>% 
    #calculate pillar scores, scale to 0-1 by dividing by the max score for that pillar, then apply weights
    mutate(mfs_accessibility_pillar_score = (rowSums(across(contains('access_score')),na.rm = T) /17) *25,
           mfs_availability_pillar_score = (availability_score /(n_items*3)) *30,
           mfs_affordability_pillar_score = (rowSums(across(contains('afford')),na.rm = T) /(afford_scale+15)) *15,
           mfs_resilience_pillar_score = (rowSums(across(contains('resil')),na.rm = T) /(length(stock_items)*3 + 6 + 12)) *20,
           mfs_infrastructure_pillar_score = (rowSums(across(contains('infra')),na.rm = T) /10) *10,
           
           #calculate final score
           mfs_score = rowSums(across(contains('_pillar_score')), na.rm = T)) %>% 
    select(adm1, adm2, adm3, contains(c('_pillar_score', 'mfs_score')))

#export

write.csv(mfs, file = 'outputs/REACH_ETH2001_JMMI_OCT_MFS_Analysis.csv', row.names = F)

###############################################################################

