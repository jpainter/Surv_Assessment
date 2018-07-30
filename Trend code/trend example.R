
# libraries #### 
library(forecast)
library(ggseas) # ggplot for time series
library( timetk )
library( xts ) # xts objects transform more flexible
library(stlplus) # for handling time-series with missing data
library( seasonal )
library( seasonalview )
library(magrittr)
library(gridExtra)
library( tidyverse )
library( lubridate )


# Malawi.  Use discordance data to test-estimate decomposition  ####
malawi = readRDS( "Malawi/Malawi_dataset_details.rds") 
glimpse( malawi )

meta = readRDS( "Malawi/Malawi_metadata.rds") 
View(meta)

# create shortcut to list of orgUnits, creating variable for the level
ous = meta$organisationUnits[ , c('id', 'name', 'path')] %>%
    mutate( level = str_count( path , "/")) %>%
    select( -path) 
count( ous, level )

idname = c('id', 'name')  # shortcut for fetching these two columns

d = malawi %>%
    inner_join(meta$dataElements[ , idname ], by = c('dataElement' = 'id' ) ) %>%
    rename( dataElement.name = name ) %>%
    inner_join(meta$categoryOptionCombos[ , idname ], by = c('categoryOptionCombo' = 'id' ) ) %>%
    rename( categoryOptionCombo.name = name ) %>%
    inner_join(ous, by = c('orgUnit' = 'id' ) ) %>%
    rename( orgUnit.name = name )

glimpse(d)
count( d, level)

# randmly select 1 lowest level orgunit with data
l = which.max( count(d, level)$n )
lowest_level_ous_with_most_data = which( ous$level %in% l )
set.seed(79)
an_ous = ous$id[ sample( lowest_level_ous_with_most_data , 1) ]
d. = d %>% filter( orgUnit %in% an_ous )
 
# choose data element
unique(d.$dataElement.name)
select_de = c( "NMCP OPD Confirmed Malaria Cases Through Microscopy",
               "NMCP OPD Confirmed Malaria Cases Through RDT" ,
               "HMIS Malaria- New Cases (5 & Above)" ,
               "HMIS Malaria – New Case (under 5)"
               )
d.wide = d. %>% filter( dataElement.name %in%  select_de ) %>%
    select( dataElement.name, categoryOptionCombo.name, period, value) %>%
    mutate( categoryOptionCombo.name = ifelse(categoryOptionCombo.name %in% 'default', '', categoryOptionCombo.name) ,
            de = paste0( dataElement.name , "_", categoryOptionCombo.name ) ,
            value = as.integer( value )
            ) %>%
    select( -dataElement.name, -categoryOptionCombo.name ) %>%
    spread( de, value ) %>%
    rowwise() %>%
    mutate( confirmed_U5 = sum( `NMCP OPD Confirmed Malaria Cases Through Microscopy_<5Yrs` ,
                               `NMCP OPD Confirmed Malaria Cases Through RDT_<5Yrs` 
                               ) ,
            confirmed_O5 = sum( `NMCP OPD Confirmed Malaria Cases Through Microscopy_>5Yrs` ,
                                `NMCP OPD Confirmed Malaria Cases Through RDT_>5Yrs` 
            )
    ) %>%
    select( period, `HMIS Malaria – New Case (under 5)_`, 
            `HMIS Malaria- New Cases (5 & Above)_` ,
            confirmed_U5 , 
            confirmed_O5 )

View(d.wide)    
