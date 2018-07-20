# Trend_Code_Examples.R

library( tidyverse )
library( lubridate )


source( "Trend_Code.R")

# Malawi.  Use discordance data to test-estimate decomposition  ####
malawi = readRDS( "Malawi/Malawi_dataset_details.rds") 
glimpse( malawi )

meta = readRDS( "Malawi/Malawi_metadata.rds") 
# View(meta)

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
    rename( orgUnit.name = name ) %>%
    separate( period, into = c("year", "month") , sep = 4 , remove = FALSE ) %>%
    mutate_at( c("year", "month") , as.integer ) %>%
    mutate(
        date = as_date( ymd(paste(year, month, 15, sep = "-")) )
    )

glimpse(d)
count( d, level)

# choose data element
dataElementNames = unique(d$dataElement.name)
de.name = dataElementNames[7] # "HMIS Malaria – New Case (under 5)"
de.id = meta$dataElements[ , c("id", "name")] %>%
    filter( name %in% de.name ) %>%
    .$id

# randmly select 1 lowest level orgunit with data
l = which.max( count(d, level)$n )
lowest_level_ous_with_most_data = which( ous$level %in% l )
# set.seed(89)

# random selection  ####
an_ous =  ous.random(4) 

# TIME-Series  ####

ts_ou_de = ts.ou.de( data = d , ous =  an_ous$id , de = de.id , start_year = NA )

# Decompose ####

decompose.stl( df = ts_ou_de )

decompose.seas( df = ts_ou_de, transform = "log" )

# decompose.seas( df = ts_ou_de , transform = "none")

decompose.seas( df = ts_ou_de , transform = "auto")


decompose.3 = function( data = d , .ous  , .de  ){
    
    ts_ou_de = ts.ou.de( d = ts_ou_de , ous = .ous  , de = .de , start_year = NA )
    
    tibble( 
        stl = decompose.stl( df = ts_ou_de, plot = FALSE ) ,
        seas_log = decompose.seas( df = ts_ou_de , transform = "log" , plot = FALSE ) ,
        seas_auto = decompose.seas( df = ts_ou_de , transform = "auto" , plot = FALSE ) 
        )
}

# get results for 3 different methods
decompose.3( data = d , .ous =  an_ous$id , .de = de.id  )

# Compare results from all level 4 org units.
ous4 = ous %>% filter( level %in% 4) 

does = map( seq_along( 1:50 ),
        ~{

            bind_cols(
                ou = ous4$id[.x] ,
                orgunit = ous4$name[.x] ,
                decompose.3( data = d , .ous =  ous4$id[.x] , .de = de.id  )
        )
            
        }
        )
# data frame of results
doe = data.table::rbindlist( does )
doe





