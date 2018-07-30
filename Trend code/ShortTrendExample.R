# another trend example
# based on https://robjhyndman.com/hyndsight/tslm-decomposition/


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
    rename( orgUnit.name = name ) %>%
    separate( period, into = c("year", "month") , sep = 4 , remove = FALSE ) %>%
    mutate_at( c("year", "month") , as.integer ) %>%
    mutate(
        date = as_date( ymd(paste(year, month, 15, sep = "-")) )
    )

glimpse(d)
count( d, level)

# randmly select 1 lowest level orgunit with data
l = which.max( count(d, level)$n )
lowest_level_ous_with_most_data = which( ous$level %in% l )
# set.seed(89)

## random selection  ####
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
    select( dataElement.name, categoryOptionCombo.name, period, year, month, value) %>%
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
    select( period, year, month, `HMIS Malaria – New Case (under 5)_`, 
            `HMIS Malaria- New Cases (5 & Above)_` ,
            confirmed_U5 , 
            confirmed_O5 ) %>%
    arrange( period )

# View(d.wide)
# glimpse(d.wide)

# TIME-Series  ####

# get first year and month, last year and month, and be sure all periods are available
first_period = d.wide[1,] 
first_year = first_period$year
first_month = first_period$month

last_period = d.wide[ nrow(d.wide), ]  
last_year = last_period$year
last_month = last_period$month

periods = tibble(
    year = d.wide$year , 
    month = d.wide$month ,
    date = ymd(paste(year, month, 15, sep = "-")) 
)
nrow( periods )

expand.periods = periods %>% expand( year, month )
nrow( expand.periods )

d.wide = d.wide %>% right_join( expand.periods)

df = ts( d.wide$`HMIS Malaria – New Case (under 5)_` , start = c( first_year, first_month ), frequency = 12 )

# decompose ####

# linear deompostion--probably not a good idea!
# decompose_df <- tslm(df ~ trend + fourier(df, 2))
# 
# trend <- coef(decompose_df)[1] + coef(decompose_df)['trend']*seq_along(df)
# components <- cbind(
#     data = df,
#     trend = trend,
#     season = df - trend - residuals(decompose_df),
#     remainder = residuals(decompose_df)
# )
# autoplot(components, facet=TRUE)

# alternative decompose and coefficient of variation
stl_df = stlplus( df, s.window = 'periodic', n.p = 12  ) 
plot( stl_df)
# glimpse( stl_df$data )

stl_trend = trend( stl_df ) 
stl_seasonal = seasonal( stl_df )
rem = remainder( stl_df )
rem_mean = mean( rem, na.rm = TRUE )
rem_sd = sd( rem , na.rm = TRUE ) 
cv = rem_sd / rem_mean; cv
# coeficient of error?  ration of remainder to projected (seasonal+trend)
rem_sd / mean(seasonal+trend)

# X-13ARIMA-SEATS decompose
## need to replace missing values with zero (but not missing dates; there shoulb be no missing dates at this point)
df.1 = df
# df.1[ is.na( df )] = 1L
df.1[ df == 0 ] = 1L
seas_df <- seasonal::seas(df.1
                          , na.action = na.x13
                          # , x11 = "" 
                          # , x11 = list()
                          , regression.aictest = NULL
                          # , outlier = NULL,
                          # , transform.function = "auto"
                          , transform.function = "log"
                          # , transform.function = "none"
                     ) 

# plot(seas_df)
# summary(seas_df)
# final(seas_df)
# View( seas_df$data )

rem = seas_df$data %>% as_data_frame() %>% .$irregular %>% as.double()
seasonal = seas_df$data %>% as_data_frame() %>% .$seasonal %>% as.double()
trend = seas_df$data %>% as_data_frame() %>% .$trend %>% as.double()
raw = seasonal * trend * rem
date = as.Date( as.yearmon(time( seas_df$x ) ) ) 
                
seas_model = tibble(
    date = date ,
    raw = raw , seasonal = seasonal, remainder = rem , trend = trend
) %>%
    gather( var, value, -date) %>%
    mutate( 
        var = factor( var, levels = c('raw', 'seasonal', 'trend', 'remainder') ) 
        )

ggplot( seas_model, aes( x =  date , y = value )) +
    geom_line() +
    facet_grid( var ~ . , scales = 'free' ) +
    theme_bw()
    
rem_mean = mean( rem-1, na.rm = TRUE ) # if centered, should be ~ zero
rem_sd = sd( (rem-1) , na.rm = TRUE ) 
rem_sd



