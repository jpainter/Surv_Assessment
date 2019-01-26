# Load relevant data  ####


library(tidyverse)
library(RColorBrewer)
library(cowplot)

source('dhis2_functions.R')
source("Trend code/Trend_Code.R")

dhis_instance = "Kenya"
data_directory = NA

origin.folder = paste0(ifelse(is.na(data_directory), "" ,
                              paste0(dhis_instance, "/")) ,
                       dhis_instance , "/")

dataset.directory = paste0(dhis_instance , "/datasets/")

meta_data_file = paste0(origin.folder , dhis_instance ,  "_metadata.rds")

md = read_rds(meta_data_file)

ous = ous.translated(md)

nest.file = paste0(dataset.directory ,
                   dhis_instance,
                   "_data_nest.rds")

d.nest = readRDS(nest.file)


# data frame of datasets and data elements
dsde = map_df( 1:length(md$dataSets$dataSetElements), 
               ~map_df( md$dataSets$dataSetElements[[.x]], 
                        ~as.matrix(.x) )) %>%
    
    rename( dataElement.id = dataElement , 
            dataSet.id = dataSet ,
            categoryCombo.id = categoryCombo
            ) %>%
    
    left_join( md$dataElements %>% select( id, name, zeroIsSignificant ) ,
               by = c('dataElement.id' = 'id' )) %>%
    
    rename( dataElement = name ) %>%
    
    left_join( md$dataSets %>% select( id, name ) ,
               by = c('dataSet.id' = 'id' )) %>%
    
    rename( dataSet = name ) %>%
    
    left_join( md$categoryCombos %>% select( id, name ) ,
               by = c('categoryCombo.id' = 'id' )) %>%
    
    rename( categoryCombo = name ) 
    


# unique datasets ####

de.all = unique( d.nest$dataElement.name )

datasets = dsde %>%  filter( dataElement %in% de.all )  %>% pull( dataSet ) %>% unique

# report of which data elements can recorded as zero for each dataset (split) ####
element.form = dsde %>% 
    filter( dataElement %in% de.all ) %>%
    filter( dataSet %in% datasets[1:6]) %>%
    select( dataElement.id, dataElement, dataSet, zeroIsSignificant ) %>%
    arrange( dataElement ) %>%
    spread( dataSet , zeroIsSignificant  ) 

element.form %>% View

element.form = dsde %>% 
    filter( dataElement %in% de.all ) %>%
    filter( dataSet %in% datasets[7:length(datasets)] ) %>%
    select( dataElement.id, dataElement, dataSet, zeroIsSignificant ) %>%
    arrange( dataElement ) %>%
    spread( dataSet , zeroIsSignificant  ) 

element.form %>% View


# data.frame of ous assigned to datasets ####

ou.dataset = # data.frame with each orgunit - dataset combination (long form)
    
    md$dataSets %>% 
    
    select( id, name, organisationUnits ) %>%
    
    rename( dataSet.id = id , dataset.name = name ) %>%
    
    unnest %>% as.tibble() %>%
    
    # add in date OU 'opened'
    left_join( md$organisationUnits %>% select( id , openingDate, closedDate ) , by = "id" ) %>%
    
    rename( organistionUnit = id ) 
    
glimpse( ou.dataset ) 

element.form = # matrix of the number of OU at intersection  dataset (col heading) and data elements (rows )
    
    dsde %>% 
    
    filter( dataElement %in% de.all ) %>%
    
    inner_join( count( ou.dataset , dataSet.id ) %>% rename( n_orgUnits = n ) , by = "dataSet.id" ) %>%
    
    select( dataElement.id, dataElement, dataSet, n_orgUnits ) %>%
    
    arrange( dataElement ) %>%
    
    spread( dataSet , n_orgUnits  ) 




# choose data element ####

de.selected = de.all[3] # Confirmed Malaria (Lab confirm Only) [zero may be NA]

# de.selected = de.all[17] # IDSR Malaria [zero is recorded]


#  Data from 1 data element ( may contain >1 combos )

x = d.nest %>% filter( dataElement.name %in% de.selected )

glimpse( x )

count(x, dataElement , dataElement.name )

count(x, categoryOptionCombo.name )


categoryOptionCombo.for.dataElement = x %>% 
    
    filter( dataElement.name %in% de.selected ) %>%
    
    select( categoryOptionCombo , categoryOptionCombo.name )     %>% 
    
    unique 


categoryOptionCombo.selected = categoryOptionCombo.for.dataElement %>% 
    # filter( row_number() == 2 ) %>%
    .$categoryOptionCombo.name

dataset.for.data.element = dsde %>% 
    
    filter( dataElement %in% de.selected )



# Sum values for data.element assigned to corresponding dataset, by openingDate  ####


glimpse( dataset.for.data.element )

n.ou.year = md$organisationUnits %>%
    select( id, name, openingDate, level ) %>%
    mutate( year = year( openingDate ) ) %>%
    mutate( year = ifelse( year<= 2013, 2013, year )  ) %>%
    count( year )

n.ou.year.level = md$organisationUnits %>%
    select( id, name, openingDate, level ) %>%
    mutate( year = year( openingDate ) ) %>%
    mutate( year = ifelse( year<= 2013, 2013, year )  ) %>%
    count( year , level ) %>%
    spread( level , n )

n.ou.dataset.year =  md$organisationUnits %>%
    select( id, name, level ) %>%
    inner_join( ou.dataset , by = c( 'id' = 'organistionUnit') ) %>%
    filter( dataSet.id %in% dataset.for.data.element[2, 2] ) %>% 
    mutate( year = year( openingDate ) ) %>%
    mutate( year = ifelse( year<= 2012, 2012, year )  ) %>%
    count( year , level ) %>%
    spread( level , n )

# Numbers of facilities opened by year, by level
## limitation:  facilities may not be assigned to dataset, or may not yet submitted data
n.ou.year.level

n.ou.year 

##  numbers of opened facilities assigned to report this dataset
n.ou.dataset.year 


# Dataset of ou with reported opening and calculated first and last reporting period ####

## NB: may include multiple category options 

x.ou.submit = x %>%
    
    # filter( categoryOptionCombo.name %in% categoryOptionCombo.selected  ) %>%
    
    inner_join( ou.dataset %>% 
                    
                    filter( dataSet.id %in% dataset.for.data.element[1, 2] ) ,
                
                by = c( "orgUnit" = "organistionUnit"  ) ) %>%
    
    mutate(  open.year = year( openingDate  ) ) %>% 
    
    mutate( open.year = ifelse( open.year<= 2013, 2013, open.year ) %>% as.factor() 
             
             ) %>% # all years before 2014 are all 2014

    mutate( 
        year.first = map_chr( data %>% map('period') , ~min( .x, na.rm = TRUE ) %>% substr(. , 1, 4 )    ) ,
        
        year.last = map_chr( data %>% map('period') , ~max( .x, na.rm = TRUE ) %>% substr(. , 1, 4 )    ) ,
        
        year.list = map(  data , 
                       ~ .x %>% mutate( year = str_sub( period , 1, 4 ) %>% as.numeric() ) %>%
                           group_by( year ) %>% 
                           summarise( 
                               n = unique( period ) %>% length ,
                               s = sum( value , na.rm = TRUE  )
                               )
                       )  ,
        
        year.month.list = map(  data , 
                       ~ .x %>% mutate( year = str_sub( period , 1, 4 ) %>% as.numeric() ,
                                        month = str_sub( period , 5, 6 ) %>% as.numeric() 
                                        ) %>%
                           group_by( year , month ) %>% 
                           summarise( 
                               n = unique( period ) %>% length ,
                               s = sum( value , na.rm = TRUE  )
                           )
        ) 
        
    ) %>%
    
    rowwise() %>% mutate( 
        
            cohort = paste( year.first , year.last , sep = "-") ,
        )

glimpse(x.ou.submit)

count( x.ou.submit, year.first ) 
count( x.ou.submit, cohort, year.first ,  year.last ) 
count( x.ou.submit %>% unnest( year.list ), year  ) 
count( x.ou.submit %>% unnest( year.list ), year.first ,  year.last , year  ) 
count( x.ou.submit, open.year ) 

# Plot Total-Assigned-Reported ou ####

d = bind_rows( n.ou.year.level %>% 
                   gather( level , value , -year ) %>% 
                   mutate( value = ifelse( is.na(value), 0 , value ) ) %>%
                   mutate( ou = 'Total' , level = as.integer( level ) ) %>%
                   group_by( level) %>%
                   mutate( value = cumsum( value  ) , change = value / lag(value)) 
               
               ,
               n.ou.dataset.year %>% 
                   gather( level , value , -year ) %>% 
                   mutate( value = ifelse( is.na(value), 0 , value ) ) %>%
                   mutate( ou = 'Assigned' , level = as.integer( level )  ) %>%
                   group_by( level) %>%
                   mutate( value = cumsum( value  ), change = value / lag(value) )  
               
               ,
               count( x.ou.submit %>% unnest( year.list ), level , year  )  %>% 
                   rename( value = nn ) %>% 
                   mutate( ou = 'Reported' , change = value / lag(value) ) 
               
               ) %>% 
    inner_join( md$organisationUnitLevels %>% select( name , level ) , by = 'level'  )

levels.of.interest = d %>% filter( value > 1 & ( ou %in% 'Assigned' & change > 1.00 ) ) %>% 
                                       pull( level ) %>% unique

plot.Total.Assigned.Reported.ou = 
    
    ggplot( d %>% filter( level %in% levels.of.interest ) , 
        aes( year , value , group = ou, color = ou ) ) + 
    geom_line( size = 1 ) + 
    scale_x_continuous( limits = c(2014, 2019) ) + 
    scale_y_continuous( label = scales::comma ) +
    geom_text( x = 2018 , aes( label = change ) , 
               data = d %>% 
                   filter( level %in% levels.of.interest ) %>%
                   group_by( ou, name , level ) %>% 
                   summarise(
                              change = ( (max(value) - min(value)) / min(value) ) %>% 
                                  scales::percent() ,
                              value = max(value) 
                   ) ,
               hjust = -.1 , size = 5
                   ) +
    facet_wrap( ~ paste( 'Level' , level, "\n" ,  name ) , scale = 'free') +
    labs( title = de.selected  )

plot.Total.Assigned.Reported.ou

#  Numbers of facilities submitting reports each year, by cohort ####

d = x.ou.submit %>% 
    unnest( year.list ) %>% 
    group_by( cohort, year.first, year.last , year ) %>%
    summarise( 
        facilities = n() ,
        avg.months =  mean(n) ,
        avg.value = mean(s) ,
        annual.value = sum(s) ) %>%
    ungroup 

d.total = d %>% group_by( year ) %>% 
    summarise(
        avg.months =  sum( facilities * avg.months ) / sum( facilities ) ,
        avg.value = sum( facilities * avg.value ) / sum( facilities ) ,
        annual.value = sum( annual.value ) ,
        facilities = sum( facilities ) ) %>%
    ungroup %>%
    mutate( year.first = 'All' , cohort = 'All')


plot.n = ggplot(d , 
        aes( year , facilities , group = cohort , color = year.first , 
             size = avg.months , label = round(avg.months)  ) ) + 
    geom_line( size = 1 ) +
    geom_point() +
    geom_text( color = 'white', aes( size = avg.months * .7 ) ) +
    geom_line( data = d.total , size = 1) +
    geom_point( data = d.total ) +
    geom_text( data = d.total , color = 'white' ) +
    scale_color_manual( values = c( brewer.pal( 5 , "Accent") , "black") , 
                        labels = c(  unique(as.character(d$year.first)), 'total') ) +
    theme_bw() +
    theme( legend.position = "" ) +
    labs( title = 'Numbers of facilities reporting, by year first reported' ,
          subtitle = de.selected )

plot.n

plotly::ggplotly( plot.n )

## Sum of reported values by year , by cohort ####

plot.sum = ggplot( d , aes( year ,  annual.value  ,  group = cohort , 
                            color = year.first  , 
                            size = round(avg.value) , label = round(avg.months)  ,
                            fill = year.first ) ) + 
    geom_line( size = 1 ) +
    geom_point(  ) +
    geom_text( color = 'white', aes( size = avg.months * .7 ) ) +
    
    geom_line( data = d.total ,  color = 'black' , size = 1 ) +
    geom_point( data = d.total , color = 'black' ) +
    geom_text( data = d.total, color = 'white', aes( size = avg.months * .7 ) ) +
    
    scale_color_manual( values = c( brewer.pal( 5 , "Accent") , "black") , 
                        labels = c(  unique(as.character(d$year.first)), 'total') ) +
    # geom_col() +
    theme_bw() +
    theme( legend.position = "" ) +
    labs( title = 'Sum of values reported, by year first reported' ,
          subtitle = de.selected )


plot_grid( plot.n, plot.sum , labels = "AUTO", scale = c(1,  1) )


## Number and sum of values by month, by cohort and year ####

d = x.ou.submit %>% 
    unnest( year.month.list ) %>% 
    group_by( year.first, year, month ) %>%
    summarise( 
        facilities = n() ,
        avg.months =  mean(n) ,
        avg.value = mean(s) ,
        total.value = sum(s) 
        ) %>% group_by( year.first , year ) %>% 
    mutate( 
        facilities.pct = facilities / max( facilities ) ,
        facilities.center = scale( facilities , scale = FALSE )
    ) %>% ungroup

d.total = d %>% group_by( year , month ) %>% 
    summarise(
        avg.months =  sum( facilities * avg.months ) / sum( facilities ) ,
        avg.value = sum( facilities * avg.value ) / sum( facilities ) ,
        total.value = sum( total.value ) ,
        facilities = sum( facilities ) ) %>%
    mutate( year.first = 'All' , cohort = 'All') %>% 
    group_by( cohort , year ) %>% 
    mutate( facilities.pct = facilities / max( facilities )  ,
            facilities.center = scale( facilities , scale = FALSE )
    ) %>% ungroup


plot.avg.month = ggplot( d.total %>% 
                           select( year, month, year.first, facilities.pct , avg.value )  , 
                       aes( month , 
                            # facilities , 
                            y = avg.value  , 
                            group = year.first , color = year.first 
                            # , 
                            # size = facilities.center , 
                            # label = facilities.center  
                       ) ) + 
    facet_grid( year ~ . , scales = 'free' ) +
    geom_line( size = 1 ) +
    geom_point() +
    
    # geom_line( data = d.total , size = 1) +
    # geom_point( data = d.total ) +
    # geom_text( data = d.total, color = 'white', aes( size = facilities.center * .1 ) ) +
    scale_color_manual( values = c( brewer.pal( 5 , "Accent") , "black") , 
                        labels = c(  unique(as.character(d$year.first)), 'total') ) +
    theme_bw() +
    theme( legend.position = "" ) +
    labs( title = 'Avg Value by Month')

plot.avg.month

plot.total.versus.facilities.month = ggplot( d.total %>% 
                             select( year, month, year.first, facilities , total.value ) %>%
                                 gather( var , val, -year, -month, -year.first) , 
                         aes( month , 
                              # facilities , 
                              y = val  , 
                              group = year.first , color = year.first 
                              # , 
                              # size = facilities.center , 
                              # label = facilities.center  
                         ) ) + 
    facet_grid( var ~ year  , scales = 'free' ) +
    geom_line( size = 1 ) +
    geom_point() +
    
    # geom_line( data = d.total , size = 1) +
    # geom_point( data = d.total ) +
    # geom_text( data = d.total, color = 'white', aes( size = facilities.center * .1 ) ) +
    scale_color_manual( values = c( brewer.pal( 5 , "Accent") , "black") , 
                        labels = c(  unique(as.character(d$year.first)), 'total') ) +
    theme_bw() +
    theme( legend.position = "" ) +
    labs( title = 'Number of Facilties and Value Reported by Month')

plot.total.versus.facilities.month

# Relative change in value and # facilities reporting ####

d.total.relative.change = d.total %>% 
    select( year, month, year.first, facilities , total.value ) %>%
    gather( var, val , facilities, total.value ) %>%
    group_by( var , month ) %>%
    mutate( val = val / mean( val )  ) 

plot.month.n = ggplot( d.total.relative.change , 
                aes( month , 
                     # facilities , 
                     y = val , 
                     group = year.first , 
                     color = var 
                     # , 
                     # size = facilities.center , 
                     # label = facilities.center  
                     ) ) + 
    facet_grid( var ~ year , scales = 'free' ) +
    geom_line( size = 1 ) +
    geom_point() +
    theme_bw() +
    theme( legend.position = "" ) +
    labs( title = 'Relative change in facilities reporting and number of cases')

plot.month.n

# Relative change in value and # facilities reporting : correlation

library(ggpmisc)

ggplot( d.total.relative.change %>% spread( var, val ) %>%
            mutate( season =  cut( month %>% as.numeric(), breaks = c(0,3,6,9,12) , 
                                   labels = c('winter', 'spring', 'summer', 'fall' ) ) 
                    ) ,
        aes( x = facilities , y = total.value 
             # , color = season  
             )) +
    geom_point() +
    coord_equal() +
    geom_smooth( method  =  'gam' , se = FALSE , formula=y~x) +
    stat_poly_eq(parse=T, aes(label = ..eq.label.. ), formula=y~x) +
    # scale_x_continuous( limits = c(0.5,1.5)) +
    # scale_y_continuous( limits = c(0.5,1.5)) +
    labs( x = "Relative change in facilities reporting" ,
          y = "Relative change in cases" ,
          title = "Strong correlation of facilities reporting with cases reported" 
          )


#  Submission Dynamics and Effect on Trends ####

## 1. Crude trend by year ####

categoryOptionCombo.selected = categoryOptionCombo.for.dataElement %>% 
    filter( row_number() %in% 1 ) %>%
    .$categoryOptionCombo.name


d1 = x.ou.submit %>% 
    filter( categoryOptionCombo.name %in% categoryOptionCombo.selected  ) %>%
    
    unnest( data ) %>% 
    
    mutate( year  = zoo::as.yearmon( period, "%Y") %>% year 
            ) %>%
    
    group_by( year , categoryOptionCombo.name ) %>%
    
    summarise( 
        Facilities = n_distinct( orgUnit ) ,
        Months =  n_distinct(  orgUnit , period ) / Facilities , 
        Cases = sum(value) ,
        Reports = n() , 
        avgCases = Cases / Reports 
            
    ) %>% 
    
    mutate( reporting = "Annual"  ) %>% ungroup %>%
    
    gather( var , val, Facilities, Cases  , Months , Reports , avgCases ) %>%
    
    mutate( var = fct_relevel( var , c("Cases" , "Facilities" , "Months" , "Reports") ) ) %>%
    
    group_by( reporting, categoryOptionCombo.name , var  ) %>%
    
    # arrange( year ) + 
    mutate( change =  ( val / lag(val) ) - 1    ,
            change = ifelse( is.na( change ) ,  "" , scales::percent( change , accuracy = 2 )  ) ,
            change.sign = ifelse( change>=0 , "up" , "down")
    )

g1.plot = function( vars ){
    ggplot( data = d1 %>% filter( var %in%  levels(d1$var)[ vars ] ) ,
            aes( year, val, group = categoryOptionCombo.name , shape = categoryOptionCombo.name ) ) + 
        geom_point() +
        geom_line() + 
        geom_text(  aes( label = change , color = change.sign ) , vjust = 1.8 ) +
        # geom_text(  aes( label =  scales::comma( val ) ) , vjust = -1 , size = 2 , color = 'black') +
        expand_limits( y = c(0 )) +
        scale_shape_discrete( ) +
        scale_color_manual( values = c('up' = 'dark green', 'down'='red') , guide = FALSE) +
        scale_y_continuous( label = scales::comma ) +
        scale_x_yearmon( format = "%Y" , n = 4 ) +
        labs( title = de.selected  ,  
              # subtitle =  paste( categoryOptionCombo.selected , collapse = ", " ) ,
              y = "Count", x = 'Year', shape = "" ) +
        facet_grid( var ~ . , scales = 'free')  +
        theme_bw() +
        theme(strip.text = element_text(size = 15, face = "bold")) +
        theme(axis.ticks=element_blank() ,
              panel.background = element_blank() ,
              panel.grid.major = element_blank() ,
              panel.grid.minor = element_blank() ,
              axis.title = element_text(size = 16, face = "bold") ,
              axis.text = element_text(size = 14, face = "bold") ,
              legend.position = 'top' )
}    

base_title =  paste0( dhis_instance , "/" , dhis_instance , "_" ,
                      paste( de.selected , paste( categoryOptionCombo.selected , collapse = "-" ) )
)

base_file = gsub( '>', 'gtr' , base_title ) %>% gsub( '<', 'ltn' , . ) 

g1.plot(1) 
ggsave( paste0( base_file,   "_annual_case_count.png") , width = 8 , height = 5) 

g1.plot( 1:2 )
ggsave( paste0( base_file,    "_annual_case_count_facilities.png") , width = 8 , height = 5)

g1.plot( 1:3 )
ggsave( paste0( base_file,    "_annual_case_count_facilities_months.png") , width = 8 , height = 5)

g1.plot( 1:4 )
ggsave( paste0( base_file,    "_annual_case_count_facilities_months_submissions.png") , width = 8 , height = 5)

g1.plot( c(1,4 ) )
ggsave( paste0( base_file,    "_annual_case_count_submissions.png") , width = 8 , height = 5)


g1.plot( c(1,4, 5 ) )
ggsave( paste0( base_file,    "_annual_case_count_submissions_avgCases.png") , width = 8 , height = 5)


## 2. Crude trend by year, month ####

d2 = x.ou.submit %>% 
    filter( categoryOptionCombo.name %in% categoryOptionCombo.selected  ) %>%
            unnest( data ) %>%  
            mutate(
                year  = zoo::as.yearmon( period, "%Y") , 
                period  = zoo::as.yearmon( period, "%Y%m") 
                ) %>%
            group_by( year, period ) %>%
    summarise( 
        Facilities = n_distinct( orgUnit ) ,
        Cases = sum(value) 
        # , avg.value = Total / Facilities
    ) %>% 
    mutate( reporting = "Monthly"  )%>% ungroup %>%
    gather( var , val, Facilities, Cases  )

Junes = zoo::as.yearmon( c("2014-06", "2015-06", "2016-06", "2017-06" , "2018-06")  , "%Y-%m" )

g2 = ggplot( data = d2 ,
             aes( period , val ) ) + 
    geom_point() +
    geom_line() + 
    expand_limits( y = 0 ) +
    scale_y_continuous( label = scales::comma ) +
    scale_x_yearmon( format = "%Y" , n = 4 ) +
    labs( title = de.selected , y = "Count", x = 'Year') +
    facet_grid( var ~ . , scales = 'free') +
    geom_vline( xintercept  = Junes , color = 'blue') 

g2

ggsave( paste0( base_file,    "_monthly_case_count_facilities.png") , width = 8 , height = 5)


# Correlation of change in value and # facilities reporting 

library( ggpmisc )

ggplot( d.total.relative.change %>% spread( var, val ) %>%
            mutate( season =  cut( month %>% as.numeric(), breaks = c(0,3,6,9,12) , 
                                   labels = c('winter', 'spring', 'summer', 'fall' ) ) 
            ) ,
        aes( x = facilities , y = total.value 
             # , color = season
        )) +
    geom_point() +
    coord_equal() +
    geom_smooth( method  =  'gam' , se = FALSE , formula = y ~ x) +
    stat_poly_eq(parse=T, aes(label = ..eq.label.. ), formula = y ~ x) +
    # scale_x_continuous( limits = c(0.5,1.5)) +
    # scale_y_continuous( limits = c(0.5,1.5)) +
    labs( x = "Relative change in facilities reporting" ,
          y = "Relative change in cases" ,
          title = "Correlation of facilities reporting with cases reported" 
    )

ggsave( paste0( base_file,    "_correlation.png") , width = 8 , height = 5)




## 3. Crude trend by year, month ####

#  Numbers of facilities submitting reports each year, by cohort ####


d3 = x.ou.submit %>% 
    filter( categoryOptionCombo.name %in% categoryOptionCombo.selected  ) %>%
    unnest( data ) %>% 
    mutate(
        year  = zoo::as.yearmon( period, "%Y") %>% year , 
        month = zoo::as.yearmon( period, "%Y%m") %>% month,
        period  = zoo::as.yearmon( period, "%Y%m") 
    ) %>%
    group_by( year , month ) %>%
    summarise( 
        Facilities = n() ,
        Total = sum( value )  ) %>%
    ungroup %>%
    gather( var , val, Facilities, Total )


g3b = ggplot( data = d3 %>% filter( var %in% "Total" ) ,
             aes( factor(month) , val  ) ) + 
    geom_col() +
    facet_grid( .~year ) +
    scale_y_continuous( label = scales::comma ) +
    scale_x_discrete(  ) +
    labs( title = paste( "Reported cases of " , de.selected ) , y = "Cases", x = 'Month')

g3b

ggsave( paste0( base_file,    "_case_counts.png") , width = 9 , height = 2)


g3c = ggplot( data = d3 %>% filter( var %in% "Facilities" ) ,
              aes( factor(month) , val  ) ) + 
    geom_col() +
    facet_grid( .~year ) +
    scale_y_continuous( label = scales::comma ) +
    scale_x_discrete(  ) +
    labs( title = paste( "Number of monthly reports of" , de.selected ) , y = "Reports", x = 'Month')

g3c

ggsave( paste0( base_file,    "_monthly_reports.png") , width = 9 , height = 2)


cowplot::plot_grid( g3b , g3c , rel_heights=c(0.55, 0.55), ncol = 1, align = "v")

ggsave( paste0( base_file,    "_monthly_cases_reports.png") , width = 9 , height = 5)

# 4  Crude adjustment


# ou assigned function #### 

ou.assigned.to.dataset = function( dataSet.id , .meta = md){
    
    ou.dataset = .meta$dataSets %>% 
        filter( dataset.id %in% dataSet.id ) %>% 
        select( id, name, organisationUnits ) %>%
        rename( dataset.id = id , dataset.name = name ) %>%
        unnest 
}



# Summarise ####
    
    # earliest reporting period 
    first.period.year = min( d.nest %>% unnest(data) %>% .$period ) %>% substr( . , 1, 4)

# Function to figure out which units are adding data...'point' is not accurate enough.
## consider Kenya.  Kapkuikui Dispensary has data, but does not have coordinates, like other clinics
## it has a CHU under it,  Kabosgei Kerio Comunity Unit, with no data.  
## Consider checking if OU has any children.

    x.unnest = x %>% 
        filter( children %in% 0 ) %>% 
        inner_join( ous %>% select( orgUnit, openingDate ) ,  by = 'orgUnit' ) %>%
        select( orgUnit, openingDate, feature, data ) %>% 
        unnest( data ) %>%
        mutate( 
            
            period.year = substr( period, 1, 4 ) ,
            
            openingDate.year = substr( openingDate, 1, 4 ) %>%
                # min year set to 2010 
                ifelse( . < first.period.year , first.period.year , . ) 
            
        ) 
    
    # summarise x by year of facility opening
    sum_by_open_year = x.unnest %>%
    group_by( openingDate.year, period.year  ) %>%
    summarise( 
        value.total = sum( value )
        ) 
    

    # Total by year 
    sum_by_open_year %>% group_by( period.year ) %>% 
        summarise( value.total = sum( value.total) ) %>%
        mutate( percent_change = (value.total - lag( value.total ) )/lag( value.total ) 
                )
    
    sum_by_open_year %>% spread( period.year , value.total )

# Plot #####


ggplot( sum_by_open_year ,  aes(x = period.year, 
                                y = value.total, 
                                group = openingDate.year , color = openingDate.year )) +
    geom_line() +
        
    # scale_y_continuous(limits = c(0,NA)) +
        
    facet_wrap( ~ dataElement, 
                labeller = label_wrap_gen(width = 25, multi_line = TRUE) ,
                ncol = 4
    ) +
    
    scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
    theme_bw() +
    theme(strip.text = element_text(size = 20, face = "bold")) +
    theme(axis.ticks=element_blank() ,
          axis.title=element_text(size = 16, face = "bold") ,
          axis.text=element_text(size = 14, face = "bold") ,
          legend.position = 'none' )
