# Load relevant data  ####
library( tidyverse )
library( RColorBrewer)
library( cowplot )

source('dhis2_functions.R')
source( "Trend code/Trend_Code.R")

dhis_instance = "Kenya"
data_directory = NA

origin.folder = paste0( ifelse( is.na( data_directory ), "" , 
                                paste0( dhis_instance, "/" ) 
) ,
dhis_instance , "/")

dataset.directory = paste0( dhis_instance , "/datasets/" )

meta_data_file = paste0( origin.folder , dhis_instance ,  "_metadata.rds" ) 

md = read_rds( meta_data_file )

ous = ous.translated( md )

nest.file = paste0( dataset.directory ,
                        dhis_instance,
                        "_data_nest.rds" ) 

d.nest = readRDS( nest.file )


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

de.selected = de.all[3] # Confirmed Malaria (Lab confirm Only)


#  Data from 1 data element ( may contain >1 combos )

x = d.nest %>% filter( dataElement.name %in% de.selected )

glimpse( x )
count(x, dataElement , dataElement.name )
count(x, categoryOptionCombo.name )


# Sum values for data.element assigned to corresponding dataset, by openingDate  ####
categoryOptionCombo.for.dataElement = x %>% 
    
    filter( dataElement.name %in% de.selected ) %>%
    
    select( categoryOptionCombo , categoryOptionCombo.name )     %>% unique 
    
dataset.for.data.element = dsde %>% 
    
    filter( dataElement %in% de.selected )


glimpse( dataset.for.data.element )

n.ou.year = md$organisationUnits %>%
    select( id, name, openingDate, level ) %>%
    mutate( year = year( openingDate ) ) %>%
    mutate( year = ifelse( year<= 2013, 2013, year )  ) %>%
    count( year , level ) %>%
    spread( level , n )

n.ou.dataset.year =ou.dataset %>% 
    
    filter( dataSet.id %in% dataset.for.data.element[2, 2] ) %>%
    mutate( year = year( openingDate ) ) %>%
    mutate( year = ifelse( year<= 2012, 2012, year )  ) %>%
    count( year )

n.ou.year
n.ou.dataset.year

x.ou.submit = x %>%
    
    filter( categoryOptionCombo.name %in% categoryOptionCombo.for.dataElement[ 1 , 2]  ) %>%
    
    inner_join( ou.dataset %>% 
                    
                    filter( dataSet.id %in% dataset.for.data.element[3, 2] ) ,
                
                by = c( "orgUnit" = "organistionUnit"  ) ) %>%
    
    mutate(  open.year = year( openingDate  ) ) %>% 
    
    mutate( open.year = ifelse( open.year<= 2013, 2013, open.year ) %>% as.factor() 
             
             ) %>% # all years before 2014 are all 2014

    mutate( 
        year.first = map_chr( data %>% map('period') , ~min( .x, na.rm = TRUE ) %>% substr(. , 1, 4 )    ) ,
        
        year.last = map_chr( data %>% map('period') , ~max( .x, na.rm = TRUE ) %>% substr(. , 1, 4 )    ) ,
        
        year.n = map(  data , ~count( .x , year = str_extract( period , "^.{4}") ) )   
    ) %>%
    
    rowwise() %>% mutate( 
        
            cohort = paste( year.first , year.last , sep = "-") ,
        )

glimpse(x.ou.submit)

count( x.ou.submit, year.first ) 
count( x.ou.submit, cohort, year.first ,  year.last ) 
count( x.ou.submit %>% unnest( year.n ), year  ) 
count( x.ou.submit %>% unnest( year.n ), year.first ,  year.last , year  ) 
count( x.ou.submit, open.year ) 

#  Numbers of facilities submitting reports each year 
d = count(  x.ou.submit %>% unnest( year.n ),  cohort , year.first , year.last , year ) %>%
    
    inner_join( 
        x.ou.submit %>% unnest( year.n ) %>% 
            group_by( year.first , year ) %>% 
            summarise( avg.months = mean( n ) ) , 
        by = c("year.first", "year")
    )

d.t = count(  x.ou.submit %>% unnest( year.n ),  year ) %>% 
    mutate( year.first = 'Total', n = nn) %>%
    
    inner_join( 
        x.ou.submit %>% unnest( year.n ) %>% 
            group_by(  year ) %>% 
            summarise( avg.months = mean( n ) ) , 
        by = c( "year")
    )

plot.n = ggplot(d , 
        aes( year , nn , group = cohort , color = year.first , 
             size = avg.months , label = round(avg.months)  ) ) + 
    geom_line( size = 1 ) +
    geom_point() +
    geom_text( color = 'white', aes( size = avg.months * .7 ) ) +
    # geom_line( data = d.t , size = 1) +
    # geom_point( data = d.t ) +
    # geom_text( data = d.t , color = 'white' ) +
    scale_color_manual( values = c( brewer.pal( 5 , "Accent") , "black") , 
                        labels = c(  unique(as.character(d$year.first)), 'total') ) +
    theme_bw() +
    theme( legend.position = "" ) +
    labs( title = 'Numbers of facilities reporting, by year openedDate')

## Sum of reported values
s =  x.ou.submit %>%
    
    rowwise() %>% 
    
    mutate( 
        
        cohort = paste( year.first , year.last , sep = "-") 
        
    ) %>%
    
    unnest( data ) %>%
    
    mutate( 
        year = substr( period , 1, 4 ) ,
        
        cohort = paste( year.first , year.last , sep = "-") 
        
        ) %>%
    
    group_by( year.first , year.last , cohort ,  year ) %>%
    
    summarise( s = sum( value, na.rm = TRUE ) ,
               
               n = n() ,
               
               facilities = n_distinct( orgUnit ) ,
               
               avg.months = n / ( facilities )
               
               ) %>%

    group_by( year ) %>%
    
    mutate( total = sum( s, na.rm = TRUE ) , 
            pct.total = s / total  
            ) %>%
    
    arrange( year.first , year.last , year ) 


plot.sum = ggplot( s , aes( year ,  s ,  group = cohort , 
                           color = year.first  , 
                           size = round(avg.months) , label = round(avg.months)  ,
                           fill = year.first ) ) + 
    geom_line( size = 1 ) +
    geom_point(  ) +
    geom_text( color = 'white', aes( size = avg.months * .7 ) ) +
    geom_line( aes( y = total ), color = 'black' , size = 1 ) +
    geom_point( aes( y = total ) , color = 'black' ) +
    geom_text( color = 'white', aes(  y = total , size = avg.months * .7 ) ) +
    scale_color_manual( values = c( brewer.pal( 6 , "Accent") , "black") , 
                        labels = c(  unique(as.character(s$year.first)), 'total') ) +
    # geom_col() +
    theme_bw() +
    # theme( legend.position = "" ) +
    labs( title = 'Sum of values reported, by year openedDate')


plot_grid( plot.n, plot.sum , labels = "AUTO", scale = c(1,  1) )


#  Submission Dynamics and Effect on Trends




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
