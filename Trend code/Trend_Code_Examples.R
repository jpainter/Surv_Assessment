# Trend_Code_Examples.R

library( tidyverse )
library( lubridate )


source( "Trend code/Trend_Code.R")
source("dhis2_functions.R")

# Malawi.  Use discordance data to test-estimate decomposition  ####
malawi = readRDS( "Malawi/Malawi_dataset_details.rds") 
glimpse( malawi )

meta = readRDS( "Malawi/Malawi_metadata.rds") 
# View(meta)

# create shortcut to list of orgUnits, creating variable for the level
ous = meta$organisationUnits[ , c('id', 'name', 'path')] %>%
    rowwise() %>%
    mutate( level = str_count( path , "/") , 
            parent_ou = parse_parent_ous(path) ) %>% 
    left_join( meta$organisationUnits[ , c('id', 'name')] 
                %>% rename( parent_ou.name = name ) ,
                by = c( 'parent_ou' = 'id' )) %>%
    ungroup() %>%
    select( -path) 

count( ous, level )
count( ous, parent_ou.name )

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
# clinics submitting data
d %>% group_by(level) %>% summarise(unit = n_distinct(orgUnit))

# choose data element
dataElementNames = unique(d$dataElement.name)
de.name = dataElementNames[7] # "HMIS Malaria – New Case (under 5)"
de.id = meta$dataElements[ , c("id", "name")] %>%
    filter( name %in% de.name ) %>%
    .$id

# clinics submitting this data element
d %>% 
    filter( dataElement %in% de.id ) %>%
    group_by(level) %>% summarise(unit = n_distinct(orgUnit))


# randmly select 1 lowest level orgunit with data
l = which.max( count(d, level)$n )
lowest_level_ous_with_most_data = which( ous$level %in% l )
# set.seed(89)

# random selection  ####
an_ous =  ous.random(4) 

# TIME-Series  ####
an_ous 
ts_ou_de = ts.ou.de( data = d , ous =  an_ous$id , de = de.id , start_year = NA )
ts_ou_de

# Random Decompose Example ####

decompose.stl( df = ts_ou_de )

decompose.seas( df = ts_ou_de, transform = "log" 
                , arima = "(1 1 1)(1 1 1)"
                # , regression_variables = NULL
                # , smooth = TRUE
                # , missing_one = TRUE
                )

# decompose.seas( df = ts_ou_de , transform = "none")

decompose.seas( df = ts_ou_de , transform = "auto")


# get results for 3 different methods
decompose.3( tsd  = d , .ous =  an_ous$id , .de = de.id  )


# All org units. ####
# Test: moh = "tY7DfIReFcY"
# .x = which( ous$id %in% "lZsCb6y0KDX" ) # MOH

n = nrow(ous)

CoeffVars = list()
p <- progress_estimated(n)
for ( .x in 1:n ){
    
    # print( .x )
    
    description =             
        data_frame(
            ou = ous$id[.x] ,
            level = ous$level[.x] ,
            parent_ou = ous$parent_ou.name[.x] ,
            orgunit = ous$name[.x] 
    )
    
    p$pause(0)$tick()$print()
    
    # if data in time-series, then decompose; otherwise skip
    ts_ou_de = ts.ou.de( data = d , ous =  ous$id[.x] , de = de.id , start_year = NA )

    if (is.na( ts_ou_de) )  next

    data.summary = ts.summary( ts_ou_de ) 
    
    CoeffVars[[.x]]  = bind_cols( description, data.summary ) %>%
        group_by( level, ou, orgunit )  %>%
        nest() %>%
        mutate( 
            # ts = list( ts_ou_de ),
            seas_0 = list( decompose.seas( df = ts_ou_de 
                                           , transform = "log"  
                                           , plot = FALSE
                                           , cov = FALSE 
                                           , arima = "(0 1 1)(0 1 1)"
                                           , impute = TRUE
            )
            ), 
            seas_1 = list( decompose.seas( df = ts_ou_de 
                                     , transform = "log"  
                                     , plot = FALSE
                                     , cov = FALSE 
                                     , arima = "(0 1 1)(1 1 1)"
                                     , impute = TRUE
                                     )
            )
            )
}

# NB decompose.seas returns list with 3 elements
# 1- coefficient of variation
# 2 - ggplot
# 3 seas model

CoeffVar = data.table::rbindlist( CoeffVars , fill = TRUE ) 

saveRDS( CoeffVar , "Malawi_CoeffVar.rds")

CoeffVar = readRDS( "Malawi_CoeffVar.rds")
View( CoeffVar %>% arrange( level) )


c = CoeffVar %>% 
    mutate( cov0 = map_dbl( CoeffVar$seas_0 , 1) ,
            cov1 = map_dbl( CoeffVar$seas_1 , 1) ) %>%
    select( -seas_0, -seas_1)

glimpse(c)
ggplot( c, aes( cov0, cov1 )) + geom_point() + facet_wrap( ~ level )

larger_cov1 = which( c$cov1 > c$cov0 )
larger_cov0 = which( c$cov0 > c$cov1 )
c[ larger_cov1 ,] %>% 
    select( -seas_0, -seas_1) %>%
    mutate( difference = cov1-cov0) %>%
    arrange( difference )

which( c$cov1>4)

# Raw versus adjusted (rva)
n = nrow(CoeffVar)
rva = CoeffVar[1:n, ] %>% 
    mutate(
        raw = map(1:n, ~ts.ou.de( data = d , ous =  CoeffVar$ou[.x] , de = de.id , start_year = NA ) )

       ,  adj = map( 1:n ,
                   ~if ( is.na( CoeffVar$seas_0[.x][[1]][[1]] ) ){
                       return(NA)
                       } else { return( CoeffVar$seas_0[.x][[1]][[3]]$x ) }
                           ) 
       ) %>%
    select( -seas_0, -seas_1) 

to.yearly = function(ts){  
    if ( length( ts ) <12 ) return( NA) 
    
    aggregate.ts( ts, nfrequency = 1 )  
    }
rva$raw.y = map( 1:nrow(rva), ~to.yearly( rva$raw[.x][[1]] ) ) 
rva$adj.y = map( 1:nrow(rva), ~to.yearly( rva$adj[.x][[1]] ) ) 

rva_ts_diff = function( adj, raw ){
    a = adj[[1]]
    r = raw[[1]]
    if ( is.na(a) & is.na(r)) return( NA )
    if ( is.na(r) ) return( a )
    if ( is.na(a) ) return( a )
    return( sum( a , -1*r , na.rm =  TRUE ) )
}
rva$diff.y = map( 1:nrow(rva), ~rva_ts_diff( rva$adj.y[.x], rva$raw.y[.x] ) ) 

rva_diff = function( adj, raw ){
    a = sum( as.integer( adj[[1]]) , na.rm = TRUE )
    r = sum( as.integer( raw[[1]]) , na.rm = TRUE )
    if ( a == 0 ) return( 0 )
    return( a - r )
}

rva$diff = map_dbl(   1:n  , ~rva_diff( rva$adj[.x], rva$raw[.x] ) )

save( rva, file = 'rva.test.rda')
load( 'rva.test.rda')

# tsibble? ####
library(tsibble)
raw = map(1, ~ts.ou.de( data = d , ous =  CoeffVar$ou[.x] , de = de.id , start_year = NA ) )
raw.t = raw[[1]] %>% as.tsibble()
raw.t %>% index_by(year = year(index)) %>% 
    summarise( v=sum(value, na.rm = TRUE))


ts = rva %>%
    select( level, orgunit, raw, adj ) %>%
    group_by( level, orgunit ) %>%
    mutate( raw.ts = map( raw, ~if (length(.x) > 1 ) as.tsibble(.x) ) , 
            adj.ts = map( adj, ~if (length(.x) > 1 ) as.tsibble(.x) )
) %>%
    nest()


ts.y  = ts %>% 
    mutate( 
        raw.year = map( data, ~if ( is.tsibble( .$raw.ts[[1]] ) ){
            .$raw.ts[[1]] %>% index_by(year = year(index)) %>% 
                summarise( v=sum(value, na.rm = TRUE)) 
            }
            )
    
    , adj.year = map( data, ~if ( is.tsibble( .$adj.ts[[1]] ) ){
        .$adj.ts[[1]] %>% index_by(year = year(index)) %>% 
            summarise( v=sum(value, na.rm = TRUE)) 
        }
        )
    )

ts.y$raw.year[[1]]
map( ts.y$raw.year, 2 )

raw.y = ts.y %>% unnest( raw.year %>% map( as.data.frame) ) %>% rename( raw = v )  

adj.y = ts.y %>% unnest( adj.year %>% map( as.data.frame) )  %>% rename( adj = v )  

diff.y = inner_join( raw.y, adj.y , by = c("level", "orgunit", "year") ) %>%
    mutate( diff = adj - raw ,
            percent_adj = diff / raw )

# plot raw and adjusted for level 4 only because higher levels are not adjusted
#' NB level ==4 is a proxy for facility, but preferable to filter by faciltiy--e.g. coordinates
#' are a point, not a polygon.  

ggplot( 
    diff.y %>% 
        filter( level == 4 ) %>%
        group_by( year  ) %>% 
        summarise_at( c('raw', 'adj', 'diff') , sum, na.rm = TRUE ) %>%
        gather( var, val,  -year )
       ,  aes( year, val , group = var, color = var )
    ) +
    geom_line() +
    scale_y_continuous( label = scales::unit_format(unit = "M", scale = 1/1e6) )



# Spot check.  Get decompose.3 for an orgUnit ID ####

    .x = which( ous$id %in% c$ou[615])
    
    ts_ou_de = ts.ou.de( data = d , ous =  ous$id[.x] , de = de.id , start_year = NA )
    ts_ou_de = tail( ts_ou_de, 36)
    decompose.seas( df = ts_ou_de, transform = "log" 
                    , arima = "(0 1 1)(0 1 1)"
                    , title = ous$name[ .x ]
                    # , regression_variables = NULL
                    # , smooth = TRUE
                    # , missing_one = TRUE
                    , impute = TRUE
    )


    bind_cols(
        ou = ous$id[ .x ] ,
        level = ous$level[.x] ,
        parent_ou = ous$parent_ou.name[.x] ,
        orgunit = ous$name[.x] ,
        decompose.3( tsd = ts.ou.de( data = d , ous =  ous$id[ .x ] , de = de.id , start_year = NA ),
                     show_plot = TRUE )  
    )

# Summary of facilities in north zone ####
    
    parent.id = "lZsCb6y0KDX" 
    id = which( ous$id %in%  parent.id ) 

    # start by plotting raw data from all clinics
    d.clinics_within_parent = d %>%
        filter( parent_ou %in% parent.id , dataElement %in% de.id ) %>%
        mutate( parent = FALSE )
    
    d.parent = d %>%
        filter( orgUnit %in% parent.id , dataElement %in% de.id ) %>%
        mutate( parent = TRUE )
    
    d.plot = bind_rows( d.parent, d.clinics_within_parent ) %>%
        group_by( orgUnit ) %>%
        mutate( value = as.integer( value ) , 
                scaled_value = scale( value )
        )
    
    ggplot( d.plot , aes(x = date, y = value , group = orgUnit )) +
        geom_line(  aes( color = parent
                         # , alpha = parent 
                         ) ) +
        guides( color = FALSE, alpha = FALSE  ) +
        theme_minimal()
    
    # scale data 
    ggplot( d.plot , aes(x = date, y = scaled_value , group = orgUnit )) +
        geom_line(  aes( color = parent ) ) +
        guides( color = FALSE, alpha = FALSE  ) +
        theme_minimal()
    
    ggplot( d.plot , aes(x = date, y = value , group = orgUnit )) +
        geom_line(  aes( color = parent ) ) +
        guides( color = FALSE, alpha = FALSE  ) +
        facet_grid( orgUnit ~ ., scale = 'free') +
        theme_minimal()
    
    # decompose parent

    decompose.seas( df = ts.ou.de( data = d , ous =  ous$id[ id ] , de = de.id , start_year = NA ),
                    plot = TRUE , title = ous$name[ id ] )  
    
    
    # which child has the large # in 2016?
    big_clinic = d.clinics_within_parent[ which.max(d.clinics_within_parent$value),  ] %>% .$orgUnit
    id = which( ous$id %in%  big_clinic ) 
    decompose.seas( 
        df = ts.ou.de( data = d , ous =  ous$id[ id ] , de = de.id , start_year = NA )
        , transform = "log"
        , plot = TRUE 
        # , detect_outliers = TRUE
        , title = ous$name[ id ] 
    )  
    
    
    # inidvidual facilities
    facility.id = "WIiA3ENQI5A"  
    id = which( ous$id %in%  facility.id ) 
    decompose.seas( 
        df = ts.ou.de( data = d , ous =  ous$id[ id ] , de = de.id , start_year = NA )
        , transform = "log"
        , plot = TRUE 
        # , detect_outliers = TRUE
        , title = ous$name[ id ] 
        )  
    
    
    # children
    CoeffVar %>% 
        semi_join( d.clinics_within_parent , by = c("ou" = "orgUnit")) %>%
        mutate( 
            n = n_distinct( ou ) , 
            seas_auto = map_dbl( seas_log , ~str_split( .x,"%")[[1]][1] %>% as.double) 
        ) %>%
        summarise_if( is.numeric, median , na.rm = TRUE ) 
    # Seas_auto: median ~ 23; mean ~ 30; compared with crude ~ 10.6 
    
    # see range of missing values
    parent.name = ous$name[.x]
    CoeffVar %>% 
        filter( parent_ou %in% parent.name ) %>%
        mutate( seas_auto = map_dbl( seas_log , ~str_split( .x,"%")[[1]][1] %>% as.double) ) %>%
        mutate( 
            n_missing = ifelse( is.na(n_missing) , 60, n_missing ) , # if NA, set to all 60 months
            proportion_missing = n_missing / 60 
            ) %>% # out 60 months
        ggplot() + geom_histogram( aes( x = proportion_missing ) , binwidth = 0.05, col="grey" ) + 
        scale_x_continuous("missing data", seq(0,1,0.1) , labels = scales::percent ) +
        scale_y_continuous( breaks = seq(0,100, 2) ) 
    

     # BoxPlot of coeff.varCoeffVar
    
    parent.name = ous$name[.x]
    CoeffVar %>% 
        # filter( parent_ou %in% parent.name ) %>%
        mutate( seas_auto = map_dbl( seas_log , ~str_split( .x,"%")[[1]][1] %>% as.double) ) %>%
        ggplot( aes(x = 'zones' , y = seas_auto)) + geom_boxplot() +
        facet_wrap(~level, scales = 'free')
        
        
        
        
        
        
        
        #  Summary ####


glimpse(CoeffVar)
CoeffVar.gather = CoeffVar %>% gather( var, value, stl, seas_log, seas_auto ) %>%
    mutate( value = map_dbl( value , ~str_split( .x,"%")[[1]][1] %>% as.double) )

CoeffVar. = CoeffVar.gather  %>% spread( var, value ) 

# boxplot
ggplot( CoeffVar.gather %>% filter( var %in% "stl"  ) , 
        aes( 
            x= level , # reorder( parent_ou, value) ,
            y = value, group = level)
        ) +
    geom_boxplot() + 
    geom_jitter(width = 0.2) + 
    geom_boxplot(outlier.colour = "red", outlier.shape = 1) +
    labs( x = "Org Unit Level", y = "Coefficient of Variation" , 
          title = "Example of Variation within Facilities",
          subtitle = de.name ) + 
    theme_minimal()


CoeffVar.gather %>% 
    group_by( var , level ) %>%
    summarise_at( .vars = "value", .funs = c( 'mean' , 'min' , 'max') , na.rm = TRUE ) 




# lolipop chart 
library(ggalt)
library(scales)

gg <- ggplot( CoeffVar.gather , 
              aes( 
                  y= reorder( parent_ou, value) ,
                  x = value)
)
gg <- gg + geom_lollipop(point.colour="steelblue", point.size=2, horizontal=TRUE)
gg <- gg + facet_wrap(~var)
gg

# gg <- gg + scale_x_continuous(expand=c(0,0)
#                               , labels=percent
#                               , breaks=seq(0, 1, by=0.2)
#                               , limits=c(0, 1)
#                               )
gg <- gg + labs(x=NULL, y=NULL, 
                title="Variation",
                subtitle="by method",
                caption="malawi dhis")


gg <- gg + theme_minimal()
gg <- gg + theme(panel.grid.major.y=element_blank())
gg <- gg + theme(panel.grid.minor=element_blank())
gg <- gg + theme(axis.line.y=element_line(color="#2b2b2b", size=0.15))
gg <- gg + theme(axis.text.y=element_text(margin=margin(r=0, l=0)))
gg <- gg + theme(plot.margin=unit(rep(30, 4), "pt"))
gg <- gg + theme(plot.title=element_text(face="bold"))
gg <- gg + theme(plot.subtitle=element_text(margin=margin(b=10)))
gg <- gg + theme(plot.caption=element_text(size=8, margin=margin(t=10)))

gg



