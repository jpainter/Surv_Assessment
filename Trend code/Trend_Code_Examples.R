# Trend_Code_Examples.R

library( tidyverse )
library( lubridate )
library( knitrProgressBar )


source( "Trend code/Trend_Code.R")
source("dhis2_functions.R")

# Malawi.  Use discordance data to test-estimate decomposition  ####
malawi = readRDS( "Malawi/Malawi_dataset_details.rds") %>% select( -level )
glimpse( malawi )

meta = readRDS( "Malawi/Malawi_metadata.rds") 
# View(meta)

# create shortcut to list of orgUnits, creating variables for level and feature type
# ous = ous.translated( meta , open.only = FALSE)

# use updated ous
ous.updated = readRDS( "Malawi/Malawi_updated_facilities.rds") %>% 
    rename( orgUnit = id, orgUnit.name = name , feature = feature_type ,
            parent_ou = parent.id , parent_ou.name = parent.name
    ) %>%
    mutate( lat = lat. , long = long. )

glimpse(ous.updated)
names(ous.updated)
imputed.ous = ous.updated %>% filter( impute == TRUE ) %>% .$orgUnit

ous = ous_from_metatdata( meta ) %>%
    select( -shortName , -created , -lastUpdated, -level.name ) %>%
    rename( orgUnit = id, parent_ou = parent.id )
names(ous)


ous[ imputed.ous , ] = ous.updated[ imputed.ous , names(ous)]


glimpse( ous )
count( ous, level, feature  )
count( ous, parent_ou.name )

d = dataset.translated( malawi, .ous = ous , .meta = meta  ) %>%
    left_join( ous.updated %>% select( orgUnit , lat , long ) , by = 'orgUnit')  %>%
    # combine data element with category option combo
    unite( de.coc, dataElement, categoryOptionCombo ) %>%
    unite( de.coc.name , dataElement.name , categoryOptionCombo.name ) 


glimpse(d)
count( d, level )
count( d, feature , level )

# clinics submitting any data
d %>% group_by(level, feature ) %>% 
    summarise(unit = n_distinct(orgUnit)) %>%
    spread( feature , unit )

# choose data element ####
de.coc.all = unique(d$de.coc.name)
de.coc.selected = de.coc.all[7] # "HMIS Malaria – New Case (under 5)"

# Nest data by orgUnit ,  de.coc.name 
# NB: careful about frequency--don't want to mix monthly and weekly
# NB: at present, only monthly values downloaded, so not an issue

d.nest = d %>% 
    complete( nesting(orgUnit, de.coc.name, period) ) %>%
    mutate( value = as.integer( value ) ) %>%
    group_by( level, feature, orgUnit ,  orgUnit.name , lat, long , parent_ou.name ,  de.coc.name ) %>%
    nest( period, value ) 
    
glimpse( d.nest )

# model d.nest for selected element/catoegory: "HMIS Malaria – New Case (under 5)" ####
x = d.nest %>% filter( de.coc.name %in% de.coc.selected )
count( x, level )

# tests...
# df  = ts.ou( x[7, ] )
# decompose.seas( df , verbose = TRUE  ) 
# decompose.seas.coe( df , verbose = FALSE  ) 
# decompose.stl( df   ) 
# decompose.stl( df  , transform = "log" ) 

x.dec = x %>% 
    mutate( 
        ts = map( data,   ~ts.df(.x)  ) ,
        n = map_dbl( ts, ~sum( !is.na(.x) ) ) ,
        total = map_dbl( ts , ~sum( .x, na.rm =  TRUE ))  
            ) %>%
    mutate( 
        dec = map_dbl( ts, ~decompose.stl( .x , transform = "log", plot = FALSE )
                   ) 
        )

count(x.dec, level, feature )
    
summary( x.dec$dec  )


max_dec = max( x.dec$dec, na.rm = TRUE )

hist( x.dec$dec , breaks = seq( 0 , max_dec + .25 , .25 )  )

# consider different groups, based on dec
x.dec$quality = cut( x.dec$dec, breaks = c( 0, .5, 1 , 2, Inf) , labels = letters[1:4] )
count( x.dec, quality )
ggplot( x.dec ) + geom_histogram( aes( dec ), binwidth = .1 ) + facet_wrap( ~quality , scales = 'free')

# Overall histo
gg = ggplot( x.dec ) + 
    geom_histogram( aes( dec ), binwidth = .1 ) + 
    scale_x_continuous(limits = c(0,2)) 
gg <- gg + labs( x='\n Coefficient of Variation', y='Count \n')
gg <- gg + theme_bw()
gg <- gg + theme(panel.grid.minor=element_blank())
gg <- gg + theme(panel.grid.major.y=element_blank())
gg <- gg + theme(panel.grid.major.x=element_line())
gg <- gg + theme(axis.ticks=element_blank() ,
                 axis.title=element_text(size = 16, face = "bold") ,
                 axis.text=element_text(size = 14, face = "bold") )
gg <- gg + theme( legend.position="none" )
gg <- gg + theme( panel.border=element_blank() )
gg
ggsave( 'cv_histogram.png' , width = 6, height = 4 )

# histo by level
gg = x.dec %>% ggplot() + 
    geom_histogram(aes(quality), stat = 'count') + 
    facet_wrap(~level)
gg <- gg + labs( x='\n Score', y='Count \n')
gg <- gg + theme_bw()
gg <- gg + theme(panel.grid.minor=element_blank())
gg <- gg + theme(panel.grid.major.y=element_blank())
gg <- gg + theme(panel.grid.major.x=element_line())
gg <- gg + theme(axis.ticks=element_blank() ,
                 axis.title=element_text(size = 16, face = "bold") ,
                 axis.text=element_text(size = 14, face = "bold") )
gg <- gg + theme( legend.position="none" )
gg <- gg + theme( panel.border=element_blank() )
gg
ggsave( 'cv_histogram_levels.png' , width = 6, height = 4 )


# plot dec by wt
ggplot( x.dec ) + geom_point( aes( x = total, y = dec , color = factor(level) ), alpha = .3 )

# EXAMPLES ####
# show example dec:  LOW 
low_dec = which( x.dec$dec < .15 )
length( low_dec )

ous_ids = x.dec$orgUnit[low_dec]
ous_names = x.dec$orgUnit.name[low_dec]

pick_one = sample( low_dec , 1 ) 

sample_dec( index = 1700 , method = 'stl' )
sample_dec( index = 1700 , method = 'seas' )
sample_dec( index = pick_one , method = 'seas', smooth = TRUE , impute = TRUE )


# show example dec:  MED 
med_dec = which( x.dec$dec >  .15 & x.dec$dec < .75 )
length( med_dec )
pick_one = sample( med_dec , 1 ) 
sample_dec( index = pick_one  , method = 'seas' )


# show example dec:  HIGH 
high_dec = which( x.dec$dec > .75 )
length( high_dec )
pick_one = sample( high_dec , 1 ) 
sample_dec( index = pick_one , method = 'seas' )

# show example dec:  UNCONTROLLED 
high_dec = which( x.dec$dec > 2 & x.dec$total > 100 )
length( high_dec )
pick_one = sample( high_dec , 1 ) 
x.dec[ pick_one , ]$ts[[1]]
sample_dec( index = pick_one  )
sample_dec( index = pick_one , year = 2015  )
sample_dec( index = pick_one , method = 'seas' )
sample_dec( index = pick_one , method = 'seas' , year = 2015 )
sample_dec( index = pick_one , method = 'seas',  impute = TRUE , zero_as_missing = TRUE )
sample_dec( index = pick_one , method = 'seas',  impute = TRUE , zero_as_missing = TRUE , detect_outliers = TRUE)

### FIxed seasonal==use seasonal from time series a and appliy it to timeseries b ####

# TODO: look at seasonal curve by region.  Is it smooth; is it ~same in each region? Does time-period change?
pick_one.a = sample( low_dec , 1 ) 
df.a  = x.dec[ pick_one.a , ]$ts[[1]]
df.name = paste(  data$parent_ou.name[ pick_one.a ]  , " : " ,
                  data$orgUnit.name[ pick_one.a ] , " (Total Cases = " ,
                  scales::comma( data$total[ pick_one.a ] ), ")" )
seas_df.a = decompose.seas( df.a  , transform = "log" , title = df.name )
seasonal = seas_df.a[[1]][[1]]$data %>% as_data_frame() %>% .$seasonal %>% as.double()

pick_one.b = sample( low_dec , 1 ) 
df.b  = x.dec[ pick_one.b , ]$ts[[1]]
df.name = paste(  data$parent_ou.name[ pick_one.b ]  , " : " ,
                  data$orgUnit.name[ pick_one.b ] , " (Total Cases = " ,
                  scales::comma( data$total[ pick_one.b ] ), ")" )
seas_df.b = decompose.seas( df.b  , transform = "log" , title = df.name )

df.log = log( df.b )
df.seasonal = df / seasonal
date = as.Date( as.yearmon(time( seas_df.b[[1]][[1]]$x ) ) ) 
as.integer( df.seasonal )
smooth_trend( )


seas_output = tibble(
    date =  date  ,
    observed = as.integer( df ) , 
    seasonal = seasonal , 
    trend = as.integer( df/seasonal ) ,
    remainder = NA
    
) %>%
    gather( var, value, -date) %>%
    mutate( 
        var = factor( var, levels = c('observed', 'trend', 'seasonal', 'remainder') ) 
    )

g = ggplot( seas_output, aes( x =  date , y = value )) +
    geom_line() +
    facet_grid( var ~ . , scales = 'free' ) +
    theme_bw() +
    labs( title = df.name , 
          subtitle = paste( "Coefficient of Variation:" , scales::percent(coe) ) ,
          caption = paste( transform , ":" , seas_df$model$arima$model ) )
g

#### OUTLIERS #####
library(tsoutliers)
library( forecast )
df = x.dec[ 574 , ]$ts[[1]]
decompose.seas( df )
scales::comma( sum( df , na.rm = TRUE ) )  # total with outlier

fit = arima( df , order = c(0,1,1) , seasonal = list( order = c(0,1,1) ) )
resid = residuals( fit )
pars = coefs2poly(fit)
coefs <- coef(fit)
outliers = locate.outliers( resid, pars )
outliers 
res <- locate.outliers.oloop( df , fit, types = c( "AO")) # "AO", "LS", "TC"
res$outliers
discards = discard.outliers(res, df, method = "bottom-up", tsmethod.call = fit$call)$outliers$ind 
df[ discards ] = NA 
scales::comma( sum( df , na.rm = TRUE ) )  # total without outlier
decompose.seas( df ,  impute = TRUE , zero_as_missing = TRUE )

##### MISSING #####
not_many = which( x.dec$n >27 & x.dec$n < 40 )
df = x.dec[ not_many[1] , ]$ts[[1]]
decompose.seas( df )
scales::comma( sum( df , na.rm = TRUE ) )  # total with outlier
decompose.seas( df ,  impute = TRUE , zero_as_missing = TRUE )

# get start (a) and stop (b) times
    time.min = map( x.dec$ts, ~time(.x) %>% as.Date() %>% min( na.rm = TRUE ) ) 
    a = min(time.min[[1]]) 
    time.max = map( x.dec$ts, ~time(.x) %>% as.Date() %>% max( na.rm = TRUE ) ) 
    b = max(time.max[[1]]) 

all.dates <- seq(  a , b , by="month")
all.dates.frame <- ts(  rep( NA, length( all.dates) ), all.dates ) 




# Summary by level ####
x.dec  %>% .$dec %>% hist(. , breaks = seq( 0 , max( ., na.rm = TRUE ) + .25 , .25 ) )
x.dec %>% filter( quality %in% 'd' ) %>% .$dec %>% hist(. , breaks = seq( 0 , max( ., na.rm = TRUE ) + .25 , .25 ) )

x.dec %>% group_by( level , feature ) %>%
    summarise(
        wt.mean = weighted.mean( dec , total , na.rm = TRUE )
    )
# map  ####
library(sp)
region.dec = x.dec %>% filter( level == 2 , feature %in% 'Polygon' ) 
regions = admins[ match( region.dec$orgUnit, admins$id ), ]$polygons 
map.region = SpatialPolygonsDataFrame( regions , region.dec, match.ID = FALSE )

district.dec = x.dec %>% filter( level == 3 , feature %in% 'Polygon' ) 
districts = admins[ match( district.dec$orgUnit, admins$id ), ]$polygons 
map.district = SpatialPolygonsDataFrame( districts , district.dec, match.ID = FALSE )

factpal <- colorFactor( 
    c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00'), 
    map.district$quality, reverse = TRUE )

binpal <- colorBin( brewer.pal( 5, "Reds"), 
                    domain = map.district$dec , bins =5, 
                    na.color = "#bdbdbd", pretty = TRUE
)

# Find a center point for each region
library(rgeos)
centers.region <- data.frame(gCentroid(map.region, byid = TRUE))
centers.region$name <- map.data$orgUnit.name
centers.region$dec <- map.data$dec

centers.district <- data.frame(gCentroid(map.district, byid = TRUE))
centers.district$name <- map.district$orgUnit.name
centers.district$dec <- map.district$dec



library(leaflet)
regionMap = leaflet(  ) %>%
    
    addTiles(  urlTemplate = 
                   "http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png"
    )  %>% 
    
    addPolygons( data = map.region ,
                 group = 'Region' ,
                 color = "black",
                 weight = 1 ,
                 opacity = 1 ,
                 # label = ~paste( scales::percent(dec) ),
                 # labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, textsize = "14px") ,
                 popup = ~paste( orgUnit.name , percent( dec ) )  ,
                 fillColor =  ~binpal(dec),
                 fillOpacity = .5
    ) %>%
    addPolygons( data = map.district ,
                 group = 'District' , 
                 color = "black", 
                 weight = 1 , 
                 opacity = 1 ,
                 # label = ~paste( scales::percent(dec) ),
                 # labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, textsize = "14px") ,
                 popup = ~paste( orgUnit.name  , percent( dec ) )  ,
                 fillColor =  ~binpal(dec),
                 fillOpacity = .5
    ) %>%
    addLabelOnlyMarkers(data = centers.district,
                        group = 'District' ,
                        lng = ~x, lat = ~y, label = ~dec,
                        labelOptions = labelOptions(noHide = F, textOnly = TRUE,
                                                    textsize = "15px" )
                        ) %>% 
    addLegend(position = "bottomright", pal = binpal, values = map.district$dec,
              title = "Error",
              opacity = 1 ) %>%
    # Layers control
    addLayersControl(
        overlayGroups = c("Region", "District"), 
        # options = layersControlOptions(collapsed = FALSE),
        position = "topright"
    )  
                 
regionMap





# Try all elements... ####

d.nest = d %>% 
    complete( nesting(orgUnit, de.coc.name, period) ) %>%
    mutate( value = as.integer( value ) ) %>%
    group_by( level, feature, orgUnit ,  orgUnit.name , lat., long. , 
              parent_ou.name ,  de.coc.name ) %>%
    nest( period, value ) 

glimpse( d.nest )

pb <- progress_estimated( nrow(d.nest) )

d.nest.dec = d.nest %>% 
    mutate( 
        ts = map( data,   ~ts.df(.x)  ) ,
        n = map_dbl( ts, ~sum( !is.na(.x) ) ) ,
        total = map_dbl( ts , ~sum( .x, na.rm =  TRUE ) )  
    ) %>%
    mutate( 
        dec = map( ts, ~decompose.stl( .x , transform = "log", plot = FALSE, .pb = pb )  )
        ) 

# Summary by element 
d.nest.dec %>% 
    group_by( de.coc.name , level ) %>%
    summarise(
        n = n()  ,
        wt.mean = weighted.mean( dec %>% unlist , total , na.rm = TRUE )  
    ) %>% 
    spread( level, wt.mean ) %>% View()


# earlier versions ####

# clinics submitting this data element
d %>% 
    filter( de.coc.name %in% de.coc.selected ) %>%
    group_by(level) %>% 
    summarise( unit = n_distinct(orgUnit)) %>%
    inner_join( count( ous, level ) , by = "level")

## number clinics by level

# randmly select 1 lowest level orgunit with data
l = which.max( count(d, level)$n )
lowest_level_ous_with_most_data = which( ous$level %in% l )
# set.seed(89)

# random selection  ####
an_ous =  ous.random(4) 

# TIME-Series  ####
de.id = 'E4LZP3KUbOG' # MALAWI: NMCP OPD Confirmed Malaria Cases Through RDT

low_dec
ous_ids = x.dec$orgUnit[low_dec]
ts_ou_de = ts.ou.de( data = d , ous =  ous_ids[1] , de = de.id , start_year = NA )
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



