# Deprecated (May 1, 2019) Leaflet maps of orgUnits 


```{r orgUnitMap}

# pal = colorNumeric('OrRd', d$value.scale )
binpal <- colorBin(
    c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00'), 
    clinics$Year , 2014:2018 )

binpal.admins.count <- colorBin(
    brewer.pal( 5, "Reds"), 
    admins.count$n )

factpal <- colorFactor( 
    c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00'), 
    clinics$Year, reverse = TRUE )

orgUnitMap = leaflet(  data = facilities  ) %>%
    
    addTiles(  urlTemplate = 
                   "http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png"
    )  %>%
    
    addPolygons( data = admins[ admins$level %in% 2 , ]$polygons  ,  
                 group = 'Regions' , 
                 color = "black", 
                 weight = 1 , 
                 opacity = .5 ,
                 fillColor = "white" ,
                 fillOpacity = 0
                 
    ) %>%
    addPolygons( data = admins[ admins$level %in% 3 , ]$polygons ,  
                 group = 'Districts' , 
                 color = "black", 
                 weight = 1 , 
                 opacity = .5 ,
                 fillColor = "white" ,
                 fillOpacity = 0
                 
    ) %>%
    addPolygons( data = admins[ admins$level %in% 4:6 , ]$polygons,  
                 group = 'Sub-districts' , 
                 color = "black", 
                 weight = 1 , 
                 opacity = .5 ,
                 fillColor = "white" ,
                 fillOpacity = 0
                 
    ) %>%
    
    addCircleMarkers( data = admins.count[ !is.na( admins.count$n ) , ] ,
                      group = "Facility count" ,
                      ~long, ~lat  # use mix of actual and imputed values 
                      , radius =  ~  n^.5 ,
                      weight =  1 ,
                      label = ~paste( n )  ,
                      labelOptions = labelOptions( 
                          permanent = TRUE ,
                          noHide = FALSE, 
                          textOnly = TRUE, 
                          textsize = "14px"
                      ) ,
                      # clusterOptions = markerClusterOptions() ,
                      popup = ~paste( n )  ,
                      color = "Grey" , # ~binpal.admins.count(n)  , 
                      opacity = .7 ,
                      fillColor = "Grey" , # ~binpal.admins.count(n) , 
                      fillOpacity = .75
                      
    ) %>%
    
    addCircles( data = facilities %>% filter( !impute ) ,
                group = "Actual" ,
                ~long., ~lat.  # use mix of actual and imputed values 
                , radius =  ~level ,
                weight =  2 ,
                label = ~paste( name , "(level:", level, ")", "Year:", Year )  ,
                labelOptions = labelOptions(noHide = FALSE, textOnly = TRUE, textsize = "14px") ,
                popup = ~paste( name , "(level:", level, ")", "Year:", Year )  ,
                color = ~factpal(Year)  , opacity = .7 ,
                fillColor = ~factpal(Year) , 
                fillOpacity = .75
    ) %>%
    
    addCircles( data = facilities %>% filter( impute ) ,
                group = "Imputed" ,
                ~long., ~lat.  # use mix of actual and imputed values 
                , radius =  ~level ,
                weight =  2 ,
                label = ~paste( name , "(level:", level, ")", "Year:", Year )  ,
                labelOptions = labelOptions(noHide = FALSE, textOnly = TRUE, textsize = "14px") ,
                popup = ~paste( name , "(level:", level, ")", "Year:", Year )  ,
                color = ~factpal(Year)  , opacity = .7 ,
                fillColor = ~factpal(Year) , 
                fillOpacity = .75
    ) %>%
    
    addScaleBar() %>%
    
    # addLegend(pal = factpal, values = ~level , opacity = 1 ) %>%
    addLegend( pal = factpal, 
               values = ~Year, 
               opacity = 1 ) %>%
    
    # Layers control
    addLayersControl(
        overlayGroups = c("Actual", "Imputed", 
                          'Regions' , 'Districts',
                          'Sub-districts' ,
                          "Facility count"), 
        # options = layersControlOptions(collapsed = FALSE),
        position = "topright"
    )  


orgUnitMap


```


```{r load_base_map}

base_map_file =  paste0( dhis_instance, "_base_map.rda") 

load( base_map_file )

```


```{r orgUnitMaps_by_level}

library( ggrepel )

admins_file =  paste0( dataset.directory , dhis_instance, "_admin_boundaries.rds" ) 
admins = readRDS( admins_file)

updated_facilities_file =  paste0( dataset.directory , dhis_instance, "_updated_facilities.rds" ) 
facilities = readRDS( updated_facilities_file )


library(sf)

admin.levels = unique( admins$level )[ order( unique( admins$level ) )] 

# level.maps = map( admin.levels , ~
#   st_as_sf( admins[ admins$level %in% .x , ]$polygons ,  "Spatial" ) %>%
#                     bind_cols( 
#                       admins[ admins$level %in% .x , 
#                               c(2, 4:9, 13:14 )] 
#                                )
# )

level.map1 =
    st_as_sf( admins[ admins$level %in% 1, ]$polygons ,  "Spatial" ) %>%
    bind_cols( 
        admins[ admins$level %in% 1 , 
                c(2, 4:9, 13:14 )] 
    )

level.map2 =
    st_as_sf( admins[ admins$level %in% 2 , ]$polygons ,  "Spatial" ) %>%
    bind_cols( 
        admins[ admins$level %in% 2 , 
                c(2, 4:9, 13:14 )] 
    )


level.map3 =
    st_as_sf( admins[ admins$level %in% 3 , ]$polygons ,  "Spatial" ) %>%
    bind_cols( 
        admins[ admins$level %in% 3 , 
                c(2, 4:9, 13:14 )] 
    )



bbox <- st_bbox( level.map1 ) 

# base +  geom_sf( aes( fill = name ) , alpha = .1 , data = level.map2 ) 


gg.admin = function( level.map ){
    base +
        geom_sf( aes( fill = name ) , alpha = .1 , data = level.map )  +
        
        geom_text_repel( aes( label = name , x = long, y = lat ) , 
                         data = level.map ,
                         # size = 5 ,
                         nudge_x      = 3 ,
                         direction    = "y",
                         hjust        = -1,
                         segment.size = 0.5 ,
                         segment.alpha = .3
        ) +
        
        # expand_limits( xlim = c( bbox['xmin'], bbox['xmax'] + 5 ) ) +
        
        expand_limits( x = c( bbox['xmin'],  bbox['xmax'] + 1 )) +
        
        theme_bw( base_size = 9 ) +
        theme( legend.position="none" ) 
    
    
}

gg.admin( level.map1 )
gg.admin( level.map2 )
gg.admin( level.map3 )

library( gridExtra )
grid.arrange( gg.admin( level.map2 ) ,  gg.admin( level.map3 ) , nrow = 1 )




```

```{r orgUnitMaps_by_facility}


updated_facilities_file =  paste0( dataset.directory , dhis_instance, "_updated_facilities.rds" ) 
facilities = readRDS( updated_facilities_file )

library(sf)

facilities.sf = st_as_sf( facilities %>% filter( !is.na( lat. ) & !is.na( long. ) ), 
                          coords=c('long.','lat.') , 
                          crs = st_crs( level.map1 ) ) 

chw_per_facility = facilities %>% filter( level == 5 ) %>% count(  parent.id  )


ggplot() + geom_sf( data = facilities.sf %>% filter( level == 4 ) , aes( fill = Year))

gg.admin( level.map2 ) + 
    geom_sf( data = facilities.sf %>% 
                 filter( level == 4 ) %>%
                 left_join( chw_per_facility , by = c( "id" = "parent.id") ) , 
             aes(  ) ,
             alpha = .1 ) 

# ggplot() + geom_sf( data = facilties.sf %>% filter( level == 5 ) , aes( fill = Year))




```


```{r, eval=FALSE}
# 2017 data
data.2017 = data %>%
    filter( orgUnit %in% clinics$id ) %>% 
    mutate( year = substr( period , 1, 4 )  )  %>%
    filter( year %in% 2017 ) %>%
    group_by( year ,  orgUnit, dataElement ) %>%
    summarise( value = sum( as.double(value) ) ) %>%
    left_join( select( md$organisationUnits , id , name  , path) %>%
                   mutate( level = map_int( path,
                                            ~length( gregexpr("/", .x, perl = TRUE)[[1]]) 
                   ) ),
               by = c("orgUnit" = 'id' )
    ) %>%
    ungroup %>%
    rename( HF = name, id = dataElement ) %>%
    left_join( select( md$dataElements, id, name ) ,
               by = c( 'id' ))  %>%
    rename( dataElement = name ) %>%
    select( year , level, HF, dataElement, value ) 

# numbers of HF submitting data (note, does not reflect number of months.  could be 1 or 12)
# count(data.2017, level, dataElement)
```

```{r, fig.width= 8, fig.height=10, eval=FALSE }

# select data.element to show by finding most frequent
de_freq = data  %>%
    mutate( year = substr( period, 1, 4 )) %>%
    inner_join( select( md$dataElements, id, name  ) , 
                by = c('dataElement'='id' )) %>%
    count( year, name ) %>%
    arrange( desc(year), desc(n) ) 

de_most_freq = de_freq[1, ]


d = left_join( clinics , 
               filter( data.2017, 
                       dataElement %in% de_most_freq 
                       # value > 0 
               ) %>%
                   select( year, HF, value ),  
               by = c('name' = 'HF') 
) %>%
    mutate( 
        value = ifelse( is.na( value ), 0 , value ) ,
        value.scale = ifelse( !is.na(value) & value>0 , 
                              1 + log(1 + value/ mean(value, na.rm = TRUE) ) , 
                              2 ) 
    ) %>%
    filter( abs(long) <= 180 , abs(lat) <= 180 )

# NB: add test to see if coords within country; if not, try reversing


pal = colorNumeric('OrRd', d$value.scale )
binpal <- colorBin("plasma", d$value , 5, pretty = TRUE)
factpal <- colorFactor( brewer_pal("qual", 2)(7),  d$level)

leaflet( d ) %>% 
    addTiles(  urlTemplate = 
                   "http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png"
    )  %>%
    addCircles( 
        ~long, ~lat, 
        radius =  500 ,
        weight =  ~value.scale ,
        label = ~paste( name , "(level:", level, ")" , comma( value ) )   ,
        labelOptions = labelOptions(noHide = FALSE, textOnly = TRUE, textsize = "14px") ,
        popup = ~paste( name , "(level:", level, ")" , comma( value ) ) , 
        color = ~binpal(value)  , opacity = 1 ,
        fillColor = ~binpal(value) , fillOpacity = .6
    ) %>%
    addScaleBar() %>%
    # addLegend(pal = factpal, values = ~level , opacity = 1 ) %>%
    addLegend(pal = binpal, values = ~value, opacity = 1 ) 


```

