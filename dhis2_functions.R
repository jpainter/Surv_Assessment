
# functions to get dhis data
require(tidyselect)
require( jsonlite )
require(httr)
require(assertthat)
require( progress )

# Login ####
loginDHIS2<-function( baseurl, username, password) {
  url<-paste0( baseurl, "api/me" )
  r <- GET(url, authenticate(username, password) )
  assert_that( r$status_code == 200L ) }
  
# JSON helper function ####
## gets json text from url and converts to data frame 
get = function( source_url , .print = TRUE , ...){
    
    if ( .print ) print( paste( "downloading from" , source_url , "...") )
    
    from_url = GET( source_url )
    
    if ( from_url$status_code != 200 ) return()
    
    g = fromJSON( 
        
        suppressMessages( content( from_url , "text") ) 
    )
    
    return( g )
    
}

get_resources = function( i , .pb = pb){
    
    print( paste( "Metadata item-", i , ":" , resources$plural[i] ) ) 
    
    update_progress(.pb)
    
    url.schema <- paste0( resources$href[i] , ".json?fields=:all&paging=false" )
    schema = get( url.schema  ) 
    
    if ( !is.null( schema) ) return( schema )
    
    # paging #####    
    url.page.1 <- paste0( resources$href[i] , 
                          ".json?fields=:all&page=1" )
    
    get_url = get( url.page.1  )
    
    n_pages = get_url$pager$pageCount 
    
    if ( is.null( n_pages ) ) return()
    
    if ( n_pages > 1 )  pb2 <- progress_estimated( n_pages )
    
    schema.pages = list()
    
    for ( j in 1:n_pages ){
        
        print( paste( paste( resources$plural[i] ) , ":" ,
                      "getting page" , j ) 
        )
        
        if ( !is.null( n_pages ) && n_pages > 1 )  update_progress( pb2 )
        
        url.page <- paste0( resources$href[i] , 
                            ".json?fields=:all&page=" , j )
        
        get_url = get( url.page , .print = FALSE )[2]  
        
        # try again
        if ( is.null( get_url ) ){
            origin.login()
            get_url = get( url.page )
        }   
        
        # if still null, break    
        if ( is.null( get_url ) ){ 
            print( paste( "page" , j , "is NULL" ) )
            break 
        }
        
        schema.pages[[j]] = get_url
        
    }
    
    # exit if not a list
    col = resources$plural[i] 
    
    if ( length( schema.pages ) == 0 | 
         length( schema.pages[[1]][col][[1]] )  == 0 ){
        
        return( data.frame( id = NA ) )
        
    } 
    
    if ( length(schema.pages) == 1 ){
        
        s = schema.pages[[1]]
        
    } else {
        
        # s = data.table::rbindlist( schema.pages , fill = TRUE )
        
        s = map( 1:length(schema.pages) , 
                 ~schema.pages[[.x]]
        ) 
    }
    
    
    return( s )
    # end paging ####
}

# Fetch Metadata ####
 metadataDHIS2 = function(
  baseurl ,
  element, # e.g. data_elements, indicators, osu 
  fields = 'name,id,domainType,description,displayShortName,formName,code,lastUpdated,dataElementGroups,formName,code' ,
  version  # option to specify version
 
  ){

# The whole shebang...All Metadata   ####
  if (element %in% 'all'){
    
    url<-paste0( baseurl, "api/metadata.json" )
    met = get( url ) 

    }
   
  
# Data Categories ####
   if (element %in% 'categories'){
     
     url<-paste0(baseurl,"api/categories?&paging=false")
     cgs = fromJSON(content(GET(url),"text")) 
     dataCategories = cgs[[1]] %>% as_tibble()
     return(dataCategories) 
     
     
   }

# Data Elements       ####
  ## selected data element fields
  if ( element %in% 'data_elements' ){
    
    if( !is.null(fields) ) {
    
    url<-paste0(baseurl,"api/dataElements.json?fields=", fields, "&paging=false")
    
  } else {
    
    # data element fields
    url<-paste0(baseurl,"api/dataElements.json?fields=:all&paging=false")
    
  }
    
    els = fromJSON(content(GET(url),"text"))
    
    # extract data element groups
    deg = els[[1]][, c('id', "dataElementGroups")] %>% bind_rows 
    
    # create data frame from each list element
    a = function(d){

      group_ids = unlist(d$dataElementGroups)

      data_frame(

        id = if( is.null( group_ids) ){ d$id
          } else { rep( d$id, length(group_ids)  ) } ,

        group = if( is.null( group_ids) ){ NA
          } else { group_ids }
    )
    }
    
    deg. = map( 1:nrow(deg), function(x) a(deg[x, ]) ) %>% bind_rows() %>%
      distinct()
    
    # look up names for dataElementGroups
    url<-paste0(baseurl,"api/dataElementGroups?&paging=false")
    elgs = fromJSON(content(GET(url),"text")) 
    dataElementGroupNames = elgs[[1]] %>% as_tibble() %>%
      distinct() %>%
      rename( group_id = id , groupName = displayName )
    
    # extract categoryCombos
    cat = els[[1]][, c('id', "categoryCombo")] 
    
    # create data frame from each category combo
    b = function(d){
      
      cat_ids = unlist( d$categoryCombo )
      
      data_frame(
        
        id = if( is.null( group_ids) ){ d$id
        } else { rep( d$id, length(group_ids)  ) } ,
        
        group = if( is.null( group_ids) ){ NA
        } else { group_ids }
      )
    }
    
    cat = map( 1:nrow(deg), function(x) a(deg[x, ]) ) %>% bind_rows() %>%
      distinct()
    
    # look up names for dataElementGroups
    url<-paste0(baseurl,"api/dataElementGroups?&paging=false")
    elgs = fromJSON(content(GET(url),"text")) 
    dataElementGroupNames = elgs[[1]] %>% as_tibble() %>%
      distinct() %>%
      rename( group_id = id , groupName = displayName )
    
    
    # extract atomic elements because not sure if other elements worth effort 
    els_atomic = map_lgl(els[[1]], is_atomic ) 
    els = els[[1]][, els_atomic ] 
    
    de = els %>%  as_tibble() %>% 
      left_join( deg., by = 'id' ) %>%
      left_join( dataElementGroupNames, by = c('group' = 'group_id') ) %>%
      group_by( id ) %>%
      mutate( n_groups = n() ) %>%
      ungroup

    return(de)
  
  }
  
  
# Data Element Groups ####
  if ( element %in% 'data_element_groups' ){
    
    url<-paste0(baseurl,"api/dataElementGroups?&paging=false")
    elgs = fromJSON(content(GET(url),"text")) 
    dataElementGroups = elgs[[1]] %>% as_tibble()
    return(dataElementGroups) 
    
  }
  
# Indicators  ####
  if ( element %in% 'indicators' ){

      url<-paste0(baseurl,"api/indicators?&paging=false")
      els = fromJSON(content(GET(url),"text")) 
      dataIndicators = els[[1]] %>% as_tibble()
      return(dataIndicators) 
      
  }
  
# Program Indicators  ####
  if ( element %in% 'program_indicators' ){
    
    url<-paste0(baseurl,"api/programIndicators?&paging=false")
    els = fromJSON(content(GET(url),"text")) 
    dataIndicators = els[[1]] %>% as_tibble()
    return(dataIndicators) 
    
  }
  
  
# Organisational unit (osu) ids  ####
  if (element %in% c('osu', 'orgUnits') ){
    url<-paste0(baseurl,"/api/organisationUnits.json?&paging=false")
    ous_from_server<-fromJSON(content(GET(url),"text"))
    ous = reduce( ous_from_server , bind_cols )
    return(ous)
  }
  
# geoFeatures ####
  if (element %in% 'geoFeatures'){
    
    
  # geoFeatures_from_server = fromJSON( content(GET(url),"text") ) %>% as_tibble()
    
    geoFeatures_download = function( level ){
      url<-paste0(baseurl,"api/geoFeatures.json?ou=ou:LEVEL-", level, "&paging=false")
      fromJSON( content(GET(url),"text") ) %>% as_tibble()
    }
    
    geoFeatures_from_server = map( 0:8 , geoFeatures_download )
    geoFeatures = reduce(geoFeatures_from_server, bind_rows)
    
    # glimpse(geoFeatures)
    
    # remove potential duplicates
    geoFeatures = geoFeatures[ !is.na(geoFeatures$id) ,]
    # geoFeatures = geoFeatures[ !duplicated(geoFeatures) ,]
    
    return(geoFeatures)
  }
  
  ## for description of properties, see table 1.59, 
  ## https://docs.dhis2.org/2.22/en/developer/html/ch01s32.html
  
# geoJson  #####
  if (element %in% 'geojson'){

  #   url<-paste0(baseurl,"api/organisationUnits.geojson?level=",level,"&paging=false")
  #   geojson_from_server = map(2:3, fromJSON( content(GET(url),"text") ) )
  #   geojson = reduce(geojson_from_server, bind_rows)
  # }
    
    geojson_download = function( level ){
      url<-paste0(baseurl,"api/geoFeatures.json?ou=ou:LEVEL-", level, "&paging=false")
      fromJSON( content(GET(url),"text") ) %>% as_tibble()
    }
    
    geojson_from_server = map( 1:5, geojson_download )
    geojson = reduce(geojson_from_server, bind_rows)
    
    # remove potential duplicates
    geojson = geojson[!duplicated(geojson),]
    
    return(geojson)
  }

 }
  
  # TEST / NOT RUN
  # # try kenya..
  # baseurl<-"https://hiskenya.org/"
  # username<-"abuff"
  # password<-"Malaria2012"
  # loginDHIS2(baseurl,username,password)
  # 
  # # try local docker..
  # baseurl<-"http://localhost:8085/"
  # username<-"admin"
  # password<-"district"
  # loginDHIS2(baseurl,username,password)
  # 
  # orgUnits = metadataDHIS2( baseurl, 'osu' )
  # View(orgUnits)
  # 
  # 
  # 
  # # APPS from appstore
  # url<-paste0(baseurl,"api/appStore")
  # apps = fromJSON( content(GET(url),"text") ) 
  # 
  # # Metadata repo
  # url<-paste0(baseurl,"api/synchronization/metadataRepo")
  # metadata = fromJSON( content(GET(url),"text") ) 
  # 
  # # System info
  # url<-paste0(baseurl,"api/system/info")
  # systemInfo = fromJSON( content(GET(url),"text") ) 
  
 ## Correct coordinate text to have balanced brackets
 fix_coordinate_brackets = function( coordinates ){
     
     fix_coordinates = gsub( "(?<=[0-9])\\]\\]," , "\\]\\]\\]," , 
                         coordinates , 
                         perl = TRUE ) %>% 
                    gsub( ",\\[\\[(?=[-+]?[0-9])" , ",\\[\\[\\[" ,
                            . , 
                            perl = TRUE )
     
     return( fix_coordinates )
 }
 
 ## Convert coordinates to SpatialPolygons
 coords_to_spatialPolygons = function( coordinates ){
     
     fix_coordinates = fix_coordinate_brackets( coordinates )
     
     # convert to geoJson looking text
     str_js = paste( '{ "type": "MultiPolygon", "coordinates": ' ,
                     fix_coordinates , ' }' ) 
     
     char_js = map( str_js, ~ FROM_GeoJson( .x ) ) 
     
     polys = map( 1:length(char_js) , 
                  ~map( .x , 
                        ~map( char_js.[[.x]]$coordinates , Polygon ) 
                  )[[1]]
     )
     
     pgons = map( 1:length(polys),  ~ Polygons( polys[[.x]]  , as.character(.x)  ) )
     spoly = SpatialPolygons( pgons )
     proj4string( spoly ) = CRS("+proj=longlat +datum=WGS84")
     
     return( spoly )
     plot(spoly)
 }
 
 
 is.in.parent = function( clinic.id , parent.id , plot = FALSE , fix = FALSE , .pb = NULL){
     
     update_progress(.pb) 
     
     long  = clinics$long[ clinics$id %in% clinic.id ]
     lat  =  clinics$lat[ clinics$id %in% clinic.id ]
     
     if ( is.na(long) | is.na(lat) ) return( FALSE )
     
     parent.polygon = admins$polygons[ admins$id %in% parent.id ]
     
     if ( length( parent.polygon@polygons )== 0 ) return( FALSE )
     
     # flat projection
     localCRS = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
     
     # earth projection
     standardCRS = "+proj=longlat +datum=WGS84"
     
     parent.polygon <- spTransform( parent.polygon, 
                                    CRS( localCRS )
     ) 
     # 10 km buffer
     parent.polygon = rgeos::gBuffer(parent.polygon , width = 10000) 
     
     # if no decimal place, numbers are huge..
     if ( abs(long) > 180 ) return( FALSE )
     if ( abs(lat) > 180 ) return( FALSE )
     
     clinic.coords = c(  long , lat )
     
     clinic.coords.matrix = matrix(clinic.coords, nrow = 1 )
     
     clinic.spatialPoint = SpatialPoints( clinic.coords.matrix  , 
                                          proj4string = CRS( standardCRS  )
     )
     
     clinic.spatialPoint  = spTransform( clinic.spatialPoint , CRS( localCRS ) ) 
     
     if ( plot ){ 
         plot( parent.polygon )
         points(clinic.spatialPoint, col = 'red' )
     }
     
     is.in = sp::over( clinic.spatialPoint ,  parent.polygon )
     
     is.in = ifelse( is.na( is.in ) , FALSE, TRUE )
     
     # Potential fixes
     ## reverse lat-long
     if (is.in == FALSE & fix ){
         
         clinic.coords = c( lat , long ) # reverse
         
         clinic.coords.matrix = matrix(clinic.coords, nrow = 1 )
         
         clinic.spatialPoint = SpatialPoints( clinic.coords.matrix  , 
                                              proj4string = CRS( standardCRS  )
         ) %>%
             spTransform( . , CRS( localCRS ) ) 
         
         is.in = sp::over( clinic.spatialPoint ,  parent.polygon )
         
         is.in = ifelse( is.na( is.in ) , FALSE, TRUE )
         
         if ( is.in ) return( is.in )
         
     }
     
     return( is.in )
 }
 

 impute.location = function( parent.id , plot = FALSE  ){
     
     parent.polygon = admins$polygons[ admins$id %in% parent.id ]
     
     if ( length( parent.polygon@polygons ) == 0 ) return( list( NA , NA )   )
     
     clinic.spatialPoint = spsample( parent.polygon , 1 , type = "random", iter = 10 ) 
     
     if ( plot ){ 
         plot( parent.polygon )
         points(clinic.spatialPoint, col = 'red' )
     }
     
     long = clinic.spatialPoint@coords[1]
     lat = clinic.spatialPoint@coords[2]
     
     return( list( long, lat ) )
 }
 
 
 renderInventory <- function(source, dir = '../HMIS/DHIS2/') {
   source = tolower( source )
   rmarkdown::render(
     paste0( dir, "DHIS2_Inventory.Rmd"), 
     params = list(
       login_file = source
   ))
 }
 
 
 # Retry function to use when querying database
 # borrowed from: https://stackoverflow.com/questions/20770497/how-to-retry-a-statement-on-error
 
 library(futile.logger)
 library(utils)
 
 retry <- function(expr, isError=function(x) "try-error" %in% class(x), 
                   maxErrors=5, sleep=0) {
     attempts = 0
     retval = try( eval(expr) )
     
     while ( isError(retval) ) {
         attempts = attempts + 1
         
         if (attempts >= maxErrors) {
             msg = sprintf("retry: too many retries [[%s]]", capture.output(str(retval)))
             flog.fatal(msg)
             stop(msg)
             
         } else {
             msg = sprintf("retry: error in attempt %i/%i [[%s]]", attempts, maxErrors, 
                           capture.output(str(retval)))
             # flog.error(msg)
             # warning(msg)
         }
         
         if (sleep > 0) Sys.sleep(sleep)
         
         retval = try(eval(expr))
     }
     return(retval)
 }
 
 # TEST
 # renderInventory('uganda')

# organisationalUnits
 
 feature_type = function( coordinate ){
     n = length( gregexpr( '[' , coordinate , fixed = TRUE)[[1]] )
     
     if ( is.na( coordinate ) ) return(NA)
     if (n==1) return('Point') 
     if (n>1) return('Polygon') 
     
     
 }
 
 parse_parent_ous = function( path ){
     breaks = gregexpr("/", path , perl = TRUE)[[1]]
     n = length( breaks ) 
     if ( n == 0 ) return( NA ) 
     if ( n == 1 ) return( substr( path , breaks[1] + 1 , length(path) ) ) 
     substr( path , breaks[n-1] + 1 , breaks[n]-1 )
 }
 

# API Data Calls ####
 
 # Function to create string of dates. 
 # Default is for every month during last five years 
 
 date_code = function( 
     years = NULL , 
     months = NULL ){
     
     if ( is.null( months ) )  months = 1:12
     
     if ( is.null( years ) ){
         
         this.year = year( Sys.Date() )
         FiveYrsPrevious = this.year - 4
         
         years = FiveYrsPrevious:this.year
     }
     
     period = character()
     
     for ( year in years ){
         for (month in seq_along(months)){
             this_period = paste0( ";" , 
                                   year , 
                                   ifelse( month < 10 , paste0("0", month) , month )
             )
             period =  c(period, this_period )
         }
     }
     
     # remove first ;
     period =  paste( period, collapse = "")
     period = substring( period, 2, nchar(period))
     
     return( period )
 }
 
 # api_data. Gets data from server. 
 
 api_data = function( periods = NA , 
                      levels = NA , 
                      de.vars = NA , # a data.frame like _key_data_elements.rds
                      file = "" ,
                      dsde = NULL , 
                      details = FALSE , 
                      submissions =  FALSE 
 ){
     
     if ( is.null( file ) ){
         
         cat("Need to give name of file where data will be saved")
         return()
         
     }
     

     if ( file.exists( file ) ) existing.data = read_rds( file ) %>% as.tibble()
     

     if ( all( is.na( periods )  ) ){
         
         periods = strsplit( date_code(), ";" , fixed = TRUE )[[1]]
         
     } 
 
     ##### Set list of elements to ask for
     # if not vars selected, get list from last data totals
     
     if ( nrow( de.vars ) == 0 ){
         
         # if it exists, get list of data elements from totals file
         if ( file.exists( file ) ){
             
             dataElements = readRDS( file ) %>% 
                 count( dataElement ) %>%
                 inner_join( md$dataElements %>% select(id, name) ,
                             by = c("dataElement" = "id") ) %>%
                 .$name

            } else { return }

     } else {
         
         dataElements = de.vars 
     }
     
     # dataElement.ids =  de.vars$dataElement.id
     # dataElement.names = de.vars$dataElement
       
     
     if ( submissions ){ # substitute dataSet associated with dataElement
         
         stopifnot( !is.null( dsde) )
         
         # get datasets associated with data_totals dataElements
         dataElements =  data_totals %>% 
             # link datasets
             inner_join( dsde , by = "dataElement" 
             ) %>%
             count( dataSet ) %>% 
             # convert ids to names
             rename( id = dataSet ) %>%
             left_join( md$dataSets %>% select( name, id ), 
                        by = "id" 
             ) 
     }
     
     stopifnot( origin.login()  )
     
     print( baseurl )
     
     ##### cycle through each period, each data element...
     
     ndei = nrow( dataElements ) * length( periods )
     pb <- progress_estimated( ndei )
     
     data = list()
     
     # TODO: initialize with expected size: e.g.
     data  = vector(mode = "list", 
                    length = length( periods ) )
     
     for ( period in seq_along( periods ) ){

         data.de = list()
         
         # todo: allocate size of list :
         data.de = vector(mode = "list", 
                          length = length( dataElements$id )
                          )
         
         for ( element in  seq_along( dataElements$id ) ){
             
             update_progress(pb) 
    
             # if dataElement in same period already exists...
             if ( exists( "existing.data" ) ){
                 
                 in.period =existing.data$period %in% periods[ period ] 
                 in.element = existing.data$dataElement %in% dataElements$id[ element ] 
 
                 existing.value = existing.data[ in.period & in.element , ]
                 
                 if ( nrow( existing.value ) > 0  ){
                     
                     # use previously downloaded data, then go to next
                     data.de[[ element ]] = existing.value
                     next()
                 }
             }
             
             
             de.ids = dataElements$id[ element ]
             
             if ( submissions ){
                 
                 reports = c( 'ACTUAL_REPORTS', 'ACTUAL_REPORTS_ON_TIME', 'EXPECTED_REPORTS' )
                 # get ids
                 de.ids = dataElements[ element , ]$id 
                 
                 # concatenate with report types
                 de.ids = paste0( de.ids, ".", reports , collapse = ';')
             }
             
             
             if ( details ){
                 
                 de.index = which( md$dataElements$id %in% dataElements$id[ element ] )
                 
                 # data.frame of dataElement-id and categorycomb0-id
                 de.catCombo = data_frame( 
                     dataElement = md$dataElements$id[ de.index ] ,
                     dataElement.name = md$dataElements$name[ de.index ] ,
                     categoryCombo = md$dataElements$categoryCombo$id[ de.index ] 
                 )
                 
                 # CategoryOptions for each categoryCombo
                 catOptCombos =  data_frame( 
                     categoryOptionCombo = md$categoryOptionCombos$id ,
                     categoryOptionCombo.name = md$categoryOptionCombos$name ,
                     categoryCombo = md$categoryOptionCombos$categoryCombo$id
                 )
                 
                 de.catOptCombo = de.catCombo %>% 
                     inner_join( catOptCombos , by = "categoryCombo")
                 
                 # string to paste in to data request    
                 de.ids = paste( paste0( de.catOptCombo$dataElement, "." , 
                                                  de.catOptCombo$categoryOptionCombo) ,
                                          collapse  = ";" )
                 
                 print( paste( periods[ period ], "Element" , element ,
                               "/" , length( dataElements$id ) ,
                               ":" , dataElements$name[ element ],
                               ":" , nrow( de.catOptCombo ) , "categories" 
                 )
                 )
                 
             }
 
                 
            print( paste( periods[ period ], "Element" , element ,
                               "/" , length( dataElements$id ) ,
                               ":" , dataElements$name[ element ])
                 )

             
             data.level = list()
             
             for ( level in seq_along( levels ) ){
                 
                 # If no value for level 1, skip other levels
                 if ( level > 1 && !is.data.frame( fetch ) ) next()
                 
                 # print( paste( levels[level] , ifelse( details, "Details", "") ) )
                 
                 #Assemble the URL ( before starting, double check semicolons for dx dimension )
                 url <- paste0( baseurl, "api/analytics/dataValueSet.json?" ,
                                
                                "&dimension=ou:", levels[level] , 
                                
                                "&dimension=pe:" , periods[period] ,
                                
                                "&dimension=dx:" , 
                                
                                # malaria
                                de.ids ,
                                
                                "&displayProperty=NAME")
                 
                 # print( url )
                 
                 print( paste( "Level:", level , " ") )
                 
                 
                 # Fetch data
                 fetch <- retry( get(url, .print = FALSE )[[1]] ) # if time-out or other error, will retry 
                 
                 # if returns a data frame of values (e.g. not 'server error'), then keep
                 if ( is.data.frame( fetch ) ){ 
                     
                     data.level[[ level ]] = fetch %>% 
                         select( -storedBy, -created, -lastUpdated, -comment ) %>%
                         mutate( 
                             level = str_sub( level , -1 ) %>% as.integer() 
                         )
                     
                     print( paste( nrow(fetch), "records." ) )
                 
                     } else {
                         
                     # print( "no records" )
                 }
             }
             
             data.de[[ element ]] = data.table::rbindlist( data.level, fill = TRUE )
             
             # print( paste( dataElements[ element ]  , "has" , 
                           # scales::comma( nrow( data.de[[ element ]] ) ) , 
                           # "records"  ) ) 
             
         }
         
         # combine data
         data[[ period ]] = data.table::rbindlist( data.de , fill = TRUE )
         
         print( paste( "...Period" , periods[period]  , "has", 
                       scales::comma( nrow( data[[period]] ) ) , 
                       "records."  ) )
         
     }
     
     # combine period data
     d = data.table::rbindlist( data , fill = TRUE)
     
     print( paste( "TOTAL", 
                   scales::comma( nrow( d ) ), 
                   "records"  ) )
     
     if (!exists( "existing.data") )  existing.data = d[0, ]
     
     data = bind_rows( 
         existing.data %>% filter( !is.na(value) )
         , d )
     
     return( d )
 }
 
 
 
 
 api_last12months_national_data = function( 
     periods = "LAST_YEAR" , 
     levels = "LEVEL-1" , 
     aggregationType = 'COUNT' , 
     de.include = de.include ,
     submissions =  FALSE,
     details = FALSE ,
     file = reported_data_file
     
 ){
     
     stopifnot( origin.login()  )
     
     if ( file.exists( file ) ) existing.data = read_rds( file )
     
     dataElement.ids =  de.include %>% .$id 
     dataElement.names = de.include %>% .$name
     
     
     if ( submissions ){ # substitute dataSet associated with dataElement
         
         # get datasets associated with data_totals dataElements
         dataElements =  data_totals %>% 
             # link datasets
             inner_join( dsde , by = "dataElement" 
             ) %>%
             count( dataSet ) %>% 
             # convert ids to names
             rename( id = dataSet ) %>%
             left_join( md$dataSets %>% select( name, id ), 
                        by = "id" 
             ) %>%
             .$name
         
         
     }
     
     # print( baseurl )
     # print( paste( "details:" , details ) )
     # print( paste( "submissions:" , submissions ) )
     
     data = list()
     
     data.de = list()
     
     # pb <- progress_bar$new(
     #     format = " downloading [:bar] :percent eta: :eta",
     #     total = nrow( de.include ), 
     #     clear = FALSE, width= 60
     # )
     
     ndei = nrow( de.include )
     pb <- progress_estimated( ndei )

     
     for ( element in  seq_along( dataElement.ids ) ){
         
         # pb$tick()
         
         update_progress(pb) 
         
         # de.name = paste(
         #     md$dataElements %>%
         #         select( id, name ) %>%
         #         filter( trimws(name) %in% dataElements[ element ] ) %>%
         #         .$id ,
         #     collapse  = ";" )
         
         
         # print( paste( periods, "Element" , element ,
         # "/" , length( dataElement.ids ) ,
         # ":" , dataElement.names[ element ] ) 
         # )
         
         data.level = list()
         for ( level in seq_along( levels ) ){
             
             # If no value for level 1, skip other levels
             if ( level > 1 && !is.data.frame( fetch ) ) next()
             
             # print( paste( levels[level] , ifelse( details, "Details", "") ) )
             
             #Assemble the URL ( before starting, double check semicolons for dx dimension )
             url <- paste0( baseurl, 
                            "api/analytics/dataValueSet.json?" ,
                            # "api/analytics/dataValueSet.json?" ,
                            "&dimension=ou:", levels[level] , 
                            "&dimension=pe:" , periods ,
                            "&dimension=dx:" , dataElement.ids[ element ] ,
                            "&displayProperty=NAME",
                            "&aggregationType=" , aggregationType )
             
             # print( url )
             
             # skip if already exists
             if ( exists( "existing.data" ) ){
                 
                  existing.value = existing.data %>%
                      
                      filter( 
                          
                          dataElement %in% dataElement.ids[ element ] 
                          ) 
                  
                 if ( nrow( existing.value ) > 0  ) next()
             }
             
             # Fetch data
             fetch <- retry( get(url, .print = FALSE)[[1]] ) # if time-out or other error, will retry 
             
             # if returns a data frame of values (e.g. not 'server error'), then keep
             if ( is.data.frame( fetch ) ){ 
                 
                 data.level[[ level ]] = fetch %>% 
                     select( -storedBy, -created, -lastUpdated, -comment )
                 
                 # print( paste( periods, ":" , nrow(fetch), "records." ) )
                 
             } else {
                 data.level[[ level ]] = data_frame( 
                     dataElement = dataElement.ids[ element ] ,
                     period =  periods ,
                     orgUnit =  levels[level] ,
                     value = NA
                 )
                 print( "no records" )
             }
             
         }
         
         data.de[[ element ]] = data.table::rbindlist( data.level, fill = TRUE )
         
         print( paste( dataElement.names[ element ]  , "has" , 
                       scales::comma( nrow( data.de[[ element ]] ) ) , 
                       "records"  ) ) 
         
     }
     
     # combine data
     data = data.table::rbindlist( data.de , fill = TRUE )
     
     if (!exists( "existing.data") )  existing.data = data[0, ]
     
     data = bind_rows( 
         existing.data %>% filter( !is.na(value) )
         , data )
     
     return( data )
 }

 # parse_parent_ous: Get Parent OUS/ path ####
 # NB: must run with rowwise() or else lower levels return NA (why?)
 # probably because gregexpr works on whole column so n is always the longest value
 parse_parent_ous = function( path ){
     breaks = gregexpr("/", path , perl = TRUE)[[1]]
     n = length( breaks ) 
     if ( n == 0 ) return( NA ) 
     if ( n == 1 ) return( substr( path , breaks[1] + 1 , length(path) ) ) 
     substr( path , breaks[n-1] + 1 , breaks[n]-1 )
 }
 

 # Determine which facilities reporting during previox xx intervals ####
 
 continuous = function( data = submission , months = 24  
                        ){
     
     periods = count( data, period) %>% .$period 
     
     # convert to date
     periods = fast_strptime( periods  , "%Y%m") 
     
     max_period = max(periods, na.rm = TRUE)
     
     first_month_in_interval = max_period %m-% months( months )
     
     continuous.interval = interval( first_month_in_interval , max_period ) 
     
     available.interval = interval( min(periods, na.rm = TRUE) , 
                                    max(periods, na.rm = TRUE) ) 
     
     n_periods = time_length( available.interval , 'months')
     
     
     if (n_periods < months){
         print( paste(
             'There are only', n_periods, 'intervals; the function will test for contiunous submission for up to', n_periods, 'intervals.' 
         ) )
         
         continuous.interval = available.interval 
         
     }
     
     
     s = data %>%
         
         # Remove any rows with all missing data
         select( -starts_with( 'NA' ) ) %>%
         filter( complete.cases(.) ) %>%
         
         mutate( dates = fast_strptime( as.character( period ) , "%Y%m") %>%
                     as.POSIXct()
         ) %>%
         filter( 
             dates %within% continuous.interval
         ) %>% 
         group_by(
             dataElement , orgUnit 
         ) %>%
         summarise( 
             n = n() , 
             continuous = n() >= months
         ) 
     
     
     # mutate( 
     #     all = factor( continuous  ,
     #                   levels = c( TRUE, FALSE ) ,
     #                   labels = c("Clinics continuosly reporting" , 
     #             'Other clinics' )
     #             )
     # )
     
     
     return(s)
 }
 
 # Datasets: html table of features
 dataset.ous.n = function( dataset, ous ){
     
     a = md$dataSets[ md$dataSets$name %in%  dataset  ,
                      c( 'name', "organisationUnits" ) ] %>% 
         rename( dataset = name ) 
     
     # If no orgUnits assigned, do not unnest 
     if ( nrow(a$organisationUnits[[1]]) == 0 ){ 
         a = a %>% mutate( id = as.character( NA ) )
         
     } else { 
         a = unnest( a )
     }
     
     b = a %>%
         left_join( ous %>% select( id, orgUnit.name , level ) , 
                    by = "id" )  %>%
         # level names
         left_join( 
             select( md$organisationUnitLevels, level, name  ), 
             by = 'level' ) %>%
         rename( levelName = name ) 
     
     
     
     t = count( b, level, levelName, dataset ) %>% 
         spread( dataset, n ) %>%
         # rename( Unassigned = `<NA>` ) %>%
         kable( "html", caption = dataset ) %>%
         kable_styling(bootstrap_options = c("striped", "hover"))%>%
         column_spec(1, bold = T) 
     
     return(t)
 }
 
 
