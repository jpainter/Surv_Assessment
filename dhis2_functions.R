# functions to get dhis data

# Login ####
loginDHIS2<-function( baseurl, username, password) {
  url<-paste0( baseurl, "api/me" )
  r <- GET(url, authenticate(username,password) )
  assert_that( r$status_code == 200L ) }
  
# JSON helper function ####
## gets json text from url and converts to data frame 
get = function( source_url , ...){
    
    g = fromJSON( suppressMessages(
        content( GET( source_url ), "text") ) 
    )
    
    return( g )
    
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
 
# api_data #####
 api_data = function( periods = periods.vector , 
                      levels = levels.vector , 
                      de.vars = de.include ,
                      file = "" ,
                      details = FALSE , 
                      submissions =  FALSE
                      
 ){
     
     if ( file.exists( file ) ){
         
         data.file = readRDS( file ) 
         
         # select periods not in file
         
         # select level not in file 
         
         # select vars not in file 
         
     }
     
     # Set list of elements to ask for
     dataElements = de.vars %>% .$dataElement
       
     
     if ( submissions ){ # substitute dataSet associated with dataElement
         
         # get datasets associated with data_totals dataElements
         dataElements =  data_totals %>% 
             # link datasets
             inner_join( dsde , by = "dataElement" 
             ) %>%
             count( dataSet ) %>% .$dataSet %>%
             # convert ids to names
             rename( id = dataSet ) %>%
             left_join( md$dataSets %>% select( name, id ), 
                        by = "id" 
             ) %>%
             rename( dataSet =  name )
     }
     
     
     login_origin()
     
     data = list()
     for ( period in seq_along( periods.vector ) ){
         
         print( paste( periods.vector[period] ) )
         
         data.de = list()
         for ( element in  seq_along( dataElements ) ){
             
             # data elements:
             if ( details == TRUE & submissions == TRUE ) return()
             if ( details == FALSE & submissions == FALSE ){
                 
             de.ids = paste( 
                 md$dataElements %>% 
                     select( id, name ) %>%
                     filter( trimws(name) %in% de.vars$dataElement[ element ] ) %>%
                     .$id ,
                 collapse  = ";" )
             }
             
             if (details){
                 
                 de.index = which( md$dataElements$id %in% de.ids )
                 
                 # data.frame of dataElement-id and categorycomb0-id
                 de.catCombo = data_frame( 
                     dataElement = md$dataElements$id[ de.index ] ,
                     dataElement.name = md$dataElements$name[ de.index ] ,
                     categoryCombo = md$dataElements$categoryCombo$id[ de.index ] 
                 )
                 
                 # CategoryOptions for each categoryCombo
                 catOptCombos =  data_frame( 
                     categoryOptionCombo = md$categoryOptionCombos$id ,
                     categoryCombo = md$categoryOptionCombos$categoryCombo$id
                 )
                 
                 de.catOptCombo = de.catCombo %>% 
                     inner_join( catOptCombos , by = "categoryCombo")
                 
                 # string to paste in to data request    
                 de.ids = paste( paste0( de.catOptCombo$dataElement, "." , 
                                         de.catOptCombo$categoryOptionCombo) ,
                                 collapse  = ";" )
             } 
             
             if ( submissions ){
                 
                 reports = c( 'ACTUAL_REPORTS', 'ACTUAL_REPORTS_ON_TIME', 'EXPECTED_REPORTS' )
                 de.ids = paste0( dataElements[ element ], ".", reports , collapse = ';')
             }
             
             print( paste( periods[ period ], "Element" , element ,
                           "/" , length( dataElements ) ,
                           ":" , dataElements[ element ] ) 
             )
             
             data.level = list()
             for ( level in seq_along( levels ) ){
                 
                 # If no value for level 1, skip other levels
                 if ( level > 1 & !is.data.frame( fetch ) ) next()
                 
                 print( levels[level] )
                 
                 #Assemble the URL ( before starting, double check semicolons for dx dimension )
                 url <- paste0( baseurl, "api/analytics/dataValueSet.json?" ,
                                "&dimension=ou:", levels[level] , 
                                "&dimension=pe:" , periods[period] ,
                                "&dimension=dx:" , 
                                
                                # malaria
                                de.ids ,
                                # opd summary
                                # "KNrK5VWTZkx;NLKRV7bYbVy" , # population
                                "&displayProperty=NAME")
                 
                 # Fetch data
                 fetch <- retry( get(url)[[1]] ) # if time-out or other error, will retry 
                 
                 # if returns a data frame of values (e.g. not 'server error'), then keep
                 if ( is.data.frame( fetch ) ){ 
                     
                     data.level[[level]] = fetch %>% 
                         select( -storedBy, -created, -lastUpdated, -comment )
                     
                     print( paste( nrow(fetch), "records. " ) )
                 }
             }
             
             data.de[[ element ]] = data.table::rbindlist( data.level, fill = TRUE )
             
             print( paste( de.vars$dataElement[ element ]  , "has" , 
                           scales::comma( nrow( data.de[[ element ]] ) ) , 
                           "records"  ) ) 
             
         }
         
         # combine data
         data[[ period ]] = data.table::rbindlist( data.de , fill = TRUE )
         
         print( paste( "Period has", 
                       scales::comma( nrow( data[[period]] ) ) , 
                       "records"  ) )
         
     }
     
     # combine period data
     d = data.table::rbindlist( data , fill = TRUE)
     
     print( paste( "TOTAL", 
                   scales::comma( nrow( d ) ), 
                   "records"  ) )
     
     return( d )
 }
