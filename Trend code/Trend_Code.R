# Trend_Code.R


library( lubridate )
library(forecast)
library(forecast)
library(seasonal)
library(stlplus)
library(zoo)
library( tsibble )
library(knitrProgressBar)
library( tidyverse )

dataset.translated = function( dataset_detail = NULL, .ous = NULL, .meta = NULL ){
    
    stopifnot( !is.null( dataset_detail ) & !is.null(.meta) )
    
    stopifnot( 'categoryOptionCombo' %in% names( dataset_detail ) )
    
    if ( is.null( ous ) ){
        
        if( !is.null( .meta ) ){ 
            
            ous = ous.translated( .meta , open.only = FALSE) 
            
        } else {
            print( "Function dataset.translated() stopped. Need to provide ous or meta")
        }
    }
    
    idname = c('id', 'name')  # shortcut for fetching these two columns
    
    # remove levels from dataset so that it doesn't conflict
    if ( "level" %in% names( dataset_detail )   ) {
        
        dataset_detail = dataset_detail %>% dplyr::select( -level )
    }
    
    d = dataset_detail %>% 
        inner_join( .meta$dataElements[ , idname ], by = c('dataElement' = 'id' ) ) %>%
        rename( dataElement.name = name ) %>%
        inner_join( .meta$categoryOptionCombos[ , idname ], by = c('categoryOptionCombo' = 'id' ) ) %>%
        rename( categoryOptionCombo.name = name ) %>%
        inner_join( ous, by = 'orgUnit' ) %>%
        separate( period, into = c("year", "month") , sep = 4 , remove = FALSE ) %>%
        mutate_at( c("year", "month") , as.integer ) %>%
        mutate(
            date = as_date( ymd(paste(year, month, 15, sep = "-")) )
        ) %>%
        as.tibble()
}

ous.translated = function(  .meta = NULL, 
                            open.only = TRUE  # limit to clinics currently open, only
                            ){
    
    # NB:  children missing  from some metadata (eg. Malawi )
    
    stopifnot( !is.null( .meta ) )
    
  
     meta_cols = c( 'id', 'name', 'openingDate', 'closedDate', 'path', 'coordinates' 
                    # , 'children'
                    )
    
    .meta$organisationUnits[ , meta_cols]  %>% 
        
        as.tibble() %>% 
        
        filter( if ( open.only ){ is.na(closedDate) } else { TRUE }  ) %>%  
        
        mutate(
            feature = map_chr( coordinates, ~feature_type(.x ) ) 
            # , children = map_dbl( children, ~length(.) )
        ) %>%

        rowwise() %>%
        mutate( 
            level = str_count( path , "/") , 
            parent_ou = parse_parent_ous(path) 
                
                ) %>% 
        
        left_join( .meta$organisationUnitLevels[ , c('level', 'name')] %>% 
                       rename( level.name = name ) ,
                   by = 'level' ) %>%
        
        left_join( .meta$organisationUnits[ , c('id', 'name')] 
                   %>% rename( parent_ou.name = name ) ,
                   by = c( 'parent_ou' = 'id' )
                   ) %>%
        
        ungroup() %>% 
        
        # select( -path , -closedDate ) %>%
        
        rename( orgUnit = id, orgUnit.name = name )
    
    
}

ous.random = function( .level){  #samples OU id within OU level
    
    ous_within_level = ous %>% filter( level %in% .level )
    
    random_row_within_level = sample( 1:nrow( ous_within_level ) , 1 )
    
    ous_within_level[ random_row_within_level , ]
}

# TIME-Series  ####

ts.df = function( data , start = "201401" , end = NA , .pb = NULL ){
    
    # progress bar
    update_progress( .pb ) 
    
    # create time-series for selcted OU and dataElement (total: need to sum over category combo)
    d_ou_de = data
    
    if ( nrow( d_ou_de ) == 0) return( ts() )
    
    values = d_ou_de$value
    
    if ( is.na( end ) ) end = as.yearmon( Sys.Date() , "%Y%m" )
    
    if ( is.na( start ) )  start = d_ou_de$period[1] 
    
    start = start %>% as.yearmon(. , "%Y%m")
    
    
    seq_dates <- seq.Date( as.Date( start ) , as.Date( end ) , by = "month") %>%
      as.yearmon( . , "%Y%m" ) 
    
    
    dataset.all.dates = data_frame( period = seq_dates ) %>%
      left_join( 
        data_frame( value = values , 
                    period = d_ou_de$period %>% as.yearmon(. , "%Y%m")
                    ) ,
        by = 'period'
                 )
    
    ts_ou_de = ts( dataset.all.dates$value , 
                   start = dataset.all.dates$period[1] , 
                   end = dataset.all.dates$period[ nrow( dataset.all.dates ) ] ,
                   frequency = 12 
    )
    
    return( ts_ou_de )
}

ts.ou = function( data , start_year = NA ){
    
    # create time-series for selcted OU and dataElement (total: need to sum over category combo)
    d_ou_de = unnest( data ) %>% select( period, value )
    
    if ( nrow( d_ou_de) == 0) return( ts() )
    
    if ( !is.na(start_year) ){
        d_ou_de = d_ou_de %>% filter( sub_str( period, 1, 4 ) >= start_year )
    }
    
    values = d_ou_de$value
    periods = d_ou_de %>% 
        mutate( 
            year = substr( period, 0 , 4 ) %>% as.integer() ,
            month = substr( period, 5, 6 ) %>% as.integer()
            )
    
    ts_ou_de = ts( values , 
                   start = c( periods[1,] %>% .$year , periods[1,] %>% .$month  ) , 
                   end = c( periods[nrow(periods),] %>% .$year , periods[nrow(periods),] %>% .$month  ) ,
                   frequency = 12 
    )
    
    return( ts_ou_de )
}


ts.ou.de = function(data , ous , de , start_year = NA ){
    
    # create time-series for selcted OU and dataElement (total: need to sum over category combo)
    d_ou_de = data %>% 
        filter( orgUnit %in% ous , dataElement %in% de ) %>%
        group_by( dataElement, period, year, month, date, orgUnit ) %>%
        summarise( value = sum( as.integer(value) , na.rm = TRUE ) ) %>%
        ungroup 
    
    if (nrow( d_ou_de) == 0) return( ts() )
    
    if ( !is.na(start_year) ){
        d_ou_de = d_ou_de %>% filter( year >= start_year )
    }
    
    # Expand this ou's dataset to include all months available in the overall dataset
    periods = tibble(
        year = d_ou_de$year , 
        month = d_ou_de$month ,
        date = ymd(paste(d_ou_de$year, d_ou_de$month, 15, sep = "-")) 
    ) %>% 
        expand( year, month ) %>%
        arrange( year, month )
    
    d_ou_de = d_ou_de %>% 
        right_join( periods , by = c("year", "month") ) 
    
    ts_ou_de = ts( d_ou_de$value , 
                            start = c( periods[1,] %>% .$year , periods[1,] %>% .$month  ) , 
                            end = c( periods[nrow(periods),] %>% .$year , periods[nrow(periods),] %>% .$month  ) ,
                            frequency = 12 
    )
    
    return( ts_ou_de )
}

ts_to_df = function( ts ){
  
  # starts with time-series object, return data frame with
  # - date , and 
  # - Y 
  ts %>%
  data.frame(
    date = as.Date( time( . ) ), 
    Y = as.matrix( . )
  )
}

# Decompose functions ####
decompose.stl = function( df , year = NULL ,
                          transform = ""
                          , plot = TRUE , .coe = TRUE , title = "" , 
                          .pb = NULL ,
                          ...
                          ){
    # progress bar
    update_progress( .pb ) 
    
    if (!is.null(year) ){
        ts = window( df, year )
    } else { 
        ts = df 
    }
    
    
    if ( sum(!is.na( ts )) < 27 ) return(NA)

    
    if ( transform %in% "log" ){
        ts[ ts == 0 ] = 1
        ts = log( ts )
    } 
    
    # STL decompose and coefficient of variation
    stl_model = try( stlplus( ts, s.window = 'periodic', n.p = 12  ) )
    
    if ( class(stl_model) %in% "try-error" ) return( NA )
    
    # if (plot){
    #     print( plot( stl_model) ) 
    #     # glimpse( stl_df$data )
    # }

    raw = ts 
    stl_trend = trend( stl_model ) 
    stl_seasonal = seasonal( stl_model )
    rem = remainder( stl_model )
    rem_mean = mean( rem, na.rm = TRUE )
    rem_sd = sd( rem , na.rm = TRUE ) 
    cv = rem_sd / rem_mean
    # cv
    
    # coeficient of error?  ration of remainder to projected (seasonal+trend)
    # coe = scales::percent( rem_sd / mean(stl_seasonal + stl_trend) )
    coe = rem_sd / mean( raw , na.rm = TRUE )
    
    
    stl_output = tibble(
        date = as.yearmon( time( ts ) ) ,
        raw = raw , 
        seasonal = stl_seasonal, 
        remainder = rem , 
        trend = stl_trend
    ) 
    
    if ( transform %in% "log" ){
        stl_output = stl_output %>%
            mutate( 
                date = as.Date( as.yearmon( time( ts ) ) ),
                raw = raw , 
                seasonal = exp( seasonal )
                , trend = exp( trend ) 
                , remainder = exp( remainder )
                )
        
        coe = sd( stl_output$remainder   , na.rm = TRUE  ) 
    }
        
    
    stl_output = stl_output %>%        
        gather( var, value, -date ) %>%
        mutate( 
            var = factor( var, levels = c('raw', 'seasonal', 'trend', 'remainder') ) 
        )
    

    
    if (plot){ 
        g = ggplot( stl_output, aes( x =  date , y = value )) +
            geom_line() +
            facet_grid( var ~ . , scales = 'free' ) +
            theme_bw() +
            labs( title = title , 
                  subtitle = paste( "Coefficient of Variation:" , scales::percent(coe) ) ,
                  caption = paste( transform, ":" , "" ) )
        print(g)
    }
    
    if (.coe){
        return( coe )
    } else {
        return( stl_model )
    }
    
}

## X-13ARIMA-SEATS decompose FASTer version, COE only ####
decompose.seas.coe = function( df , plot = TRUE , cov = TRUE , model = TRUE , 
                           transform = "log" ,
                           detect_outliers = FALSE ,
                           title = "", 
                           arima = "(0 1 1)(0 1 1)12" ,
                           regression_variables = NULL ,
                           missing_one = FALSE , # deprecated
                           smooth = FALSE , # take moving average before analysing
                           impute = FALSE ,
                           verbose = FALSE ,
                           ...
){
    

    if (detect_outliers){ .outliers = ""} else { .outliers = NULL} 
    
    # impute missing values
    if (impute){
        library(imputeTS)
        df.adjusted = na.seadec( df , "random" ) 
        df.adjusted = ifelse( df.adjusted<0, 0, df.adjusted )
    } else {
        df.adjusted = df 
    }
    
    ## need to replace missing values with zero (but not missing dates; there should be no missing dates at this point)
    
    if (smooth){
        s = ma( df.adjusted , order = 2 , centre = FALSE)
        df.adjusted = s
    } 
    
    if ( transform  %in% "log" ){
        
        df.adjusted[ df.adjusted == 0 ] = 1L
    }
    
    seas_df <- try( 
        
        seasonal::seas( df.adjusted
                        , na.action =  na.x13
                        # , x11 = "" # optional use of X11 model
                        # , x11 = list()
                        , arima.model = arima 
                        , regression.variables = regression_variables
                        , regression.aictest = NULL
                        , outlier = .outliers
                        , transform.function = transform 
                        , seats.noadmiss = "no"
                        
        )
    )
    
    
    # if fails, try with seasonal approximation (seats.noadmiss = 'yes')
    if ( class(seas_df) %in% "try-error" ){
        
        seas_df <- try( 
            
            seasonal::seas( df.adjusted
                            , na.action =  na.x13
                            # , x11 = "" # optional use of X11 model
                            # , x11 = list()
                            , arima.model = arima 
                            , regression.variables = regression_variables
                            , regression.aictest = NULL
                            , outlier = .outliers
                            , transform.function = transform 
                            , seats.noadmiss = "yes"
                            
            )
        )
        
    } 
    
    # if still fails, NA
    if ( class(seas_df) %in% "try-error" ) return( NA )
    
    if ( !is.ts(seas_df$data) ) return( NA )
    
    # plot(seas_df)
    # summary(seas_df)
    # final(seas_df)
    # View( seas_df$data )
    
    # if no seasonal componenet (e.g. seasonal value always 1), repeat as X11 model
    if ( "seasonal" %in% colnames(seas_df$data) ){
        seasonal = seas_df$data %>% as_data_frame() %>% .$seasonal %>% as.double()
    } else {
        seasonal = rep( 1 , nrow( seas_df$data ))
    }
    
    # test if seasonal always 1 
    is_not_1 = sum( seas_df$data %>% as_data_frame() %>% .$seasonal != 1 , na.rm = TRUE )
    
    if ( is_not_1 == 0 ){
        
        seas_df = update( seas_df, x11 = "", transform.function = "auto" )
        
        seasonal = seas_df$data %>% as_data_frame() %>% .$seasonal %>% as.double()
    }
    
    # remainder and trend , and raw
    rem = seas_df$data %>% as_data_frame() %>% .$irregular %>% as.double()
    
    trend = seas_df$data %>% as_data_frame() %>% .$trend %>% as.double()
    
    raw = as.integer( df ) 
    
    adjusted = as.integer( df.adjusted )
    
    if (length( raw ) == 0 ) return( NA )
    
    date = as.Date( as.yearmon(time( seas_df$x ) ) ) 
    
    
    rem_mean = mean( rem-1, na.rm = TRUE ) # if centered, should be ~ zero
    rem_sd = sd( (rem-1) , na.rm = TRUE ) 
    
    # if ( seas_df$spc$transform$`function` %in% "log" ){
    if ( seas_df$udg["finmode"] %in% "multiplicative" | transform  %in% "log" ){
        
        coe =  rem_sd
        
    } else {
        
        coe =  rem_sd / mean(seasonal + trend)
    }
    
    
    return( coe )
}

## X-13ARIMA-SEATS decompose ####
decompose.seas = function( df , year = NULL , plot = TRUE , 
                           cov = TRUE , 
                           model = TRUE , 
                           transform = "log" ,
                           detect_outliers = FALSE ,
                           title = "", 
                           arima = "(0 1 1)(0 1 1)12" ,
                           regression_variables = NULL ,
                           missing_one = FALSE , # deprecated
                           smooth = FALSE , # take moving average before analysing
                           impute = FALSE ,
                           zero_as_missing = FALSE , 
                           verbose = FALSE 
                           ){
    
    
    if ( !is.null(year) ){
        ts = window( df, year )
    } else { 
        ts = df 
    }
    
    # return NA if not enough data to model
    # if ( sum(!is.na( ts )) < 27 ) return(NA)
    # if ( sum( df > 0 , na.rm = TRUE )< 26 )  return(NA)
    
    
    if (detect_outliers){ .outliers = ""} else { .outliers = NULL} 
    
    # impute missing values
    if (impute){
        
        library(imputeTS)
        
        if ( zero_as_missing ) ts[ ts==0 ] = NA 
        
        df.adjusted = na.seadec( ts , "kalman" ) 
        df.adjusted = ifelse( df.adjusted < 0, 0, df.adjusted )
        
    } else {
        df.adjusted = ts 
    }
    
    ## need to replace missing values with zero (but not missing dates; there should be no missing dates at this point)

    if (smooth){
        s = ma( df.adjusted , order = 2 , centre = FALSE)
        df.adjusted = s
    } 
    
    if ( transform  %in% "log" ){
        
        df.adjusted[ df.adjusted == 0 ] = 1L
    }
    
      seas_df <- 
        
        try( 
 
        seasonal::seas( df.adjusted
                              , na.action =  na.x13
                              # , x11 = "" # optional use of X11 model
                              # , x11 = list()
                              , arima.model = arima 
                              , regression.variables = regression_variables
                              , regression.aictest = NULL
                              , outlier = .outliers
                              , transform.function = transform 
                              , seats.noadmiss = "no"
                       
                       )
    )
    
    
    # if fails, try with seasonal approximation (seats.noadmiss = 'yes')
    if ( class(seas_df) %in% "try-error" ){
        
        seas_df <- try( 
            
            seasonal::seas( df.adjusted
                            , na.action =  na.x13
                            # , x11 = "" # optional use of X11 model
                            # , x11 = list()
                            , arima.model = arima 
                            , regression.variables = regression_variables
                            , regression.aictest = NULL
                            , outlier = .outliers
                            , transform.function = transform 
                            , seats.noadmiss = "yes"
                            
            )
        )
        
    } 
    
    # if still fails, NA
    if ( class(seas_df) %in% "try-error" ) return( NA )
    
    if ( !is.ts(seas_df$data) ) return( NA )
    
    # plot(seas_df)
    # summary(seas_df)
    # final(seas_df)
    # View( seas_df$data )
    
    # if no seasonal componenet (e.g. seasonal value always 1), repeat as X11 model
    if ( "seasonal" %in% colnames(seas_df$data) ){
        seasonal = seas_df$data %>% as_data_frame() %>% .$seasonal %>% as.double()
    } else {
        seasonal = rep( 1 , nrow( seas_df$data ))
    }
    
    # test if seasonal always 1 
    is_not_1 = sum( seas_df$data %>% as_data_frame() %>% .$seasonal != 1 , na.rm = TRUE )
    
    if ( is_not_1 == 0 ){
        
        seas_df = update( seas_df, x11 = "", transform.function = "auto" )

        seasonal = seas_df$data %>% as_data_frame() %>% .$seasonal %>% as.double()
    }
    
    # remainder and trend , and raw
    rem = seas_df$data %>% as_data_frame() %>% .$irregular %>% as.double()
    
    trend = seas_df$data %>% as_data_frame() %>% .$trend %>% as.double()
    
    observed = as.integer( ts ) 
    
    adjusted = as.integer( df.adjusted )

    if (length( raw ) == 0 ) return( NA )
    
    date = as.Date( as.yearmon(time( seas_df$x ) ) ) 
    

        rem_mean = mean( rem-1, na.rm = TRUE ) # if centered, should be ~ zero
        rem_sd = sd( (rem-1) , na.rm = TRUE ) 
        
        # if ( seas_df$spc$transform$`function` %in% "log" ){
        if ( seas_df$udg["finmode"] %in% "multiplicative" | transform  %in% "log" ){
            
            coe =  rem_sd
            
        } else {
            
            coe =  rem_sd / mean(seasonal + trend)
        }
        
        
        return_list = list() 

        if ( model ){
            
            return_list = list( seas_df )
            
        } 
        
        
        if (cov){
            
            if (verbose) print( coe )
            return_list = c( return_list, coe ) 
            
        }  
        
        if (plot){
            
            seas_output = tibble(
                date = date ,
                observed = observed , adjusted = adjusted , 
                seasonal = seasonal, remainder = rem , 
                trend = trend
                
            ) %>%
                gather( var, value, -date) %>%
                mutate( 
                    var = factor( var, levels = c('observed', 'adjusted', 'trend', 'seasonal', 'remainder') ) 
                )
            
            # drop adjusted if same as raw
            if ( identical( observed, adjusted ) ) {
                seas_output = seas_output %>%
                    filter( !var %in% 'adjusted' ) %>%
                    mutate( var = droplevels( var )
                            )
            }
            
            g = ggplot( seas_output, aes( x =  date , y = value )) +
                geom_line() +
              facet_wrap( ~ var , ncol =1  , scales = 'free', strip.position = 'top') +
              theme_bw() +
              theme(strip.text = element_text(size = 20, face = "bold"))
                labs( title = title , 
                      subtitle = paste( "Coefficient of Variation:" , scales::percent(coe) ) ,
                      caption = paste( transform , ":" , seas_df$model$arima$model ) )
            
            print( g )
            return_list = list( return_list, g ) 
        }
        
        return( return_list )
}

## fixed_arima ####
library( forecast )
fixed_arima = function( df , plot = TRUE , .cov = TRUE , 
                           detect_outliers = FALSE ,
                           title = "", 
                           ...
){
    
    arima_df <- try( 
        
        forecast::Arima(df , order = c(1,1,1)  , seasonal = c(1,1,1) )
    )
    
    if ( class(arima_df) %in% "try-error" ) return( NA )
    
    if ( !is.ts(arima_df$x) ) return( NA )

        
    seasonal = arima_df$x
    
    # remainder and trend , and raw
    rem = residuals( arima_df )
    
    trend = arima_df$fitted
    
    raw = as.integer( df ) 
    
    if (length( raw ) == 0 ) return( NA )
    
    date = as.Date( as.yearmon(time( seas_df$x ) ) ) 
    
    arima_output = tibble(
        date = date ,
        raw = raw , 
        seasonal = seasonal, 
        remainder = rem , 
        trend = trend
    ) %>%
        gather( var, value, -date) %>%
        mutate( 
            var = factor( var, levels = c('raw', 'seasonal', 'trend', 'remainder') ) 
        )
    
    rem_mean = mean( rem-1, na.rm = TRUE ) # if centered, should be ~ zero
    rem_sd = sd( (rem-1) , na.rm = TRUE ) 
    
    coe = scales::percent( rem_sd )

    
    if (plot){ 
        g = ggplot( arima_output, aes( x =  date , y = value )) +
            geom_line() +
            facet_grid( var ~ . , scales = 'free' ) +
            theme_bw() +
            labs( title = title , 
                  subtitle = paste( "Coefficient of Variation:" , coe ) ,
                  caption = paste( transform, ":" , seas_df$model$arima$model ) )
        print(g)
    }
    
    if (.coe){         
        return( coe )
    } else {
        return( seas_model )
    }
    
    
}

## forecast_ets ####
library( forecast )
forecast_ets = function( df , plot = TRUE , .cov = TRUE , 
                        detect_outliers = FALSE ,
                        title = "", 
                        ...
){
    
    ets_df <- try(  forecast::ets( df.1  )  )
    
    if ( class(ets_df) %in% "try-error" ) return( NA )
    
    if ( !is.ts(ets_df$x) ) return( NA )
    
    
    seasonal = ets_df$states
    
    
    # remainder and trend , and raw
    rem = residuals( ets_df$residuals )
    
    trend = seas_df$data %>% as_data_frame() %>% .$trend %>% as.double()
    
    raw = as.integer( df ) 
    
    if (length( raw ) == 0 ) return( NA )
    
    date = as.Date( as.yearmon(time( seas_df$x ) ) ) 
    
    seas_output = tibble(
        date = date ,
        raw = raw , 
        seasonal = seasonal, 
        remainder = rem , 
        trend = trend
    ) %>%
        gather( var, value, -date) %>%
        mutate( 
            var = factor( var, levels = c('raw', 'seasonal', 'trend', 'remainder') ) 
        )
    
    rem_mean = mean( rem-1, na.rm = TRUE ) # if centered, should be ~ zero
    rem_sd = sd( (rem-1) , na.rm = TRUE ) 
    
    # if ( seas_df$spc$transform$`function` %in% "log" ){
    if ( seas_df$udg["finmode"] %in% "multiplicative" ){
        
        coe = scales::percent( rem_sd )
        
    } else {
        
        coe = scales::percent( rem_sd / mean(seasonal + trend) )
    }
    
    
    if (plot){ 
        g = ggplot( seas_output, aes( x =  date , y = value )) +
            geom_line() +
            facet_grid( var ~ . , scales = 'free' ) +
            theme_bw() +
            labs( title = title , 
                  subtitle = paste( "Coefficient of Variation:" , coe ) ,
                  caption = paste( transform, ":" , seas_df$model$arima$model ) )
        print(g)
    }
    
    if (.coe){         
        return( coe )
    } else {
        return( seas_model )
    }
    
    
}

# Show examples ####
sample_dec = function( data = x.dec, index = 1 ,  method = 'stl' , ... ){
    
    df  = x.dec[ index , ]$ts[[1]]
    df.name = paste(  data$parent_ou.name[ index ]  , " : " ,
                      data$orgUnit.name[ index ] , " (Total Cases = " ,
                      scales::comma( data$total[ index ] ), ")" )
    
    if (method %in% 'stl') decompose.stl( df  , transform = "log" , title = df.name, ...) 
    
    if (method %in% 'seas') decompose.seas( df  , transform = "log" , title = df.name , model = FALSE , cov = FALSE ) 
}

# Moving Average : a manual version for remving trend and seasonality ####
trends = function( df ){
    
    # multiplicative
    trend = ma(df , order = 12, centre = T)
    # plot( df ); lines( trend )
    
    
}


## multiple methods
decompose.3 = function( tsd = ts_ou_de  , show_plot = FALSE, title = "", ... ){
    
    tibble( 
        n_missing = sum( is.na( tsd) ) 
        , mean_value = mean( tsd , na.rm = TRUE ) 
        , median_value = median( tsd , na.rm = TRUE )  
        , stl = decompose.stl( df = tsd , plot = show_plot ) 
        , seas_log = decompose.seas( df = tsd , transform = "log" , plot = show_plot )
        , seas_auto = decompose.seas( df = tsd , transform = "auto" , plot = show_plot )
    ) 
}

ts.summary = function( tsd  ){
    
    tibble( 
        n_missing = sum( is.na( tsd) ) 
        , mean_value = mean( tsd , na.rm = TRUE ) 
        , median_value = median( tsd , na.rm = TRUE )
    )  
}

arimas = function( tsd = ts_ou_de  , show_plot = FALSE, title = "", ... ){
    
    tibble( 
        n_missing = sum( is.na( tsd) ) 
        , mean_value = mean( tsd , na.rm = TRUE ) 
        , median_value = median( tsd , na.rm = TRUE )  
        , seas_0 = decompose.seas( df = tsd , transform = "log" , plot = show_plot
                                   , arima = "(0 1 1)(0 1 1)"  )
        , seas_1 = decompose.seas( df = tsd , transform = "log" , plot = show_plot
                                   , arima = "(0 1 1)(1 1 1)" )
    )  
}

# Displays ####

quality_histogram = function( element = NULL , 
                              d.nest.dec. = d.nest.dec ,
                              instance. = instance ,
                              .pb = NULL ){
    
    # progress bar
    update_progress( .pb ) 
    
    x.dec = d.nest.dec. %>% filter( de.coc.name %in% element )
    
    x.dec$quality = cut( x.dec$dec, breaks = c( 0, .5, 1 , 2, Inf) , labels = letters[1:4] )
    
    x.dec = x.dec %>% arrange( level )
    
    # Change names for Kenya.demo
    if ( instance. %in% 'Kenya.demo' ){
        
        kenya.labels = c( 'National' , 'Region', "District", "Sub-District" , "Ward" , "Facility", "Community" )
        
        kenya.levels = c( 'National' , 'SNU1', "SNU2", "SNU3" , "SNU4" , "Health Facility", "Community Unit" )
        
        
        x.dec$level.name = factor( x.dec$level.name, 
                                   levels = kenya.levels ,
                                   labels = kenya.labels ,
                                   ordered = TRUE )
        
        
    }
    
    hist = x.dec %>% ggplot() + 
        geom_bar( aes(quality)  ) + 
        scale_x_discrete( 'Quality' , drop = FALSE ) +
        facet_wrap(~level.name, scales = 'free' ) +
        labs( title = element , subtitle = instance.  )
    
    print( hist )
}

