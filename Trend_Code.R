# Trend_Code.R

library( tidyverse )
library( lubridate )
library(forecast)
library(forecast)
library(seasonal)
library(stlplus)
library(zoo)

ous.random = function( .level){  #samples OU id within OU level
    
    ous_within_level = ous %>% filter( level %in% .level )
    
    random_row_within_level = sample( 1:nrow( ous_within_level ) , 1 )
    
    ous_within_level[ random_row_within_level , ]
}

# TIME-Series  ####
ts.ou.de = function(data , ous , de , start_year = NA ){
    
    # create time-series for selcted OU and dataElement (total: need to sum over category combo)
    d_ou_de = d %>% 
        filter( orgUnit %in% ous, dataElement %in% de ) %>%
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



# Decompose functions ####
decompose.stl = function( df , plot = TRUE , coe = TRUE ){
    
    if ( sum(!is.na( df )) < 27 ) return(NA)
    
    # STL decompose and coefficient of variation
    stl_model = stlplus( df, s.window = 'periodic', n.p = 12  ) 
    
    if (plot){
        print( plot( stl_model) ) 
        # glimpse( stl_df$data )
    }

    
    stl_trend = trend( stl_model ) 
    stl_seasonal = seasonal( stl_model )
    rem = remainder( stl_model )
    rem_mean = mean( rem, na.rm = TRUE )
    rem_sd = sd( rem , na.rm = TRUE ) 
    cv = rem_sd / rem_mean
    # cv
    
    if (coe){
        # coeficient of error?  ration of remainder to projected (seasonal+trend)
        coe = scales::percent( rem_sd / mean(stl_seasonal + stl_trend) )
        return( coe )
    } else {
        return( stl_model )
    }
    
}

decompose.seas = function( df , plot = TRUE , coe = TRUE , transform = "log"){
    
    if ( sum(!is.na( df )) < 27 ) return(NA)
    
    # X-13ARIMA-SEATS decompose
    ## need to replace missing values with zero (but not missing dates; there should be no missing dates at this point)
    df.1 = df
    # df.1[ is.na( df )] = 1L
    df.1[ df == 0 ] = 1L
    seas_df <- try( 
        suppressMessages( 
        seasonal::seas(df.1
                              , na.action = na.x13
                              # , x11 = "" 
                              # , x11 = list()
                              , regression.aictest = NULL
                              # , outlier = NULL,
                              , transform.function = transform 
                              # , transform.function = "log"
                              # , transform.function = "none"
                       )
        )
    )
    
    if ( class(seas_df) %in% "try-error" ) return( NA )
    
    if ( !is.ts(seas_df$data) ) return( NA )
    
    # plot(seas_df)
    # summary(seas_df)
    # final(seas_df)
    # View( seas_df$data )
    
    rem = seas_df$data %>% as_data_frame() %>% .$irregular %>% as.double()
    seasonal = seas_df$data %>% as_data_frame() %>% .$seasonal %>% as.double()
    trend = seas_df$data %>% as_data_frame() %>% .$trend %>% as.double()
    
    if ( seas_df$spc$transform$`function` %in% "log" ){
        raw = seasonal * trend * rem
    } else {
        raw = seasonal + trend + rem 
    }
    
    if (length( raw ) == 0 ) return( NA )
    
    date = as.Date( as.yearmon(time( seas_df$x ) ) ) 
    
    seas_model = tibble(
        date = date ,
        raw = raw , seasonal = seasonal, remainder = rem , trend = trend
    ) %>%
        gather( var, value, -date) %>%
        mutate( 
            var = factor( var, levels = c('raw', 'seasonal', 'trend', 'remainder') ) 
        )
    
    if (plot){ 
        g = ggplot( seas_model, aes( x =  date , y = value )) +
            geom_line() +
            facet_grid( var ~ . , scales = 'free' ) +
            theme_bw()
        print(g)
    }
    
    if (coe){ 
        rem_mean = mean( rem-1, na.rm = TRUE ) # if centered, should be ~ zero
        rem_sd = sd( (rem-1) , na.rm = TRUE ) 
        
        if ( seas_df$spc$transform$`function` %in% "log" ){
            
            coe = scales::percent( rem_sd )
            
        } else {
            
            coe = scales::percent( rem_sd / mean(seasonal + trend) )
        }
        
        
        return( coe )
    } else {
        return( seas_model )
    }
    
    
}
