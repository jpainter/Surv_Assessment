#time_series_dhis

# libraries #### 
library(ggseas) # ggplot for time series
library( timetk )
library( xts ) # xts objects transform more flexible
library(stlplus) # for handling time-series with missing data
library( seasonal )
library( seasonalview )
library(magrittr)
library(gridExtra)
library( tidyverse )
library( lubridate )

 
# Malawi.  Use discordance data to test-estimate decomposition  ####
  malawi = readRDS( "~/Dropbox/_Malaria/Projects/discordance/Malawi/rhis_malawi.rds") 
  glimpse( malawi )
  
  tidy_rhis = function( data = rhis ){
    
    rename = data %>% 
      separate( Periodname, c("Month", "Year")) %>%
      rename( district = organisationunitname,
              region = Region ,
              MAL_U5 = `Mal _U5`,
              MAL_O5 = `Mal _5+`, 
              Fevercases_U5 = Fever_cases_U5 ,
              Opsuspectdcatest4mamic_O5 = `Opsuspectdcatest4mamic_5+` ,
              Opsuspectdcatest4mardt_O5 = `Opsuspectdcatest4mardt_5+`,
              Opsuspectdcatest4mardt_U5 = `)psuspectdcatest4mardt_U5`,
              Opconfirmdmacasmic_O5 = `Opconfirmdmacasmic_5+` , 
              Opconfirmdmacasrdt_O5 = `Opconfirmdmacasrdt_5+`
      ) 
    
    d = rename %>% mutate(
      TESTdone_O5 = Opsuspectdcatest4mamic_O5    + Opsuspectdcatest4mardt_O5 ,
      TESTdone_U5 = Opsuspectdcatest4mamic_U5 + Opsuspectdcatest4mardt_U5 ,
      TESTve_O5 = Opconfirmdmacasmic_O5 + Opconfirmdmacasrdt_O5 ,
      TESTve_U5 = Opconfirmdmacasmic_U5 + Opconfirmdmacasrdt_U5 ,
      
      TPR_O5 = `TESTve_O5` / `TESTdone_O5` ,
      TPR_U5 = TESTve_U5 / TESTdone_U5
      
      
    ) %>% 
      # move into long format
      gather( measure, value, -Month, -Year, -district, -region) %>% 
      filter( !(measure %in% names(rename)[c(5, 9:20)] )  ) %>%
      # rename measures c()
      mutate(
        date = as_date( ymd(paste0(Year, Month, 1, sep = "-")) ),
        value = as.numeric( value ),
        measure = factor( measure, 
                          levels = c(
                            "MAL_U5",
                            "MAL_O5",
                            "Fevercases_U5",
                            "TESTdone_U5", "TESTdone_O5",
                            "TESTve_U5", "TESTve_O5",
                            "TPR_U5", "TPR_O5"
                          )),
        measure.type = ifelse( measure %in% "TPR", "TPR", "Cases") 
      ) 
    # %>%
    #     separate( measure, c("measure", "group"), sep = "_" ) %>%
    #     mutate(
    #        group = factor( group , levels = c("U5", "O5"))
    #     )
    
    return(d)
  }
  
  d = tidy_rhis( malawi )
  glimpse( d )
  
  # select data elements, eg. mal-u5
  data = d %>% filter( measure %in% "MAL_U5") %>%
    mutate(
      scaled = ifelse( measure.type == "Cases",
                      scale( value, center = FALSE ), value )
    )
  glimpse( data )

    
  spread.data = data %>%
    select( district, value, date ) %>%  # Select value or scaled
    spread( district, value )
  glimpse( spread.data )
  

# Sample #### 
  
  # fill in missing dates
  dates = spread.data %>% count(date) %>% select( date )
  
  sample = spread.data %>% 
    full_join( dates, by = "date") %>%
    select( date, `Mwanza-DHO` ) %>% 
    rename( value = `Mwanza-DHO`) %>% 
    filter( !is.na( value) )
  
  # lag-2 to smooth monthly data
  sample2 = spread.data %>% 
    full_join( dates, by = "date") %>%
    select( date, `Mwanza-DHO` ) %>% 
    rename( value = `Mwanza-DHO`) %>% 
    filter( !is.na( value) ) %>%
    mutate(
      value = ( value + lag(value))/2 ,
      date = date %>% ymd
    )
      
# convert to ts object 
       
# Example ggsdc  ####
  gseas = ggsdc( data = sample3 , aes(x = index, y = y ),
          method = "decompose", 
          # start = c( min( year( sample2$date ) ), min( month( sample2$date ) ) ) ,
          frequency = 12, s.window = 12, type = "multiplicative" ) +
      geom_line() +
      # stat_rollapplyr(width = 12, FUN = mean, color = 'red') +
      # geom_hline( yintercept =  with(sample_ts_df_summary, mean-3*sd) , color = 'blue') +
      # geom_hline( yintercept =  with(sample_ts_df_summary, mean+3*sd) , color = 'blue') +
      ggtitle("DHIS2 Decomposition") + theme_minimal()

  gseas

# function ggdsc, extracts remainder to calculate sd ####
ggRemainder = function( data , method = "stl" ){
  
  start = c( min( year( data$date ) ), min( month( data$date ) ) )
  
  gseas = ggsdc( data, aes(x = date, y = value ),
                method = method, start = start ,
                frequency = 12, s.window = 12, type = "multiplicative" ) + 
    geom_line() + 
    ggtitle("DHIS2 Decomposition") + theme_minimal()
  
  # extract remainder data
  irr.df = layer_data( gseas, 1) %>% filter( PANEL %in% 4)

  gg.irr = ggplot( irr.df , aes( x, y ) ) + 
      geom_line() +
      # stat_rollapplyr( width = 12, FUN = function(x) mean(x) + sd(x) , color = 'green')  +
      stat_rollapplyr( width = 12, FUN = function(x) sd(x) , color = 'blue') 

  # extract rolling sd and model with loess
  gg.irr.df = layer_data( gg.irr , 2 ) 
  gg.irr.loess = loess( y ~ x, gg.irr.df , span = 12,
                        control=loess.control(surface="direct")
                        )

  # use loess model of sd to get data points for plotting
  gg.irr.loess.pred = data.frame( x = gg.irr.df$x ,
                                  y = predict( gg.irr.loess, gg.irr.df$x )
  )
  
  # redo plot with smoothed values
  irr.loess = ggplot( gg.irr.df , aes( x, y ) ) +
    geom_line() +
    geom_line( data = gg.irr.loess.pred , aes( x, y ), color = 'blue' ) +
    labs( title = "Rolling SD" ,  subtitle = method )
  
  irr.loess
} 
  
  # test
  ggRemainder(sample, method = 'decompose')
  
  # show all
  for (method in c("stl", "decompose",
                     "seas")){
      p = ggsdc( sample, aes(x = date, y = value ),
             method = method, start = start ,
             frequency = 12, s.window = 12, type = "multiplicative" ) + 
          geom_line() + 
          stat_rollapplyr( width = 12, FUN = function(x) sd(x) , color = 'blue') +
          labs(title = "DHIS2 Decomposition", subtitle = method ) + 
          theme_minimal()
      
      assign( paste0( "p.", method), p)
      
      # extract rolling sd and model with loess
      p.sd.loess = loess( y ~ x, filter( p$data, component == 'irregular' ) )
      
      # use loess model of sd to get data points for plotting
      p.sd =  filter(p$data, component == 'irregular') %>%
          group_by( component ) %>% 
          mutate( sd = rollapplyr( y , width = 12, align = 'right' ,
                                  FUN = function(x) sd(x), na.pad = TRUE ) 
                  )
      
      p.sd.loess = loess( y ~ x, p.sd , span = 12,
                          control=loess.control(surface="direct")
      )
  }
  grid.arrange( p.stl, p.decompose, p.seas, ncol = 3)
  
  
  
  


  
# Mimic ggsdc with seas ####
  # decompose sample data
  data = sample
  start = c( min( year( data$date ) ), min( month( data$date ) ) )
  d =  ts( data[,-1], frequency = 12 , start = start )
  # glimpse(d)
  
  dec = seas( d , regression.aictest = NULL, transform.function = "log")
  
  # outputs
  # plot( dec )
  # glimpse( dec )
  # monthplot( dec )
  # view(dec)
  # static( dec )
  predict( dec, newdata = d )
  series( dec , "forecast.forecasts")
  identify( dec ) # manual outlier selection
  
  # Construct df with components
  df = data.frame( date = as.Date(as.yearmon(time(d)))  ,
                   raw = unname( as.matrix(d) ) ,
                   final = dec$data[ , 'final'] ,
                   seasonal = dec$data[ , 'seasonal'] ,
                   trend = dec$data[ , 'trend'] ,
                   irregular = dec$data[ , 'irregular'] 
                   ) %>% 
    mutate( trend = trend / mean( dec$data[ , 'trend'] , na.rm = TRUE) ) %>%
    gather( series, value, -date) 

  
  ggplot( df %>% filter(!series %in% 'final'), aes( x=date, y = value)) + 
    geom_line() +
    facet_grid( series ~ . , scales = 'free')
  
  
# Function to make data.frame with decomposed data ####
seasonalDF = function( data , method = 'seas' , 
                       type = "multiplicative" , 
                       s.window = 12 ,
                       frequency = 12 , 
                       lagMonth = FALSE ,
                       tf = "log" , ... ){
  
  data = data[ complete.cases( data ) , ]
  
  if ( lagMonth ) data = mutate( data, 
                                 value = ifelse( is.na(lag(value)), 
                                                 value , 
                                                 ( value + lag(value))/2 )
  )
  
  start = c( min( year( data$date ) ), min( month( data$date ) ) )
  
  d =  ts( data[,-1], frequency = frequency , start = start )
  
  if (  method == 'seas' ){

    dec = seas( d , regression.aictest = NULL, transform.function = tf )

    df = data_frame( date = as.Date( as.yearmon(time(dec$x) ) )  ,
                     raw = as.numeric(dec$x ) ,
                     # final = dec$data[ , 'final'] ,
                     seasonal = as.double( dec$data[ , 'seasonal'] ),
                     trend = as.double( dec$data[ , 'trend']  ),
                     random = as.double( dec$data[ , 'irregular'] )
    ) %>% 
      mutate( trend = trend / mean( dec$data[ , 'trend'] , na.rm = TRUE) ) %>%
      gather( series, value, -date) 
    
    df$series = factor( df$series, levels = c( 'raw', 'seasonal', 'trend', 'random') )
    
    return( df )
    
  } else if ( method == "decompose" ){
    
    dec = decompose( d , type = type  )
    
    df = data_frame( date = as.Date( as.yearmon(time(d) ) )  ,
                     raw = as.numeric( unname( as.matrix(d) ) ) ,
                     seasonal = as.numeric( dec$seasonal ) ,
                     trend =  as.numeric( dec$trend ) ,
                     random =  as.numeric( dec$random ) 
    ) %>% 
      mutate( trend = trend / mean( as.numeric( dec$trend ) , na.rm = TRUE) ) %>%
      gather( series, value, -date) 
    
    df$series = factor( df$series, levels = c( 'raw', 'seasonal', 'trend', 'random'))
    
    return( df )
    
  } else if (method == "stl") {
    if (is.null(s.window)) {
      stop("A value is needed for s.window.  See ?stl for help.")
    }
    
    dec <- stl( d[, 'value'], s.window = s.window) 
    
    df = data_frame( date = as.Date( as.yearmon(time(d) ) )  ,
                     raw = as.numeric( unname( as.matrix(d) ) ) ,
                     seasonal = as.numeric( dec$time.series[, 1] ) ,
                     trend =  as.numeric( dec$time.series[, 2] ) ,
                     random =  as.numeric( dec$time.series[, 3] ) 
    ) %>% 
      mutate( trend = trend / mean( as.numeric( dec$time.series[, 2] ) , na.rm = TRUE) ) %>%
      gather( series, value, -date) 
    
    df$series = factor( df$series, levels = c( 'raw', 'seasonal', 'trend', 'random'))
    
    return( df )
  }
    
    
}

# take seasonal DF and plot 
ggdf = function( df, orgUnit = "" ){
  a = ggplot( df %>% filter(series %in% 'raw') , aes( x=date, y = value)) + 
    geom_line() +
    facet_grid( series ~ . , scales = 'free') +
    ggtitle( orgUnit )
  
  # Components only (no raw data)
  b = ggplot( df %>% filter(!series %in% c('raw' , 'final') ), aes( x=date, y = value)) + 
    geom_line() +
    facet_grid( series ~ . , scales = 'free') +
    ggtitle("Seas") 
  
  gridExtra::grid.arrange( a, b , heights = c(2,3)) 
} 

df = seasonalDF( data = sample , method = "seas") #seasonal
ggdf(df)

df = seasonalDF( sample , method = "decompose" ) #decompose
ggdf(df)

df = seasonalDF( sample , method = "stl" ) #STL
ggdf(df)

# distribution of noise
x = filter( df, series =='random') %>% .$value 
summary( x )
sd(x)

# Lists of districts  ####

listing = spread.data[, c(1, 17) ] %>%
  gather( district, value, -date ) %>% 
  filter( !is.na( value) ) %>%
  nest( -district ) %>%
  mutate(  
    df.seas = map( data, ~seasonalDF( .x, method = "seas", lagMonth = TRUE ) )
    )

# unnest( listing[1,], df.seas )
# ggdf( unnest( listing[1,], df.seas ), orgUnit = listing[1,]$district )

ggdf_u = function( listing = listing, row = 2 ){
  df = unnest( listing[ row, ], df.seas )
  ggdf(df, orgUnit = df$district  )
}

index = 1:nrow( listing )
map( index, ~ggdf_u( listing, row = .x ) )

  
  
# Mapping Decomposition functions ####
  tots = function( df ){ zoo( df$value, df$date )}
  tots_log = function( df ){ zoo( log( df$value ),  df$date )}
  deco = function( ts ){ stlplus( ts, s.window = 'periodic', n.p = 12  ) }
  deco.m = function( tsex ){ 
    model = stlplus( tsex, s.window = 'periodic', n.p = 12  ) 
    model$data$raw = exp( model$data$raw )
    model$data$seasonal = exp( model$data$seasonal )
    model$data$trend = exp( model$data$trend )
    model$data$remainder = exp( model$data$remainder )
    return( model )
  }
  
  undoing = spread.data %>%
    gather( district, value, -date ) %>% 
    filter( !is.na( value) ) %>%
    nest( -district ) %>%
    mutate(  
      # plots = map( data , ggRemainder ) ,
      ts.m =  map( data ,  tots_log )  , # .m refers for mulitplicative models
      decomp.m = map( ts.m, deco.m ) ,
      plots.m = map( decomp.m, plot ) 
    )
  
  undoing$plots.m
  undoing$plots

# Use annual function to extract all undoing data at once ####
  annuals = function( data ){
    
    df = bind_cols( data$decomp.m[[1]]$data , date = time( data$ts.m[[1]] ) ) %>%
      select( raw, seasonal, trend, remainder, date ) %>% 
      mutate( year = as.character( year(date) )  ) %>%
      group_by( year ) %>%
      summarise( mean.raw = mean( raw, na.rm = TRUE) ,
                 mean.seasonal = mean( seasonal, na.rm = TRUE) ,
                 mean.trend = mean( trend, na.rm = TRUE) ,
                 mean.remainder = mean( remainder, na.rm = TRUE) ,
                 sd.remainder = sd( remainder, na.rm = TRUE) , 
                 signal = mean( trend , na.rm = TRUE) ,
                 noise = mean( abs(raw - seasonal*trend) , na.rm = TRUE) 
      ) %>%
      mutate( snr = signal / noise )
    
    overall_yrs = df %>%
      ungroup() %>%
      summarise( annual_sd = sd( mean.remainder, na.rm = TRUE) , 
                 annual_mean = mean( mean.remainder, na.rm = TRUE) ,
                 annual_snr = mean( snr, na.rm = TRUE) 
      )
    
    # all = data_frame( year = 'all', 
    #                   mean.remainder = overall_yrs$annual_mean ,  
    #                   sd.remainder = overall_yrs$annual_sd ,
    #                   snr = overall_yrs$annual_snr
    #                   ) 
    # 
    #  df = bind_rows( df, all )                           
    
    return(  df )
  }
  
  all.annuals = map( seq_len(nrow(undoing)) , ~annuals( undoing[.x, ] ) )
  undoing$annuals = all.annuals
  
  
  # Forest plot of annual mean remainder
  ggplot( unnest( undoing, annuals ) , aes( x = year )) +
    geom_pointrange( aes( y = mean.remainder, ymin = mean.remainder - 2*sd.remainder, ymax = mean.remainder + 2*sd.remainder)) +
    # coord_flip() + 
    theme_bw() +
    facet_wrap( ~district, scales = 'free')
  
  # Forest plot of sd.remainder
  ggplot( unnest( undoing, annuals ) , aes( x = year ,  y = sd.remainder, color = district)) +
    geom_point() + geom_line( aes( group = district ) ) + geom_smooth() +
    # coord_flip() + 
    theme_bw() 


 
