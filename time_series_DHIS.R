#time_series_dhis

# libraries #### 
library( forecast )
library(zoo)
library(seasonal)
library(ggseas)
library( xts ) # xts objects transform more flexible
library(stlplus) # for handling time-series with missing data
library( dygraphs )
library( tidyverse )
library( lubridate )

 # simple ts example with built-in datset nottem ####
  glimpse( nottem )
  
  # get date from ts 
  date_decimal( as.numeric(time( nottem )) )
  
  decompose_mult = decompose(nottem, "multiplicative")
  plot(decompose_mult)
  rand_mult = decompose_mult$random
  
  rand_mult.xts = as.xts( rand_mult )
  rand_mult_df = as_data_frame(rand_mult.xts) 
  rand_mult_df$time = rownames(rand_mult_df)
  glimpse( rand_mult_df )
  mean( rand_mult_df$V1, na.rm =  TRUE  )
  sd( rand_mult_df$V1, na.rm =  TRUE  )

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
  
  # XTS and TS ####
  tsd = xts( spread.data %>% select( -date) , order.by = spread.data$date )
  
  # convert to ts class for decomposition
  glimpse( tsd )
  time( tsd )
  ts.d = as.ts( tsd )
  glimpse(ts.d)
  time( ts.d )
  
  # Decompose STL #### 
  # use stlplus because of missing values
  ts.d.seasonal = stlplus( ts.d , s.window = 'periodic', n.p = 12 )  
  plot( ts.d.seasonal )
  
  # Dygraph Display ####
  color_palette  = RColorBrewer::brewer.pal( 12 , "Paired")
  # remove yellow 
  color_palette = color_palette[ !color_palette %in% "#FFFF99"]
  
  dygraph( tsd , 
           group = "a") %>%
    dyLegend(show = "always", hideOnMouseOut = TRUE , 
             labelsDiv = "legendDivID",
             labelsSeparateLines = TRUE 
    )   %>%
    dyOptions(colors = color_palette, 
              strokeWidth = 3 ,
              axisLabelFontSize = 16  
    ) %>%
    dyHighlight(highlightSeriesOpts = list(strokeWidth = 4)) %>%
    dyRangeSelector( height = 100 ) %>%
    # dyRoller(rollPeriod = as.integer( input$rolling ), showRoller = FALSE) %>%
    dyAxis("x", axisLabelFormatter="function(d) { return d.getFullYear() }") %>%
    dyAxis("x", ticker="function(a, b, pixels, opts, dygraph, vals) {
           return Dygraph.getDateAxis(a, b, Dygraph.ANNUAL, opts, dygraph)
           // or TWO_HOURLY or SIX_HOURLY...
           }") 

  # SD ####

  decompose_mult = decompose(nottem, "multiplicative")
  plot(decompose_mult)
  rand_mult = decompose_mult$random
  
  tsd_df = as_data_frame(tsd) 
  tsd_df$time = rownames(tsd_df)
  glimpse( tsd_df )
  
  devtools::install_github("R-Finance/xtsExtra")
  
  # GGSEAS ####
  # https://www.r-bloggers.com/is-my-time-series-additive-or-multiplicative/
  # unfotunately, requires data set with no NA
  
  ## NB: Does not neet time-series: can strat with DF!
  library(ggseas) # ggplot for time series

  sample = spread.data %>% 
    select( date, `Zomba-DHO` ) %>% 
    rename( value = `Zomba-DHO`) %>% 
    filter( !is.na( value) )
  
  # sample_ts = sample %>%
  #   # ts( .$value, frequency = 12 ) 
  #   tots( )
  # 
  # # get values in data_frame
  # library(timetk)
  # sample_ts_df = tk_tbl( sample_ts )
  # 
  # # calc mean and sd 
  # sample_ts_df_summary = sample %>%
  #   summarise( mean = mean( value, na.rm =TRUE ), 
  #              sd = sd( value, na.rm =TRUE )
  #              )
  
  # ggsdc(sample_ts, aes(x = date, y = value), 
        
  ggsdc(sample, aes(x = date, y = value ),
        method = "decompose", 
        frequency = 12, s.window = 12, type = "multiplicative" ) + 
    stat_rollapplyr(width = 12, FUN = mean, color = 'red') +
    geom_line() + 
    # geom_hline( yintercept =  with(sample_ts_df_summary, mean-3*sd) , color = 'blue') +
    # geom_hline( yintercept =  with(sample_ts_df_summary, mean+3*sd) , color = 'blue') + 
    ggtitle("DHIS2 Decomposition") + theme_minimal()
  
  # plot observed (scaled)
  ggplot(sample, aes(x = date, y = scale(value) )) + 
    stat_rollapplyr(width = 12, FUN = function(x) mean(x) + 3*sd(x), color = 'red') +
    # stat_rollapplyr(width = 12, FUN = sd, color = 'blue') +
    stat_rollapplyr(width = 12, FUN = function(x) mean(x) - 3*sd(x), color = 'red') +
    geom_line() + 
    # geom_hline( yintercept =  with(sample_ts_df_summary, mean-3*sd) , color = 'blue') +
    # geom_hline( yintercept =  with(sample_ts_df_summary, mean+3*sd) , color = 'blue') + 
    ggtitle("DHIS2 Values") + theme_minimal()
  
  # FORECAST ####

  tsmod <- stlm(USAccDeaths, method = 'ets')
  plot(forecast(tsmod, h=12))
  
  # Mapping Decomposition functions ####
  tots = function( df){ zoo( df$value, df$date )}
  tots_log = function( df){ zoo( log( df$value ),  df$date )}
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
    nest( -district ) %>%
    mutate(  
      # ts =  map( data ,  tots ) ,
      ts.m =  map( data ,  tots_log ) , # .m refers for mulitplicative models
      # decomp = map( ts, deco ) ,
      decomp.m = map( ts.m, deco.m ) ,
      # mean = map_dbl( decomp, ~mean( .x$data$remainder, na.rm = TRUE ) ) ,
      # sd = map_dbl( decomp, ~sd( .x$data$remainder, na.rm = TRUE ) ),
      # plots = map( decomp, plot ) , 
      mean.raw.m = map_dbl( decomp.m, ~mean( .x$data$raw, na.rm = TRUE ) )  ,
      sd.raw.m = map_dbl( decomp.m,  ~sd( .x$data$raw, na.rm = TRUE ) ) ,
      mean.m = map_dbl( decomp.m, ~mean( .x$data$remainder, na.rm = TRUE ) )  ,
      sd.m = map_dbl( decomp.m,  ~sd( .x$data$remainder, na.rm = TRUE ) ) ,
      plots.m = map( decomp.m, plot ) 
      )
  
  glimpse( undoing )
  
  # Forrest plot of all mean remainder +- 2 SD
  ggplot( unnest( undoing, mean.m ) , aes( x = district )) +
    geom_pointrange( aes( y = mean.m, ymin = mean.m - 2*sd.m, ymax = mean.m + 2*sd.m)) +
    coord_flip() + theme_bw()
  
  # machinga has a really fat fingered value  
  undoing$plots.m[[ which(undoing$district %in% 'Machinga-DHO') ]]
  
  not_machinga = filter( undoing, !district %in% 'Machinga-DHO')
  
  # Forest plot
  ggplot( unnest( not_machinga, mean.m ) , aes( x = district )) +
    geom_pointrange( aes( y = mean.m, ymin = mean.m - 2*sd.m, ymax = mean.m + 2*sd.m)) +
    coord_flip()
  
  # overall Mean and SD (unweighted) on monthly basis
  unnest( undoing, mean.m ) %>% 
    select( district, mean.m, sd.m) %>% 
    filter( !district %in% 'Machinga-DHO' ) %>% 
    summarise_if( is.numeric, mean )
  
  # checkout Lilongwe_DHO 
  undoing$plots[[ which(undoing$district %in% 'Lilongwe-DHO') ]]
  undoing$plots.m[[ which(undoing$district %in% 'Lilongwe-DHO') ]]
  
  lil = undoing[ which(undoing$district %in% 'Lilongwe-DHO') , ]
  # mean and sd of remainder for lil
  summarise_if( lil$decomp.m[[1]]$data , is.numeric, c( 'mean', 'sd' ) , na.rm = TRUE ) %>%
    select( starts_with( 'remainder' ))
  
  plot( lil$decomp.m[[1]] ) 
  
  lil.df = bind_cols( lil$decomp.m[[1]]$data , date = time( lil$ts.m[[1]]) ) %>%
    select( raw, seasonal, trend, remainder, date ) %>% 
    gather( key, value, -date )
  
  glimpse( lil.df )
  
  ggplot( lil.df  ,  aes( x = date  ) )  +
    geom_line( aes( y = value )  ) +
    facet_grid( key ~ . , scales = 'free') +
    theme_bw()
  
  # YEARLY Remainder ####
  lil.df %>% 
    spread( key, value ) %>%
    group_by( year(date) ) %>%
    summarise( annual_remainder = mean( remainder, na.rm = TRUE) ) %>%
    ungroup() %>%
    summarise( annual_mean = mean( annual_remainder, na.rm = TRUE) ,
               annual_sd = sd( annual_remainder, na.rm = TRUE) 
               )
  
  annual.mean.m = function( data ){
    df = bind_cols( data$decomp.m[[1]]$data , date = time( data$ts.m[[1]] ) ) %>%
      select( raw, seasonal, trend, remainder, date ) %>% 
      group_by( year(date) ) %>%
      summarise( annual_remainder = mean( remainder, na.rm = TRUE) ) %>%
      ungroup() %>%
      summarise( annual_mean = mean( annual_remainder, na.rm = TRUE) 
      )
    return( df$annual_mean)
  }
  
  # annual.mean.m( data = lil )  
  # annual.mean.m( undoing[1, ] )
  
  annual.sd.m = function( data ){
    df = bind_cols( data$decomp.m[[1]]$data , date = time( data$ts.m[[1]] ) ) %>%
      select( raw, seasonal, trend, remainder, date ) %>% 
      group_by( year(date) ) %>%
      summarise( annual_remainder = mean( remainder, na.rm = TRUE) ) %>%
      ungroup() %>%
      summarise( annual_sd = sd( annual_remainder, na.rm = TRUE) 
      )
    return( df$annual_sd)
  }
  
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
  
  # annuals( undoing[1, ] )
 
 annual.mean.ms = map_dbl( seq_len(nrow(undoing)) , ~annual.mean.m( undoing[.x, ] ) )
 undoing$annual.mean.m = annual.mean.ms
 
 annual.sd.ms = map_dbl( seq_len(nrow(undoing)) , ~annual.sd.m( undoing[.x, ] ) )
 undoing$annual.sd.m = annual.sd.ms
 
 # Forest plot of annual mean remainder
 ggplot( unnest( undoing, annuals ) , aes( x = year )) +
   geom_pointrange( aes( y = mean.remainder, ymin = mean.remainder - 2*sd.remainder, ymax = mean.remainder + 2*sd.remainder)) +
   # coord_flip() + 
   theme_bw() +
   facet_wrap( ~district, scales = 'free')
 
 # Forest plot of SNR
 ggplot( unnest( undoing, annuals ) , aes( x = year )) +
   geom_line( aes( y = snr, group = district ) ) +
   geom_hline( yintercept = 2 , color = 'red', linetype = 2) +
   # coord_flip() + 
   theme_bw() +
   facet_wrap( ~district )
 
 
 # check out low performance of Likoma
 lik =  undoing[ which(undoing$district %in% 'Likoma-DHO') , ]
 lik.df = bind_cols( lil$decomp.m[[1]]$data , date = time( lil$ts.m[[1]]) ) %>%
   select( raw, seasonal, trend, remainder, date ) %>% 
   mutate( 
     noise = raw - seasonal*trend ,
     snr = trend / abs(noise) ) %>%
   gather( key, value, -date )
  
 ggplot( lik.df  ,  
         aes( x = date  ) 
 )  +
   geom_line( aes( y = value )  ) +
   facet_grid( key ~ . , scales = 'free') +
   theme_bw()
 
 ggplot( unnest(lik , annuals ) , aes( x = year )) +
   geom_line( aes( y = snr, group = district ) ) +
   geom_hline( yintercept = 2 , color = 'red', linetype = 2) +
   # coord_flip() + 
   theme_bw() +
   facet_wrap( ~district )
 
 
 