
#' # Seasonal Decomposition

#'Time series decomposition is a mathematical procedure which transforms a time series into multiple different time series’, often is split into 3 components:

#' **Seasonal:** Patterns that repeat with a fixed period of time. For example, rainfall ,  mosquito abundance, and malaria cases frequently have yearly seasonality.

#' **Trend:** The underlying trend of the series across several seasons.

#' **Random:** Also called “noise”, this is the residuals after the seasonal and trend series are removed.

#' A centered moving average can be used to smooth the time series and detect the underlying trend. To perform the decomposition, it is vital to use a moving window of the exact size of the seasonality.

#' ## An example borrowed from `r-exercises.com`

    river_data = read_csv("Trend code/PAICOL.csv")
    # Convert strings values into dates
    river_data$DATE=as.Date(river_data$DATE,origin=river_data$DATE[1])
    g = ggplot(river_data,aes(DATE,LEVEL))+geom_line(aes(color="LEVEL"))+
        geom_line(data=river_data,aes(DATE,RAIN,color="RAIN"))
    g

#' Further information
#' - [R for Hydrologists]^[https://www.r-exercises.com/2018/04/17/r-for-hydrologists-seasonality-and-trend-decomposition/]
