# testing plumber

# plumber.R

#' Echo the parameter that was sent in
#' @param msg The message to echo back.
#' @get /echo
function(msg=""){
    list(msg = paste0("The message is: '", msg, "'"))
}

#' Plot out data from the iris dataset
#' @param spec If provided, filter the data to only this species (e.g. 'setosa')
#' @get /plot
#' @png
function(spec){
    myData <- iris
    title <- "All Species"
    
    # Filter if the species was specified
    if (!missing(spec)){
        title <- paste0("Only the '", spec, "' Species")
        myData <- subset(iris, Species == spec)
    }
    
    plot(myData$Sepal.Length, myData$Petal.Length,
         main=title, xlab="Sepal Length", ylab="Petal Length")
}


## RUN
# pr <- plumber::plumb("testing_Plumber.R")
# pr$run()
## You should see a message about your API running on your computer on port 8000. The API will continue running in your R session until you press the Esc key. If you’re running this code locally on your personal machine, you should be able to open http://localhost:8000/echo or http://localhost:8000/plot in a web browser to test your new API endpoints.
