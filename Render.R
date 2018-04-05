# Render doucments

library(knitrBootstrap) # NB: does not work with DT::datatable
library(rmarkdown)

# function to open an html document
    openHTML <- function(x) browseURL(paste0('file://', file.path(getwd(), x)))


# This script creates the review documents 

    dhis_instance = "Uganda"
    data_directory = "../HMIS/DHIS2"
    
# list of options to pass to the markdown documents
    params.list = list(
        origin_login_file = dhis_instance , # selects dhis2 instance
        data_directory = data_directory ,  # directory for login and data files
        cache = FALSE
    )

# Metadata
    render('Metatdata_Review.Rmd', 
           # 'knitrBootstrap::bootstrap_document' ,
           params = params.list ,
           envir = new.env()
           )
    openHTML('Metatdata_Review.html')

# Select data elements to review
    de.review.xlsx = paste0( data_directory , "/" ,
                             dhis_instance , "/" ,
                             dhis_instance , "_" , 
                             "dataElements.xlsx" )
    
    # if exist, open excel file
    if ( file.exists( de.review.xlsx ) ) browseURL( de.review.xlsx )
    
# Data
    render('Data_Review.Rmd', 
           # 'knitrBootstrap::bootstrap_document' ,
           params = params.list ,
           envir = new.env()
           )
    
    openHTML('Data_Review.html')


# After running this script, the output will appear in the current directory
# - open Metadata_Review.html in browser
# - open Data_Review.html in browser 
