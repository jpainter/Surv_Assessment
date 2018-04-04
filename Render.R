# Render doucments

library(knitrBootstrap) # NB: does not work with DT::datatable
library(rmarkdown)

# function to open an html document
    openHTML <- function(x) browseURL(paste0('file://', file.path(getwd(), x)))


# This script creates the review documents 

# list of options to pass to the markdown documents
    params.list = list(
        origin_login_file = "Malawi", # selects dhis2 instance
        data_directory = "../HMIS/DHIS2" ,  # directory for login and data files
        cache = FALSE
    )

# Metadata
    render('Metatdata_Review.Rmd', 
           # 'knitrBootstrap::bootstrap_document' ,
           params = params.list
           )
    openHTML('Metatdata_Review.html')

# Data
    render('Data_Review.Rmd', 
           # 'knitrBootstrap::bootstrap_document' ,
           params = params.list
           )
    
    openHTML('Data_Review.html')


# After running this script, the output will appear in the current directory
# - open Metadata_Review.html in browser
# - open Data_Review.html in browser 
