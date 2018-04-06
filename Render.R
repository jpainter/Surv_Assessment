# Render doucments

library(rmarkdown)
library(magrittr)

# function to open an html document
    openHTML <- function(x) browseURL(paste0('file://', file.path(getwd(), x)))


# This script creates the review documents 

    dhis_instance = "Uganda"
    data_directory = "../HMIS/DHIS2" # folder containing login_file 
    output_format = 'word_document'
    
    # this folder will hold output of the metadata and data reviews
    output_directory = "./" # "." is the directory where this file, Render.R, sits. 
    if (!dir.exists( dhis_instance ) ) dir.create( dhis_instance )
    
# list of options to pass to the markdown documents
    params.list = list(
        dhis_instance = dhis_instance , # selects dhis2 instance
        data_directory = data_directory ,  # root data directory
        output_directory = output_directory ,
        cache = FALSE ,
        echo = FALSE 
    )

    outputs = data_frame( 
        output_format = c(  "html_document", 'word_document'),
        output_suffix = c(  "html", 'doc')
    )
    
    output.suffix = outputs %>% 
        filter( output_format == .output_format ) %>% 
        .$output_suffix
    
# Metadata
    output.filename = paste0( output_directory, dhis_instance, "/", 
                             dhis_instance, "_Metadata_Review." ,
                             output.suffix
                             ) 
    
    render('Metadata_Review.Rmd', 
           output_file = output.filename ,
           output_format = output_format ,
           # 'knitrBootstrap::bootstrap_document' ,
           params = params.list ,
           envir = new.env()
           )
    
    openHTML( output.filename )

# Select data elements to review
    de.review.xlsx = paste0( output_directory , 
                             dhis_instance , "/" ,
                             dhis_instance , "_" , 
                             "dataElements.xlsx" )
    
    # if exist, open excel file
    if ( file.exists( de.review.xlsx ) ) browseURL( de.review.xlsx )
    
# Data
    
    # if variables selected, download and analyze
    if ( file.exists( de.review.xlsx ) ) { 
        
        de.include = readxl::read_xlsx( de.review.xlsx  ) %>% 
            filter( !is.na(Include) ) %>% .$Include
    
        if ( length( de.include ) > 0 ){ 
            output.filename = paste0(output_directory , dhis_instance, "/", 
                                     dhis_instance, "_Data_Review.html" ) 
            
            render('Data_Review.Rmd', 
                   output_file = output.filename ,
                   output_format = output_format ,
                   # 'knitrBootstrap::bootstrap_document' ,
                   params = params.list ,
                   envir = new.env()
                   )
            
            openHTML( output.filename )
            
        } else {
            cat( message("Select data elements to include"))
        }
    }


# After running this script, the output will appear in the current directory
# - open Metadata_Review.html in browser
# - open Data_Review.html in browser 
