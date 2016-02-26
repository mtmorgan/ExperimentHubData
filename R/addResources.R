### =========================================================================
### addResources() 
### -------------------------------------------------------------------------
###

## NOTE: 'HubRoot' is the local prefix; 'pathToData' is used both
##        locally (to find the file) and remotely (to store the file).
## NOTE: This function replaces AnnotationHubData::updateResources().
##       An alternative is to make updateResources() more flexible ...
addResources <- function(pathToPackage, insert=FALSE, ...)
{

    if (insert) {
        if(is.null(url <- getOption("EXPERIMENT_HUB_SERVER_POST_URL")))
            stop(paste0("When 'insert=TRUE' option ",
                        "EXPERIMENT_HUB_SERVER_POST_URL must be set ",
                        "in the global environment or .Rprofile"))
    }

    ## generate metadata
    message("generating metadata ...") 
    metadata <- makeExperimentHubMetadata(pathToPackage)

    ## insert in db
    if(insert) {
        message("inserting metadata in db ...") 
        pushMetadata(metadata, url)
    }

    message("complete!") 
    metadata
}
