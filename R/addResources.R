## =========================================================================
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
        ## check if any new records already exist
        new <- sapply(metadata, function(x) basename(x@RDataPath))
        query <- "SELECT rdatapath FROM rdatapaths"
        con <- dbconn(ExperimentHub())
        current <- basename(dbGetQuery(con, query)[,1])
        dbDisconnect(con)

        dups <- duplicated(current)
        if (any(dups))
            warning(paste0("sqlite db has duplicated filenames: ",
                    paste(current[dups], collapse=", ")))
        exists <- new %in% current 
        if (any(exists)) {
            warning(paste0("metadata records with filenames that exist in ",
                    "the sqlite db were not inserted: ", 
                    paste(new[exists], collapse=", ")))
            metadata <- metadata[!exists]
        }

        message("inserting metadata in db ...") 
        pushMetadata(metadata, url)
    }

    message("complete!") 
    metadata
}
