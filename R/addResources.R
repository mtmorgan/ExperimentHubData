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
        url <- getOption("EXPERIMENT_HUB_SERVER_POST_URL")
        if (is.null(url))
            stop(paste0("When 'insert=TRUE' option ",
                        "EXPERIMENT_HUB_SERVER_POST_URL must be set ",
                        "in the global environment or .Rprofile"))
    }

    ## generate metadata
    message("generating metadata ...") 
    metadata <- makeExperimentHubMetadata(pathToPackage)

    ## duplicates and pre-existing records
    all <- sapply(metadata, function(x) basename(x@RDataPath))
    query <- "SELECT rdatapath FROM rdatapaths"
    con <- dbconn(ExperimentHub())
    current <- basename(dbGetQuery(con, query)[,1])
    dbDisconnect(con)

    dups <- duplicated(current)
    if (any(dups))
        warning(paste0("ExperimentHub db has duplicated filenames: ",
                paste(current[dups], collapse=", ")))

    exists <- all %in% current 
    if (any(exists)) {
        warning(paste0(sum(exists), " metadata titles are duplicates of ",
                       "records in ExperimentHub and will not be inserted"))
        metadata <- metadata[!exists]
    }

    ## insert in db
    if (insert) {
        message(paste0("inserting ", sum(!exists), " records ...")) 
        pushMetadata(metadata, url)
    }

    message("complete!") 
    metadata
}
