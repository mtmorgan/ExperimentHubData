### =========================================================================
### readMetadataFromCsv(), makeExperimentHubMetadata()
### -------------------------------------------------------------------------
###

readMetadataFromCsv <- function(pathToPackage) 
{
    mat <- rbind(c("Title", "character"),
                 c("Description", "character"),
                 c("BiocVersion", "character"),
                 c("Genome", "character"),
                 c("SourceType", "character"),
                 c("SourceUrl", "character"),
                 c("SourceVersion", "character"),
                 c("Species", "character"),
                 c("TaxonomyId", "integer"),
                 c("Coordinate_1_based", "logical"),
                 c("DataProvider", "character"),
                 c("Maintainer", "character"),
                 c("RDataClass", "character"),
                 c("DispatchClass", "character"),
                 c("Tags", "character"),
                 c("ResourceName", "character"))
    meta <- read.csv(file.path(pathToPackage, "inst/extdata/metadata.csv"),
                     col.names=mat[,1], colClasses=mat[,2],
                     stringsAsFactors=FALSE)

    ## Rogue columns
    expected <- mat[,1]
    missing <- !expected %in% names(meta)
    if (any(missing))
        stop(paste0("missing fields in metadata.csv: ", 
                    paste(expected[missing], collapse=", ")))
    invalid <- !names(meta) %in% expected 
    if (any(invalid))
        warning(paste0("invalid fields in metadata.csv will be ignored: ", 
                    paste(names(meta)[invalid], collapse=", ")))

    ## All fields must be length 1
    apply(meta, 1, 
        function(xx) {
            valid <- sapply(xx, function(field) length(field) == 1L)
            if (any(!valid))
                stop(paste0("all fields in metadata.csv must be a character ",
                     "string of length 1"))
        }
    )

    meta$RDataDateAdded <- rep(Sys.time(), nrow(meta))
    package <- basename(pathToPackage)
    meta$RDataPath <- paste0(package,"/",meta$ResourceName)
    meta$PreparerClass <- package 
    meta
}

makeExperimentHubMetadata <- function(pathToPackage) 
{
    meta <- readMetadataFromCsv(pathToPackage)
    description <- read.dcf(file.path(pathToPackage, "DESCRIPTION"))
    bcv <- unlist(strsplit(description[, "biocViews"], ",", fixed=TRUE),
                  use.names=FALSE)
    apply(meta, 1, 
        function(xx) {
        browser()
            ## BiocVersion and Tags can be comma separated
            xx["BiocVersion"] <- 
                strsplit(as.character(xx["BiocVersion"]), ",", fixed=TRUE)
            tags <- unlist(strsplit(as.character(xx["Tags"]), ",", fixed=TRUE)) 
            xx["Tags"] <- list(c(tags, bcv))
            with(xx, 
                 ExperimentHubMetadata(Title=Title, Description=Description, 
                                       BiocVersion=BiocVersion, Genome=Genome, 
                                       SourceType=SourceType, 
                                       SourceUrl=SourceUrl,
                                       SourceVersion=SourceVersion, 
                                       Species=Species, TaxonomyId=TaxonomyId,
                                       Coordinate_1_based=Coordinate_1_based, 
                                       DataProvider=DataProvider,
                                       Maintainer=Maintainer, 
                                       RDataClass=RDataClass, Tags=Tags, 
                                       RDataDateAdded=RDataDateAdded, 
                                       RDataPath=RDataPath, 
                                       DispatchClass=DispatchClass,
                                       PreparerClass=PreparerClass)) 
        }
    )
}
