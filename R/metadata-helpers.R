### =========================================================================
### readMetadataFromCsv(), makeExperimentHubMetadata()
### -------------------------------------------------------------------------
###

readMetadataFromCsv <- function(pathToPackage) 
{
    meta <- read.csv(file.path(pathToPackage, "inst/extdata/metadata.csv"),
                     colClasses="character", stringsAsFactors=FALSE)
    ## Check columns
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
                 c("ResourceName", "character"))

    expected <- mat[,1]
    missing <- !expected %in% names(meta)
    if (any(missing))
        stop(paste0("missing fields in metadata.csv: ", 
                    paste(expected[missing], collapse=", ")))
    extra<- !names(meta) %in% expected 
    if (any(extra))
        warning(paste0("extra fields in metadata.csv will be ignored: ", 
                    paste(names(meta)[extra], collapse=", ")))

    ## All fields length 1
    apply(meta, 1, 
        function(xx) {
            valid <- sapply(xx, function(field) length(field) == 1L)
            if (any(!valid))
                stop(paste0("all fields in metadata.csv must be a character ",
                     "string of length 1"))
        }
    )

    ## Enforce non-character data type
    meta$TaxonomyId <- as.integer(meta$TaxonomyId)
    meta$Coordinate_1_based <- as.logical(meta$Coordinate_1_based)

    ## Real time assignments
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
    Tags <- strsplit(gsub("\\s", "", description[,"biocViews"]), ",")[[1]]
    lapply(seq_len(nrow(meta)),
        function(x) {
            with(meta[x,], 
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
