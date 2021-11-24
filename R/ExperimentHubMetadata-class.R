### =========================================================================
### ExperimentHubMetadata objects
### -------------------------------------------------------------------------
###

setClass("ExperimentHubMetadata",
    contains="HubMetadata",
    representation(
        ExperimentHubRoot="character"
    ),
    prototype = prototype(
        ExperimentHubRoot=NA_character_
    )
)

## -----------------------------------------------------------------------------
## Constructor
##
globalVariables(c("BiocVersion", "Coordinate_1_based", "DataProvider",
                  "Description", "DispatchClass", "Genome", "Location_Prefix",
                  "Maintainer", "RDataClass", "RDataDateAdded", "RDataPath",
                  "SourceType", "SourceUrl", "SourceVersion", "Species",
                  "TaxonomyId", "Title", "PreparerClass"))

makeExperimentHubMetadata <- function(pathToPackage, fileName=character())
{
    stopifnot(length(fileName) <= 1)
    description <- read.dcf(file.path(pathToPackage, "DESCRIPTION"))
    .views <- strsplit(gsub("\\s", "", description[,"biocViews"]), ",")[[1]]
    if (length(.views) <= 1) stop("Add 2 or more biocViews to your DESCRIPTION. Required: ExperimentHub or ExperimentHubSoftware")
     AnnotationHubData:::.checkValidViews(.views)
    ## filter views for common/not useful terms
    .views = setdiff(.views,
                     c("ExperimentData","ExperimentHub","SpecimenSource","PackageTypeData",
                       "Software", "AssayDomain", "BiologicalQuestion","ResearchField", "Technology", "WorkflowStep")
                     )


    meta <- AnnotationHubData:::.readMetadataFromCsv(pathToPackage, fileName=fileName)
    .package <- unname(description[,"Package"])
    meta$PreparerClass <- .package


    ## check for Tags in metadata
    ## filter out packageName as already tracked in database with preparerclass
    if (length(meta$Tags)){
        .tags <- strsplit(meta$Tags, ":")
        .tags <- lapply(.tags,
                        FUN<- function(x, views, packageName){
                            setdiff(sort(unique(c(x, views))), packageName)},
                        views = .views, packageName=.package)
        if (any(unlist(lapply(.tags, FUN=length)) < 1))
            stop("Add 1 or more Tags to each resource by either\n",
                 "  adding 'Tags' column to metadata or\n",
                 "  adding additional meaningful biocViews terms in DESCRIPTION")
    }else{
        if (length(.views)){
            .tags = vector("list", nrow(meta))
            .tags <- lapply(.tags,
                            FUN<- function(x, views, packageName){
                                setdiff(sort(unique(views)), packageName)},
                            views = .views, packageName=.package)
            if (any(unlist(lapply(.tags, FUN=length)) < 2))
                stop("Add 2 or more Tags to each resource by either\n",
                     "  adding 'Tags' column to metadata or\n",
                     "  adding additional meaningful biocViews terms in DESCRIPTION")
        }else{
            stop("Add 1 or more Tags to each resource by either\n",
                 "  adding 'Tags' column to metadata or\n",
                 "  adding additional meaningful biocViews terms in DESCRIPTION")
        }
    }
    
 
    
    .RDataPaths <- meta$RDataPath
    .Location_Prefix <- meta$Location_Prefix
    if (any(.Location_Prefix %in% "https://bioconductorhubs.blob.core.windows.net/annotationhub/")){
        .Location_Prefix[which(.Location_Prefix == "https://bioconductorhubs.blob.core.windows.net/annotationhub/")] = 'https://bioconductorhubs.blob.core.windows.net/experimenthub/'
    }
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
                                       RDataClass=RDataClass, Tags=.tags[[x]],
                                       RDataDateAdded=RDataDateAdded,
                                       RDataPath=.RDataPaths[[x]],
                                       DispatchClass=DispatchClass,
                                       PreparerClass=PreparerClass,
                                       Location_Prefix=.Location_Prefix[[x]]))
        }
    )
}


ExperimentHubMetadata <-
    function(ExperimentHubRoot=NA_character_,
        BiocVersion=BiocManager::version(),
        SourceUrl=NA_character_,   ## not necessary because no downloads
        SourceType=NA_character_,
        SourceVersion=NA_character_,
        SourceLastModifiedDate=as.POSIXct(NA_character_),
        SourceMd5=NA_character_,
        SourceSize=NA_real_,
        DataProvider=NA_character_,
        Title=NA_character_,
        Description=NA_character_,
        Maintainer=NA_character_,
        Species=NA_character_,
        TaxonomyId=NA_integer_,
        Genome=NA_character_,
        Tags=NA_character_,
        RDataClass=NA_character_,
        RDataDateAdded=as.POSIXct(NA_character_),
        RDataPath=NA_character_,
        Coordinate_1_based=TRUE,
        Notes=NA_character_,
        DispatchClass=NA_character_,
        PreparerClass=NA_character_,
        Location_Prefix='https://bioconductorhubs.blob.core.windows.net/experimenthub/')
{
    ## FIXME: move these checks to a general validity method
    ##        on HubMetadata that can be reused
    if (missing(TaxonomyId) | is.na(TaxonomyId)) {
        if (!is.na(Species) &&
            requireNamespace("GenomeInfoDb", quietly=TRUE))
            TaxonomyId <- GenomeInfoDb:::lookup_tax_id_by_organism(Species)
        else
            TaxonomyId <- NA_integer_
    }
    TaxonomyId <- as.integer(TaxonomyId)
    if(!(isSingleInteger(TaxonomyId) || is.na(TaxonomyId)))
        stop(paste0("ExperimentHubMetdata objects can contain",
                    " only one taxonomy ID or NA"))

    # This is already done in readMetadataFromCsv?
    #Coordinate_1_based <- as.logical(Coordinate_1_based)

    if (missing(RDataPath))
        stop("RDataPath must be defined")

    RDataDateAdded <-
        as.POSIXct(strsplit(
            as.character(RDataDateAdded), " ")[[1]][1], tz="GMT")

    AnnotationHubData:::.checkThatSingleStringAndNoCommas(SourceType)
    AnnotationHubData:::.checkThatSingleStringAndNoCommas(Location_Prefix)
    AnnotationHubData:::.checkThatSingleStringOrNA(Genome)
    AnnotationHubData:::.checkThatSingleStringOrNA(Species)
    AnnotationHubData:::.checkThatSingleStringOrNAAndNoCommas(SourceVersion)
    AnnotationHubData:::.checkRDataClassConsistent(RDataClass)
    AnnotationHubData:::.checkValidMaintainer(Maintainer)
    AnnotationHubData:::.checkFileLengths(RDataPath, DispatchClass)
    AnnotationHubData:::.checkValidSingleString(Title)
    AnnotationHubData:::.checkValidSingleString(Description)

    new("ExperimentHubMetadata",
        ExperimentHubRoot=ExperimentHubRoot,
        HubRoot=ExperimentHubRoot,
        BiocVersion=package_version(BiocVersion),
        Coordinate_1_based=Coordinate_1_based,
        DataProvider=DataProvider,
        Description=Description,
        Genome=Genome,
        Maintainer=Maintainer,
        Notes=Notes,
        RDataClass=RDataClass,
        RDataDateAdded=as.POSIXct(RDataDateAdded),
        RDataPath=RDataPath,
        SourceUrl=SourceUrl,
        SourceVersion=SourceVersion,
        SourceType=SourceType,
        Species=Species,
        Tags=Tags,
        TaxonomyId=TaxonomyId,
        Title=Title,
        Location_Prefix=Location_Prefix,
        DispatchClass=DispatchClass,
        PreparerClass=PreparerClass,
        ## FIXME: how to determine
        SourceSize=NA_real_,
        SourceMd5=NA_character_,
        SourceLastModifiedDate=as.POSIXct(NA_character_), #from url in AHD
        ## NOTE: not relevant
        Recipe=NA_character_
    )
}


## -----------------------------------------------------------------------------
## Validity
##
setValidity("ExperimentHubMetadata",function(object) {

    AnnotationHubData:::.ValidHubs(object)
})

## ------------------------------------------------------------------------------
## show
##

setMethod("show", "ExperimentHubMetadata",
   function(object)
{
    cat(class(object), " object: \n", sep='')
    for (slt in c("Title", "Description", "BiocVersion", "Genome",
                  "Species", "TaxonomyId", "Location_Prefix",
                  "RDataClass", "RDataDateAdded",
                  "RDataPath", "SourceLastModifiedDate", "SourceType",
                  "SourceUrl", "SourceVersion", "Tags", "DispatchClass")) {
        value <- slot(object, slt)
        txt <- paste0(slt, ": ", paste0(as.character(value), collapse=" "))
        cat(strwrap(txt), sep="\n  ")
    }
    cat("Package: ", object@PreparerClass, '\n', sep='')
})
