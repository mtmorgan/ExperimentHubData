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

ExperimentHubMetadata <-
    function(ExperimentHubRoot=NA_character_, 
        BiocVersion=biocVersion(),
        SourceUrl=NA_character_, 
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
        Location_Prefix='http://s3.amazonaws.com/experimenthub/')
{
    ## FIXME: move these checks to a general validity method
    ##        on HubMetadata that can be reused
    if (is.na(TaxonomyId)) {
        if (!is.na(Species) &&
            requireNamespace("AnnotationHubData", quietly=TRUE))
            TaxonomyId <- GenomeInfoDb:::.taxonomyId(Species)
    }
    ## FIXME: where should these coercions be handled?
    Coordinate_1_based <- as.logical(Coordinate_1_based)
    TaxonomyId <- as.integer(TaxonomyId)
    if(!(isSingleInteger(TaxonomyId) || is.na(TaxonomyId)))
        stop(paste0("ExperimentHubMetdata objects can contain",
                    " only one taxonomy ID or NA"))

    if(any(is.na(SourceUrl)))
        stop(paste0("ExperimentHubMetdata SourceUrl slot cannot contain NAs"))

    if (missing(RDataPath)) { 
        ## Add two characters: one for substr starting AT clipChars
        ## and one for extra slash
        clipChars <- nchar(Location_Prefix) + 2 
        RDataPath <- substr(SourceUrl, clipChars, nchar(SourceUrl))
    }

    RDataDateAdded <-
        as.POSIXct(strsplit(
            as.character(RDataDateAdded), " ")[[1]][1], tz="GMT")

    lapply(c(SourceType, Location_Prefix, RDataClass),
        AnnotationHubData:::.checkThatSingleStringAndNoCommas) 
    lapply(c(Genome, Species), 
        AnnotationHubData:::.checkThatSingleStringOrNA)
    AnnotationHubData:::.checkThatSingleStringOrNAAndNoCommas(SourceVersion)

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

## ------------------------------------------------------------------------------
## Getters and setters
## 

## TODO

## -----------------------------------------------------------------------------
## Validity 
## 

## TODO

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
