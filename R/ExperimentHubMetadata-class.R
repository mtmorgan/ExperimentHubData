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
    ## Differences from makeAnnotationHubMetadata:
    ## - package put in PreparerClass slot
    stopifnot(length(fileName) <= 1)
    meta <- AnnotationHubData:::.readMetadataFromCsv(pathToPackage, fileName=fileName)
    package <- basename(pathToPackage)
    meta$PreparerClass <- package

    if ("tags" %in% tolower(names(meta)))
        message("Tags are specified by biocViews entry in the",
                " DESCRIPTION file.\nIgnoring Tags in the metadata file.")
    description <- read.dcf(file.path(pathToPackage, "DESCRIPTION"))
    .tags <- strsplit(gsub("\\s", "", description[,"biocViews"]), ",")[[1]]
    if (length(.tags) <= 1) stop("Add 2 or more biocViews to your DESCRIPTION")
    .checkValidViews(.tags, "ExperimentData")
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
                                       RDataClass=RDataClass, Tags=.tags,
                                       RDataDateAdded=RDataDateAdded,
                                       RDataPath=RDataPath,
                                       DispatchClass=DispatchClass,
                                       PreparerClass=PreparerClass))
        }
    )
}


.checkValidViews <- function(views, repo){

    msg = list()
    biocViewsVocab <- NULL
    data("biocViewsVocab", package="biocViews", envir=environment())
    # check all valid terms
    if (!all(views %in% nodes(biocViewsVocab))){
        badViews <- views[!(views %in% graph::nodes(biocViewsVocab))]
        badViewsVec <- paste(badViews, collapse=", ")
        msg["invalid"] = paste0("Invalid biocViews term[s].\n    ", badViewsVec, "\n")
    }
    # check all come from same biocViews main category
    parents <- unlist(lapply(views, BiocCheck:::getParent, biocViewsVocab), use.names=FALSE)
    if (!all(parents == repo))
        msg["Category"] = paste0("All biocViews terms must come from the ", repo, " category.\n")
    # check that hub term present
    if (repo == "AnnotationData" || repo == "ExperimentData"){
        repo = paste0(gsub(repo, pattern="Data", replacement=""), "Hub")
        if (!(repo %in% views))
            msg["Hub"] = paste0("Please add ", repo, " to biocViews list in DESCRIPTION.\n")
    }
    if (length(msg) != 0){
        myfunction <- function(index, msg){paste0("[", index, "] ", msg[index])}
        fmt_msg <- unlist(lapply(seq_along(msg), msg = msg, FUN=myfunction))
        stop("\n",fmt_msg)
    }
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
        Location_Prefix='http://s3.amazonaws.com/experimenthub/')
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
