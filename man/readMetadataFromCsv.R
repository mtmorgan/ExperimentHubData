\name{readMetadataFromCsv}

\alias{readMetadataFromCsv}

\title{
  Read metadata file for ExperimentHub resources into a data.frame
}

\description{
  Read metadata file for ExperimentHub resources into a data.frame 
}

\usage{
  readMetadataFromCsv(pathToPackage)
}

\arguments{
  \item{pathToPackage}{
    full path to package
  }
}

\details{
  \itemize{
    \item{readMetadataFromCsv:}{
      Reads the meatdata.csv file located in inst/extdata of the
      \code{pathToPackage}. The function performs checks for required columns
      and data types and can be used by package authors to validate their
      metadata.csv.  The function is used internally by
      \code{makeExperimentHubMetadata}.

      The rows of metadata.csv represent individual \code{ExperimentHub}
      resources (i.e., data objects) and the columns are the metadata
      fields. All fields should be a single character string of length 1.

      Required fields in metadata.csv:
      \itemize{
        \item Title: \code{character}. Name of the resource.

        \item Description: \code{character}. Brief description of the resource,
              similar to the 'Description' field in a package DESCRIPTION file.

        \item BiocVersion: \code{character}. The first Bioconductor version
              the resource was made available for. Unless removed from
              the hub, the resource will be available for all versions 
              greater than or equal to this field.

        \item Genome: \code{character}. Genome.

        \item SourceType: \code{character}. Format of original data, e.g., FASTA,
              BAM, BigWig, etc.

        \item SourceUrl: \code{character}. Optional location of original
              data files.

        \item SourceVersion: \code{character}. Version of original data.

        \item Species: \code{character}. Species.

        \item TaxonomyId: \code{character(1)}. Taxonomy ID.

        \item Coordinate_1_based: \code{logical}. TRUE if data are 1-based.

        \item DataProvider: \code{character}. Name of original data provider. 

        \item Maintainer: \code{character}. Maintainer name and email in the
              following format: Maintainer Name <username@address>.

        \item RDataClass: \code{character}. R / Bioconductor class the data
              are stored in.
      }

      Note there is no \sQuote{Tags} field in metadata.csv. The metadata
      \sQuote{Tags} are taken from the \sQuote{biocViews} field in the
      package DESCRIPTION file. \sQuote{Tags} are used as search terms and must 
      be valid ExperimentalData biocViews.
    }
  }
}

\value{
    A data.frame. 
}

\seealso{
  \itemize{
    \item \code{\link{addResources}}
    \item \code{\link{makeExperimentHubMetadata}}
    \item \code{\link{ExperimentHubMetadata}} class
  }
}

\examples{

## Each resource needs a separate row of metadata. This example is for a
## single resource. If you have multiple resources the arguments below
## would be character vectors that produced multiple rows in the data.frame.

meta <- data.frame(
    Title = "RNA-Sequencing dataset from study XYZ",
    Description = paste0("RNA-seq data from study XYZ containing 10 normal ",
                         "and 10 tumor samples represented as a",
                         "SummarizedExperiment")
    BiocVersion = "3.3",
    Genome = "GRCh38",
    SourceType = "BAM",
    SourceUrl = "http://www.path/to/original/data/file",
    SourceVersion = "Jan 01 2016",
    Species = "Homo sapiens",
    TaxonomyId = 9606,
    Coordinate_1_based = TRUE,
    DataProvider = "GEO",
    Maintainer = "Your Name <youremail@provider.com>",
    RDataClass = "SummarizedExperiment",
    DispatchClass = "SummarizedExperiment",
    ResourceName = "DescriptiveName.Rda")
)

## Write the data out as 'metadata.csv' and put in the inst/extdata 
## package directory.
write.csv(meta, file="metadata.csv", row.names=FALSE)

## Test metadata.csv with readMetadataCsv():
\dontrun{
readMetadataFromCsv("path/to/mypackage")
}

}

\keyword{methods}
