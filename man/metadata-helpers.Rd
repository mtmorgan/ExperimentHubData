\name{makeMetadataFromCsv}

\alias{makeExperimentHubMetadata}
\alias{readMetadataFromCsv}
\alias{metadata}

\title{Metadata File Helpers}

\description{
  Read metadata file for an ExperimentHub resource into R 
}

\usage{
  makeExperimentHubMetadata(pathToPackage)
  readMetadataFromCsv(pathToPackage)
}

\arguments{
  \item{pathToPackage}{
    full path to package
  }
}

\details{
  \itemize{
    \item{makeExperimentHubMetadata:}{
      Called by \code{addResources}. This function reads in the metadata.csv 
      file of a contributed package and creates a \link{ExperimentHubMetadata}
      object to be inserted in the ExperimentHub database.
    }
    \item{readMetadataFromCsv:}{
      Called by \code{makeExperimentHubMetadata}. This function reads in
      the metadata.csv file located in inst/extdata of the 
      \code{pathToPackage}. It performs checks for required columns and 
      data types and can be used by package authors to validate their 
      metadata.csv.

      The rows of metadata.csv represent individual \code{ExperimentHub}
      resources (i.e., data objects) and the columns are the metadata
      fields. All fields should be a single character string of length 1.
      \code{BiocVersion} and \code{Tags} can have multiple comma-separated
      values within the string.

      Required fields in metadata.csv:
      \itemize{
        \item Title: \code{character}. Name of the resource.

        \item Description: \code{character}. Brief description of the resource,
              similar to the 'Description' field in a package DESCRIPTION file.

        \item BiocVersion: \code{character}. All Bioconductor versions
              the resource should be made available for; comma separated.

        \item Genome: \code{character}. Genome.

        \item SourceType: \code{character}. Format of original data, e.g., FASTA,
              BAM, BigWig, etc.

        \item SourceUrl: \code{character}. Location of original data.

        \item SourceVersion: \code{character}. Version of original data.

        \item Species: \code{character}. Species.

        \item TaxonomyId: \code{character(1)}. Taxonomy ID.

        \item Coordinate_1_based: \code{logical}. TRUE if data are 1-based.

        \item DataProvider: \code{character}. Name of original data provider. 

        \item Maintainer: \code{character}. Maintainer name and email in the
          following format: Maintainer Name <username@address>.

        \item RDataClass: \code{character}. R / Bioconductor class the data
          are stored in.

        \item Tags: \code{character}. Free-form search terms to be used with
          \code{query} on an ExperimentHub object. The \code{biocViews} terms
          from the package DESCRIPTION are added to the Tag field when
          adding metadata to the database.
      }
    }
  }
}

\value{
    \code{readMetadataFromCsv()} returns a data.frame and 
    \code{makeExperimetHubMetadata()} returns a list of
    \code{ExperimentHubMetadata} objects.
}

\seealso{
  \itemize{
    \item \code{\link{addResources}}
    \item \code{\link{ExperimentHubMetadata}}
  }
}

\examples{
## TODO
}

\keyword{methods}
