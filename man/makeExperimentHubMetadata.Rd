\name{makeExperimentHubMetadata}

\alias{makeExperimentHubMetadata}

\title{
  Make ExperimentHubMetadata objects from csv file of metadata
}

\description{
  Make ExperimentHubMetadata objects from metadata.csv file located in 
  the "inst/extdata/" package directory of an ExperimentHub package.
}

\usage{
  makeExperimentHubMetadata(pathToPackage)
}

\arguments{
  \item{pathToPackage}{
    Full path to data package including the package name; no trailing slash
  }
}

\details{
  \itemize{
    \item{makeExperimentHubMetadata:}{
      Reads the resource metadata in the metadata.csv file into a
      \link{ExperimentHubMetadata} object. The \link{ExperimentHubMetadata}
      is inserted in the ExperimentHub database. Intended for internal
      use or package authors checking the validity of package metadata.
    }
  }
}

\value{
    A list of \code{ExperimentHubMetadata} objects.
}

\seealso{
  \itemize{
    \item \code{\link{addResources}}
    \item \code{\link{readMetadataFromCsv}}
    \item \code{\link{ExperimentHubMetadata}} class
  }
}

\examples{
## makeExperimentHubMetadata() reads data from inst/scripts/metadata.csv 
## into ExperimentHubMetadata objects. These objects are used to insert
## metadata into the production database. This function is used internally
## by addResources() and is not intended to be called directly.

## For an example of how this works we can use the GSE62944 ExperimentHub
## package. Download the source tarball from:

# http://www.bioconductor.org/packages/devel/data/experiment/html/GSE62944.html

## and unpack it. Set 'pathToPackage' to point to the downloaded source. 
## Then call the function:
\dontrun{
makeExperimentHubMetadata("path/to/mypackage")
}
}

\keyword{methods}
