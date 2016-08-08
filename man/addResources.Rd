\name{addResources}
\alias{addResources}


\title{addResources}

\description{
  Add resource metadata to a local ExperimentHub database
}

\usage{
addResources(pathToPackage, insert = FALSE, ...)
}

\details{
  When \code{insert} is TRUE, \code{addResources} attempts to add the metadata
  to the local database. A warning is thrown if duplicate files names are
  found in the local database or if any filenames in the new metadata match
  those in the existing database. Only metadata records with non-duplicated
  file names are added to the database. The file name is the name of
  the data file with the \sQuote{rda} extension.
}

\arguments{
  \item{pathToPackage}{
    Full path to data package including package name; no trailing slash.
  }
  \item{insert}{
    A \code{logical} to control if metadata are inserted in the ExperimentHub
    db. By default this option is FALSE which is a useful state in which
    to test a new recipe and confirm the metadata fields are correct.
 
    When \code{insert = TRUE}, the "EXPERIMENT_HUB_SERVER_POST_URL" global
    option must be set to the http location of the ExperimentHubServer in the
    global environment or .Rprofile. This option controls Additionally, AWS
    command line tools must be installed on the local machine to push files to
    S3 buckets.  See https://aws.amazon.com/cli/ for installation instructions.
  }
  \item{\dots}{
    TDB. Currently not used.
  }
}

\value{
  A list of \link{ExperimentHubMetadata} objects.
}

\seealso{
  \itemize{
    \item \link{ExperimentHubMetadata}
    \item \link[AnnotationHubData]{AnnotationHubMetadata}
  }
}

\examples{
  \dontrun{
    ## Generate metadata for inspection
    addResources("/home/vobencha/mypackage", insert=FALSE)
    ## Inset metadata into ExperimentHub database
    addResources("/home/vobencha/mypackage", insert=TRUE)
  }
}

\keyword{methods}
