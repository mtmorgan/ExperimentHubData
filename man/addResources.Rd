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
  This function is used by the Bioconductor Core team to add new
  metadata to the production database.

  When \code{insert} is TRUE, \code{addResources} attempts to add the metadata
  to the local database. (NOTE: A local database can be created with the
  ExperimentHub docker). Records in ExperimentHub must have unique file names.
  If the new metadata have duplicate file names a warning is thrown
  and the records are omitted from those added to the database.

  This function does not add data to an AWS S3 bucket. ExperimentHub packages
  do not have 'recipes' that generate data on the fly. Instead, data files 
  are provided by the maintainer in final form and added to the appropriate
  S3 location in a separate step.
}

\arguments{
  \item{pathToPackage}{
    Full path to data package including package name.
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
