\name{addResources}
\alias{addResources}


\title{addResources}

\description{
  Add new resources to ExperimentHub 
}

\usage{
addResources(pathToPackage, insert = FALSE, ...)
}

\arguments{
  \item{pathToPackage}{
    Local path to data package; no trailing slash.
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
    Arguments passed to \code{pushResources} and \code{pushMetadata}. See 
    the man pages for a description of arguments.
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
## TODO
}

\keyword{methods}
