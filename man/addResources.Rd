\name{addResources}
\alias{addResources}


\title{addResources}

\description{
  Add new resources to ExperimentHub 
}

\usage{
addResources(pathToPackage, metadataOnly = TRUE, insert = FALSE, ...)
}

\arguments{
  \item{pathToPackage}{
    Local path to data package; no trailing slash.
  }
  \item{metadataOnly}{
    A \code{logical} to specify the processing of metadata only or both
    metadata and data files.

    When FALSE, metadata are generated and data files are downloaded,
    processed and pushed to their final location in S3 buckets. 
    \code{metadata = TRUE} produces only metadata and is useful for 
    testing.
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

\details{
   \itemize{
     \item addResources: 

           \code{addResources} is responsible for creating and inserting
           metadata records as well as pushing data files to AWS S3 buckets.
 
           By manipulating the \code{metadataOnly} and \code{insert}
           arguments the insertion of metadata can be decoupled from the pushing
           of data files to S3. This is useful for testing or performing a
           single step vs both.

      \item global options: 

            TODO:
  }
}

\value{
  A list of \code{ExperimentHubMetadata} objects.
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
