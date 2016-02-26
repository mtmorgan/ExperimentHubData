\name{ExperimentHubMetadata-class}
\docType{class}

% Class:
\alias{class:ExperimentHubMetadata}
\alias{ExperimentHubMetadata-class}
\alias{ExperimentHubMetadata}

% Constructors:
\alias{ExperimentHubMetadata}

% Accessors:

% Methods:

% Other:

% Show:
\alias{show,ExperimentHubMetadata-method}


\title{Class \code{"ExperimentHubMetadata"} and methods}

\description{

  \code{ExperimentHubMetadata} is used to represent record(s) in the
  server data base.

}

\usage{
ExperimentHubMetadata(ExperimentHubRoot=NA_character_, 
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
        Location_Prefix='http://s3.amazonaws.com/experimenthub/')
}

\arguments{
    \item{ExperimentHubRoot}{
      \code{character(1)} Prefix of the local path to
      resources to be added to ExperimentHub
    }
    \item{SourceUrl}{
      \code{character()} URL where resource(s) can be found
    }
    \item{SourceType}{
      \code{character()} which indicates what kind of
      resource was initially processed.  The preference is to name the
      type of resource if it's a single file type and to name where the
      resources came from if it is a compound resource.  So Typical
      answers would be like: 'BED','FASTA' or 'Inparanoid' etc.
    }
    \item{SourceVersion}{
      \code{character(1)} Version of original file
    }
    \item{SourceLastModifiedDate}{
      \code{POSIXct()} The date when the
      source was last modified.  Leaving this blank should allow the
      values to be retrieved for you (if your sourceURL is valid).
    }
    \item{SourceMd5}{
      \code{character()} md5 hash of original file
    }
    \item{SourceSize}{
      \code{numeric(1)} Number of bytes in original file
    }
    \item{DataProvider}{
      \code{character(1)} Where did this resource come from?
    }
    \item{Title}{
      \code{character(1)} Title for this resource
    }
    \item{Description}{
      \code{character(1)} Description of the resource
    }
    \item{Species}{
      \code{character(1)} Species name
    }
    \item{TaxonomyId}{
      \code{character(1)} NCBI code
    }
    \item{Genome}{
      \code{character(1)} Name of genome build
    }
    \item{Tags}{
      \code{character()} Free-form tags
    }
    \item{RDataClass}{
      \code{character(1)} Class of derived object (e.g. \sQuote{GRanges})
    }
    \item{RDataDateAdded}{
      \code{POSIXct()} Date added to ExperimentHub; used to determine snapshots.
    }
    \item{RDataPath}{
      \code{character(1)} file path to serialized form
    }
    \item{Maintainer}{
      \code{character(1)} Maintainer name and email address, 
      \sQuote{A Maintainer \url{a.maintainer@email.addr}}
    }
    \item{BiocVersion}{
      \code{character(1)} Under which resource was built
    }
    \item{Coordinate_1_based}{
      \code{logical(1)} Do coordinates start with 1 or 0?
    }
    \item{Location_Prefix}{
      \code{character(1)} This was added for resources where the metadata only
      is stored and the resource itself comes from a third party web site.  The
      location prefix says the base path where the resource is coming from, and
      the default value will be from our own site.
    }
    \item{Notes}{
      \code{character()} Notes about the resource.
    }
}

\value{
  An instance of the class.
}

\seealso{
  \itemize{
    \item \link[AnnotationHubData]{AnnotationHubMetadata}
  }
}

\examples{
## TODO
}

\keyword{classes}
