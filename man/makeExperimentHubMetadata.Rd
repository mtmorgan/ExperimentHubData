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
  makeExperimentHubMetadata(pathToPackage, fileName=character())
}

\arguments{
  \item{pathToPackage}{
    Full path to data package including the package name; no trailing slash
  }
  \item{fileName}{Name of single metadata file located in
  "inst/extdata". If none is provided the function looks for a file
  named "metadata.csv".
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

    \itemize{
    \item{Formatting metadata files:}{

      \code{makeExperimentHubMetadata} reads .csv files of metadata
      located in "inst/extdata". Internal functions perform checks for
      required columns and data types and can be used by package authors
      to validate their metadata before submitting the package for
      review.

      The rows of the .csv file(s) represent individual \code{Hub}
      resources (i.e., data objects) and the columns are the metadata
      fields. All fields should be a single character string of length 1.

      Required Fields in metadata file:
      \itemize{
	\item Title: \code{character(1)}. Name of the resource. This can be
	      the exact file name (if self-describing) or a more complete
	      description.

	\item Description: \code{character(1)}. Brief description of the
	      resource, similar to the 'Description' field in a package
	      DESCRIPTION file.

	\item BiocVersion: \code{character(1)}. The first Bioconductor version
	      the resource was made available for. Unless removed from
	      the hub, the resource will be available for all versions
	      greater than or equal to this field.

	\item Genome: \code{character(1)}. Genome.

	\item SourceType: \code{character(1)}. Format of original data, e.g., FASTA,
	      BAM, BigWig, etc.

	\item SourceUrl: \code{character(1)}. Optional location of original
	      data files. Multiple urls should be provided as a comma separated
	      string.

	\item SourceVersion: \code{character(1)}. Version of original data.

	\item Species: \code{character(1)}. Species.

	\item TaxonomyId: \code{character(1)}. Taxonomy ID.

	\item Coordinate_1_based: \code{logical}. TRUE if data are 1-based.

	\item DataProvider: \code{character(1)}. Name of company or institution
	      that supplied the original (raw) data.

	\item Maintainer: \code{character(1)}. Maintainer name and email in the
	      following format: Maintainer Name <username@address>.

	\item RDataClass: \code{character(1)}. R / Bioconductor class the data
	      are stored in, e.g., GRanges, SummarizedExperiment,
	      ExpressionSet etc.

	\item DispatchClass: \code{character(1)}. Determines how data are
	      loaded into R. The value for this field should be
	      \sQuote{Rda} if the data were serialized with \code{save()} and
	      \sQuote{Rds} if serialized with \code{saveRDS}. The filename
	      should have the appropriate \sQuote{rda} or \sQuote{rds}
	      extension.

	      A number of dispatch classes are pre-defined in
	      AnnotationHub/R/AnnotationHubResource-class.R with the suffix
	      \sQuote{Resource}. For example, if you have sqlite files, the
	      AnnotationHubResource-class.R defines SQLiteFileResource so
	      the DispatchClass would be SQLiteFile. Contact
	      maintainer@bioconductor.org if you are not sure which class
	      to use.

	\item Location_Prefix: \code{character(1)}. Do not include this field
	      if data are stored in the Bioconductor AWS S3; it will be
	      generated automatically.

	      If data will be accessed from a location other than AWS S3
	      this field should be the base url.

	\item RDataPath: \code{character(1)}.This field should be the
	      remainder of the path to the resource. The
	      \code{Location_Prefix} will be prepended to
	      \code{RDataPath} for the full path to the resource.
	      If the resource is stored in Bioconductor's AWS S3
	      buckets, it should start with the name of the package associated
	      with the metadata and should not start with a leading
	      slash. It should include the resource file name.

	\item Tags: \code{character() vector}.
	      \sQuote{Tags} are search terms used to define a subset of
	      resources in a \code{Hub} object, e.g, in a call to \code{query}.

	      For ExperimentHub resources, \sQuote{Tags} are automatically
	      generated from the \sQuote{biocViews} in the DESCRIPTION.
	      \sQuote{Tags} values supplied by the user are not be entered in
	      the database and are not part of the formal metadata. This
	      'controlled vocabulary' approach was taken to limit the search
	      terms to a well defined set and may change in the future.

	      \sQuote{Tags} for AnnotationHub resources are a free-form field
	      of search terms defined by the user.  The package name is added
	      as one of the \sQuote{Tags} before the metadata are finalized.
	      Multiple \sQuote{Tags} are specified as a colon separated
	      string, e.g., tags for two resources would look like this:

	      \preformatted{
	      Tags=c("tag1:tag2:tag3", "tag1:tag3")
	      }
      }
      NOTE: The metadata file can have additional columns beyond the 'Required
      Fields' listed above. These values are not added to the Hub database but
      they can be used in package functions to provide an additional level of
      metadata on the resources.
    }
  }
}

\value{
    A list of \code{ExperimentHubMetadata} objects.
}

\seealso{
  \itemize{
    \item \code{\link{addResources}}
    \item \code{\link{ExperimentHubMetadata}} class
    \item \link[AnnotationHubData]{makeAnnotationHubMetadata}
  }
}

\examples{
## makeExperimentHubMetadata() reads data from inst/scripts/<files>.csv
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
