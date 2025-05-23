% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_functions.R
\name{export_xvg}
\alias{export_xvg}
\title{export xvg data}
\usage{
export_xvg(xvg_data, file, sep = "\\t", row.names = FALSE, merge = FALSE, ...)
}
\arguments{
\item{xvg_data}{An object of class \code{xvg_data}, or a list of \code{xvg_data} objects, as returned by \code{read_xvg()}.}

\item{file}{Path to the output file (any extension is acceptable).}

\item{sep}{Field separator (e.g., "\\t" for TSV, "," for CSV). Default is "\\t".}

\item{row.names}{Logical, whether to write row names. Default is FALSE.}

\item{merge}{Logical, whether to merge multiple xvg_data objects before exporting. Default is FALSE.}

\item{...}{Additional arguments passed to \code{write.table()}.}
}
\value{
\if{html}{\out{<div class="sourceCode">}}\preformatted{    Invisibly returns the path to the written file.
}\if{html}{\out{</div>}}
}
\description{
write the data component of an \code{xvg_data} object (or multiple objects) to a delimited text file,
controlled via the \code{sep} parameter rather than file extension detection.
}
\examples{
\dontrun{
xvg <- read_xvg(system.file("extdata/rmsd.xvg", package = "xvm"))
# Export as TSV
export_xvg(xvg, "rmsd.tsv", sep = "\t")
# Export as CSV
export_xvg(xvg, "rmsd.csv", sep = ",")
}
}
