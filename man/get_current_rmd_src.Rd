% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/odg_graphics.R
\name{get_current_rmd_src}
\alias{get_current_rmd_src}
\title{Find Name of Current Rmd-Source Document}
\usage{
get_current_rmd_src(ps_path, ps_cwd)
}
\arguments{
\item{ps_path}{name of and path to odg-graphics file}

\item{ps_cwd}{current working directory}
}
\value{
s_rmd_src_result resulting Rmd-source file
}
\description{
Given the value passed by the parameter ps_path, we can
assume that there must be a Rmd-source file in the
current working directory ps_cwd which contains the path
to the odg graphics file given in ps_path. Hence, we
can search through all Rmd-files in ps_cwd for the
value in ps_path. If we find a match in a single Rmd-source
file, we return the name of that file, otherwise we
return NULL.
}
