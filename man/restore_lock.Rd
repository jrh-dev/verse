% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/restore_lock.r
\name{restore_lock}
\alias{restore_lock}
\title{Restore the project library}
\usage{
restore_lock(...)
}
\arguments{
\item{...}{Unused arguments, for future development.}
}
\value{
Invisible, the function is called for it's side effects.
}
\description{
Used to restore the project library; all packages contained
within the lock file are installed.
}
\details{
The function does NOT check whether packages are already installed
in the project library when it is called, rather all packages are
re-installed.
}
