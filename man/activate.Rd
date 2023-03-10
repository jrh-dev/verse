% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/activate.r
\name{activate}
\alias{activate}
\title{Activate verse}
\usage{
activate(...)
}
\arguments{
\item{...}{Unused arguments, for future development.}
}
\value{
Invisible, the function is called for it's side effects.
}
\description{
Used to activate an existing verse setup. Activation requires
the current working directory to contain a custom \code{.Rprofile} and
\code{verse.lock} file created by verse.
}
\details{
Once activated packages are installed in the project library using
\code{verse::install()}.

Packages within the private library are checked for compliance with
the lock file upon activation. If the private library is not found then it
will be created and rebuilt with package installation taking place based on
the lock file.

Base packages are not recorded in the lock file and are accessed according
to the users additional library paths in the way they would if verse was not
being used.
}
