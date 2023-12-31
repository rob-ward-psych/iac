% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/iac_networks.R
\name{connect_units}
\alias{connect_units}
\title{Connect units in a network}
\usage{
connect_units(
  network,
  from,
  to,
  weight,
  directives = "reciprocal",
  from_dims = c(),
  to_dims = c(),
  verbose = FALSE
)
}
\arguments{
\item{network}{The network getting connected up, usually created by
\code{read_network()}, but probably \code{new_network()} in this case}

\item{from, to}{All the \emph{from} units are connected to all the \emph{to} units. \code{from}
and \code{to} can be a pool name, or a vector of string indices which will be
converted into indices into network$weights.}

\item{weight}{The numeric value of the connection weight}

\item{directives}{A vector of strings giving optional detail about the
connection. Legal directives are: "oneway", "reciprocal" (default), "add",
"self", and "others". See the documentation on spatial networks.}

\item{from_dims, to_dims}{In pools with spatial organisation, these show the
corresponding dimensions of the pools. See the documentation on spatial
networks.}

\item{verbose}{Prints a description of the units being connected to the
console (default = FALSE)}
}
\value{
A network where \code{network$weights} is updated with the new connection
values
}
\description{
Used internally by \code{read_network()}.
\strong{Not needed} if you are using \code{read_network()} to create your network.
These and other functions are only used if you are creating your network
direct from code. Know that when a network is read from .yaml file, it uses
these routines so you don't have to.
}
\details{
\code{connect_units()} is a horrible monster workhorse function internally, but it is called
using the same keywords as used when specifying networks with the .yaml file
and \code{read_network()}
}
