% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/iac_networks.R
\name{new_network}
\alias{new_network}
\title{Create a new network in code}
\usage{
new_network()
}
\value{
A list which is ready to be populated as a network
}
\description{
Used internally by \code{read_network()}.
\strong{Not needed} if you are using \code{read_network()} to create your network.
These and other functions are only used if you are creating your network
direct from code. Know that when a network
is read from .yaml file, it uses these routines so you don't have to.
}
\seealso{
add_pool(), connect_units()
}
