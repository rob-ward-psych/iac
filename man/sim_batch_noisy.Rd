% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/iac_cycle.R
\name{sim_batch_noisy}
\alias{sim_batch_noisy}
\title{Run sims with extra noise on specific units and/or connections}
\usage{
sim_batch_noisy(
  network,
  nsims,
  ncycles,
  noisy_units = NULL,
  xtra_unit_noise = 0,
  noisy_cxn_mat = NULL,
  xtra_cxn_noise = 0,
  randwalk = TRUE
)
}
\arguments{
\item{network}{The network, with external inputs set and noise > 0}

\item{nsims}{Number of simulations to run}

\item{ncycles}{Number of cycles for a single simulation}

\item{noisy_units}{A vector of unitnames which will be disrupted}

\item{xtra_unit_noise}{The random noise added as input to any noisy_units,
called as runif(min = -xtra_unit_noise, max = xtra_unit_noise) and applied
as external input ot the noisy units.}

\item{noisy_cxn_mat}{An Nx2 matrix format, where N = the number of
connections and \verb{[, 1]} = row of weight matrix (sender), and \verb{[, 2]} = col of
weight matrix (receiver). The connections corresponding to the sender and receiver
units of \code{network$weights} will be disrupted}

\item{xtra_cxn_noise}{The random noise to be added to the weights if noisy_cxn_mat
is specified.}

\item{randwalk}{If TRUE (default), then all the specified noisy connections will
accumulate noise over cycles. If set to FALSE, then the the noisy connections
are reset to their original values each cycle before noise is added. This
parameter only has effect on weights specified in noisy_cxn_mat.}
}
\value{
A dataframe containing the activations (indexed by the unitnames of
the network), for each cycle (column "cycle") for each simulation
(column "simno"). This output can then be used directly by RTs_generate()
}
\description{
This works like sim_batch, but adds noise to disrupt processing. Noise can be
added to the activation of specific units during cycle(), and also noise can
be added to the weights of specific connections. To add noise to specific
units, use the noisy_units and xtra_unit_noise parameters. To add noise to
the weights, use the noisy_cxn_src, noisy_cxn_dst, and xtra_cxn_noise
parameters.
}
\details{
These effects are in addition to the global noise parameter.
}
