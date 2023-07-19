
#' Run the network
#'
#' The network is run in discrete cycles. Each cycle, the input for all units
#' is calculated, and then activation values for all units are updated.
#'
#' The first step of a cycle
#' is determining the _input_ coming in to each unit. The input to a
#' unit is based on the weighted activity of units connecting to it, and
#' may optionally include random noise.
#'
#' The second step of the cycle is to _update_ unit activations based on their
#' input. The activation function is direct from McClelland & Rumelhart (1981).
#'
#'
#' @param network A network, usually created with `read_net()`
#' @param ncycles The number of cycles to run (default = 1)
#' @param verbose Prints the cycle number and current sum-squared activation
#' difference from the previous cycle
#' @param warnings If TRUE, will give a warning if there's no external input (default TRUE)
#' @param OG If TRUE, will run network cycles using loop-based code based on
#' McClelland's original PDP Handbook, link below. Otherwise runs a vectorised version which
#' is not optimised but much faster, especially with networks of 100+ units
#' (default FALSE)
#' @return A network with updated activation state and activation log,
#'  suitable for `plot_log()`
#' @seealso read_net(), set_external(), plot_log(), reset()
#' @export
#'
cycle = function(network, ncycles = 1,
                 warnings = TRUE, verbose = FALSE, OG = FALSE) {

  # OG refers to whether cycle uses the original getinput and update
  # functions from McClelland's textbook (OG = TRUE), or my own more vectorised
  # versions (OG = FALSE). The original have an extremely clear structure
  # of for-loops and look bullet-proof. The revisions I've made seem to
  # produce the same results within a certain floating point error, but
  # will be quite a bit faster for large networks (eg, 15x for network of
  # 300 units)

  # getinput gathers all the incoming activations for the units
  getinput = function(activations, weights, extinput, estr, alpha, gamma,
                      noise = 0, OG=FALSE) {
    nunits = length(activations)
    if(OG) {
      # original-gangster loop-based version directly from McClelland's original PDP Handbook
      # https://web.stanford.edu/group/pdplab/originalpdphandbook/Chapter%202.pdf
      netinput = excitation = inhibition = rep(0, nunits)
      for(i in 1:nunits) {
        excitation[i] = inhibition[i] = 0
        for(j in 1:nunits) {
          if(activations[j] > 0) {
            if(weights[i, j] > 0) {
              excitation[i] = excitation[i] + weights[i, j] * activations[j]
            }
            else if(weights[i, j] < 0) {
              inhibition[i] = inhibition[i] + weights[i, j] * activations[j]
            }
          }
        }
        netinput[i] = estr*extinput[i] + alpha*excitation[i] + gamma*inhibition[i]
      }
      if(noise > 0) {
        netinput = netinput + stats::rnorm(nunits, 0, noise)
      }
    }
    else {
      # RW's vectorised version of the original
      weights_positive = ifelse(weights > 0, weights, 0)
      weights_negative = ifelse(weights < 0, weights, 0)
      activeunits = ifelse(activations > 0, activations, 0)
      excitation = colSums(weights_positive * activeunits) * alpha
      inhibition = colSums(weights_negative * activeunits) * gamma
      external = extinput * estr
      netinput = external + excitation + inhibition
      if(noise > 0) {
        netinput = netinput + stats::rnorm(nunits, 0, noise)
      }
    }
    return(netinput)
  }

  update = function(activations, netinput, min, max, rest, decay, OG) {
    if(OG) {
      # original gangster loop version based directly on McClelland's original PDP Handbook
      # https://web.stanford.edu/group/pdplab/originalpdphandbook/Chapter%202.pdf
      for(i in 1:length(activations)) {
        if(netinput[i] > 0) {
          activations[i] = activations[i] +
            (max - activations[i]) * netinput[i] -
            decay * (activations[i] - rest)
        }
        else {
          activations[i] = activations[i] +
            (activations[i] - min) * netinput[i] -
            decay * (activations[i] - rest)
        }
      }
    }
    else {
      # vectorised version of the original
      input_positive = ifelse(netinput > 0, netinput, 0)
      input_negative = ifelse(netinput < 0, netinput, 0)
      delta = (max - activations) * input_positive +
        (activations - min) * input_negative
      delta = delta - decay * (activations - rest)
      activations = activations + delta
    }
    activations[activations > max] = max
    activations[activations < min] = min
    return(activations)
  }

  # checks and warnings before proceeding
  # None of this particularly relates to the algorithm, just checking that
  # parameters and network seem to be reasonably specified
  preflight = function(ncycles) {
    # make sure the required params are defined
    required_params = c('alpha', 'gamma', 'estr', 'max', 'min', 'rest', 'decay', 'noise')
    if(!(setequal(required_params, names(network$params)))) {
      missingp = setdiff(required_params, names(network$params))
      extrap = setdiff(names(network$params), required_params)
      if(length(missingp) > 0) {
        message(sprintf('Missing network parameter(s): %s', paste(missingp, collapse=', '))) }
      if(length(extrap) > 0) {
        message(sprintf('Unrecognised network parameter(s): %s', paste(extrap, collapse=', '))) }
      stop()
    }
    if(is.null(ncycles)) {
      stop('cycle() called without specifying ncycles')
    }
    # check for easy to make errors when running the network
    if(warnings & all(network$external == 0)) {
      warning('No external input specified, change with set_external()')
    }
    if(warnings & dim(network$log)[1] == 0 & any(network$activations != network$params[['rest']])) {
      warning('The log shows this is the first cycle for the network, but activations not at resting levels; use reset() to intitialise')
    }
    return(TRUE)
  }

  # alloc_log appends ncycles worth of space into the log
  alloc_log = function(ncycles) {
    log = data.frame(matrix(NA, nrow = ncycles, ncol = 1 + network$nunits))
    colnames(log) = colnames(network$log)
    log = rbind(network$log, log)
    return(log)
  }

  # check that arguments are right and all params specified
  checked = preflight(ncycles)
  curr_log_index = max(which(!is.na(network$log$cycle)))
  curr_log_cycle = network$log$cycle[curr_log_index]
  network$log = alloc_log(ncycles)

  # Finally ready to do the actual computations!! getinput() and cycle()
  # Most of the what's below is chrome, eg progress bar
  cycleno = 1
  if(verbose) {
    pb = utils::txtProgressBar(max=ncycles, style = 3)
  }
  while(cycleno <= ncycles) {
    # step 1 -- calculate the input coming into each unit
    input = getinput(network$activations, network$weights, network$external,
                     estr = network$params[['estr']],
                     alpha = network$params[['alpha']],
                     gamma = network$params[['gamma']],
                     noise = network$params[['noise']], OG)
    # step 2 update activations based on their input and the activation parameters
    network$activations = update(network$activations, input,
                                 min = network$params[['min']],
                                 max = network$params[['max']],
                                 rest = network$params[['rest']],
                                 decay = network$params[['decay']] ,OG)
    if(verbose & exists('pb')) {
      Sys.sleep(.001)
      utils::setTxtProgressBar(pb, cycleno)
    }
    # append another entry to the log
    # Remember the log index and ncycle start misaligned, log$cycle[1] = 0
    # Also repeated cycles without reset mean we need to be sure we are
    # inserting the activations into the right slot and recording the right
    # cycle number
    curr_log_index = curr_log_index + 1
    curr_log_cycle = curr_log_cycle + 1
    network$log[curr_log_index, ] = c(curr_log_cycle, network$activations)

    cycleno = cycleno + 1
  }
  if(verbose & exists('pb')) { close(pb) }

  return(network)
}

#' Resets the network for a new run.
#'
#' This means returning activation levels
#' to rest (which is usually _not_ 0), and clearing the activation log.
#' HOWEVER, the external inputs are maintained. Use `clear_external(network)`
#' to zero those. Separating reset of network activation values from reset of
#' external inputs seemed to make things tidier when running repeated simulations
#' @param network The network
#' @return The network with resting activation and cleared activation log
#' @seealso read_net(), set_external(), cycle(), plot_log()
#' @export
#'
reset = function(network) {
  network$activations[1:length(network$activations)] = network$params[['rest']]
  network$log = data.frame(matrix(nrow=1, ncol=1+network$nunits))
  colnames(network$log) = c('cycle', network$unitnames)
  network$log[1, ] = c(0, network$activations)
  return(network)
}

#' Set the external input to the network.
#'
#' This external input will remain
#' until it is cleared by clear_external, or until set_external is called again.
#'
#' @param network The network
#' @param units A vector specifying which units to receive external input.
#' This can be either numeric values specifying indices, or probably better,
#' a vector of unit names
#' @param activations A numeric vector giving the value of external input
#' for each of the specified units
#' @return The network with external inputs updated for subsequent calls
#' to `cycle()`
#' @seealso read_net(), cycle(), reset(), clear_external()
#' @export
#'
set_external = function(network, units, activations) {
  stopifnot(all(units %in% network$unitnames))
  network = clear_external(network)
  network$external[units] = activations
  return(network)
}

#' Zero the external input to the network.
#'
#' This is usually not needed, as `set_external()` zeroes external inputs before
#' setting to new values.
#'
#' @param network The network
#' @return The network without external input
#' @seealso read_net(), cycle(), reset(), set_external()
#' @export
#'
clear_external = function(network) {
  network$external[1:length(network$external)] = 0
  return(network)
}

#' Run a batch of simulations.
#'
#' This is useful when you want to generate a
#' distrbution of results while using the noise parameter. Without noise, every
#' run of a network will be the same, and so there's not much point in running
#' a batch.
#'
#' The `noise` param can be set in the network .yaml file, or manually as
#' `network$params$noise = .01` (or whatever value)
#'
#' @param network The network
#' @param nsims The number of simulations in this batch
#' @param ncycles The number of cycles for each sim
#' @return A dataframe where columns represent sim number, cycle, and every unit
#' in the network, and rows provide activations for each unit on each cycle of
#' each sim.
#' @seealso read_net(), cycle(), plot_acts()
#' @examples
#' \dontrun{
#' # This would take time to run
#' library(lattice)
#' library(directlabels)
#' jetsh = read_net(iac_example("jets_sharks.yaml"))
#' jetsh = set_external(jetsh, "Ken", 1.0)
#' # add random noise to the network
#' jetsh$params$noise = .01
#' outK = sim_batch(jetsh, nsims = 10, ncycles = 50)
#' outK$task = "Ken"
#' jetsh = reset(jetsh)
#' jetsh = set_external(jetsh, "Al", 1.0)
#' outA = sim_batch(jetsh, nsims = 10, ncycles = 50)
#' outA$task = "Al"
#' # compare the two tasks
#' out = rbind(outA, outK)
#' plot_acts(out, cond = "task", roi = c("Ken", "Al", "jets", "sharks"))
#'}
#' @export
#'
sim_batch = function(network, nsims, ncycles) {
  # preallocate dataframe df to hold everything from this batch
  df = data.frame(simno = rep(1:nsims, each=(ncycles+1)),
                  cycle = rep(0:ncycles, times = nsims),
                  act = matrix(-999, nrow = nsims*(ncycles+1), ncol = network$nunits))
  colnames(df) = c('simno', 'cycle', network$unitnames)
  acts = 3:ncol(df)
  i = 1
  pb = utils::txtProgressBar(min=0, max=nsims, initial = 0, style = 3)
  for(sim in 1:nsims) {
    network = reset(network)
    df[i, acts] = network$activations # cycle 0 = resting state activations
    i = i + 1
    for(cy in 1:ncycles) {
      network = cycle(network, ncycles=1, warnings = TRUE, verbose = FALSE, OG = FALSE)
      df[i, acts] = network$activations
      i = i + 1
    }
    utils::setTxtProgressBar(pb, sim)
    Sys.sleep(.01)
  }
  close(pb)
  return(df)
}

#' Run sims with extra noise on specific units and/or connections
#'
#' This works like sim_batch, but adds noise to disrupt processing. Noise can be
#' added to the activation of specific units during cycle(), and also noise can
#' be added to the weights of specific connections. To add noise to specific
#' units, use the noisy_units and xtra_unit_noise parameters. To add noise to
#' the weights, use the noisy_cxn_src, noisy_cxn_dst, and xtra_cxn_noise
#' parameters.
#'
#' These effects are in addition to the global noise parameter.
#'
#' @param network The network, with external inputs set and noise > 0
#' @param nsims Number of simulations to run
#' @param ncycles Number of cycles for a single simulation
#' @param noisy_units A vector of unitnames which will be disrupted
#' @param xtra_unit_noise The random noise added as input to any noisy_units,
#' called as runif(min = -xtra_unit_noise, max = xtra_unit_noise) and applied
#' as external input ot the noisy units.
#' @param noisy_cxn_mat An Nx2 matrix format, where N = the number of
#' connections and `[, 1]` = row of weight matrix (sender), and `[, 2]` = col of
#' weight matrix (receiver). The connections corresponding to the sender and receiver
#' units of `network$weights` will be disrupted
#' @param xtra_cxn_noise The random noise to be added to the weights if noisy_cxn_mat
#' is specified.
#' @param randwalk If TRUE (default), then all the specified noisy connections will
#' accumulate noise over cycles. If set to FALSE, then the the noisy connections
#' are reset to their original values each cycle before noise is added. This
#' parameter only has effect on weights specified in noisy_cxn_mat.
#' @returns A dataframe containing the activations (indexed by the unitnames of
#' the network), for each cycle (column "cycle") for each simulation
#' (column "simno"). This output can then be used directly by RTs_generate()
#' @export
#'
sim_batch_noisy = function (network, nsims, ncycles, noisy_units = NULL, xtra_unit_noise = 0,
                             noisy_cxn_mat = NULL, xtra_cxn_noise = 0, randwalk = TRUE) {
  original_weights = network$weights
  # preallocate df to hold all results (nunits * nsims * ncycles)
  dfout = data.frame(simno = rep(1:nsims, each = (ncycles +  1)),
                     cycle = rep(0:ncycles, times = nsims),
                     act = matrix(-999, nrow = nsims * (ncycles + 1), ncol = network$nunits))
  colnames(dfout) = c("simno", "cycle", network$unitnames)
  acts = 3:ncol(dfout)
  if (!is.null(noisy_cxn_mat)) {
    num_noisy_cxns = nrow(noisy_cxn_mat)
  } else {
    num_noisy_cxns = 0
  }
  num_noisy_units = ifelse(is.null(noisy_units), 0, length(noisy_units))
  i = 1
  pb = utils::txtProgressBar(min = 0, max = nsims, initial = 0, style = 3)
  for (sim in 1:nsims) {
    network = iac::reset(network)
    network$weights = original_weights
    dfout[i, acts] = network$activations
    i = i + 1
    for (cy in 1:ncycles) {
      if (num_noisy_cxns > 0) {
        cxn_noise = stats::runif(num_noisy_cxns, -xtra_cxn_noise, xtra_cxn_noise)
        network$weights[noisy_cxn_mat] = network$weights[noisy_cxn_mat] + cxn_noise
      }
      if (num_noisy_units > 0) {
        unit_noise = stats::runif(num_noisy_units, -xtra_unit_noise, xtra_unit_noise)
        network$external[noisy_units] = network$external[noisy_units] + unit_noise
      }
      network = iac::cycle(network, ncycles = 1, warnings = TRUE, verbose = FALSE, OG = FALSE)
      dfout[i, acts] = network$activations
      if (num_noisy_cxns > 0 & randwalk == FALSE) {
        network$weights = original_weights
      }
      if (num_noisy_units > 0) {
        network$external[noisy_units] = network$external[noisy_units] - unit_noise
      }
      i = i + 1
    }
    utils::setTxtProgressBar(pb, sim)
    Sys.sleep(0.01)
  }
  close(pb)
  return(dfout)
}
