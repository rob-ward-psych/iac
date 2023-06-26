#' Create a new network in code
#'
#' Used internally by read_network().
#' **Not needed** if you are using read_network() to create your network.
#' These and other functions are only used if you are creating your network
#' direct from code. Know that when a network
#' is read from .yaml file, it uses these routines so you don't have to.
#' @returns A list which is ready to be populated as a network
#' @seealso add_pool(), connect_units()
#' @export
#'
new_network = function() {
  network = list(pools = list(), nunits = 0, unitnames = c(),
                 activations = 0, external = 0, log = 0, weights = 0)
  return(network)
}

#' Generate unitnames for a pool
#'
#' Used internally by read_network().
#' **Not needed** if you are using read_network() to create your network.
#' These and other functions are only used if you are creating your network
#' direct from code. Know that when a network
#' is read from .yaml file, it uses these routines so you don't have to.
#' This is a utility to make it easier to code systematic
#' unitnames inside a pool.
#' The general idea is that names = paste0(prefix, basenames).
#' expand.grid is used on basenames and so the above is exactly true if
#' basenames is 1D. Otherwise, all combos of vector values are generated,
#' with the first vector changing fastest
#' @param prefix A string value pasted without spaces in front of each generated
#' name
#' @param basenames A list of vectors corresponding to the shape of the pool
#' @returns A vector of appropriate unitnames
#' @export
#'
gen_unitnames = function(prefix, basenames) {
  unitnames =  apply(as.matrix(expand.grid(basenames)), 1,
                     function(x) {paste(x, collapse='')})
  unitnames = paste0(prefix, unitnames)
  return(unitnames)
}

#' Convert unit specifications to numeric indices
#'
#' A utility function that converts a pool name or a vector of unit names
#' into a numeric vector that indexes the units in the network. If a numeric
#' vector is passed it is return directly
#' @param network The relevant network
#' @param unitspec A vector of pool names and/or unitnames in the network
#' @returns A numeric vector of indices corresponding to the names in unitspec
#' @examples
#' v = spec2indices(jetssharks, c('Ken', 'Al'))
#' @noRd
#'
spec2indices = function(network, unitspec) {
  poolnames = names(network$pools)
  unitnames = network$unitnames
  indices = vector()
  for(v in unitspec) {
    if(v %in% poolnames) {
      indices = append(indices, network$pools[[v]]$units)
    }
    else if(v %in% unitnames) {
      indices = append(indices, which(network$unitnames %in% v))
    }
    else if(is.numeric(v)) {
      indices = append(indices, v)
    }
    else {
      stop(sprintf("Can't find \"%s\" in the network", v))
    }
  }
  return(indices)
}

#' Connect units in a network
#'
#' Used internally by read_network().
#' **Not needed** if you are using read_network() to create your network.
#' These and other functions are only used if you are creating your network
#' direct from code. Know that when a network is read from .yaml file, it uses
#' these routines so you don't have to.
#'
#' connect_units() is a monster workhorse function internally, but it is called
#' using the same keywords as used when specifying networks with the .yaml file
#' and read_network()
#' @param network The network getting connected up, usually created by
#'   read_network(), but probably new_network() in this case
#' @param from,to All the from units are connected to all the to units. `from`
#'   and `to` can be a pool name, or a vector of string indices which will be
#'   converted into indices into network$weights.
#' @param weight The numerical value of the connection weight
#' @param from_dims,to_dims In pools with spatial organisation, these show the
#'   corresponding dimensions of the pools. See the documentation on spatial
#'   networks.
#' @param directives A vector of strings giving optional detail about the
#'   connection. Legal directives are: "oneway", "reciprocal" (default), "add",
#'   "self", and "others". See the documentation on spatial networks.
#' @param verbose Prints a description of the units being connected to the
#'   console (default = FALSE)
#' @returns A network where network$weights is updated with the new connection
#'   values
#' @export
#'
connect_units = function(network, from, to, weight, directives = 'reciprocal',
                         from_dims = c(), to_dims = c(),
                         verbose = FALSE) {
  pp_indices_string = function(indices) {
    indices = as.integer(indices)
    if(length(indices) > 2 &
       identical(indices, seq(from = min(indices), to = max(indices)))) {
      s = sprintf('%d:%d', min(indices), max(indices))
    }
    else {
      s = paste(indices, collapse = ',')
    }
    return(s)
  }

  # setslices() does all the hard work for spatial pool connection.
  #  It allows multi-dimensional pools (a and b) to systematically connect
  #  with each other, specifying the corresponding pool dimensions
  # (ai and bi). In addition it allows for spatial mapping, so that a unit
  #  in one pool can connect with different weights to corresponding and
  #  nearby units in a second pool
  setslices = function(a, b, ai, bi, w, dist='cityblock') {
    # a and b are multi-dimensional pools of units
    # ai and bi refer to relevant dimensions of a and b, respectively.
    # dist = city or euclid, for determining spatial weighting
    # new dist option = all, weights to all units in pool regardless
    # of distance

    # arr is the larger a•b matrix
    # new_dims gives the dimensions of arr
    # ai_i and bi_i index into arr
    # ai_i and bi_i must refer to dimensions of arr which are
    #   the same size
    new_dims = c(a, b)
    ndims = length(new_dims)
    arr = array(0, dim = new_dims)
    ai_i = ai
    bi_i = bi + length((a))

    stopifnot(identical(new_dims[ai_i], new_dims[bi_i]))

    # map the dimensions of ai_i onto bi_i
    corres = cbind(ai_i, bi_i)

    # ai.i and bi.i are "mapped" dimensions, all others are "free"
    # The full arr is an expanded grid of all mapped and free
    #  dimensions
    # For example, a fully expanded set of two 3x2 feature maps
    #  connecting to a 3x2 master map of locations would have
    #  dimensions and sizes like:
    #   fm_x (size 3) * fm_y (2) * features (2) * mm_x (3) * mm_y (2)
    #   full.array = expand.grid(1:3, 1:2, 1:2, 1:3, 1:2)
    #  The expnded grid shows how each unit in each dimension of
    #   the connection can be connected to other units
    #  Units with the same values on the 'mapped' dimensions will be
    #   given a weight. That weight will be the same for all the
    #   variation on the 'free' dimensions
    #  Note that "same value" can be thought of as distance=0
    #  We can also give units within a certain distance on the mapped
    #   dimensions a weight based on their distance

    # Create the expanded grid
    # specs holds the sizes for each dimension of the connection,
    specs = list()
    for(i in 1:ndims) {
      specs[[i]] = seq(new_dims[i])
    }
    cxn_list = expand.grid(specs)
    distances = cbind(cxn_list[, corres[, 1]] - cxn_list[, corres[, 2]])
    euclidean = sqrt(rowSums(distances^2))
    cityblock = rowSums(abs(distances))
    entirepool = rep(0, nrow(distances))
    distances = cbind(distances, cityblock, entirepool)

    # iterate thru all possible connections
    # if a weight has been specified for their distance, then assign it
    #  use the dist argument to determine metric
    cxn_list = as.matrix(cxn_list)
    for(i in seq(nrow(cxn_list))) {
      v = cxn_list[i, ]
      d = distances[i, dist]
      d = floor(d + 1)  # eg a distance of 0 will use w[1]
      ###
      if(d > length(w)) { weight = 0 } else {weight = w[d]}
      arr[matrix(v, 1)] = weight
    }
    info= cbind(cxn_list, distances, arr = as.vector(arr))
    # return absoluately everything! Might be handy for debugging
    return(list(grid = cxn_list, info = info, arr = arr,
                specs= specs, corres = corres))
  }



  # put all the messy argument checking in one place
  # check_args() will stop with an error if there's a problem
  check_args = function(network, from, to, weight, directives,
                        from_dims, to_dims) {
    nf = length(spec2indices(network, from))
    if(nf == 0) {
      message('Error specifying unit connection, "from" given as:')
      print(from)
      message(paste('"from" must be either:',
                    'the name of a single pool',
                    'a vector or one or more unitnames',
                    'a vector of one or more indices', sep = '\n'))
      stop()
    }
    nt = length(spec2indices(network, to))
    if(nt == 0) {
      message('Error specifying unit connection, "to" given as:')
      print(to)
      message(paste('"to" must be either:',
                    'the name of a single pool',
                    'a vector or one or more unitnames',
                    'a vector of one or more indices', sep = '\n'))
      stop()
    }
    if(length(directives) > 0 &
       !all(directives %in% c('oneway', 'self', 'others', 'reciprocal', 'add'))) {
      message('Error specifying unit connection, "directives" given as:')
      print(directives)
      message(paste('"directives" (if given) specifies aspects of the connection.',
                    'Either "oneway" or "reciprocal" (reciprocal is default)',
                    'Either "self" or "others" to limit within-pool connections',
                    'Use "add" to add to rather than replace existing connection weight', sep = '\n'))
      stop()
    }
    if(('self' %in% directives & 'others' %in% directives) |
       ('oneway' %in% directives & 'reciprocal' %in% directives)) {
      message('Error specifying unit connection, "directives" given as:')
      print(directives)
      message('Directive must not specify both "self" and "others", or both "oneway" and "reciprocal"')
      stop()
    }
    if(!is.numeric(weight) |
       (length(weight) != 1 & length(weight) != nf*nt) &
       (is.null(from_dims) & is.null(to_dims))) {
      message('Error specifying unit connection, "weight" given as:')
      print(weight)
      message('"weight" must be a single numeric value or if you really know what you are doing, a vector of length from*to')
      message('If instead you are trying make weights vary by distance, specify the corresponing spatial dimensions using from_dims and to_dims')
      stop()
    }
    # from_dims, to_dims -- ugh this is complicated stuff to check
    if(!is.null(from_dims) | !is.null(to_dims)) {
      frompool = network$pools[[from]]
      topool = network$pools[[to]]
      if(is.null(frompool) | is.null(topool)) {
        message('Error: from_dims and to_dims are specified only when you want to connect')
        message('spatially organised pools. The pool names must be specified in the from and to arguments.')
        stop()
      }
      fromshape = frompool[['shape']]
      toshape = topool[['shape']]
      if(is.null(from_dims) | is.null(to_dims)) {
        message('Error: from_dims and to_dims are specified only when you want to connect')
        message('spatially organised pools. Both from_dims and to_dims must be specified.')
        stop()
      }
      if(any(from_dims > length(fromshape) | from_dims < 1)) {
        message('Error: from_dims are specified outside the dimensions of the "from" pool.')
        message(sprintf('Shape of from pool "%s" has %d dimensions: (%s)\nTherefore dimensions given in from_dims should be between 1 and %d, but specified as (%s)',
                        frompool[['name']], length(fromshape), paste(fromshape, collapse=','), length(fromshape),
                        paste(from_dims, collapse=',')))
        stop()
      }
      if(any(to_dims > length(toshape) | to_dims < 1)) {
        message('Error: to_dims are specified outside the dimensions of the "to" pool.')
        message(sprintf('Shape of to pool "%s" has %d dimensions: (%s)\nTherefore dimensions given in to_dims should be between 1 and %d, but specified as (%s)',
                        topool[['name']], length(toshape), paste(toshape, collapse=','), length(toshape),
                        paste(to_dims, collapse=',')))
        stop()
      }
    }
  }

  weight = unlist(weight)
  check_args(network, from, to, weight, directives, from_dims, to_dims)
  w_passed_as_arg = weight # the original weight argument saved for verbose output

  # Ready to make connections
  # first check to see if this is a connect_pools situation
  # this will be because from_dims, to_dims are specified
  # If it is, create the appropriate weight vector using setslices()
  if(!is.null(from_dims)) {
    from_pool = network$pools[[from]]
    to_pool = network$pools[[to]]
    res = setslices(from_pool[['shape']], to_pool[['shape']],
                    from_dims, to_dims, weight)
    weight = as.vector(res$arr)
  }

  # with all that out of the way, making the actual connections is simple
  fromindices = spec2indices(network, from)
  toindices = spec2indices(network, to)
  cxns = expand.grid(fromindices, toindices)
  cxns[, 'weight'] = weight
  if('others' %in% directives) {
    # expand.grid produces Var1 and Var2 as colnames
    cxns = subset(cxns, Var1 != Var2)
  }
  else if('self' %in% directives) {
    cxns = subset(cxns, Var1 == Var2)
  }
  v1 = cxns[,1]; v2=cxns[,2]; v3 = cxns[,3]
  coord_forw = cbind(v1, v2)
  coord_back = cbind(v2, v1)
  if(!'add' %in% directives) {
    network$weights[coord_forw] = v3
    if(! 'oneway' %in% directives) {
      network$weights[coord_back] = v3
    }
  }
  else if('add' %in% directives) {
    network$weights[coord_forw] = network$weights[coord_forw] + v3
    if(! 'oneway' %in% directives) {
      network$weights[coord_back] = network$weights[coord_back] + v3    }
  }

  if(verbose) {
    message(sprintf("%s [%s] -> %s [%s]; weight = [%s] (%s)",
                    paste(from, collapse = '+'),
                    pp_indices_string(fromindices),
                    paste(to, collapse = '+'),
                    pp_indices_string(toindices),
                    paste(w_passed_as_arg, collapse = ', '),
                    paste(directives, collapse = ',')))
  }
  return(network)
}

#' Add a pool to the network
#'
#' Used internally by read_network().
#' **Not needed** if you are using read_network() to create your network.
#' These and other functions are only used if you are creating your network
#' direct from code. Know that when a network
#' is read from .yaml file, it uses these routines so you don't have to.
#'
#' @param network The network getting connected up, usually created by
#' read_network(), but probably new_network() in this case
#' @param poolname Name of the pool, needs to be a legal R variable name
#' @param shape A vector specifying the size of each dimension within the pool The numerical value of the connection weight
#' @param unitnames A vector of unitnames, one for each unit in the pool. These
#' need to be legal R variable names. Usually they would be generated automatically
#' using the gen_unitnames() function
#' @param verbose Gives details about the units as they are being added, useful
#' @returns A network with the new pool
#' @export
#'
add_pool = function(network, poolname, shape, unitnames, verbose = FALSE) {
  pool = list()
  pool[['name']] = poolname
  pool[['shape']] = as.numeric(shape)
  pool[['size']] = prod(pool[['shape']])
  pool[['units']] = (1:pool[['size']] + network$nunits)
  pool[['unitnames']] = unitnames
  if(length(unitnames) != pool[['size']]) {
    message('Error: Number of unitnames does not match sizez of pool')
    message(sprintf('pool "%s" (size=%d) unitnames (n=%d) [%s]',
                    pool[['name']], pool[['size']],
                    length(unitnames),
                    paste(pool[['unitnames']], collapse=',')))
    stop()
  }
  all_names = c(pool[['name']], pool[['unitnames']],
                network$unitnames, names(network$pools))
  if(any(table(all_names) != 1)) {
    stop(sprintf('repeated name in network: <%s>\n',
                 attr(which(table(all_names) != 1), 'names')))
  }
  # good to go, add the new pool to the network
  # makes sure all the network properties are up-to-date
  network$pools[[pool[['name']]]] = pool
  network$nunits = network$nunits + pool[['size']]
  network$unitnames = c(network$unitnames, pool[['unitnames']])
  network$activations = matrix(0, nrow=1, ncol=network$nunits,
                               dimnames = list(0, network$unitnames))[1, ]
  network$external = matrix(0, nrow=1, ncol = network$nunits,
                            dimnames = list(0, network$unitnames))[1, ]
  network$log = data.frame(matrix(nrow=0, ncol = 1+(network$nunits)))
  network$weights = matrix(0, nrow = network$nunits, ncol = network$nunits,
                           dimnames = list(network$unitnames, network$unitnames))
  if(verbose) {
    message(sprintf('%s: shape=[%s] unitnames=[%s]',
                    pool[['name']], paste(pool[['shape']], collapse = ','),
                    paste(pool[['unitnames']], collapse = ',')))
  }
  return(network)
}

#' Create a network from a file
#'
#' This is the intended method for network creation. Details are given in the
#' document iac_creating_networks.md.
#'
#' @param yaml_file The .yaml file with the network description
#' @param verbose If TRUE (default) then information describing the network
#' is printed to the console as the network is being assembled
#' @param network This will normally be NULL. However, instead of reading a
#' .yaml file to create a new network, you can also read a .yaml file which
#' will add pools or connections to an existing network. In this case, the
#' existing network being elaborated is passed as an argument here.
#' @returns A new network as described in the .yaml file
#' @export
#'
read_net = function(yaml_file, verbose = TRUE, network = NULL) {
  if(!requireNamespace("yaml", quietly=TRUE)) {
    stop("The yaml package needs to be installed to use this function.",
         call. = FALSE)
  }
  param_defaults = list(alpha = NA, gamma = NA, estr = NA,
                        min = NA, max = NA, rest = NA, decay = NA,
                        noise = NA)
  load_params = function(src) {
    params = param_defaults
    paramnames = names(params)
    for(i in seq(length(src))) {
      key = names(src)[i]
      value = src[[i]]
      if(!key %in% names(params)) {
        stop(sprintf('Unrecognised parameter <%s>', key))
      }
      params[[key]] = value
    }
    for(i in seq(length(params))) {
      key = names(params)[i]
      value = params[[i]]
      if(is.na(value)) {
        warning(sprintf('parameter <%s> not assigned, assigning default of %g',
                        key, param_defaults[[key]]), call. = FALSE)
        params[[key]] = param_defaults[[key]]
      }
    }
    return(params)
  }

  mknetwork = function(poolinfo, verbose = verbose, network = NULL) {
    if(is.null(network)) { network = new_network()}
    poolattrs = c('name', 'shape', 'prefix', 'basenames', 'unitnames')
    for(pool in poolinfo) {
      if(!all(names(pool) %in% poolattrs)) {
        message(sprintf('Unrecognised pool attributes: %s',
                        names(pool)[which(!names(pool) %in% poolattrs)]))
        message(sprintf('Legal attributes are: <%s>',
                        paste(poolattrs, collapse = ', ')))
        stop()
      }
      poolname = pool[['name']]
      poolshape = as.numeric(pool[['shape']])
      poolsize = prod(poolshape)
      unitnames = pool[['unitnames']]
      if(is.null(pool[['basenames']])) {
        basenames = lapply(poolshape, function(x) return(seq(x)))
      }
      else {
        basenames = pool[['basenames']]
      }
      if(is.null(pool[['prefix']])) {
        prefix = poolname
      }
      else {
        prefix = pool[['prefix']]
      }
      if(is.null(unitnames)) {
        unitnames = gen_unitnames(prefix, basenames)
      }
      network = add_pool(network, poolname, poolshape, unitnames, verbose)
    }
    return(network)
  }

  # read the spec and process each connection
  netspec = yaml::yaml.load_file(yaml_file)
  if(verbose) {message('== pools')}
  network = mknetwork(netspec$pools, verbose, network)
  if(verbose) {message('== connections')}
  cxnattrs = c('from', 'to', 'weight', 'directives', 'from_dims', 'to_dims')
  for(cxn in netspec$connections) {
    if(!all(names(cxn) %in% cxnattrs)) {
      message(sprintf('Unrecognised connection attributes: %s',
                      names(cxn)[which(!names(cxn) %in% cxnattrs)]))
      message(sprintf('Legal attributes are: <%s>',
                      paste(cxnattrs, collapse = ', ')))
      stop()
    }
    from = cxn[['from']]
    to = cxn[['to']]
    weight = cxn[['weight']]
    directives = cxn[['directives']]
    from_dims = cxn[['from_dims']]
    to_dims = cxn[['to_dims']]
    directives = if(is.null(directives)) 'reciprocal' else directives
    from_dims = if(is.null(from_dims)) c() else from_dims
    to_dims = if(is.null(to_dims)) c() else to_dims
    network = connect_units(network, from, to, weight,
                            from_dims = from_dims, to_dims = to_dims,
                            directives=directives, verbose=verbose)
  }
  if(!is.null(netspec$parameters)) {
    network$params = load_params(netspec$parameters)
  }
  network = reset(network)
  if(verbose) {message("Network sucessfully read and ready to go.")}
  return(network)
}


