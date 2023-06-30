
#' Plot a network log with selected unit activations
#'
#' @param toplot A network or network$log to be plotted. Alternatively, a
#'   dataframe of unit activations, with a column named 'cycle', can be used. If
#'   "cycle" is not present, a vector from 1 to nrow of the data.frame is used.
#' @param roi A vector specifying which columns to plot, this can be either
#'   numeric values specifying indices, or ideally, unit names
#' @param cycles An optional vector of cycle numbers to be plotted. The default
#'   is to plot all cycles,
#' @param mar Optionally call the mar graphics parameter, useful for making more
#'   room for labels on the right (default = c(4,4,2,3))
#' @param ... The various optional arguments for plot, eg xlim, ylab, main, etc
#' @examples
#' # load the jets-sharks example
#' jetsh = read_net(iac_example("jets_sharks.yaml"), verbose =FALSE)
#' # do a partial retrieval of Ken's properties from his name
#' jetsh = set_external(jetsh, "Ken", 1.0)
#' jetsh = iac::cycle(jetsh, ncycles = 100)
#' # Ken is a burglar in the Sharks, what is retrieved from his name?
#' # plot Ken's properties and some others
#' plot_log(jetsh, roi=c("Ken", "_Ken", "jets", "sharks", "burglar", "bookie"),
#' main="Ken is a burgling Shark")
#'
#' @export
#'
plot_log = function(toplot, roi = NULL, cycles = NULL,
                           mar = c(4,4,2,3), ...) {
  # plot_fn uses matplot, which uses matrices
  # Assumes dimnames(M)[[1]] = cyclenos; dimnames(M)[[2]] = unitnames
  plot_fn = function(M, x, unitnames, ...) {
    lastx = x[length(x)]
    # set margin to give some room on the right for unit labels
    oldmar = graphics::par()[['mar']]
    graphics::par(mar= mar, xpd = TRUE)
    graphics::matplot(x, M, xlab = 'Cycle', ylab = 'Activation', type = 'l', ...)
    graphics::mtext(unitnames, side = 4, las = 1, at = M[length(x), unitnames ])
    graphics::legend("topleft", unitnames, col=1:length(unitnames), lwd=2)
    graphics::par(mar=oldmar)
  }

  # the rest of this fn is just making sure that either a network or a matrix
  # with unitnames (eg copy of network$log), or a dataframe with unitnames,
  # and so can be plotted
  if(is.data.frame(as.list(toplot)$log)) {
    if(is.null(dim(toplot$log))) {
      stop('No activations to plot (network log is initialised but empty)
       Has cycle() been called, and were the results assigned back to the network?')
    }
    M = toplot$log
  }
  else if(is.data.frame(toplot)) {
    M = toplot
  }
  else {
    stop('This function plots a data.frame of activations with a column named "cycle"')
  }
  # find the x values (cycle); default is simply 1:nrow()
  # if a set of cycles is specified find the values x values that correspond
  if(!"cycle" %in% colnames(M)) {
    M$cycle = seq(nrow(M))
  }
  x = M$cycle
  # check whether plotting subset of cycles
  if(!is.null(cycles)) {
    xindices = which(x %in% cycles)
    M = as.matrix(M[xindices, ])
    x = x[xindices]
  }

  # sort the unitnames
  allunitnames = colnames(M)
  if(is.null(roi)) { roi = allunitnames }
  else if(!all(roi %in% allunitnames)) {
    message(sprintf('roi <%s> is not in the list of unitnames',
                    roi[which(!roi %in% allunitnames)]))
    stop()
  }
  # next two lines needed to put things in matrix form for matplot call
  M = as.matrix(M[, roi])
  colnames(M) = roi
  # wow finally ready to plot
  plot_fn(M, x, roi, ...)
}

#' Plot selected units when using sim_batch
#'
#' Make a quick plot of selected unit activations, using the lattice and
#' directlabels packages. There is a bug where an roi of a single unit
#' doesn't work.
#'
#' @param toplot A dataframe of unit activations. Unlike the
#'   regular plot_log fn, this can also plot the output of
#'   `sim_batch()`. The plotted results are the mean activation of each cycle. One
#'   of the columns must be named "cycle" (as will be the case if a `network$log`
#'   or output from sim_batch is plotted)
#' @param roi A vector specifying which columns to plot, this can be either
#'   numeric values specifying indices, or ideally, unit names
#' @param condition The name of a column which will serve as a conditional
#'   variable, that is, unit activations will be plotted as function of the
#'   different levels of this factor (e.g. "accuracy" might have correct and
#'   incorrect, meaning all unit activations will have a value for corret and
#'   incorrect trials)
#' @param cycles An optional vector of cycle numbers to be plotted. The default
#' is to plot all cycles,
#' @param labelstyle A label style that is valid for the directlabels package
#'   (default = "last.bumpup")
#' @param ... The various optional arguments for `lattice::xyplot`, xlab, layout, etc
#' @seealso sim_batch()
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
#'
#' @export
#'
plot_acts = function(toplot, roi, condition=NULL, cycles = NULL, labelstyle="last.bumpup", ...) {
  if(!(requireNamespace("lattice", quietly=TRUE) &
       requireNamespace("directlabels", quietly=TRUE))) {
    stop("The lattice and directlabels packages need to be installed to use this function.",
         call. = FALSE)
  }
  if(is.numeric(roi)) { roi = colnames(toplot)[roi] }
  if(is.numeric(condition)) { condition = colnames(toplot)[condition]}
  stopifnot(all(c(roi, "cycle", condition) %in% colnames(toplot)))
  # aggregate to get means if necessary
  o = stats::aggregate(toplot[roi], by=toplot[c("cycle", condition)], mean)
  if(!is.null(cycles)) {
    o = o[which(o$cycle %in% cycles), ]
  }
  roi = paste(roi, collapse = " + ")
  if(is.null(condition)) {
    f = stats::formula(paste(roi, "~ cycle"))
    nfacs = 1
  }
  else {
    nfacs = length(table(o[[condition]]))
    f = stats::formula(paste(roi, "~ cycle |", condition))
  }
  p = lattice::xyplot(f, data = o, type ='l', layout=(c(nfacs,1)), xlab="Cycle",
             ylab="Activation", ...)
  print(directlabels::direct.label(p, labelstyle))
}

#' Extract the weights between two pools
#'
#' @param network The network
#' @param from,to The names of existing pools within network
#' @returns A matrix from within `network$weights`, where rows are the units in
#' the `from` pool, and columns are units from the `to` pool.
#' This matrix is suitable for `plot_weights()` or other inspection
#' @examples
#' # load the jets_sharks network
#' jetsh = read_net(iac_example("jets_sharks.yaml"), verbose = FALSE)
#' weights_slice(jetsh, from='gang', to='instance')
#' @export
#' @seealso plot_weights()
#'
weights_slice = function(network, from, to) {
  # make sure "from" and "to" refer to pools within the network
  stopifnot(from %in% names(network$pools))
  stopifnot(to %in% names(network$pools))
  W = network$weights[network$pools[[from]]$units, network$pools[[to]]$units]
  return(W)
}

#' Plot a heatmap of a weight matrix (or subset).
#'
#' The sending (or "from") units are plotted on the y-axis, the receiving (or
#' "to") units on the x-axis. Each weight or connection is represented by a
#' square in the plot. White squares represent positive weights, black squares
#' for negative weights. The area of each square is proportional to the weight.
#' All squares are normalised relative to the weight of largest magnitude.
#' @param weights The weights to be plotted. This can  be the full weights
#'   matrix (e.g., `network$weights`), but for bigger networks it can be more
#'   useful to look at a subset. This subset of weights can be specified
#'   manually (e.g., `network$weights[1:5, 20:29]`), or conveniently using
#'   `weights_slice()`, which makes it easy to extract the connections between two
#'   pools within a network.
#' @param ... The various optional arguments for plot, eg main, etc
#'
#' @examples
#' # load the jets_sharks network
#' jetsh = read_net(iac_example("jets_sharks.yaml"), verbose = FALSE)
#' # plot all the weights
#' plot_weights(jetsh$weights)
#' # just the instance -> gang connections
#' plot_weights(weights_slice(jetsh, from='instance', to='gang'))
#' @export
#'
plot_weights = function(weights, ...) {
  drawblob = function(area, row, col, maxarea) {
    spacing = .05
    if(area < 0) {blobcolor='black'; area = -area } else {blobcolor='white'}
    scaling = (1 - spacing*2)
    blobside = sqrt(area) * scaling
    row = row - blobside/2; col = col -  blobside/2
    graphics::rect(col, row, col + blobside, row + blobside,
                   col=blobcolor, border=NA)
  }
  wide = ncol(weights)
  high = nrow(weights)
  # Frame
  graphics::plot(c(1,wide), c(1, high), type='n', axes = FALSE, xlab=NA, ylab=NA, asp = 1, ...)
  graphics::axis(1, cex.axis=.5, at=1:wide, labels=attr(weights, 'dimnames')[[2]], las=2)
  graphics::axis(2, cex.axis=.5, at=1:high, labels=attr(weights, 'dimnames')[[1]], las=2)
  graphics::mtext('< To', side = 1, at=wide+2)
  graphics::mtext('From v', side=2, at=high+1.5, las=2)
  graphics::rect(0,0,wide+1, high+1, col='grey', border=NA)
  maxweight = max(abs(weights))
  # draw each weight blob
  for(row in 1:high) {
    for(col in 1:wide) {
      drawblob(weights[row, col], row, col, maxweight)
    }
  }
}

#' Inspect the NON-ZERO weights from or to a unit or units
#'
#' A way to quickly check network connections
#'
#' @param network The network
#' @param from,to A vector of unitnames (not pool names). Exactly one of these
#' is specified. Currently, if both are, `to` is ignored.
#' @returns The vector of non-zero weights from or to the specified units.
#' @examples
#' # load the jets_sharks network
#' jetsh = read_net(iac_example("jets_sharks.yaml"), verbose = FALSE)
#' # see the non-zero connections from the Ken instance (_Ken)
#' show_weights(jetsh, from = '_Ken')
#'
#' @export
#'
show_weights = function(network, from = NULL, to = NULL) {
  M = network$weights
  if(!is.null(from)) {
    if(length(from) == 1) {
      vec = M[from, which(M[from, ] != 0)]
    }
  }
  else if(!is.null(to)) {
    if(length(to) == 1) {
      vec = M[which(M[, to] != 0), to]
    }
  }
  return(vec)
}

#' Get path to IAC example
#'
#' iac comes bundled with some example files in its inst/extdata directory. This
#' function make them easy to access.
#' @param path Name of file. If NULL all the example files are listed.
#' @examples
#' # returns a list of all available files
#' iac_example()
#' # load the jets-sharks network that comes with the iac package
#' jetsh = iac_example("jets_sharks.yaml")
#' net = read_net(jetsh)
#' # open an example network for inspection in RStudio
#' file.edit(iac_example("what_where.yaml"))
#' @export
#'
iac_example = function (path = NULL)
{
  if (is.null(path)) {
    dir(system.file("extdata", package = "iac"))
  }
  else {
    system.file("extdata", path, package = "iac", mustWork = TRUE)
  }
}
