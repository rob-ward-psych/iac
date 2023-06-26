
#' Plot selected unit activations by cycle
#'
#' @param toplot Usually a network -- network$log is then used for plotting.
#' Alternatively, a dataframe of unit activations, with a column named 'cycle',
#' can be used. If "cycle" is not present, a vector from 1 to nrow of the
#' data.frame is used.
#' @param roi A vector specifying which columns to plot, this can be either
#' numeric values specifying indices, or ideally, unit names
#' @param cycles An optional vector of cycle numbers to be plotted. The default
#' is to plot all cycles,
#' @param mar Optionally call the mar graphics parameter, useful for making more
#' room for labels on the right (default = c(4,4,2,3))
#' @inheritDotParams graphics::matplot
#' @export
#'
plot_activation = function(toplot, roi = NULL, cycles = NULL, mar = c(4,4,2,3), ...) {
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

#' Extract the weights between two pools
#'
#' @param network The network
#' @param from,to The names of existing pools within network
#' @return A matrix from network$weights matrix, where rows are the units in
#' the `from` pool, and columns are units from the `to` pool.
#' This matrix is suitable for plot_weights() or other inspection
#' @seealso plot_weights(), read_net()
#' @export
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
#'   matrix (e.g., network$weights), but for bigger networks it can be more
#'   useful to look at a subset. This subset of weights can be specified
#'   manually (e.g., network$weights[1:5, 20:29]), or conveniently using
#'   weights_slice(), which makes it easy to extract the connections between two
#'   pools within a network.
#' @inheritDotParams graphics::plot
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