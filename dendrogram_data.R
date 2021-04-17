#' Extract data frame from dendrogram object for plotting using ggplot.
#'
#' Extract data frame from dendrogram object for plotting using ggplot
#'
#' @param x object of class "dendrogram", e.g. the output of as.dendrogram()
#' @param type The type of plot, indicating the shape of the dendrogram.  "Rectangle" will draw
#' rectangular lines, while "triangle" will draw triangular lines.
#' @param ... ignored
#' @seealso [ggdendrogram()]
#' @family dendro_data methods
#' @family dendrogram/hclust functions
#' @keywords internal
dendrogram_data <- function(x, type = c("rectangle", "triangle"), ...) {
  
  x <- as.dendrogram(x)
  # Initialise variables that used to be in parameter list
  center <- FALSE
  edge.root <- is.leaf(x) || !is.null(attr(x, "edgetext"))
  
  type <- match.arg(type)
  hgt <- attr(x, "height")
  if (edge.root && is.logical(edge.root)) {
    edge.root <- 0.0625 * if (is.leaf(x)) 1 else hgt
  }
  mem.x <- .memberDend(x)
  yTop <- hgt + edge.root
  if (center) {
    x1 <- 0.5
    x2 <- mem.x + 0.5
  }
  else {
    x1 <- 1
    x2 <- mem.x
  }
  xl. <- c(x1 - 1 / 2, x2 + 1 / 2)
  yl. <- c(0, yTop)
  
  ret <- gg.plotNode(x1, x2, x,
                     type = type, center = center,
                     ddsegments = NULL, ddlabels = NULL)
  names(ret$segments) <- c("x", "y", "xend", "yend")
  names(ret$labels) <- c("x", "y", "label")
  ret
}

gg.plotNode <- function(x1, x2, subtree, type, center, ddsegments = NULL, ddlabels = NULL) {
  inner <- !is.leaf(subtree) && x1 != x2
  yTop <- attr(subtree, "height")
  
  bx <- plotNodeLimit(x1, x2, subtree, center)
  xTop <- bx$x
  
  if (is.leaf(subtree)) {
    ddlabels <- rbind(ddlabels, data.frame(x = xTop, y = 0, text = asTxt(attr(subtree, "label"))))
  }
  else if (inner) {
    for (k in seq_along(subtree)) {
      child <- subtree[[k]]
      yBot <- attr(child, "height")
      
      if (is.null(yBot)) {
        yBot <- 0
      }
      xBot <- if (center) {
        mean(bx$limit[k:(k + 1)])
      } else {
        bx$limit[k] + .midDend(child)
      }
      
      if (type == "triangle") {
        # *************************
        ddsegments <- rbind(ddsegments, segmentsHV(xTop, yTop, xBot, yBot))
      }
      else {
        # *************************
        ddsegments <- rbind(ddsegments, segmentsHV(xTop, yTop, xBot, yTop))
        ddsegments <- rbind(ddsegments, segmentsHV(xBot, yTop, xBot, yBot))
      }
      
      plotNode_result <- gg.plotNode(bx$limit[k], bx$limit[k + 1],
                                     subtree = child,
                                     type, center, ddsegments, ddlabels)
      ddsegments <- plotNode_result$segments
      ddlabels <- plotNode_result$labels
    }
  }
  list(segments = ddsegments, labels = ddlabels)
}

segmentsHV <- function(x0, y0, x1, y1) {
  data.frame(x0, y0, x1, y1) # AdV
}

asTxt <- function(x) {
  if (is.character(x) || is.expression(x)) {
    x
  } else
    if (is.null(x)) "" else as.character(x)
}


# .memberDend -------------------------------------------------------------

### Code copied from stats:::.memberDend

.memberDend <- function(x) {
  r <- attr(x, "x.member")
  if (is.null(r)) {
    r <- attr(x, "members")
    if (is.null(r)) {
      r <- 1L
    }
  }
  r
}


# plotNodeLimit -----------------------------------------------------------

### Code copied from stats:::plotNodeLimit

plotNodeLimit <- function(x1, x2, subtree, center) {
  inner <- !is.leaf(subtree) && x1 != x2
  if (inner) {
    K <- length(subtree)
    mTop <- .memberDend(subtree)
    limit <- integer(K)
    xx1 <- x1
    for (k in 1L:K) {
      m <- .memberDend(subtree[[k]])
      xx1 <- xx1 + (if (center) {
        (x2 - x1) * m / mTop
      } else {
        m
      })
      limit[k] <- xx1
    }
    limit <- c(x1, limit)
  }
  else {
    limit <- c(x1, x2)
  }
  mid <- attr(subtree, "midpoint")
  center <- center || (inner && !is.numeric(mid))
  x <- if (center) {
    mean(c(x1, x2))
  } else {
    x1 + (if (inner) {
      mid
    } else {
      0
    })
  }
  list(x = x, limit = limit)
}

# .midDend --------------------------------------------------------------------

### Code copied from stats:::.midDend


.midDend <- function(x) {
  if (is.null(mp <- attr(x, "midpoint"))) 0 else mp
}
