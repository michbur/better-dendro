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
  
  dd_env <- new.env()
  assign(x = "ddlabels", value = NULL, inherits = FALSE, envir = dd_env)
  assign(x = "ddsegments", value = NULL, inherits = FALSE, envir = dd_env)
  
  plotNode_newR(x1, x2, x,
                type = type, center = center,
                dd_env = dd_env)
  res <- NULL
  res[["segments"]] <- get("ddsegments", envir = dd_env)
  res[["labels"]] <- get("ddlabels", envir = dd_env)
  
  names(res[["segments"]]) <- c("x", "y", "xend", "yend")
  names(res[["labels"]]) <- c("x", "y", "label")
  res
}

plotNode_newR <- function(x1, x2, subtree, type, center, dd_env)
{
  wholetree <- subtree
  depth <- 0L
  llimit <- list()
  KK <- integer()
  kk <- integer()
  
  repeat {
    inner <- !is.leaf(subtree) && x1 != x2
    yTop <- attr(subtree, "height")
    bx <- plotNodeLimit(x1, x2, subtree, center)
    xTop <- bx$x
    depth <- depth + 1L
    llimit[[depth]] <- bx$limit
    
    asTxt <- function(x) # to allow 'plotmath' labels:
      if(is.character(x) || is.expression(x) || is.null(x)) x else as.character(x)
    
    if (is.leaf(subtree)) {
      ddlabels_local <- rbind(get("ddlabels", envir = dd_env), 
                              data.frame(x = xTop, y = 0, text = asTxt(attr(subtree, "label"))))
      assign(x = "ddlabels", value = ddlabels_local, inherits = FALSE, envir = dd_env)
    }
    else if (inner) {
      for (k in seq_along(subtree)) {
        child <- subtree[[k]]
        ## draw lines to the children and draw them recursively
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
          ddsegments_local <- rbind(get("ddsegments", envir = dd_env), 
                                    segmentsHV(xTop, yTop, xBot, yBot))
          assign(x = "ddsegments", value = ddsegments_local, 
                 inherits = FALSE, envir = dd_env)
        } else {
          ddsegments_local <- rbind(get("ddsegments", envir = dd_env), 
                                    segmentsHV(xTop, yTop, xBot, yBot))
          ddsegments_local <- rbind(ddsegments_local, segmentsHV(xTop, yTop, xBot, yTop))
          ddsegments_local <- rbind(ddsegments_local, segmentsHV(xBot, yTop, xBot, yBot))
          assign(x = "ddsegments", value = ddsegments_local, 
                 inherits = FALSE, envir = dd_env)
        }
      }
    }
    
    if (inner && length(subtree)) {
      KK[depth] <- length(subtree)
      if (storage.mode(kk) != storage.mode(KK))
        storage.mode(kk) <- storage.mode(KK)
      
      ## go to first child
      kk[depth] <- 1L
      x1 <- bx$limit[1L]
      x2 <- bx$limit[2L]
      subtree <- subtree[[1L]]
    }
    else {
      repeat {
        depth <- depth - 1L
        if (!depth || kk[depth] < KK[depth]) break
      }
      if (!depth) break
      length(kk) <- depth
      kk[depth] <- k <- kk[depth] + 1L
      x1 <- llimit[[depth]][k]
      x2 <- llimit[[depth]][k + 1L]
      subtree <- wholetree[[kk]]
    }
  } ## repeat
  invisible()
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
