
pct_ <- function(vec, adj=0.50) {
  (vec + adj)/sum(vec)
}

woe_ <- function(pct1, pct2) {
  log(pct1 / pct2)
}

woe_tbl_ <- function(tbl) {
  pcts <- apply(tbl, 2, pct_)
  woe_(pcts[,1], pcts[,2])
}

iv_ <- function(tbl) {
  if (nrow(tbl) <= 1) return(0)

  pct1 <- pct_(tbl[,1])
  pct2 <- pct_(tbl[,2])
  woe  <- woe_(pct1, pct2)

  (pct1 - pct2) * woe
}

split_at_index_ <- function(tbl, i) {
  s <- seq.int(i)
  list(left=tbl[s,,drop=F], right=tbl[-s,,drop=F])
}

#' @param i index at which to split
#' @param tbl two-way table summarizing an independent vector and a 0/1 factor
evalulate_split_ <- function(i, tbl, min.cnt=25, min.res=10, mono=0) {
  z <- t(sapply(split_at_index_(tbl, i), colSums))

  iv <- iv_(z)

  if (any(z == 0)) {
    return(-4)

  } else if (any(rowSums(z) < min.cnt)) {
    return(-1)

  } else if (any(z[,2] < min.res)) {
    return(-2)

  } else if (mono != 0) {

    woe <- woe_tbl_(z)
    if (sign(diff(woe)) != mono) {
      return(-3)

    } else {
      return(sum(iv))

    }
  }

  return(sum(iv))
}

## find best split iteratively
## BIG TODO: Cache this result
## TODO: Add an attribute to the tbl that can be used to see if it is new or not?
find_best_split_index_ <- function(tbl, min.cnt=25, min.res=10, mono=0) {
  ivs <- sapply(seq.int(nrow(tbl) - 1), evalulate_split_, tbl, min.cnt=min.cnt, min.res=min.res, mono=mono)

  if (any(ivs > 0)) {
    which.max(ivs)
  } else {
    -1
  }

}

iv_list_of_tables_ <- function(l) {
  ## need a function that can calculate the iv of a list of tbls
  z <- t(sapply(l, colSums))
  sum(iv_(z))
}


Box <- setRefClass("Box", fields=c(
  x = "numeric",
  y = "factor",
  tbls = "list",
  vals = "numeric",
  same = "logical"
), methods = list(

  initialize = function(x, y) {
    stopifnot(!any(is.na(y)))

    f <- !is.na(x)
    x <<- x[f]
    y <<- factor(y[f])
    same <<- FALSE

    stopifnot(identical(length(levels(.self$y)), 2L))
  },

  tabulate = function(eps=0.01) {

    same <<- FALSE
    q <- unique(sort(quantile(x, seq(0, 1, eps), names = FALSE)))
    vals <<- q
    tbls <<- list(table(q[findInterval(x, q, all.inside = FALSE)], y))

  },

  show = function() {
    res <- t(sapply(tbls, colSums))
    row.names(res) <- NULL
    print(res)
  },

  get_iv = function() {
    iv_list_of_tables_(tbls)
  },

  get_breaks = function() {
    c(-Inf, head(vals[cumsum(sapply(tbls, nrow))], -1), Inf)
  },

  make_labels = function() {

    brks <- round(get_breaks(), 3)
    paste(head(brks, -1), tail(brks, -1), sep =  " - ")

  }

  )
)

### Crack the bins into smaller bins
Box$methods(crack = function(iv.inc.min=0.001, min.cnt=25, min.res=10, mono=0) {

  iv_current <- iv_list_of_tables_(tbls)

    best <- list(iv=-Inf, tbls=NULL, i=NULL)
    for (i in seq_along(tbls)) {

      ## check if bin was already evaluated, if so use old value
      if (is.null(attr(tbls[[i]], "k"))) {
        k <- find_best_split_index_(tbls[[i]], min.cnt=min.cnt, min.res=min.res, mono=mono)
      } else {
        k <- attr(tbls[[i]], "k")
      }
  
      ## If a valid split was found
      if (k > 0) {

        ## if k is negative, ignore the split, otherwise evaluate the mkiv
        zs <- split_at_index_(tbls[[i]], k)
        iv <- iv_list_of_tables_(c(zs, tbls[-i]))
        
        attr(tbls[[i]], "k") <<- k
        
        if (iv - iv_current > iv.inc.min) {
          if (iv > best$iv) {
            best <- list(iv=iv, tbls=zs, i=i)
          }
        }
      }
    }
    
    ## check if best is null or not
    if (is.null(best$tbls)) {
      same <<- TRUE
      return(invisible())
    } else { ## There was a split that matched criteria

      same <<- FALSE
      tbls <<- append(tbls, best$tbls, best$i)[-best$i]
    }
})


Box$methods(pack = function(iv.dec.max=0.0005) {
  ## loop over tbls and calculate the iv increase by merging each bin

    current_iv <- iv_list_of_tables_(tbls)

    # best <- list(iv=Inf, i=NULL) ## evaluate all heals? Maybe come back to this
    for (i in seq.int(length(tbls) - 1)) {

      if (identical(length(tbls), 1L)) return(invisible()) ## break if only one bin

      tmp <- tbls
      tmp[[i]] <- do.call(rbind, tmp[i:(i+1)])
      tmp[[i + 1]] <- NULL

      iv <- iv_list_of_tables_(tmp)

      if (current_iv - iv < iv.dec.max) { ## merge them
        tbls <<- tmp
        same <<- FALSE
        i <- 1 ## start over
        current_iv <- iv
      }
    }
})


Box$methods(bento = function(iv.inc.min=0.001, iv.dec.max=0.0005, min.cnt=25, min.res=10, mono=0, max.bin=10, eps=1.1) {
  
  while(!same) { ## no longer changing
    ## record what bins look like here?
    old <- tbls
    crack(iv.inc.min, min.cnt = min.cnt, min.res = min.res, mono=mono)
    pack(iv.dec.max)

    if (identical(old, tbls)) {
      same <<- TRUE
    }

  }

  if (length(tbls) <= max.bin) {
    return(invisible())

  } else {
    same <<- FALSE
    bento(iv.inc.min = iv.inc.min, iv.dec.max = iv.dec.max * eps, min.cnt=min.cnt, min.res=min.res, mono=mono, max.bin=max.bin)
  }

})

compiler::enableJIT(1)


data(titanic, package="onyx")
x <- titanic$Age
y <- titanic$Survived


b <- Box(mtcars$disp, as.integer(mtcars$mpg > 20))
b$tabulate(eps = 0.01)
## make this faster with caching
b$bento(iv.inc.min = 0.001, iv.dec.max = 0.0005, min.cnt = 5, min.res = 2, mono = 0, max.bin = 5, eps=1.01)
res <- b$show()

sum(res)
val <- rbind(
  res, table(is.na(x), y)[2,]
)

library(onyx)
b2 <- bin(data.frame(x), y, min.cnt = 10, min.res = 5, mono = 0, max.bin = 5)


x <- scale(titanic$Fare)

vs <- sapply(seq.int(1:6), function(i) sum(aggregate(x~SibSp < i, titanic, var)[,2]))



mtcars$mpg






