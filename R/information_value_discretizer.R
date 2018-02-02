#' @include opts.R

Information_Value_Discretizer <- setRefClass("Information_Value_Discretizer", fields=c(
    opts = "iv_opts",
    tbls = "list",
    vals = "numeric",
    same = "logical"),
  methods = list(initialize = function(opts) {
    .self$opts <<- opts
  })
)



Information_Value_Discretizer$methods(
  tabulate_ = function(x, y) {

    f <- !is.na(x)
    x <- x[f]
    y <- factor(y[f])

    stopifnot(!any(is.na(y)))
    stopifnot(identical(length(levels(y)), 2L))

    same <<- FALSE
    q <- unique(sort(quantile(x, seq(0, 1, opts$epsilon), names = FALSE)))
    vals <<- q
    tbls <<- list(mjollnir::table2(q[findInterval(x, q, all.inside = FALSE, rightmost.closed = TRUE)], y))

  })

Information_Value_Discretizer$methods(
  fit = function(x, y) {

    tabulate_(x, y) ## create the crosstab used for discretizing

    break_and_heal_()

  })

Information_Value_Discretizer$methods(
  break_and_heal_ = function(iv.dec.max=.self$opts$iv.dec.max) {

    while(!same) { ## no longer changing
      ## record what bins look like here?
      # print("breaking and healing")
      old <- tbls
      break_()
      heal_(iv.dec.max)

      if (identical(old, tbls)) {
        same <<- TRUE
      }
    }

    if (length(tbls) <= opts$max.bin) {
      return(invisible())

    } else {
      same <<- FALSE
      break_and_heal_(iv.dec.max = iv.dec.max * opts$expansion)
    }

  })

  #' @param i index at which to split
  #' @param tbl two-way table summarizing an independent vector and a 0/1 factor
Information_Value_Discretizer$methods(
  evaluate_split_ = function(i, tbl) {

    z <- t(sapply(split_at_index_(tbl, i), colSums))

    iv <- iv_(z)

    if (any(z == 0)) {
      return(-1)

    } else if (any(rowSums(z) < opts$min.cnt)) {
      return(-2)

    } else if (any(z[,2] < opts$min.res)) {
      return(-3)

    } else if (opts$mono != 0) {

      woe <- woe_tbl_(z)
      if (sign(diff(woe)) != opts$mono) {
        return(-4)

      } else {
        return(sum(iv))

      }
    }

    return(sum(iv))
  })

Information_Value_Discretizer$methods(
  find_best_split_index_ = function(tbl) {

    ivs <- sapply(seq.int(nrow(tbl) - 1), .self$evaluate_split_, tbl)

    if (any(ivs > 0)) which.max(ivs) else - 1

  })

Information_Value_Discretizer$methods(
  calc_iv_ = function() {

    iv_list_of_tables_(tbls)

  })

Information_Value_Discretizer$methods(
  break_ = function() {

    iv_current <- calc_iv_()

    best <- list(iv=-Inf, tbls=NULL, i=NULL)
    for (i in seq_along(tbls)) {

      k <- find_best_split_index_(tbls[[i]])

      ## If a valid split was found
      if (k > 0) {

        ## if k is negative, ignore the split, otherwise evaluate the mkiv
        zs <- split_at_index_(tbls[[i]], k)
        iv <- iv_list_of_tables_(c(zs, tbls[-i]))

        if ((iv - iv_current) > opts$iv.inc.min && iv > best$iv) {
          best <- list(iv=iv, tbls=zs, i=i)
        }
      }
    }

    ## check if best is null or not
    if (is.null(best$tbls)) {
      same <<- TRUE ## no change in the table structure
      return(invisible())
    } else { ## There was a split that matched criteria
      same <<- FALSE
      tbls <<- append(tbls, best$tbls, best$i)[-best$i]
    }
  })

Information_Value_Discretizer$methods(
  heal_ = function(iv.dec.max = .self$opts$iv.dec.max) {

    current_iv <- calc_iv_()

    for (i in seq.int(length(tbls) - 1)) {

      if (identical(length(tbls), 1L)) return(invisible()) ## exit if only one bin

      tmp <- tbls
      tmp[[i]] <- do.call(rbind, tmp[i:(i+1)]) ## temporarily merge adjacent bins
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

