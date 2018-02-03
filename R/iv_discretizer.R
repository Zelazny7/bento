#' @include opts.R discretizer_class.R

#' @title Information Value Discretizer
#' @rdname Information_Value_Discretizer_class
#' @export Information_Value_Discretizer
Information_Value_Discretizer <- setRefClass("Information_Value_Discretizer", fields=c(
    opts = "iv_opts",
    tbls = "list",
    vals = "numeric",
    same = "logical",
    breaks = "numeric"
  ),
  contains="Discretizer")

Information_Value_Discretizer$methods(
  tabulate_ = function(x, y, w=rep(1, length(x))) {
    
    f  <- !is.na(x)
    x_ <- x[f]
    y_ <- factor(y[f])
    w_ <- w[f]

    stopifnot(!any(is.na(y_)))
    stopifnot(identical(length(levels(y_)), 2L))

    same <<- FALSE
    q <- unique(sort(quantile(x_, seq(0, 1, opts$epsilon), names = FALSE)))
    x2 <- q[findInterval(x_, q)]
    tbls <<- list(fast_table(x2, y_, w_))
    vals <<- sort(unique(x2))

  })

# @param i index at which to split
# @param tbl two-way table summarizing an independent vector and a 0/1 factor
Information_Value_Discretizer$methods(
  evaluate_split_ = function(i, tbl) {

    z <- t(sapply(split_at_index_(tbl, i), colSums))
    metric <- metric_(z)

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
        return(sum(metric))
      }
    }

    return(sum(metric))
  })

Information_Value_Discretizer$methods(
  metric_ = function(z) {
    "Calculate metric on a cross-tab of x and y"
    iv_(z)
  })

Information_Value_Discretizer$methods(
  metric_list_of_tables_ = function(l) {
    "Metric calculated on a list of tables"
    z <- t(sapply(l, colSums))
    sum(metric_(z))
  })

