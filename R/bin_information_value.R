bin_by_information_value <- function(x, y, min.cnt=25, min.res=10, min.iv=0.001, max.bin=6, mono=0, eps=0.01) {

  ## histogram of the data first
  if (!is.null(eps)) {
    q <- unique(sort(quantile(x, seq(0, 1, eps), names = FALSE)))
    tbl <- table(q[findInterval(x, q)], y)
  } else {
    q <- sort(unique(x))
    tbl <- table(x, y)
  }

  recurse <- function(tbl, vals, nbins={e=new.env();e$cnt=2;e}) {
    #browser()
    if (nrow(tbl) == 1 | nbins$cnt > max.bin) return()

    ## aggregate
    cume  <- apply(tbl, 2, function(x) cumsum(x))
    tots  <- colSums(tbl)
    decum <- t(tots - t(cume))

    ## Weight of evidence stats
    woe_left  <- woe(cume, tots)
    woe_right <- woe(decum, tots)

    iv_left  <- iv(cume, tots, woe_left)
    iv_right <- iv(decum, tots, woe_right)
    iv_total <- iv_left + iv_right

    ## terminal conditions boolean vector
    f <- switch(mono + 2, woe_left < woe_right, TRUE, woe_left > woe_right) &
      rowSums(cume) > min.cnt &
      rowSums(decum) > min.cnt &
      cume[,'1'] > min.res &
      decum[,'1'] > min.res &
      iv_total > min.iv

    ## no conditions met
    if (!isTRUE(any(f))) return()

    ## find the best split position
    i <- seq_along(f)[f][which.max(iv_total[f])]
    nbins$cnt <- nbins$cnt + 1

    ## increment bin count
    result <- split_at_index(tbl, i)

    if (nbins$cnt %% 2 == 0) {
      c(vals[i],
        recurse(result$left, vals[1:i], nbins=nbins),
        recurse(result$right, vals[-(1:i)], nbins=nbins))
      
    } else {
      
      c(vals[i],
        recurse(result$right, vals[-(1:i)], nbins=nbins),
        recurse(result$left, vals[1:i], nbins=nbins))
    }
  }

  result <- recurse(tbl, vals=q)
  if(!is.null(result)) {
    return(sort(result))
  } else {
    NULL
  }

}

