## Weight of evidence based discretization

data(titanic, package="onyx")

x <- rep(titanic$Fare, 1000)
y <- rep(titanic$Survived, 1000)

## discretize based on percentile? yes
q <- unique(sort(quantile(x, seq(0, 1, 0.01), names = FALSE)))
x2 <- q[findInterval(x, q)]

tbl <- mjollnir::table2(x2, y)
# tbl <- table(x, y)

cume <- apply(tbl, 2, function(x) cumsum(x))
tots <- colSums(tbl)
decum <- t(tots - t(cume))


recurse <- function(tbl, vals, nbins={e=new.env();e$cnt=2;e}) {
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
  if (!any(f)) return()

  ## find the best split position
  i <- seq_along(f)[f][which.max(iv_total[f])]
  nbins$cnt <- nbins$cnt + 1

  ## increment bin count
  result <- split_at_index(tbl, i)

  c(vals[i],
    recurse(result$left, vals[1:i], nbins=nbins),
    recurse(result$right, vals[-(1:i)], nbins=nbins))
}


res <- recurse(tbl, vals=q) ## what to return? ### TODO: fix this thing here

d <- cut(x, c(-Inf, res, Inf))

e <- new.env()
# onyx::bin(data.frame(x), y)



r1 <- bin_by_information_value(x, y, min.cnt = 50, min.res = 10, min.iv = 0.01, mono = 0, max.bin = 10, eps = NULL)

