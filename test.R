## Weight of evidence based discretization

data(titanic, package="onyx")

x <- titanic$Fare
y <- titanic$Survived

## discretize based on percentile? yes
q <- unique(sort(quantile(x, seq(0, 1, 0.01), names = FALSE)))
x2 <- q[findInterval(x, q)]

tbl <- table(x2, y)
tbl <- table(x, y)

cume <- apply(tbl, 2, function(x) cumsum(x))
tots <- colSums(tbl)
decum <- t(tots - t(cume))


recurse <- function(tbl, vals) {
  # print("recursing")
  ## base cases
  
  ## calculate stuff
  # browser()
  cume  <- apply(tbl, 2, function(x) cumsum(x))
  tots  <- colSums(tbl)
  decum <- t(tots - t(cume))
  
  ## calculate more stuff
  woe_left  <- woe(cume, tots)
  woe_right <- woe(decum, tots)
  
  iv_left  <- iv(cume, tots, woe_left)
  iv_right <- iv(decum, tots, woe_right)
  iv_total <- iv_left + iv_right
  
  ## Terminal conditions
  f <- switch(mono + 2, woe_left < woe_right, TRUE, woe_left > woe_right) &
    rowSums(cume)  > min.cnt &
    rowSums(decum) > min.cnt &
    cume[,'1']  > min.res & 
    decum[,'1'] > min.res &
    iv_total > min.iv
  
  ## no conditions met
  if (!any(f)) return()
  
  ## else split and recurse
  # i <- which.max(iv_total[f])
  i <- seq_along(f)[f][which.max(iv_total[f])]
  #print(i)
  # print(i)
  
  result <- split_at_index(tbl, i)
  print(result$left)
  
  c(vals[i],
    recurse(result$left, vals[1:i]),
    recurse(result$right, vals[-(1:i)]))
}

min.cnt = 50
min.res = 10
min.iv = 0.001
mono = 0
res <- recurse(tbl, vals=sort(unique(x))) ## what to return? ### TODO: fix this thing here
