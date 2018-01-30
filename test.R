## Weight of evidence based discretization


## test data
x <- rnorm(1e6)
y <- rbinom(x, 1, pnorm(x))
brks <- hist(x, plot=FALSE, breaks = 100)

x2 <- brks$breaks[findInterval(x, brks$breaks)]
tbl <- table(x2, y)

cume <- apply(tbl, 2, function(x) cumsum(x))
tots <- colSums(tbl)
decum <- t(tots - t(cume))


iv_left  <- iv(cume, tots, woe(cume, tots))
iv_right <- iv(decum, tots, woe(decum, tots))

which.max(rowSums(cbind(iv_left, iv_right)))


i <- find_best_split(cume, tots)

tmp <- split_at_index(cume, i)
l1 <- do.call(find_best_split, tmp$left)
r1 <- do.call(find_best_split, tmp$right)

