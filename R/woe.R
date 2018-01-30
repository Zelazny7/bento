
# x is a cumulative table of 0s and 1s
woe <- function(cume, totals, adj=0.50) {
  log(((cume[,1] + adj)/totals[1])/((cume[,2] + adj)/totals[2]))
}

# x is table of 0s and 1s
# totals is the total 0s and 1s in a 2-length vector
# woe is output from woe
iv <- function(cume, totals, woe) {
  (cume[,1]/totals[1] - cume[,2]/totals[2]) * woe
}

## find best split uses cumulative totals
find_best_split <- function(cume, tots) {
  woe_l <- woe(cume, tots)

  decum <- t(tots - t(cume))
  woe_r <- woe(decum, tots)

  iv_all_splits <- rowSums(cbind(woe_l, woe_r))

  which.max(iv_all_splits)
}

split_at_index <- function(mat, i) {

  s <- seq.int(i)

  left  <- mat[s,]
  right <- sweep(mat[-s,], 2L, tail(left, 1))

  ## return left and right and respective totals in a list
  list(
    left = list(
      cume = left,
      tots = tail(left, 1)
    ),
    right = list(
      cume = right,
      tots = tail(mat, 1) - tail(left, 1)
    )
  )
}

