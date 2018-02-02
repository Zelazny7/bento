
pct_ <- function(vec, adj=0.50) {
  (vec + adj)/sum(vec)
}

woe_ <- function(pct1, pct2, adj=0.50) {
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


iv_list_of_tables_ <- function(l) {
  ## need a function that can calculate the iv of a list of tbls
  z <- t(sapply(l, colSums))
  sum(iv_(z))
}


fast_table <- function(x, y, w=rep(1, length(x))) {
  cpp_table(as.numeric(x), as.numeric(y), as.numeric(w))
}
