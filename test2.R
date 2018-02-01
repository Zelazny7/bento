
pct_ <- function(vec, adj=0.50) {
  (vec + adj)/sum(vec)
}

woe_ <- function(pct1, pct2, adj=0.50) {
  log(pct1 / pct2)
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


tbl <- table(titanic$Age, factor(titanic$Survived, levels = 0:1))

## find best split iteratively
find_best_split_index_ <- function(tbl) {
  ivs <- sapply(seq.int(nrow(tbl) - 1), function(i) {
    z <- t(sapply(split_at_index_(tbl, i), colSums))
    sum(iv_(z))
  })
  
  which.max(ivs)
}

## need a function that can calculate the iv of a list of tbls

iv_list_of_tables_ <- function(l) {
  z <- t(sapply(l, colSums))
  sum(iv_(z))
}


x <- rep(titanic$Age, 1000)
y <- rep(titanic$Survived, 1000)
tbl <- table(x, factor(y, levels = 0:1))

i <- find_best_split_index_(tbl)
tbls <- split_at_index_(tbl, i)

## loop over tbls and split each one and calculate the iv:
system.time(
for (j in 1:20) {
  
## this is the break step
  
candidates <- list()
for (i in seq_along(tbls)) {
  
  k <- find_best_split_index_(tbls[[i]])
  
  zs <- split_at_index_(tbls[[i]], k)
  iv <- iv_list_of_tables_(c(zs, tbls[-i]))
  
  candidates[[i]] <- list(iv=iv, index=k, tbls=zs)
  
}

## find the best bin to break
best <- which.max(sapply(candidates, "[[", iv))

## replace the tables with it
tbls <- append(tbls, candidates[[best]]$tbls, best)[-best]

})

## add the heal step

## also add the checks for various conditions (min.res, min.cnt, max.bin, etc...)

## TODO add unit tests