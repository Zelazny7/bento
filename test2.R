
data(titanic, package="onyx")
x <- titanic$Fare
y <- titanic$Survived


opts <- discretizer_options("iv", max.bin=5, epsilon=0.1)
disc <- Information_Value_Discretizer$new(opts)

disc$fit(x, y)


b <- Box(x, y)
b$tabulate(eps = 0.01)






## make this faster with caching
b$bento(iv.inc.min = 0.001, iv.dec.max = 0.0005, min.cnt = 10, min.res = 5, mono = 0, max.bin = 5)


# b$breaks()

b2 <- onyx::bin(data.frame(x), y, min.cnt = 10, min.res = 5, mono = 0, max.bin = 5)
b2$variables$x$summary()['IV']

## bounce back between crack and packing until under max.bins
## TODO: add drop=False everywhere
## TODO: add ability

res <- b$show()
ps <- apply(res, 2, pct_)
barplot(woe_(ps[,1], ps[,2]), horiz = TRUE)
b2$variables$x$plot()
## create a list of boxes

b2$variables$x$show()
res[,2]/rowSums(res)



b$get_breaks







# b$vals
# cbind(b$vals, b$tbls[[1]])


b$get_breaks()



res <- b$show()
row.names(res) <- b$make_labels()

