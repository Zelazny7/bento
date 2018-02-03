
data(titanic, package="onyx")
x <- titanic$Fare
y <- titanic$Survived


opts <- discretizer_options("iv", max.bin=20, mono=-1,  min.res=1, min.cnt=5, epsilon=0.01)
disc <- Information_Value_Discretizer$new(opts)

#x <- rep(titanic$Age, 1e3)
#y <- rep(titanic$Survived, 1e3)
disc$fit(x, y)
disc$breaks

res <- table(cut(x, disc$breaks, right = FALSE), y, useNA = "ifany")
z <- cbind(res, res[,2]/rowSums(res))
barplot(z[,3], horiz=TRUE)

