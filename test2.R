
data(titanic, package="onyx")
x <- titanic$Fare
y <- titanic$Survived


opts <- discretizer_options("iv", max.bin=12, mono=1,  min.res=10, min.cnt=25, epsilon=0.01)
disc <- Information_Value_Discretizer$new(opts)

x <- titanic$Age
disc$fit(x, y)
disc$breaks

res <- table(cut(x, disc$breaks, right = FALSE), y, useNA = "ifany")
z <- cbind(res, res[,2]/rowSums(res))
barplot(z[,3], horiz=TRUE)

