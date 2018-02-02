
data(titanic, package="onyx")
x <- titanic$Fare
y <- titanic$Survived


opts <- discretizer_options("iv", max.bin=12, min.res=10, min.cnt=25, epsilon=0.1)
disc <- Information_Value_Discretizer$new(opts)


x <- titanic$Age
disc$fit(x, y)
disc$breaks

table(cut(x, disc$breaks, right = FALSE), y, useNA = "ifany")

findInterval(1:10, 5:7, rightmost.closed = FALSE)
