
data(titanic, package="onyx")
x <- titanic$Fare
y <- titanic$Survived


opts <- discretizer_options("iv", max.bin=5, epsilon=0.1)
disc <- Information_Value_Discretizer$new(opts)



