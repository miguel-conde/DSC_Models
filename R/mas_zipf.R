range <- 1:dim(model[["m.1G"]])[1]

y <- model[["m.1G"]][order(-Count)][range,Count]

plot(range, y, log = "xy")

x <- range
mZipf <- lm(y ~ x)
mZipf
mZipf <- lm(log(y) ~ log(x))
mZipf
yy <- exp(mZipf$coefficients[1])*(range)^(mZipf$coefficients[2])
lines(x = range, y = yy, col = "red")
plot(mZipf)

range <- 1:1000

y <- model[["m.1G"]][order(-Count)][range,Count]

x <- range
mZipf <- lm(y ~ x)
mZipf
mZipf <- lm(log(y) ~ log(x))
mZipf
yy <- exp(mZipf$coefficients[1])*(range)^(mZipf$coefficients[2])
lines(x = range, y = yy, col = "blue")
plot(mZipf)

## 2G
range <- 1:dim(model[["m.2G"]])[1]

y <- model[["m.2G"]][order(-Count)][range,Count]

plot(range, y, log = "xy")

x <- range
mZipf <- lm(y ~ x)
mZipf
mZipf <- lm(log(y) ~ log(x))
mZipf
yy <- exp(mZipf$coefficients[1])*(range)^(mZipf$coefficients[2])
lines(x = range, y = yy, col = "red")
plot(mZipf)

range <- 1:1000

y <- model[["m.1G"]][order(-Count)][range,Count]

x <- range
mZipf <- lm(y ~ x)
mZipf
mZipf <- lm(log(y) ~ log(x))
mZipf
yy <- exp(mZipf$coefficients[1])*(range)^(mZipf$coefficients[2])
lines(x = range, y = yy, col = "blue")
plot(mZipf)