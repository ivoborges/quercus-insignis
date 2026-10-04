sprintf("precision %.*f, width '%*.3f'", 2, pi, 0, pi)
sprintf("precision %.*f", 5, pi)
sprintf("%.*f", 4, pi)

sprintf("%f", pi)
sprintf("%.3f", pi)
sprintf("%.5f", pi)
sprintf("%.4f", pi)

pi
round(pi)
round(pi, digits = 2)
round(pi, digits = 4)
round(pi, digits = 5)
signif(pi, digits = 1)
signif(pi, digits = 3)

x = 10.76567759879643247
x
format(round(x, 3), nsmall = 3)

format(12.3456789, digits = 4)
format(12.3456789, digits = 7)
format(12.3456789, nsmall = 2)
format(12.3456789, nsmall = 4)
format(12.3556789, nsmall = 7)
format(12.3556789, nsmall = 9)
format(12.3556789, nsmall = 10)

library(plyr)
x = 10.76567759879643247
round_any(x, 0.0001, f = floor)
round_any(x, 0.01, f = floor)
round_any(qi_gbif$decimalLatitude, 0.000000001, f = floor)
View(qi_gbif)

trunc(40.1)
trunc(-20.9)

revtrunc <- function(x) {x - floor(x)}
revtrunc2 <- function(x) x - trunc(x)
revtrunc(10.102001)
revtrunc2(10.102001)

# method 1
x = qi_gbif$decimalLatitude
a <- function(x) {x - floor(x)}
a(x)
nc1 <- nchar(x) > 5 & nchar(x) == 6 
qi_gbif$new_col <- nc1
nc2 <- nchar(x) <= 4
qi_gbif$ncol <- nc2
sum(nc2, na.rm = TRUE)

# method 2
x = qi_gbif$decimalLongitude
y = qi_gbif$decimalLatitude
function(x) { nchar(x) > 2 }
function(y) { nchar(y) > 4 }
t1 <- nchar(x) >= 4
t2 <- nchar(y) >= 7
qi_gbif$column1 <- t1
View(qi_gbif)

# functions
sumsquare <- function(a, b) {
  d <- a + b
  dd <- d * d
  return(dd)
}
sumsquare(1, 2)

v1 <- 1:3
v2 <- 5
sumsquare(v1, v2)

frs <- function(n) {
  s <- sample(letters, n)
  r <- paste0(s, collapse = "")
  return(r)
}

frs(5)

set.seed(1)
frs(10)
set.seed(1)
frs(10)
set.seed(2)
frs(20)
set.seed(2)
frs(20)

substr("Hello World", 1, 5)
substr("Hello World", 7, 10)
gsub("l", "!", "Hello World")
gsub("Hello", "Bye bye", "Hello World")
d <- c("az20", "az21", "az22", "ba30", "ba31", "ab32")
i <- grep("ba", d)
i
d[i]
grep("ba", d, value=TRUE)
grep("2", d)
grep("2$", d)
grep("^b", d)





























