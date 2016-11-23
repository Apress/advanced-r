## load checkpoint and required packages
library(checkpoint)
checkpoint("2016-09-04", R.version = "3.3.1")
library(devEMF)
library(Hmisc)
options(width = 70) # only 70 characters per line


## Components of a function

## create new function, f()
f <- function(x, y = 5) {
  x + y
}

formals(f)
args(f)

body(f)

environment(f)

environment(install.packages)

## Scoping

plot
plot <- 5
plot

search()

environment()
parent.env(.GlobalEnv)

a <- "free variable"
f <- function(x = "formal variable") {
  y <- "local variable"

  e <- environment()
  print(e)
  print(parent.env(e))

  print(a)
  print(x)
  print(y)
}

f()

f1 <- function(y = "f1 var") {
  x <- y
  a1 <- f2(x)
  rm(x)
  a2 <- f2(x)
}
f2 <- function(x) {
  if (nchar(x) < 10) {
    x <- "f2 local var"
  }
  print(x)
  return(x)
}

x <- "global var"
f1()
x <- "g var"
f1()
rm(x)
f1()

## functions for functions

f1 <- function(type = c("first", "second")) {
  type
}
f2 <- function(type = c("first", "second")) {
  type <- match.arg(type)
  type
}

f1("fi")
f2("fi")

f1("test")
f2("test")


f1b <- function(type = c("first", "second")) {
  if (type == "first") {
    x <- mean(1:5)
  } else if (type == "second") {
    x <- sd(1:5)
  }
  return(x)
}

f1b("test")


cohend <- function(x, y) {
  if (!missing(y)) {
    x <- y - x
  }

  mean(x) / sd(x)
}

cohend(x = c(0.61, 0.99, 1.47, 1.52, 0.45,
             3.34, 1.05, -1.47, 1.3, 0.33),
       y = c(-0.69, 1.6, 0.44, 1, 0.88,
             1.17, 2.4, 1.21, 0.87, 2.15))

cohend(x = c(0.61, 0.99, 1.47, 1.52, 0.45,
             3.34, 1.05, -1.47, 1.3, 0.33))


cv <- function(x, na.rm = FALSE) {
  fcall <- match.call()

  est <- sd(x, na.rm = na.rm) / mean(x, na.rm = na.rm)

  return(list(CV = est, Call = fcall))
}

cv(1:5)

cv(1:8, na.rm = TRUE)


f <- function(x) {
  if (x < 4) return("I'm done!")

  paste(x, "- Fin!")
}

f(10)
f(3)



f <- function(x) {
  if (x < 4) {
    "I'm done!"
  } else {
    paste(x, "- Fin!")
  }
}

f(10)
f(3)


cv <- function(x, na.rm = FALSE) {
  fcall <- match.call()

  est <- sd(x, na.rm = na.rm) / mean(x, na.rm = na.rm)

  print(est)
  return(invisible(list(CV = est, Call = fcall)))
}

cv(1:8, na.rm = TRUE)

res <- cv(1:8, na.rm = TRUE)
res$Call


f <- function(x) {
  on.exit(print("Game over"))
  x + 5
}

f(3)
f("a")


f <- function(x, trans = c("identity", "log")) {
  trans <- match.arg(trans)

  if (trans == "log") {
    if (any(x < 0)) stop("Log is not defined for negative values")
    if (any(x < 1e-16)) warning("Some x values close or equal to zero, results may be unstable")

    x <- log(x)
    message("x successfully log transformed")

    exp(mean(x))
  } else {
    mean(x)
  }
}


f(c(1, 2, 100))

f(c(1, 2, 100), trans = "log")

suppressMessages(f(c(1, 2, 100), trans = "log"))

f(c(0, 1, 2, 100), trans = "log")

suppressWarnings(f(c(0, 1, 2, 100), trans = "log"))

f(c(-1, 1, 2, 100), trans = "log")


#### Debugging

## debug()
meanPlot <- function(formula, d) {
  v <- all.vars(formula)
  m <- tapply(d[, v[1]], d[, v[2]],
              FUN = mean, na.rm = TRUE)

  plot(formula, data = d, type = "p")
  points(x = unique(d[, v[2]]), y = m,
         col = "blue", pch = 16, cex = 2)
}

## emf(file = "../Images/figure_04-01.emf", width = 4, height = 4)
postscript(file = "../Images/figure_04-01.eps", width = 4, height = 4)
meanPlot(mpg ~ cyl, d = mtcars)
dev.off()

debug(meanPlot)

meanPlot(mpg ~ cyl, d = mtcars)


meanPlot <- function(formula, d) {
  v <- all.vars(formula)
  d <- d[order(d[, v[2]]), ] ## sorting first
  m <- tapply(d[, v[1]], d[, v[2]],
              FUN = mean, na.rm = TRUE)

  plot(formula, data = d, type = "p")
  points(x = unique(d[, v[2]]), y = m,
         col = "blue", pch = 16, cex = 2)
}

## emf(file = "../Images/figure_04-02.emf", width = 4, height = 4)
postscript(file = "../Images/figure_04-02.eps", width = 4, height = 4)
meanPlot(mpg ~ cyl, d = mtcars)
dev.off()


## browser()
meanPlot <- function(formula, d) {
  v <- all.vars(formula)
  d <- d[order(d[, v[2]]), ] ## sorting first
  m <- tapply(d[, v[1]], d[, v[2]],
              FUN = mean, na.rm = TRUE)

  browser()

  plot(formula, data = d, type = "p")
  points(x = unique(d[, v[2]]), y = m,
         col = "blue", pch = 16, cex = 2)
}

meanPlot(mpg ~ cyl, d = mtcars)


## traceback()
lm(mpg ~ jack, data = mtcars)
traceback()

## editing functions
wtd.quantile(c(1, 2, 3, Inf, NA),
          weights = c(.6, .9, .4, .2, .6))

wtd.quantile



wtd.table

revised.wtd.table <- function (x, weights = NULL, type = c("list", "table"), normwt = FALSE,
    na.rm = TRUE)
{
    type <- match.arg(type)
    if (!length(weights))
        weights <- rep(1, length(x))
    isdate <- testDateTime(x)
    ax <- attributes(x)
    ax$names <- NULL
    if (is.character(x))
        x <- as.factor(x)
    lev <- levels(x)
    x <- unclass(x)
    if (na.rm) {
        s <- !is.na(x + weights) & is.finite(x + weights)
        x <- x[s, drop = FALSE]
        weights <- weights[s]
    }
    n <- length(x)
    if (normwt)
        weights <- weights * length(x)/sum(weights)
    i <- order(x)
    x <- x[i]
    weights <- weights[i]
    if (anyDuplicated(x)) {
        weights <- tapply(weights, x, sum)
        if (length(lev)) {
            levused <- lev[sort(unique(x))]
            if ((length(weights) > length(levused)) && any(is.na(weights)))
                weights <- weights[!is.na(weights)]
            if (length(weights) != length(levused))
                stop("program logic error")
            names(weights) <- levused
        }
        if (!length(names(weights)))
            stop("program logic error")
        if (type == "table")
            return(weights)
        x <- all.is.numeric(names(weights), "vector")
        if (isdate)
            attributes(x) <- c(attributes(x), ax)
        names(weights) <- NULL
        return(list(x = x, sum.of.weights = weights))
    }
    xx <- x
    if (isdate)
        attributes(xx) <- c(attributes(xx), ax)
    if (type == "list")
        list(x = if (length(lev)) lev[x] else xx, sum.of.weights = weights)
    else {
        names(weights) <- if (length(lev))
            lev[x]
        else xx
        weights
    }
}

wtd.table(c(1, 2, 3, Inf, NA),
          weights = c(.6, .9, .4, .2, .6))

revised.wtd.table(c(1, 2, 3, Inf, NA),
          weights = c(.6, .9, .4, .2, .6))


assignInNamespace(x = "wtd.table",
                  value = revised.wtd.table,
                  ns = "Hmisc")

wtd.quantile(c(1, 2, 3, Inf, NA),
          weights = c(.6, .9, .4, .2, .6))

wtd.Ecdf(c(1, 2, 3, Inf, NA),
          weights = c(.6, .9, .4, .2, .6))

