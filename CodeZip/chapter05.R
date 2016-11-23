## load checkpoint and required packages
library(checkpoint)
checkpoint("2016-09-04", R.version = "3.3.1")
library(devEMF)
library(ggplot2)
options(width = 70) # only 70 characters per line


#### S3 System - Classes ####
class(mtcars)

table

x <- table(mtcars$cyl)
class(x) <- c("newclass", "table")
class(x)


d <- data.frame(
  x = c(1, 3, 5),
  y = c(1, 2, 4),
  labels = c("First", "Second", "Third")
)
class(d) <- "textplot"

print(d)

class(d) <- c("textplot", "data.frame")

print(d)

textplot_data <- function(f, d) {
  stopifnot(inherits(d, "data.frame"))
  stopifnot(inherits(f, "formula"))

  newdata <- get_all_vars(formula = f, data = d)
  colnames(newdata) <- c("y", "x", "labels")
  class(newdata) <- c("textplot", "data.frame")

  return(newdata)
}

## example use
textplot_data(f = mpg ~ hp | cyl, d = mtcars[1:10, ])

#### S3 System - Methods ####

plot.textplot <- function(d) {
  ## adjust the margins through a call to the
  ## graphical parameters, and store old parameters in op
  op <- par(mar = c(4, 4, 1, 1))
  ## ensure graphing parameters restored to what
  ## they were when function completes
  on.exit(par(op))
  ## create a new plot area
  plot.new()
  ## size the plot appropriate for the data
  plot.window(xlim = range(d$x, na.rm = TRUE),
              ylim = range(d$y, na.rm = TRUE))
  ## add the text to the plot
  text(d$x, d$y, labels = d$labels)
  ## create some axes to show the limits of the data
  axis(side = 1, range(d$x, na.rm = TRUE))
  axis(side = 2, range(d$y, na.rm = TRUE))
  ## return the object invisibly
  invisible(d)
}

dat <- textplot_data(f = mpg ~ hp | cyl, d = mtcars[1:10, ])

## emf(file = "../Images/figure_05-01.emf", width = 4, height = 4)
cairo_ps(file = "../Images/figure_05-01.eps", width = 4, height = 4)
plot(dat)
dev.off()


plot

methods(plot)

plot.table

graphics:::plot.table

m <- lm(mpg ~ hp * vs + factor(cyl), data = mtcars)
summary(m)

class(m)
methods(ggplot)


ggplot.lm <- function(data, mapping, vars, ...) {
  newdat <- do.call(expand.grid, vars)
  yvar <- as.character(formula(data)[[2]])
  d <- as.data.frame(predict(data, newdata = newdat, se.fit = TRUE))
  d <- within(d, {
    LL <- fit + qnorm(.025) * se.fit
    UL <- fit + qnorm(.975) * se.fit
  })
  colnames(d)[1] <- yvar
  data <- cbind(newdat, d[, c(yvar, "LL", "UL")])
  ggplot(data = data, mapping = mapping, ...)
}

## emf(file = "../Images/figure_05-02.emf", width = 4, height = 4)
cairo_ps(file = "../Images/figure_05-02.eps", width = 4, height = 4)
ggplot(m, aes(hp, mpg), vars = list(
                          hp = min(mtcars$hp):max(mtcars$hp),
                          vs = mean(mtcars$vs),
                          cyl = 8)) +
  geom_line(size=2) +
  theme_bw()
dev.off()


## tiff(file = "../Images/figure_05-03.tiff", width = 5, height = 4, units = "in",
##      res = 1200, compression="lzw+p")
cairo_ps(file = "../Images/figure_05-03.eps", width = 5, height = 4)
ggplot(m, aes(hp, mpg, linetype = factor(vs), group = factor(vs)), vars = list(
                          hp = min(mtcars$hp):max(mtcars$hp),
                          vs = c(0, 1),
                          cyl = 8)) +
  geom_ribbon(aes(ymin = LL, ymax = UL), alpha = .25) +
  geom_line(size=2) +
  theme_bw()
dev.off()

#### S4 System - Classes ####

setClass(
  Class = "textplot",
  slots = c(
    x = "numeric",
    y = "numeric",
    labels = "character"),
  prototype = list(
    x = numeric(0),
    y = numeric(0),
    labels = character(0)),
  validity = function(object) {
    stopifnot(
      length(object@x) == length(object@y),
      length(object@x) == length(object@labels))
    if (!all(nchar(object@labels) > 0, na.rm = TRUE)) {
      stop("All labels must be missing or non zero length characters")
    }
    return(TRUE)
  }
)

new("textplot",
    x = c(1, 3, 5),
    y = c(1, 2, 4),
    labels = c("First", "Second", "Third"))

new("textplot",
    x = c(1, 3, 5),
    y = c(1, 2, 4),
    labels = 1:3)

new("textplot",
    x = c(1, 3, 5),
    y = c(1, 2, 4),
    labels = c("First", "Second", ""))


## multiple errors
new("textplot",
    x = c(1, 3, 5),
    y = c(1, 2),
    labels = c("First", "Second", ""))

## diversion on formatting text strings
cat("ab", fill = TRUE)

cat("a\nb", fill = TRUE)

paste(c("a", "b"), c(1, 2), sep = "")

paste(c("a", "b"), collapse = "\n")


sprintf("First (%d), Second (%d), Third (%d)", 98, 80, 75)

sprintf("Integer %d, Numeric %0.2f, String %s, They won by 58%%",
        5, 3.141593, "some text")


## Revised to catch multiple errors and be more informative
textplot <- setClass(
  Class = "textplot",
  slots = c(
    x = "numeric",
    y = "numeric",
    labels = "character"),
  prototype = list(
    x = numeric(0),
    y = numeric(0),
    labels = character(0)),
  validity = function(object) {
    errors <- character()
    if (length(object@x) != length(object@y)) {
      errors <- c(errors,
                  sprintf("x (length %d) and y (length %d) are not equal",
                          length(object@x), length(object@y)))
    }
    if (length(object@x) != length(object@labels)) {
      errors <- c(errors,
                  sprintf("x (length %d) and labels (length %d) are not equal",
                          length(object@x), length(object@labels)))
    }
    if (!all(nchar(object@labels) > 0, na.rm = TRUE)) {
      errors <- c(errors, sprintf(
        "%d label(s) are zero length. All labels must be missing or non zero length",
        sum(nchar(object@labels) == 0, na.rm = TRUE)))
    }

    if (length(errors)) {
      stop(paste(c("\n", errors), collapse = "\n"))
    } else {
      return(TRUE)
    }
  }
)

## multiple errors more informative
textplot(
  x = c(1, 3, 5),
  y = c(1, 2),
  labels = c("First", "Second", ""))



#### S4 System - Class Inheritance ####

## Revised to catch multiple errors and be more informative
groupedtextplot <- setClass(
  Class = "groupedtextplot",
  slots = c(
    group = "factor"),
  prototype = list(
    group = factor()),
  contains = "textplot",
  validity = function(object) {
    if (length(object@x) != length(object@group)) {
      stop(sprintf("x (length %d) and group (length %d) are not equal",
                   length(object@x), length(object@group)))
    }
    return(TRUE)
  }
)

gdat <- groupedtextplot(
    group = factor(c(1, 1, 1, 1, 2, 2, 2, 2)),
    x = 1:8,
    y = c(1, 3, 4, 2, 6, 8, 7, 10),
    labels = letters[1:8])
gdat

groupedtextplot(
    group = factor(c(1, 1, 1, 1, 2, 2, 2, 2)),
    x = 1:8,
    y = c(1, 3, 4, 2, 6, 8, 7),
    labels = c(letters[1:7], ""))


#### S4 System - Methods ####

## Show S4 methods for a given function or a class
?showMethods


## define a method for the show function
## used when an object is simply typed at the console

## method
setMethod(
  f = "show",
  signature = "textplot",
  definition = function(object) {
    cat("     X: ")
    cat(head(object@x, 5), fill = TRUE)
    cat("     Y: ")
    cat(head(object@y, 5), fill = TRUE)
    cat("Labels: ")
    cat(head(object@labels, 5), fill = TRUE)
  })

## nicer way of showing data
dat <- textplot(
  x = 1:4,
  y = c(1, 3, 5, 2),
  labels = letters[1:4])
dat

## using class inheritance means that
## methods can be inherited too
gdat

## define a method for subseting
## or selecting components
setMethod(
  f = "[",
  signature = "textplot",
  definition = function(x, i, j, drop) {
    if (missing(i) & missing(j)) {
      out <- x
      validObject(out)
    } else if (!missing(i) & missing(j)) {
      out <- textplot(
        x = x@x[i],
        y = x@y[i],
        labels = x@labels[i])
      validObject(out)
    } else if (!missing(j)) {
      if (missing(i)) {
        i <- seq_along(x@x)
      }

      if (is.character(j)) {
        out <- lapply(j, function(n) {
          slot(x, n)[i]
        })
        names(out) <- j
      } else if (is.numeric(j)) {
        n <- slotNames(x)
        out <- lapply(j, function(k) {
          slot(x, n[j])[i]
        })
        names(out) <- n[j]
      } else {
        stop("j is not a valid type")
      }
    }

    return(out)
  })

## examples using it
dat[]
dat[i = 1:2]
dat[j = 1]
dat[j = "y"]
dat[i = 1:2, j = c("x", "y")]


## show method for groupedtextplot
setMethod(
  f = "show",
  signature = "groupedtextplot",
  definition = function(object) {
    n <- unique(object@group)
    i <- lapply(n, function(index) {
      cat("Group: ", index, fill = TRUE)
      show(object[which(object@group == index)])
    })
  })

gdat
