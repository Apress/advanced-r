library(checkpoint)
checkpoint("2016-09-04", R.version = "3.3.1")
library(stringdist)
library(data.table)
library(foreign)

options(width = 70) # only 70 characters per line

## options to reduce number of rows printed
## for the book
options(stringsAsFactors = FALSE,
        datatable.print.nrows = 20, ## if over 20 rows
        datatable.print.topn = 3, ## print first and last 3
        digits = 2) ## reduce digits printed by R

################################################################################
##                                                                            ##
##                       Data munging / cleaning with data.table              ##
##                                                                            ##
################################################################################

## http://www.icpsr.umich.edu/icpsrweb/ICPSR/studies/4691
## read the data in from Stata data format
d <- read.dta("ICPSR_04691/DS0001/04691-0001-Data.dta")

## convert to a data.table object
d <- as.data.table(d)
## add key
setkey(d, IDNUMR)

## structure without attributes, fixed width and only the first 20 columns
str(d, give.attr = FALSE, strict.width = "cut", list.len = 20)

#### Recoding Data ####

## recoding factors
## from the documentation, we can see the values used and that many of them
## we probably want to treat as missing
## although these have consistent labels, their numbers change depending
## how many legitimate levels there are, see examples below
table(d[, EDUCATIO])
table(d[, PLANGUAG])
table(d[, BMICLASS])
table(d[, S1Q01])

## we probably want to recode the below as missing:
## PARTIAL INTERVIEW
## NOT IN UNIVERSE
## MISSING
## LEGITIMATE SKIP
## DONT KNOW
## REFUSED

## there are two challenges to this
## 1) the numbers change, e.g., "97 - REFUSED" for EDUCATIO and "7 - REFUSED" for S1Q01
## 2) because these are factors, if we just set some values to missing, we also want to
## drop those levels

## we can solve this by using regular expressions to search for the character strings we know
## for example, we know that REFUSED always ends in that regardless of whether it starts
## with 7 - or 97 - or anything else
## we can search using regular expressions using the grep() function
## (returns the value or the numeric position of matches) or grepl() function
## (returns a logical hence the l)
## vector of whether each element matched or not
## to see this in action in a simple example:
## pattern is the regular expression used for matching
## x is the data to search
## grep() returns the positions of matches
grep(
  pattern = "abc",
  x = c("a", "abc", "def", "abc", "d"))

## grepl() returns a logical vector
grepl(
  pattern = "abc",
  x = c("a", "abc", "def", "abc", "d"))

## so far we have a very basic regular expression
## we could use this for our data, searching for "REFUSED"
## but generally it is good to be as specific as possible
## for example what if one variable has:
## "1 - REFUSED TREATMENT", "2 - DID NOT REFUSED TREATMENT", "3 - REFUSED"
## where 1 and 2 are valid responses and 3 is refused the question
## a match for REFUSED only would match all of those
## although we may ultimately want to use grepl() as logical values
## are easy to use as indexing to set specfic cases to NA
## to make sure we are grabbing the right values as we practice,
## we can use grep() with the argument, value = TRUE, which returns
## the strings that matched, making it very easy to see what we are
## matching
grep(
  pattern = "REFUSED",
  x = c(
    "1 - REFUSED TREATMENT",
    "2 - DID NOT REFUSED TREATMENT",
    "3 - REFUSED"),
  value = TRUE)

## to make it more specific we could add the -
## although better, this still gives us a false positive
grep(
  pattern = "- REFUSED",
  x = c(
    "1 - REFUSED TREATMENT",
    "2 - DID NOT REFUSED TREATMENT",
    "3 - REFUSED"),
  value = TRUE)

## to go further we can specify that - REFUSED must be the last part of the string
## that is, nothing can come after - REFUSED.  This is done using the $ in regular expressions
## which signifies the end of the string
grep( pattern = "- REFUSED$",
  x = c(
    "1 - REFUSED TREATMENT",
    "2 - DID NOT REFUSED TREATMENT",
    "3 - REFUSED"),
  value = TRUE)

## we could take this a step further by matching the following
## 1 or more numbers followed by a space followed by a dash followed by a space
## followed by REFUSED which ends the string
## this is accomplished using [0-9] which signifies the digits 0 to 9 and + which means
## 1 or more times (but not zero times)
grep( pattern = "[0-9]+ - REFUSED$",
  x = c(
    "1 - TREATMENT REFUSED TREATMENT",
    "2 - DID NOT REFUSED TREATMENT",
    "3 - JACK - REFUSED",
    "4 - REFUSED",
    "97 - REFUSED",
    "- REFUSED"),
  value = TRUE)

## as a final step, we know that if its the REFUSED we want, the string will start with
## a number as well, because it may be a negative number we will expand our previous expression
## to search for - 0 or more times at the start of the string,  followed by a number 1 or more times,
## etc.  We indicate the start of the string using ^, and we indicate 0 or more times using *
## using this regular expression will help protect us against matching legitimate values
## in the data and errantly converting them to NA as we are quite specific in what we are looking for
## and anything that does not match our pattern will be rejected
grep( pattern = "^[-]*[0-9]+ - REFUSED$",
  x = c(
    "1 - TREATMENT REFUSED TREATMENT",
    "2 - DID NOT REFUSED TREATMENT",
    "3 - JACK - REFUSED",
    "4 - REFUSED",
    "97 - REFUSED",
    "-97 - REFUSED",
    "- REFUSED",
    "TRICK CASE 4 - REFUSED"),
  value = TRUE)

## note that by default, regular expressions are also case sensitive
## in depth coverage of how to use regular expressions is beyond this book, but we
## will show and explain some examples throughout this chapter on data cleaning
## as regular expressions are quite useful for matching and working with string data
## because they allow us to encode very specific "searches"


## we want to match more than just REFUSED, however
## one option would be to create individual regular expressions
## for each and loop through, but this is cumbersome and inefficient
## instead we can modify our regular expression to indicate different
## options in some positions, this is done by using pipe, |, to separate options,
## which functions as "or"
## below we keep the stuff about the start of the string but allow
## REFUSED or MISSING to be the ending text
grep(
  pattern = "^[-]*[0-9]+ - REFUSED$|MISSING$",
  x = c(
    "1 - TREATMENT REFUSED TREATMENT",
    "2 - DID NOT REFUSED TREATMENT",
    "3 - JACK - REFUSED",
    "4 - REFUSED",
    "97 - REFUSED",
    "-97 - REFUSED",
    "-2 - MISSING",
    "- REFUSED",
    "TRICK CASE 4 - REFUSED",
    "4 - REFUSED HELP"),
  value = TRUE)

## now we are ready ready to replace responses with NA for the patterns we want
## as you can see, the regular expression has built into something rather complex
## a word of caution is that many symbols have special meanings in regular expressions
## so if you want to actually search for a special symbol, care must be taken
## for instance, as we have seen * is used to indicate zero or more times,
## not a literal asterisk.  To search for a literal character, it can be escaped
## using \ backslashes
p <- "^[-]*[0-9]+ - REFUSED$|MISSING$|DON'T KNOW$|LEGITIMATE SKIP$|PARTIAL INTERVIEW$|NOT IN UNIVERSE$"

grep( pattern = p,
  x = c(
    "1 - TREATMENT REFUSED TREATMENT",
    "2 - DID NOT REFUSED TREATMENT",
    "3 - JACK - REFUSED",
    "4 - REFUSED",
    "97 - REFUSED",
    "-97 - REFUSED",
    "-2 - MISSING",
    "96 - DON'T KNOW",
    "-4 - LEGITIMATE SKIP",
    "-3 - PARTIAL INTERVIEW",
    "-2 - NOT IN UNIVERSE",
    "- REFUSED",
    "TRICK CASE 4 - REFUSED",
    "4 - PARTIAL INTERVIEW OF DOCTOR"),
  value = TRUE)


## now that we can find cases we want to set to missing, we can do it
## rather than type each variable, we will loop through them in data table
## we set matching cases to NA and then if its a factor droplevels
## and otherwise return as is
v <- c("EDUCATIO", "PLANGUAG", "BMICLASS", "S1Q01", "S2Q01")
d[, (v) := lapply(.SD, function(x) {
  x[grepl(pattern = p, x)] <- NA
  if (is.factor(x)) droplevels(x) else x
}), .SDcols = v]

## examine results
table(d[, EDUCATIO])
table(d[, S1Q01])

## note that there is some inefficiency because the data are copied
## each time for the function run in data table
## it would be more efficient to simply select the relevant rows in data.table
## and then set those missing
## the difficulty here is that our data are factors, and we want to drop the excess levels
## or coerce them to characters anyway
## we did not coerce them to factors in the first place, the data came that way from
## the STATA data file, and this is simply part of dealing with the data that we get
## if we had all character data, we may have used a slightly more efficient idiom


## another recoding task we may wish to do is to drop the numbers
## so that only the labels remain
## this can be accomplished using the gsub() function
## which performs regular expression matching within a string
## and then replaces matches with specified values
## (which can be a zero length string)
## first we look at a simple example
gsub(
  pattern = "abc",
  replacement = "",
  x = c("a", "abcd", "123abc456"))

## Because it just uses regular expressions, we can re-use our pattern from before
## to match and remove numbers
p.remove <- "^[-]*[0-9]+ - "
gsub(
  pattern = p.remove,
  replacement = "",
  x = c(
    "1 - TREATMENT REFUSED TREATMENT",
    "2 - DID NOT REFUSED TREATMENT",
    "3 - JACK - REFUSED",
    "4 - REFUSED",
    "97 - REFUSED",
    "-97 - REFUSED",
    "-2 - MISSING",
    "96 - DON'T KNOW",
    "-4 - LEGITIMATE SKIP",
    "-3 - PARTIAL INTERVIEW",
    "-2 - NOT IN UNIVERSE",
    "- REFUSED",
    "TRICK CASE 4 - REFUSED",
    "4 - PARTIAL INTERVIEW OF DOCTOR"))

## for efficiency, we can combine this with our previous
## code to set some cases to missing
## doing it in one step avoids repetive processing
## first we read the data back in so we have a 'fresh start'

d <- read.dta("ICPSR_04691/DS0001/04691-0001-Data.dta")
d <- as.data.table(d)
setkey(d, IDNUMR)


## because gsub() coerces input to character
## regardless if we pass factors or characters to it
## we will get character out, so to know whether it was a factor or not,
## we create an object, f, which is a logical whether x started off as a factor
## then rather than drop levels, we just convert to factor if appropriate
d[, (v) := lapply(.SD, function(x) {
  f <- is.factor(x)
  x[grepl(pattern = p, x)] <- NA
  x <- gsub(pattern = p.remove, replacement = "", x)
  if (f) factor(x) else x
}), .SDcols = v]

## examine results
table(d[, EDUCATIO])
table(d[, S1Q01])
## health of child
table(d[, S2Q01])


#### Recoding Numeric Values ####

## recoding numeric values is somewhat different
## often times, it is more about values that are out of bounds
## as missing numeric values are often coded as negative numbers or
## very high numbers (e.g., 999)
## we can find values that fall between a range using the %between% operator
## in the data.table package, and we can find the complement using ! as before

## for example for height, from the documentation values
## zero or below and above 90 should be missing
table(d[!S2Q02R %between% c(0, 90), S2Q02R])

## for example for weight, from the documentation values
## zero or below and above 900 should be missing
table(d[!S2Q03R %between% c(0, 900), S2Q03R])

## again we could operate on these variables individually
## however, that becomes cumbersome for many variables
## the pattern often is if valid values are less than 9, then 9 is missing
## if values go into double digits, then >90 is missing, and so on
## we can do this by examining the maximum value and setting the bounds accordingly

## see what the variable was like originally
emf(file = "../Images/figure_8-1a.emf", width=4, height = 4)
hist(d[, .(S2Q03R)])
dev.off()

emf(file = "../Images/figure_8-1b.emf", width=4, height = 4)
hist(d[, .(S2Q02R)])
dev.off()

## variables to be recoded / have some values converted to missing
v2 <- c("S2Q02R", "S2Q03R", "AGEYR_CH")
## m for maximum
m <- sort(c(9, 99, 999))

## for each variable, loop through calling the name k
for (k in v2) {
  ## start off with j and i = 1 for each variable
  j <- i <- 1
  ## as long as j == 1 and i is less than or
  ## equal the number of values to try (9, 99, 999)
  ## keep doing the next calculation
  while(j == 1 & i <= length(m)) {
    ## if the maximum (ignoring missing) of the variable, k,
    ## is less than the maximum of the ith m value,
    ## then set j to 0 (will break the loop)
    ## and replace any values outside the range of 0 and the ith m - 9
    ## if the ith m is greater than 90 or minus some very small number (1 to the -9th)
    ## and for these cases (rows) set the variable to missing (NA_integer_)
    if (max(d[[k]], na.rm = TRUE) < m[i]) {
      j <- 0
      d[!(get(k) %between% c(0, ifelse(m[i] > 90,
        m[i] - 9, m[i] - 1e-9))), (k) := NA_integer_]
    } else {
      ## if condition not met, iterate i up 1
      i <- i + 1
    }
  }
}

## see the variable after cleansing
emf(file = "../Images/figure_8-2a.emf", width=4, height = 4)
hist(d[, .(S2Q02R)])
dev.off()

emf(file = "../Images/figure_8-2b.emf", width=4, height = 4)
hist(d[, .(S2Q03R)])
dev.off()

## height of child
table(d[, S2Q02R])

## weight of child
table(d[, S2Q03R])


## age of child
table(d[, AGEYR_CH])


################################################################################
##                                                                            ##
##                        Example - Creating New Variables                    ##
##                                                                            ##
################################################################################

## The questionnaire asks whether a doctor or health professional ever told the
## respondent that the focal child had any of 9 possible condictions
## (e.g., asthma, ADD or ADHD, depression or anxiety problems, diabetes,
## developmental delay or physical impairment).
## the goal is to create a composite variable capturing number of problems
## some problems only apply to older children and some respondents may not know
## or may have refused, so we might take the average number of 'yes' responses
## to calculate proportion of 'yes' responses out of all valid responses per
## respondent.

## first create list of the variables
v.health <- paste0("S2Q", c(19, 20, 21, 22, 23, 24, 26, 35, 37))

## get the variables, unlist from a data table into a vector
## and table that to see all possible responses
## we need to cleanup to get just Yes/No
table(unlist(d[, v.health, with = FALSE]))


## we already know how to clean these sort of variables
## using regular expressions and grep()
p <- "^[-]*[0-9]+ - REFUSED$|MISSING$|DON'T KNOW$|LEGITIMATE SKIP$|PARTIAL INTERVIEW$|NOT IN UNIVERSE$"

d[, (v.health) := lapply(.SD, function(x) {
  x[grepl(pattern = p, x)] <- NA
  if (is.factor(x)) droplevels(x) else x
}), .SDcols = v.health]

## check all responses now
## include NAs in the table
table(unlist(d[, v.health, with = FALSE]), useNA = "ifany")

## create new variable based on row means of non missing values
## do this by leverage the fact that logical values are stored as
## FALSE = 0 and TRUE = 1, so we test if the character data == "1 - YES"
## then take the row means of those logical values
d[, HealthConditions := rowMeans(d[, v.health, with = FALSE] == "1 - YES", na.rm = TRUE)]

table(round(d$HealthConditions, 2),
      useNA = 'ifany')

## we could follow a similar process to get the
## raw counts
d[, NHealthConditions := rowSums(d[, v.health, with = FALSE] == "1 - YES", na.rm = TRUE)]

## looking at the counts
## something has happened though
## there are no missing values!
## this is because with rowSums() when missing values are ignored
## if all values are missing, you end up with a 0, not a missing value
## to correct this we need to manually set to missing
## any cases / rows where all are missing
table(d$NHealthConditions, useNA = 'ifany')

## count number of missing responses per respondent
d[, NMissHealthConditions := rowSums(is.na(d[, v.health, with = FALSE]))]

## select rows where number missing equals total
## and set these to missing
d[NMissHealthConditions == length(v.health), NHealthConditions := NA_integer_]

## better
table(d$NHealthConditions, useNA = 'ifany')

## if ignoring missing values is not an issue,
## an easy way to work directly with lists is using the Reduce() function
## Reduce() takes a function (typically binary) as its first argument, and
## a vector or list of arguments
## as an example with a vector
Reduce(`+`, c(1, 2, 3))
## and another example with a list
Reduce(`+`, list(1:3, 4:6, 7:9))
## each element of the list is added equivalent to
1:3 + 4:6 + 7:9

## other operators can also be used such as multiplication
Reduce(`*`, list(1:3, 4:6, 7:9))

## division
Reduce(`/`, list(1:3, 4:6, 7:9))

## subtraction
Reduce(`-`, list(1:3, 4:6, 7:9))

## raising to a power
Reduce(`^`, list(1:3, 4:6, 3:1))

## this can then be applied to data table easily
## we cannot directly add the health variables
## because they are factor class data
## but we could write a function to deal with it
fplus <- function(e1, e2) {
  if (is.factor(e1)) {
    e1 <- as.numeric(e1) - 1
  }
  if (is.factor(e2)) {
    e2 <- as.numeric(e2) - 1
  }
  e1 + e2
}

## we select the columns, and then .SD will be a list
## of variables which we can reduce and store results in
## a new variable
d[, NHealthConditions2 := Reduce(fplus, .SD), .SDcols = v.health]

table(d$NHealthConditions2)

################################################################################
##                                                                            ##
##                           Example - Fuzzy Matching                         ##
##                                                                            ##
################################################################################

## one task that sometimes comes up is matching character strings
## For example you may have a list of registered users and then
## individuals write their name on an attendance sheet
## or you may be working with filenames where the file names are
## supposed to match certain pieces of information

## here are two lists
## the reference names are let's say our registered user list
## these are the names we believe in and want to match to
## the observed names are what was written in quickly on our
## hypothetical sign in / attendance sheets
## users may have attended more than one even and thus show up
## multiple times, or may not show up at all

reference.Names <- c("This Test", "Test Thas",
  "Jane Mary", "Jack Dun-Dee")
observed.Names <- c("this test", "test this", "test that",
  "JaNe  Mary.", "Mary Sou", "Jack Dee", "Jane Jack")


## The challenge is to find how many events each registered user
## actually signed in to

## a first pass can be done using stringdistmatrix() function
## from the stringdist package
## we use the Damerau-Levenshtein distance method
## which essentially counts how many characters have to change
## to go from one string to the other
## from this, we can see for each observed name, how many characters
## must be changed to match one of our reference names
## for example for 'this test' it takes two character changes to match
## reference name 1, 8 for reference name 2, etc.
## now this is case sensitive, by default
stringdistmatrix(reference.Names, observed.Names, method = "dl")

## if we wanted only matches, we could use approximate matching
## it takes similar arguments, but also the maximum distance allowed
## before calling nothing a match
amatch(observed.Names, reference.Names,
       method = "dl", maxDist = 4)

## we could expand a bit and make it non case sensitive
## by converting to lower case using tolower() or to upper case
## using toupper()
stringdistmatrix(
  tolower(reference.Names),
  tolower(observed.Names), method = "dl")

## we could also remove some things we don't care about
## for example, periods or other punctuation
stringdistmatrix(
  tolower(gsub("\\.", "", reference.Names)),
  tolower(gsub("\\.", "", observed.Names)), method = "dl")

## another challenge with names could be
## thinking about whether someone wrote: firstname lastname
## or if they wrote: lastname firstname
## we can 'ignore' ordering by splitting names up
## for this it is easiest to work one name at a time
reference.Names[1]
observed.Names[2]

## we can split a character string by some character
## using strsplit(), we use spaces specifically
## we use backslash s as a special character
## in regex that means any type of space
strsplit(
  x = reference.Names[1],
  split = "\\s")[[1]]

strsplit(
  x = observed.Names[2],
  split = "\\s")[[1]]

## now we can use stringdistmatrix() again
## the result shows that each sub chunk of the second observed name
## is one character manipulation away from one of the sub chunks
## from the first reference name
stringdistmatrix(
  strsplit(
    x = reference.Names[1],
    split = "\\s")[[1]],
  strsplit(
    x = observed.Names[2],
    split = "\\s")[[1]],
  method = "dl")

## if we ignore case, we get perfect matches
## the second chunk of the observed name 2 matches the first chunk
## of reference name 1, and the first chunk of the observed name 2
## perfectly matches the second chunk of the reference name 1
stringdistmatrix(
  strsplit(
    x = tolower(reference.Names[1]),
    split = "\\s")[[1]],
  strsplit(
    x = tolower(observed.Names[2]),
    split = "\\s")[[1]],
  method = "dl")

## to return the sum of the best matches we can pick the minimum and sum
## what this shows us is that ignoring cases and ignoring ordering
## the second observed name is a perfect match for the first reference name
## something we would never have gotten if we tested it as an overall string
sum(apply(stringdistmatrix(
  strsplit(
    x = tolower(reference.Names[1]),
    split = "\\s")[[1]],
  strsplit(
    x = tolower(observed.Names[2]),
    split = "\\s")[[1]],
  method = "dl"), 1, min))


## combining everything we have learned:
## 1) values to ignore, such as punctuation (if desired)
## 2) splitting strings (if desired)
## 3) ignoring cases (if desired)
## we can write a function to help with matching names
## to this we also add an optional argument to control
## whether to include 'close' matches within some degree of
## 'fuzz'
## we also count exact matches, as these are 'easy'
## this code is not neceesarily optimized for speed or efficiency
## but is a pass at combining many aspects of what we learned and then applying
## to a set of names where each written name (index argument) is
## compared against a vector of possible candidates (the 'pool')
matchIt <- function(index, pool,
                    ignore = c("\\.", "-"), split = FALSE,
                    ignore.case = TRUE, fuzz = .05, method = "dl") {
  ## for those things we want to ignore, drop them
  ## e.g., remove spaces, periods, dashes
  rawpool <- pool

  for (v in ignore) {
    index <- gsub(v, "", index)
    pool <- gsub(v, "", pool)
  }
  ## if ignore case, convert to lower case
  if (ignore.case) {
    index <- tolower(index)
    pool <- tolower(pool)
  }

  if (!identical(split, FALSE)) {
    index2 <- strsplit(index, split = split)[[1]]
    index2 <- index2[nzchar(index2)]
    pool2 <- strsplit(pool, split = split)
    pool2 <- lapply(pool2, function(x) x[nzchar(x)])

    ## calculate distances defaults to the Damerau-Levenshtein distance
    distances <- sapply(pool2, function(x) {
      sum(apply(stringdistmatrix(index2, x, method = method),
                1, min, na.rm = TRUE))
    })
  } else {
    ## calculate distances defaults to the Damerau-Levenshtein distance
    distances <- stringdist(index, pool, method = method)
  }

  ## some methods result in Infinity answers, set these missing
  distances[!is.finite(distances)] <- NA

  ## get best and worst
  best <- min(distances, na.rm = TRUE)
  worst <- max(distances, na.rm = TRUE)

  ## if fuzz, grab all distances that are within fuzz percent of
  ## the difference between best and worst, tries to capture
  ## potentially very close matches to the best
  if (fuzz) {
    usedex <- which(distances <= (best + ((worst - best) * fuzz)))
  } else {
    usedex <- which(distances == best)
  }

  ## define a distance below which its considered a
  ## perfect or exact match
  perfect <- distances[usedex] < .01
  out <- rawpool[usedex]

  ## count the number of perfect matches
  count <- sum(perfect)

  if (any(perfect)) {
    ## if there are perfect matches, just use one
    match <- out[perfect][1]

    ## return a data table of the perfect match
    ## and number of perfect matches (probably 1)
    data.table(
      Match = match,
      N = count,
      Written = NA_character_)
  } else {
    ## if no perfect matches,
    ## return a list of close matches, comma separated
    ## also return what exactly was written
    data.table(
      Match = paste(out, collapse = ", "),
      N = count,
      Written = index)
  }
}


## loop through each observed name
## and try to match it to reference names
## within 5% of the best match (.05)
output <- lapply(observed.Names, function(n) {
  matchIt(
    index = n,
    pool = reference.Names,
    ignore = c("\\.", "-"),
    split = "\\s",
    fuzz = .05)
})

## since the output is always the same
## can combine it row wise

output <- do.call(rbind, output)
## show output
output

## we can see that 'This Test' shows up in two rows
## so we can aggregate up
finaloutput <- output[, .(
  N = sum(N),
  Uncertain = paste(na.omit(Written), collapse = ", ")),
  by = Match]

## print final output
## two perfect matches for 'This Test'
## no perfect match for 'Test Thas' but a close one,
## listed under the 'Uncertain' column
## 'jack dee' is a close but uncertain match for "Jack Dun-Dee"
## and 'jane jack' is uncertain match for "Jane Mary" or "Jack Dun-Dee"
finaloutput


## although it is challenging to cover every example and use case,
## these basic tools should be enough to cover many types of data management tasks
## when used in combination
