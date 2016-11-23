## source the checkpoint.R file to load the libraries
## and ensure the same version is used
## and set the width of output is 70 characters
source("checkpoint.R")

## options to reduce number of rows printed
## for the book
options(stringsAsFactors = FALSE,
        datatable.print.nrows = 20, ## if over 20 rows
        datatable.print.topn = 3, ## print first and last 3
        digits = 2) ## reduce digits printed by R

######################################################################
##                                                                  ##
##           Read in, Clean, and Save the NCHS 2003 Data            ##
##                                                                  ##
######################################################################

#### NOTE ####

## the codebook is in the PDF file "04691-0001-Codebook.pdf"
## which indicates the variable names, and how they are coded
## and all their labels, which can be used to determine how to
## do coding in R

#### Begin Data Cleaning ####


## update path to reflect location of the Stata data file on your computer
## read.dta() comes from the foreign package and can read Stata data files into R
d <- read.dta("ICPSR_04691/DS0001/04691-0001-Data.dta")
## convert to a data table
d <- as.data.table(d)
## key the data table
setkey(d, IDNUMR)

## height in inches -- set missing values to missing in R
d[S2Q02R < 0 | S2Q02R > 90, S2Q02R := NA_integer_]
## weight in pounds -- set missing values to missing in R
d[S2Q03R < 0 | S2Q03R > 900, S2Q03R := NA_integer_]
## calculate BMI (lbs * 703)/in^2
d[, BMI := (S2Q03R * 703) / S2Q02R^2]

## BMI has some very exztreme values, can recode to missing for new BMIClean variable
quantile(d$BMI, probs = c(.01, .99), na.rm = TRUE)
d[BMI >= 11 & BMI <= 39, BMIClean := BMI]


## age position relative to other children in household
## remove ' CHILD' as repetitive and then convert to factor
## specifying the exact order or levels
d[, AGEPOS4 := factor(gsub(" CHILD", "", gsub("[0-9] - ", "", AGEPOS4)),
                      levels = c("ONLY", "OLDEST", "2ND OLDEST",
                                 "3RD OLDEST", "4TH OLDEST"))]

## child sex
## gsub() extracts just the numeric codes
## from those codes can select only the two of interest as valid levels
## in a factor object and label them
d[, Sex := factor(gsub("(-*[0-9]+)(.*)", "\\1", S1Q01),
                  levels = c("2", "1"),
                  labels = c("Female", "Male"))]

## highest household education of any adult
## again extract numeric codes and use these to recode the variable
d[, Education := factor(gsub("(-*[0-9]+)(.*)", "\\1", EDUCATIO),
                  levels = c("1", "2", "3"),
                  labels = c("< High School", "High School", "> High School"))]

## child's health
## extract numeric codes at the beginning of the labels and use to recode
d[, ChildHealth := factor(gsub("(-*[0-9]+)(.*)", "\\1", S2Q01),
                  levels = c("5", "4", "3", "2", "1"),
                  labels = c("Poor", "Fair", "Good", "Very Good", "Excellent"))]

## number of children in household
d[, TOTKIDS4 := factor(gsub("(-*[0-9]+)(.*)", "\\1", TOTKIDS4),
                  levels = c("1", "2", "3", "4"),
                  labels = c("1", "2", "3", "4+"))]

v.general <- c(
  "IDNUMR", "STATE", "AGEYR_CH", "TOTKIDS4",
  "S2Q02R", "S2Q03R", "BMI", "BMIClean", "AGEPOS4", "Sex",
               "Education", "ChildHealth")

#### Number of Health Conditions ####

## first create list of the variables
v.health <- paste0("S2Q", c(19, 20, 21, 22, 23, 24, 26, 35, 37))

## using regular expressions and grep()
p <- "^[-]*[0-9]+ - REFUSED$|MISSING$|DON'T KNOW$|LEGITIMATE SKIP$|PARTIAL INTERVIEW$|NOT IN UNIVERSE$"

d[, (v.health) := lapply(.SD, function(x) {
  x[grepl(pattern = p, x)] <- NA
  if (is.factor(x)) droplevels(x) else x
}), .SDcols = v.health]

## raw counts
d[, NHealthConditions := rowSums(d[, v.health, with = FALSE] == "1 - YES", na.rm = TRUE)]
## count number of missing responses per respondent
d[, NMissHealthConditions := rowSums(is.na(d[, v.health, with = FALSE]))]
## select rows where number missing equals total and set those counts to missing
d[NMissHealthConditions == length(v.health), NHealthConditions := NA_integer_]

## add to the 'use variable' list
v.general <- c(v.general, "NHealthConditions", "NMissHealthConditions")


## does child have at least one 'personal' doctor or nurse?
d[, S5Q01 := factor(gsub("(-*[0-9]+)(.*)", "\\1", S5Q01),
                  levels = c("0", "1"),
                  labels = c("No", "Yes"))]
v.general <- c(v.general, "S5Q01")

## ever repeat grade since kindergarten?
d[, S7Q09 := factor(gsub("(-*[0-9]+)(.*)", "\\1", S7Q09),
                  levels = c("0", "1"),
                  labels = c("No", "Yes"))]
v.general <- c(v.general, "S7Q09")


## During past week how many nights adequate sleep?
d[!S7Q20 %in% 0:7, S7Q20 := NA_integer_]
v.general <- c(v.general, "S7Q20")

## During past week how many days exercise at least 20 minutes?
d[!S7Q21 %in% 0:7, S7Q21 := NA_integer_]
v.general <- c(v.general, "S7Q21")


#### Concerned About Child Items ####
recode.concern <- function(x) {
  x <- as.integer(gsub("(-*[0-9]+)(.*)", "\\1", x))
  x[!x %in% 1:3] <- NA_integer_
  as.integer(x)
}
## variable list
v.concern <- paste0("S7Q", 30:40)
## do the actual recoding
d[, (v.concern) := lapply(.SD, recode.concern), .SDcols = v.concern]


index <- rowSums(is.na(d[, v.concern, with = FALSE])) < length(v.concern)
m <- mirt(d[index, v.concern, with = FALSE], 1)
## higher values (1, 2, 3) indicate no concern, so reverse final score to make it 'concern'
d[index, Concern := -fscores(m)]

## some checks
hist(d$Concern)

d[, .(M = mean(Concern, na.rm =TRUE)), by = S7Q30][order(S7Q30)]
d[, .(M = mean(Concern, na.rm =TRUE)), by = S7Q31][order(S7Q31)]
d[, .(M = mean(Concern, na.rm =TRUE)), by = S7Q40][order(S7Q40)]


#### Child Characteristics ####
recode.characteristics <- function(x) {
  x <- as.integer(gsub("(-*[0-9]+)(.*)", "\\1", x))
  x[!x %in% 1:4] <- NA_integer_
  as.integer(x)
}
## variable list
v.characteristics <- paste0("S7Q", c(56, 45, 53, 52, 44, 41, 54, 59, 48, 62, 63))
## do the actual recoding
d[, (v.characteristics) := lapply(.SD, recode.characteristics), .SDcols = v.characteristics]

index <- rowSums(is.na(d[, v.characteristics, with = FALSE])) < length(v.characteristics)
m <- mirt(d[index, v.characteristics, with = FALSE], 1)
d[index, NegCharacter := fscores(m)]

## some checks
hist(d$NegCharacter)
## negative items
d[, .(M = mean(NegCharacter, na.rm =TRUE)), by = S7Q56][order(S7Q56)]
d[, .(M = mean(NegCharacter, na.rm =TRUE)), by = S7Q45][order(S7Q45)]
d[, .(M = mean(NegCharacter, na.rm =TRUE)), by = S7Q62][order(S7Q62)]
## positive items
d[, .(M = mean(NegCharacter, na.rm =TRUE)), by = S7Q53][order(S7Q53)]
d[, .(M = mean(NegCharacter, na.rm =TRUE)), by = S7Q52][order(S7Q52)]
d[, .(M = mean(NegCharacter, na.rm =TRUE)), by = S7Q54][order(S7Q54)]




saveRDS(
  object = d[, c(v.general, v.health, v.concern, v.characteristics,
                 "NHealthConditions", "NMissHealthConditions",
                "Concern", "NegCharacter"), with = FALSE],
  file = "NCHS2003_Cleaned.RDS")
