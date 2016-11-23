## load checkpoint and required packages
library(checkpoint)
checkpoint("2016-09-04", R.version = "3.3.1")
library(devEMF)
library(RPostgreSQL)
library(DBI)
library(RSQLite)
library(jsonlite)
library(tm)
library(wordcloud)
library(RMongo)
library(data.table)
options(width = 70) # only 70 characters per line


## options to reduce number of rows printed
## for the book
options(stringsAsFactors = FALSE,
        datatable.print.nrows = 20, ## if over 20 rows
        datatable.print.topn = 3, ## print first and last 3
        digits = 2) ## reduce digits printed by R


########################################################################
##                                                                    ##
##            SQLite and R                                            ##
##  https://cran.r-project.org/web/packages/RSQLite/RSQLite.pdf       ##
########################################################################


#After connecting to SQLite, we also name our database.
#if the database already existed, we would link to that
#rather than creating a new one - it is the same code:
con1 = dbConnect(SQLite(), dbname = "C:/sqlite/LiteDB.db")
dbIsValid(con1)


##dbRemoveTable(con1, "Iris")
##again, if this was pre-existing, then this wouldn't happen first.
diris <- as.data.table(iris)
dbWriteTable(con1, "Iris", diris, overwrite = FALSE,
             append = FALSE)
dbListTables(con1)
dbListFields(con1, "Iris")

#If you have a database that is _horizontally_ large (e.g. many tables)
#then tables may be easily read into R as they may be small enough
diris_lite1 <- dbReadTable(con1, "Iris")
diris_lite1 <- as.data.table(diris_lite1)
all.equal(diris, diris_lite1)

##diris
##diris_lite1

##now we will get just a subset of the iris data - thereby saving memory.

##If we just want one column and all only rows 
query <- dbSendQuery(con1, "SELECT [Sepal.Length] FROM Iris WHERE Species = 'setosa'")
diris_lite2 <- dbFetch(query, n=-1)
head(diris_lite2)
dbClearResult(query)

query <- dbSendQuery(con1, "SELECT [Sepal.Length], Species FROM Iris Where Species <> 'setosa'")
diris_lite3 <- dbFetch(query, n=-1)
head(diris_lite3)
dbClearResult(query)

query <- dbSendQuery(con1, "SELECT * FROM Iris Where Species <> 'setosa'")
diris_lite4 <- dbFetch(query, n=-1)
diris_lite4 <- as.data.table(diris_lite4)
diris_lite4
dbClearResult(query)

###These next two examples demonstrate how large data may be pushed
#be mindful of your available RAM if you decide to experiment
#just choose one of these is our advice.


#534.1Mb of 70,000,000 elements
BigData <- rnorm(70000000)
BigData <-as.data.table(BigData)
#writes about 0.5Gb to our SQLite database file
#The names match both in R and SQLite
dbWriteTable(con1, "BigData", BigData)
dbListTables(con1)


##the default connection to SQLite has a 2Gb 'cap'
#1.5Gb of 200,000,000
BiggerData <- rnorm(200000000, 1)
BiggerData <-as.data.table(BiggerData)
#writes about 1.5Gb to our SQLite database file
#The names match both in R and SQLite
dbWriteTable(con1, "BiggerData", BiggerData)
dbListTables(con1)


##Delete = DROP a Table
dbRemoveTable(con1, "BiggerData")
dbListTables(con1)
#notice the size of the data base file may NOT be smaller
#the space is 'free', but still claimed by SQLite.
#VACUUM command is needed unless we enabled auto_vacuum = FULL
#VACUUM can use up to 2x the diskspace of the original file during process
#auto_vacuum is different than VACUUM
#this fits the class of database management operations better left 
#directly to SQLite rather than via R perhaps.




##disconnects from the SQLite database
dbDisconnect(con1)
dbIsValid(con1)
rm(diris_lite1, diris_lite2, diris_lite3, diris_lite4)

q()##test <- read.csv()

########################################################################
##                                                                    ##
##            PostgreSQL and R                                        ##
##                                                                    ##
########################################################################

#notice that if host = web url then you're accessing a remote database
drv <- dbDriver("PostgreSQL")

con2 <- dbConnect(drv, dbname = "postgres", host = "localhost",
                 port = 5432, user = "postgres", password = "advancedr")

###

dbGetInfo(con2)
dbListTables(con2)

##again, if this was pre-existing, then this wouldn't happen first.
diris <- as.data.table(iris)
dbWriteTable(con2, "iris", diris, overwrite = TRUE)
dbListTables(con2)
dbListFields(con2, "iris")

#If you have a database that is _horizontally_ large (e.g. many tables)
#then tables may be easily read into R as they may be small enough
diris_gre1 <- dbReadTable(con2, "iris")
diris_gre1 <- as.data.table(diris_gre1)
all.equal(diris, diris_gre1)
diris
diris_gre1

##now we will get just a subset of the iris data - thereby saving memory.

##Rather than read an entire table, we can start to select subsets.
##dbGetQuery(con2, "SET search_path TO public")


query <- dbSendQuery(con2, statement = "SELECT * FROM iris")
diris_gre2 <- dbFetch(query, n=-1)
head(diris_gre2)
dbClearResult(query)



##If we just want one column and first three rows
query <- dbSendQuery(con2, 'SELECT "Petal.Length" FROM iris')
diris_gre3 <- dbFetch(query, n=6)
head(diris_gre3)
dbClearResult(query)

##If we just want all columns and first three rows where petal.length is long
query <- dbSendQuery(con2, 'SELECT * FROM iris WHERE "Petal.Length" > 2')
diris_gre4 <- dbFetch(query, n=3)
head(diris_gre4)
dbClearResult(query)

query <- dbSendQuery(con2, 'SELECT "Sepal.Length", "Species" FROM iris WHERE "Sepal.Width" <> 2.2')
diris_gre5 <- dbFetch(query, n=-1)
head(diris_gre5)
dbClearResult(query)


###These next two examples demonstrate how large data may be pushed
#be mindful of your available RAM if you decide to experiment
#just choose one of these is our advice.


#534.1Mb of 70,000,000 elements
BigData <- rnorm(70000000)
BigData <-as.data.table(BigData)
#writes about 0.5Gb to our PostgreSQL database file
#The names match both in R and PostgreSQL
dbWriteTable(con2, "bigdata", BigData)
dbListTables(con2)



#1.5Gb of 200,000,000
#BiggerData <- rnorm(200000000, 1)
#BiggerData <-as.data.table(BiggerData)
#writes about 1.5Gb to our PostgreSQL database file
#The names match both in R and PostgreSQL
dbWriteTable(con2, "BiggerData", BiggerData)
dbListTables(con2)


##Delete = DROP a Table
dbListTables(con2)
dbRemoveTable(con2, "BiggerData")
dbListTables(con2)

####


dbDisconnect(con2)
dbUnloadDriver(drv)




# ########################################################################
# ##                                                                    ##
# ##            MongoDB and R                                           ##
# ##                                                                    ##
# ########################################################################
# #rmongodb is no longer on CRAN
# #certainly there were pieces of code that did not work with latest version
# #still, this is sad.
# 
# 
# ##This would connect us to a specific database
# con3<-mongo.create(host = "127.0.0.1:27017",name = "", username = "",
#              password = "", db = "test", timeout = 0L)
# 
# ##This would connect us to a specific server only.
# con3<-mongo.create(host = "127.0.0.1:27017",name = "", username = "",
#                    password = "", timeout = 0L)
# 
# 
# mongo.is.connected(con3)
# mongo.get.databases(con3)
# 
# 
# ##load in mock JSON data
# ch10data <- fromJSON("Ch10_MOCK_DATA.json")
# is.data.frame(ch10data)
# 
# ##convert R object to JSON
# jch10data <- toJSON(ch10data, dataframe = "columns")
# 
# ##convert JSON object to BSON (MOngoDB's format)
# mch10data<-mongo.bson.from.JSON(jch10data)
# 
# ##Notice we can insert the entire object as a single object
# ##This is likely not our goal
# mongo.insert(con3, "test.ch10data", mch10data)
# mongo.find.one(con3, "test.ch10data")
# mongo.drop(con3, "test.ch10data")
# 
# 
# ##instead, we want to create a list where each item of our list
# ##holds one document of our mock data
# ##then we insert the data into 
# is.data.frame(ch10data)
# ch10data_lst <- split(ch10data, rownames(ch10data))
# ch10bsonlst<-lapply(ch10data_lst, mongo.bson.from.list)
# mongo.insert.batch(con3, "test.ch10data", ch10bsonlst)
# 
# 
# ##here is where the reading starts.
# mongo.find.one(con3, "test.ch10data")
# 
# mfound<-mongo.find.all(con3, "test.ch10data", list(gender="Male"))
# 
# nameDT<- data.frame(Name="", blah=1:length(mfound),stringsAsFactors = FALSE)
# 
# for(i in 1:length(mfound)){
#   RecordX <- unlist(mfound[i])
#   RecordX <- as.data.frame(RecordX, stringsAsFactors = FALSE)
#   nameDT[i,"Name"]<-RecordX["first_name",]
# }
# 
# 
# firstNames<-Corpus(VectorSource(nameDT$Name))
# inspect(firstNames)
# wordcloud(firstNames, max.words = 20)
# 
# 
# mongo.destroy(con3)

########################################################################
##                                                                    ##
##            MongoDB and R     Take 2 w/ RMongo                      ##
##                                                                    ##
########################################################################

##This connects us to a specific database
con3<-mongoDbConnect("test", host = "127.0.0.1", port=27017)

##If authentication is required
#username = ""
#password = ""
#dbAuthenticate(con3,username,password)

#dbShowCollections(con3)

## This gets the first 8 entries, and
## does not have any search criteria 
output1 <- dbGetQuery(con3, 'restaurants' , "{}", 0,8 )
is.data.frame(output1)
names(output1)
output1[1,]



##this gets all restaurants who only do American cuisine.
output2 <- dbGetQuery(con3, 'restaurants' , '{"cuisine": "American "}')
is.data.frame(output2)
output2 <- as.data.table(output2)
rNames <- Corpus(VectorSource(output2$name))
inspect(rNames)
wordcloud(rNames, max.words = 20)


output21 <- dbGetQuery(con3, 'restaurants' , '{"cuisine": "/a/"}')

output4 <-dbGetQuery(con3, 'mock', '{}', 0,1)
#This gets all male users in our mock data base
output3 <- dbGetQuery(con3, 'mock', '{"gender": "Male"}')
fNames <- Corpus(VectorSource(output3$first_name))
wordcloud(fNames, max.words = 20)

diagnoses <- Corpus(VectorSource(output3$diagnoses))
wordcloud(diagnoses, max.words = 25)

dbDisconnect(con3)
