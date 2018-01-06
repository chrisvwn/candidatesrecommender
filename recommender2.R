library(data.table)

if(!require(exploratory))
{
  devtools::install_github("exploratory-io/exploratory_func")
  library(exploratory)
}

library(dplyr)

# hexToText <- function(msg){
#   hex <- sapply(seq(1, nchar(as.character(msg)), by=2), 
#                 function(x) substr(msg, x, x+1))
#   hex <- subset(hex, !hex == "00")
#   gsub('[^[:print:]]+', '', rawToChar(as.raw(strtoi(hex, 16L))))
# }

# #unzip("jobs.zip", overwrite = T, exdir = "jobs")
# 
# #jobs <- fread("jobs/jobs.tsv" )
# 
# jobs$StartDate <- as.POSIXct(jobs$StartDate)
# jobs$EndDate <- as.POSIXct(jobs$EndDate)
# jobs$State <- as.factor(jobs$State)
# jobs$Country <- as.factor(jobs$Country)
# jobs$City <- as.factor(jobs$City)
# jobs$Zip5 <- as.factor(jobs$Zip5)
# 
# #sort by date
# jobs <- jobs[order(jobs$StartDate),]
# 
# users  <- fread("users.tsv")
# 
# #unzip("apps.zip")
# 
# apps<- fread("apps.tsv")
# 
# apps$ApplicationDate <- as.POSIXct(apps$ApplicationDate)
# apps$Split <- as.factor(apps$Split)
# 
# apps <- apps[order(apps$ApplicationDate),]
# 
# startIdx <- 1
# 
# maxNumValidUsers <- 0
# maxStartIdx <- 0
# 
# day1 <- lubridate::as_date(apps$ApplicationDate[startIdx])
# 
# day6 <- day1 + lubridate::period("5 days")
# 
# day9 <- day1 + lubridate::period("8 days")
# 
# 
# train <- apps[lubridate::as_date(apps$ApplicationDate) <= day6,]
# 
# test <- apps[lubridate::as_date(apps$ApplicationDate) > day6 & lubridate::as_date(apps$ApplicationDate) <= day9,]
# 
# trainUsers <- table(train$UserID)
# 
# trainUsers <- as.integer(names(trainUsers[trainUsers >= 5]))
# 
# testUsers <- table(test$UserID)
# 
# testUsers <- as.integer(names(testUsers[testUsers >= 5]))
# 
# validUserIDs <- intersect(trainUsers, testUsers)
# 
# validUsers <- users[UserID %in% validUserIDs,]
# 
# validTrainApps <- apps[apps$UserID %in% validUsers & apps$ApplicationDate <= day6,]
# validTestApps <- apps[apps$UserID %in% validUsers & lubridate::as_date(apps$ApplicationDate) > day6 & lubridate::as_date(apps$ApplicationDate) <= day9,]
# 
# validTrainJobIDs <- unique(validTrainApps$JobID)
# validTestJobIDs <- unique(validTestApps$JobID)
# 
# validTrainJobs <- jobs[jobs$JobID %in% validTrainJobIDs,]
# validTestJobs <- jobs[jobs$JobID %in% validTestJobIDs,]
# 
# write.table(validUsers, "validUsers.tsv", sep="\t", row.names = F)
# write.table(validTrainApps, "validTrainApps.tsv", sep = "\t", row.names = F)
# write.table(validTestApps, "validTestApps.tsv", sep = "\t", row.names = F)
# write.table(validTrainJobs, "validTrainJobs.tsv", sep = "\t", row.names = F)
# write.table(validTestJobs, "validTestJobs.tsv", sep = "\t", row.names = F)

validUsers <- read.csv("validUsers.tsv", sep="\t", stringsAsFactors = F)
validTrainApps <- read.csv("validTrainApps.tsv", sep="\t", stringsAsFactors = F)
validTestApps <- read.csv("validTestApps.tsv", sep="\t", stringsAsFactors = F)
validTrainJobs <- read.csv("validTrainJobs.tsv", sep="\t", stringsAsFactors = F)
validTestJobs <- read.csv("validTestJobs.tsv", sep="\t", stringsAsFactors = F)

validTrainJobs$Description <- iconv(validTrainJobs$Description, to="UTF-8", sub="byte")
validTestJobs$Description <- iconv(validTestJobs$Description, to="UTF-8", sub="byte")

validTrainJobs$Description <- stringi::stri_escape_unicode(validTrainJobs$Description)
validTestJobs$Description <- stringi::stri_escape_unicode(validTestJobs$Description)

#clean descriptions
validTrainJobs <- exploratory::clean_data_frame(validTrainJobs)
validTestJobs <- exploratory::clean_data_frame(validTestJobs)

#remove html and other char sequences as given on the dataset page on kaggle
validTrainJobs$Description <- gsub("&[^;]+;|<[^>]+>|\\\\r|\\r|\r|\"|\\\\t|\\t|\t|\\\\n|\\n|\n|\\\\|\\+", " ", validTrainJobs$Description)
validTestJobs$Description <- gsub("&[^;]+;|<[^>]+>|\\\\r|\\r|\r|\"|\\\\t|\\t|\t|\\\\n|\\n|\n|\\\\|\\+", " ", validTestJobs$Description)

#remove referencing text also
#jobs$Requirements <- gsub("Please refer to the Job Description to view the requirements for this job|&[^;]+;|<[^>]+>|\\\\r|\\r|\r|\"|\\\\t|\\t|\t|\\\\n|\\n|\n", " ", jobs$Requirements)

#tokenize
validTrainJobsDescTokenized <- exploratory::do_tokenize(validTrainJobs, Description)
validTestJobsDescTokenized <- exploratory::do_tokenize(validTestJobs, Description)

#head(sort(table(validTrainJobsDescTokenized$token), decreasing = T))

#remove stopwords and non-alpha words 
validTrainJobsDescTokenized <- validTrainJobsDescTokenized[!exploratory::is_stopword(validTrainJobsDescTokenized$token) & exploratory::is_alphabet(validTrainJobsDescTokenized$token),]
validTestJobsDescTokenized <- validTestJobsDescTokenized[!exploratory::is_stopword(validTestJobsDescTokenized$token) & exploratory::is_alphabet(validTestJobsDescTokenized$token),]

#get word stems
validTrainJobsDescTokenized$tokenStem1 <- exploratory::stem_word(validTrainJobsDescTokenized$token)
validTestJobsDescTokenized$tokenStem1 <- exploratory::stem_word(validTestJobsDescTokenized$token)

#extract n-grams

# #first group by sentence so n-grams make sense
# validTrainJobsDescTokenized <- with(validTrainJobsDescTokenized, 
#                                     dplyr::group_by(validTrainJobsDescTokenized, 
#                                                     sentence_id))
# 
# validTestJobsDescTokenized <- with(validTestJobsDescTokenized,
#                                    dplyr::group_by(validTestJobsDescTokenized, 
#                                                    sentence_id))

validTrainJobsDescTokenized$ngrams <- exploratory::do_ngram(df = validTrainJobsDescTokenized,
                                                            token = tokenStem1, 
                                                            sentence = sentence_id,
                                                            document = document_id,
                                                            maxn=3)

validTestJobsDescTokenized$ngrams <- exploratory::do_ngram(df = validTestJobsDescTokenized,
                                                           token = tokenStem1, 
                                                           sentence = sentence_id,
                                                           document = document_id, 
                                                           maxn=3)

