# library(data.table)
# library(text2vec)
# library(tm)
# library(stringi)
# library(dplyr)
# 
# #unzip("jobs.zip", overwrite = T, exdir = "jobs")
# 
# jobs <- fread("jobs/jobs.tsv", colClasses = c("int", "factor", "character", "character", "character", "factor", "factor", "factor", "factor", "POSIXct", "POSIXct") )
# 
# jobs$StartDate <- as.POSIXct(jobs$StartDate)
# jobs$EndDate <- as.POSIXct(jobs$EndDate)
# #jobs$State <- as.factor(jobs$State)
# #jobs$Country <- as.factor(jobs$Country)
# #jobs$City <- as.factor(jobs$City)
# #jobs$Zip5 <- as.factor(jobs$Zip5)
# 
# #sort by date
# #jobs <- jobs[order(jobs$StartDate),]
# 
# users  <- fread("users.tsv", colClasses = c("integer", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "character", "integer", "integer", "factor", "factor", "integer"), na.strings = "")
# 
# users$GraduationDate <- as.Date(users$GraduationDate)
# users$CurrentlyEmployed <- as.logical(users$CurrentlyEmployed)
# users$ManagedOthers <- as.logical(users$ManagedOthers)
# 
# #unzip("apps.zip")
# 
# apps<- fread("apps.tsv", colClasses = c("integer", "factor", "factor", "POSIXct", "integer"))
# 
# apps$ApplicationDate <- as.POSIXct(apps$ApplicationDate)
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
# #train = the apps in the first 6 days
# train <- apps[lubridate::as_date(apps$ApplicationDate) <= day6,]
# 
# #test = the apps from day 7 - 9
# test <- apps[lubridate::as_date(apps$ApplicationDate) > day6 & lubridate::as_date(apps$ApplicationDate) <= day9,]
# 
# #count the apps per userID in training set
# trainUsers <- setnames(as.data.frame(table(train$UserID)), c("UserID", "Freq"))
# 
# #get the userIDs with 5 or more apps in the training set
# trainUsers <- trainUsers[trainUsers$Freq >= 5, ]
# 
# #count the apps per userID in test set
# testUsers <- setnames(as.data.frame(table(test$UserID)), c("UserID", "Freq"))
# 
# #get the userIDs with 5 or more apps in test set
# testUsers <- testUsers[testUsers$Freq >= 5, ]
# 
# #valid userIDs = userIDs with 5 or more in both training and test
# validUserIDs <- intersect(trainUsers$UserID, testUsers$UserID)
# 
# #get the valid users records
# validUsers <- users[UserID %in% validUserIDs,]
# 
# #valid train apps = made by valid users in the first 6 days
# validTrainApps <- train[train$UserID %in% validUserIDs,]
# 
# #valid test apps = made by valid users in day 7 - 9
# validTestApps <- test[test$UserID %in% validUserIDs,]
# 
# validTrainJobIDs <- unique(validTrainApps$JobID)
# validTestJobIDs <- unique(validTestApps$JobID)
# 
# validTrainJobs <- jobs[jobs$JobID %in% validTrainJobIDs,]
# validTestJobs <- jobs[jobs$JobID %in% validTestJobIDs,]
# 
# rm(jobs, users, apps)
# 
# write.table(validUsers, "validUsers.tsv", sep="\t", row.names = F)
# write.table(validTrainApps, "validTrainApps.tsv", sep = "\t", row.names = F)
# write.table(validTestApps, "validTestApps.tsv", sep = "\t", row.names = F)
# write.table(validTrainJobs, "validTrainJobs.tsv", sep = "\t", row.names = F)
# write.table(validTestJobs, "validTestJobs.tsv", sep = "\t", row.names = F)

################################ MODEL #################################
validUsers <- fread("validUsers.tsv", sep="\t", stringsAsFactors = F)
validTrainApps <- fread("validTrainApps.tsv", sep="\t", stringsAsFactors = F)
validTestApps <- fread("validTestApps.tsv", sep="\t", stringsAsFactors = F)
validTrainJobs <- fread("validTrainJobs.tsv", sep="\t", stringsAsFactors = F)
validTestJobs <- fread("validTestJobs.tsv", sep="\t", stringsAsFactors = F)

message("cleaning descriptions")
message("iconv")
validTrainJobs$Description <- iconv(validTrainJobs$Description, to="UTF-8", sub="byte")
validTestJobs$Description <- iconv(validTestJobs$Description, to="UTF-8", sub="byte")

message("stri_escape_unicode")
validTrainJobs$Description <- stringi::stri_escape_unicode(validTrainJobs$Description)
validTestJobs$Description <- stringi::stri_escape_unicode(validTestJobs$Description)

#clean descriptions
#validTrainJobs <- exploratory::clean_data_frame(validTrainJobs)
#validTestJobs <- exploratory::clean_data_frame(validTestJobs)

message("gsub")
#remove html and other char sequences as given on the dataset page on kaggle
validTrainJobs$Description <- gsub("&[^;]+;|<[^>]+>|\\\\r|\\r|\r|\"|\\\\t|\\t|\t|\\\\n|\\n|\n|\\+.*|\\\\", " ", validTrainJobs$Description)
validTestJobs$Description <-  gsub("&[^;]+;|<[^>]+>|\\\\r|\\r|\r|\"|\\\\t|\\t|\t|\\\\n|\\n|\n|\\+.*|\\\\", " ", validTestJobs$Description)

message("gsub unicode orphans")
#remove the orphaned unicodes like u020e
validTrainJobs$Description <- gsub("u.{4}|\\b.{1,2}\\b", " ", validTrainJobs$Description)
validTestJobs$Description <-  gsub("u.{4}|\\b.{1,2}\\b", " ", validTestJobs$Description)


#remove referencing text also
#jobs$Requirements <- gsub("Please refer to the Job Description to view the requirements for this job|&[^;]+;|<[^>]+>|\\\\r|\\r|\r|\"|\\\\t|\\t|\t|\\\\n|\\n|\n", " ", jobs$Requirements)

message("preparing for vocab")
#vocabulary-based vectorization
# define preprocessing function and tokenization fucntion
prep_fun <-  tolower
tok_fun <-  word_tokenizer

#the iterator which defines the actions on each doc in this case lowercase
#then tokenization
it_train <-  itoken(validTrainJobs$Description,
                  preprocessor = prep_fun,
                  tokenizer = tok_fun,
                  ids = validTrainJobs$JobID,
                  progressbar = FALSE)

it_test <-  itoken(validTestJobs$Description,
                    preprocessor = prep_fun,
                    tokenizer = tok_fun,
                    ids = validTestJobs$JobID,
                    progressbar = FALSE)

message("vocab - ngrams")
#create ngrams vocab excluding stopwords
vocab <-  create_vocabulary(it_train, stopwords = tm::stopwords(), ngram = c(1L, 2L))

message("pruning vocabs")
#remove terms that occur less than 10 times in total and
#any that appear over 50% in any doc
vocab <-  vocab %>% prune_vocabulary(term_count_min = 10,
                                   doc_proportion_max = 0.5,
                                   doc_proportion_min = 0.001)

message("creating vectorizer")
#create function that will vectorize
bigram_vectorizer = vocab_vectorizer(vocab)

message("creating dtms")
#create ngram dtm
dtm_train = create_dtm(it_train, bigram_vectorizer)
dtm_test = create_dtm(it_test, bigram_vectorizer)

# define tfidf model
tfidf <- TfIdf$new()

message("creating tfidfs")
# fit model to train data and transform train data with fitted model
dtm_train_tfidf <- fit_transform(dtm_train, tfidf)

# tfidf modified by fit_transform() call!
# apply pre-trained tf-idf transformation to test data
dtm_test_tfidf <- transform(dtm_test, tfidf)

message("grouping dtm by userID")
#group dtm_train by user and aggregate
validTrainApps <- dplyr::group_by(validTrainApps, UserID)

#sapply(validUsers$UserID, function(x) apply(dtm_train_tfidf[rownames(dtm_train_tfidf) %in% validTrainApps[which(validTrainApps$UserID == x), "JobID"],], 1, print))

message("calculate the mean TFIDF for each user")
#calculate the mean TFIDF for each user
userTfIDF <- NULL

i <- 1

for(userID in validUsers$UserID)
{
  userTrainApps <- unlist(validTrainApps[which(validTrainApps$UserID == userID), "JobID"])
  user_dtm_train_tfidf <- dtm_train_tfidf[which(rownames(dtm_train_tfidf) %in% userTrainApps),]
  userTfIDF <- rbind(userTfIDF, colMeans(as.matrix(user_dtm_train_tfidf)))
  i <- i + 1
}

message("calculate cosine similarities between user TFIDF means and all test jobs")
#calculate cosine similarities between user TFIDF means and all test jobs
#and select the top 10

topN <- 10

#get the cosine similarities between each user and each test job
user_job_sim <- sim2(userTfIDF, dtm_test_tfidf, method = "cosine", norm="l2")

message("get job recommendations per user")
#jobs recommended
user_job_sim_top_jobIDs <- t(apply(user_job_sim, 1, function(x) colnames(user_job_sim)[order(x, decreasing = T)[1:topN]]))

#add userIDs and convert to df
# user_job_sim_top_jobIDs <- setnames(
#   as.data.frame(
#     cbind(
#       validUserIDs,
#       user_job_sim_top_jobIDs
#       )
#     ),
#   c("UserID", paste0("JOBID", 1:topN))
#   )

#get the list of recommended jobs that the user actually applied to in the test period
user_recomm_in_test_apps <- sapply(
  validUsers$UserID,
  function(x)
    intersect(user_job_sim_top_jobIDs[validUsers$UserID==x,], validTestApps[validTestApps$UserID==x, "JobID"])
  )

user_recomm_perc_in_test_apps <- sapply(
  validUsers$UserID,
  function(x)
      length(intersect(user_job_sim_top_jobIDs[validUsers$UserID==x,], validTestApps[validTestApps$UserID==x, "JobID"]))/length(validTestApps[validTestApps$UserID==x, "JobID"])
)

#compare recommendations with actual train and test apps per user
#jobs applied for in train

#For visual inspection retrieve titles
user_apps_recomm_titles <- lapply(
  validUsers$UserID,
  function(x)
      c(
        x,
        validTestJobs[
          validTestJobs$JobID %in%
            user_job_sim_top_jobIDs[validUsers$UserID==x,],
          c("JobID","Title","Description")]
        )
)

user_apps_recomm_titles <- lapply(
  user_apps_recomm_titles,
  function(x)
    setNames(
      as.data.frame(x,stringsAsFactors=F),
      c("UserID", "JobID", "Title", "Description")
      )
  )

user_apps_train_titles <- lapply(
  validUsers$UserID,
  function(x)
      c(
        x,
        validTrainJobs[validTrainJobs$JobID %in%
                         unlist(
                           validTrainApps[
                             which(validTrainApps$UserID==x),
                             "JobID"]
                           ),
                       c("JobID","Title","Description")
                       ]
        )
)

user_apps_train_titles <- lapply(
  user_apps_train_titles,
  function(x){
    if(length(unlist(x[2])>0))
      setNames(
        as.data.frame(x, stringsAsFactors=F),
        c("UserID", "JobID", "Title", "Description")
        )
    }
  )

user_apps_test_titles <- lapply(
  validUsers$UserID,
  function(x)
      c(
        x,
        validTestJobs[
          validTestJobs$JobID %in%
            unlist(
              validTestApps[which(validTestApps$UserID==x),"JobID"]
              ),
          c("JobID","Title","Description")]
        )
)

user_apps_test_titles <- lapply(
  user_apps_test_titles,
  function(x)
    setNames(
      as.data.frame(x, stringsAsFactors=F),
      c("UserID", "JobID", "Title", "Description")
      )
  )


job_apps_recomm_users <- function(jobDescription)
{
  it_job <-  itoken(jobDescription,
                    preprocessor = prep_fun,
                    tokenizer = tok_fun,
                    ids = 1,
                    progressbar = FALSE)

  dtm_job = create_dtm(it_job, bigram_vectorizer)

  dtm_job_tfidf <- transform(dtm_job, tfidf)

  job_user_sim <- sim2(userTfIDF, dtm_job_tfidf, method = "cosine", norm="l2")

  job_user_sim <- cbind(1:length(job_user_sim), job_user_sim)

  job_user_sim_top_jobIDs <- job_user_sim[order(job_user_sim[,2], decreasing = T),]

  validUsers[job_user_sim_top_jobIDs[1:topN,1]]
}

# #visually inspect
#x <- 1
#message("train");user_apps_train_titles[x];message("test");user_apps_test_titles[x];message("recomm");user_apps_recomm_titles[x]

#check jobs applied for by userID
#a <- validTestJobs[which(validTestJobs$JobID %in% unlist(validTestApps[validTestApps$UserID==11647, "JobID"])),]
#write.csv(a, "recommjobs.csv")

getCRJD <- function()
{
  library("RCurl")
  library("rjson")
  
  # Accept SSL certificates issued by public Certificate Authorities
  options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
  
  h = basicTextGatherer()
  hdr = basicHeaderGatherer()
  
  
  req = list(
    
    Inputs = list(
      
      
      "Description" = list(
        "ColumnNames" = list("JobID", "WindowID", "Title", "Description", "Requirements", "City", "State", "Country", "Zip5", "StartDate", "EndDate"),
        "Values" = list( list( "", "", "Sr. Business Development Manager - Southeast Market", "<span><strong>Sr. Business Development Manager - Southeast Market<br>\rBase plus commission, TTC $120-125,000<br>\r</strong>&nbsp;\r<div><br>\r<strong>This position could be located in any of several locations in the southeast.</strong>&nbsp;<br>\r&nbsp;</div>\r<p><span>Intechra, a division of Arrow...
", "", "", "", "", "", "", "" ),  list( "value", "value", "value", "value", "value", "value", "value", "value", "value", "value", "value" )  )
      )                ),
    GlobalParameters = setNames(fromJSON('{}'), character(0))
  )
  
  body = enc2utf8(toJSON(req))
  api_key = "hOj6PdDJD/QhwKaKC9zS4aB5chCz7XTK4TNFtppKWsLWVmtrJFFPiaEMFFo44pCWTYtI6mW04yL720sZr+jgNw==" # Replace this with the API key for the web service
  authz_hdr = paste('Bearer', api_key, sep=' ')
  
  h$reset()
  curlPerform(url = "https://ussouthcentral.services.azureml.net/workspaces/5cc41351b44945bf9212822ebad95389/services/9de1981240d6453aa38d74b7b09ed5a1/execute?api-version=2.0&details=true",
              httpheader=c('Content-Type' = "application/json", 'Authorization' = authz_hdr),
              postfields=body,
              writefunction = h$update,
              headerfunction = hdr$update,
              verbose = TRUE
  )
  
  headers = hdr$value()
  httpStatus = headers["status"]
  if (httpStatus >= 400)
  {
    print(paste("The request failed with status code:", httpStatus, sep=" "))
    
    # Print the headers - they include the requert ID and the timestamp, which are useful for debugging the failure
    print(headers)
  }
  
  print("Result:")
  result = h$value()
  print(fromJSON(result))
}