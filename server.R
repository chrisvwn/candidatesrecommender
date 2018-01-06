#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(data.table)

#source("recommender3.R")

#read in stored data and models
validUsers <- fread("validUsers.tsv", sep="\t", stringsAsFactors = F)
validTrainApps <- fread("validTrainApps.tsv", sep="\t", stringsAsFactors = F)
validTestApps <- fread("validTestApps.tsv", sep="\t", stringsAsFactors = F)
validTrainJobs <- fread("validTrainJobs.tsv", sep="\t", stringsAsFactors = F)
validTestJobs <- fread("validTestJobs.tsv", sep="\t", stringsAsFactors = F)

bigram_vectorizer <- readRDS("bigram_vectorizer.RDS")
tfidf <- readRDS("tfidf.RDS")
userTfIDF <- readRDS("userTfIDF.RDS")


user_apps_train_titles <- readRDS("user_apps_train_titles.RDS")
user_apps_test_titles <- readRDS("user_apps_test_titles.RDS")
user_apps_recomm_titles <- readRDS("user_apps_recomm_titles.RDS")

#to maintain original job descriptions
validTrainJobs1 <- fread("validTrainJobs.tsv", sep="\t", stringsAsFactors = F)
validTestJobs1 <- fread("validTestJobs.tsv", sep="\t", stringsAsFactors = F)

topN <- 10

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

  ###############################################################################
  
  validUsersDF <- shiny::reactive({
    validUsers
  })
  
  ###############################################################################
  
  validJobIDs <- shiny::reactive({
    union(validTrainApps$JobID, validTestApps$JobID)
  })
  
  ###############################################################################
  
  validUserIDs <- shiny::reactive({
    validUsersDF()$UserID
  })
  
  ###############################################################################
  
  candidateDets <- shiny::reactive({
    candidates <- validUsersDF()
    candidates[which(candidates$UserID == input$candidate)]
    })

  ###############################################################################
  
  jobDets <- shiny::reactive({
    jobDet <- validTrainJobs1[which(validTrainJobs1$JobID == input$job)]
    
    if(is.null(jobDet))
      jobDet <- validTestJobs1[which(validTestJobs1$JobID == input$job)]
    
    jobDet
  })
  
  ###############################################################################
  
  allCandidateTrainApps <- shiny::reactive({
    user_apps_train_titles
  })
  
  ###############################################################################
  
  allCandidateTestApps <- shiny::reactive({
    user_apps_test_titles
  })
  
  ###############################################################################
  
  allCandidateRecommApps <- shiny::reactive({
    user_apps_recomm_titles
  })
  
  ###############################################################################
  
  output$candidate <- shiny::renderUI({
    
    shiny::selectizeInput(inputId = "candidate",
                        label = "Candidate",
                        choices = validUserIDs(),
                        selected=NULL,
                        multiple=F)
  })
  
  ###############################################################################
  
  output$selectJob <- shiny::renderUI({
    
    shiny::updateSelectizeInput(session = session, 
                                inputId = "job",
                                choices = validJobIDs(),
                                server = TRUE)
    
    shiny::selectizeInput(inputId = "job",
                          label = "Job",
                          choices = NULL,
                          selected = NULL,
                          multiple = FALSE)
  })
  
  ###############################################################################
  
  output$candidateDetails <- DT::renderDataTable({
    if(is.null(input$candidate))
      return(NULL)
    
    dets <- candidateDets()
    
    dets
  }, 
  caption="Candidate Details", 
  
  options = list(
    autoWidth = TRUE
    )
  )
  
  ###############################################################################
  
  output$jobDetails <- DT::renderDataTable({
    if(is.null(input$job))
      return(NULL)
    
    dets <- jobDets()
    
    dets
  }, caption="Job Details")
  
  ###############################################################################
  
  output$candidateTrainApps <- DT::renderDataTable({
    if(is.null(input$candidate))
      return(NULL)
    
    trainApps <- allCandidateTrainApps()
    
    cndTrainApps <- trainApps[which(validUsers$UserID == input$candidate)]
    
    cndTrainApps <- as.data.frame(cndTrainApps)
    
    #cndTrainApps$Description <- sapply(cndTrainApps$Description, substr, 1, 200)

    cndTrainApps <- cndTrainApps[,1:3]
    
    cndTrainApps
  }, caption="Candidate Apps in Training period")
  
  ###############################################################################
  
  output$candidateTestApps <- DT::renderDataTable({
    if(is.null(input$candidate))
      return(NULL)
    
    testApps <- allCandidateTestApps()
    
    cndTestApps <- testApps[which(validUsers$UserID == input$candidate)]
    
    cndTestApps <- as.data.frame(cndTestApps)
    
    #cndTestApps$Description <- sapply(cndTestApps$Description, substr, 1, 200)
    
    cndTestApps <- cndTestApps[,1:3]
    
    cndTestApps
  }, caption="Candidate Apps in Testing period")
  
  ###############################################################################
  
  output$candidateRecommApps <- DT::renderDataTable({
    if(is.null(input$candidate))
      return(NULL)
    
    recommApps <- allCandidateRecommApps()
    
    cndRecommApps <- recommApps[which(validUsers$UserID == input$candidate)]
    
    cndRecommApps <- as.data.frame(cndRecommApps)
    
    #cndRecommApps$Description <- sapply(cndRecommApps$Description, substr, 1, 200)
    
    cndRecommApps <- cndRecommApps[,1:3]
    
    cndRecommApps
  }, caption="Candidate Recommendations")
  
  ###############################################################################
  
  output$jobCandidateApps <- DT::renderDataTable({
    if(is.null(input$job))
      return(NULL)
    
    jobCndAppsUserIDs <- validTrainApps[which(validTrainApps$JobID == input$job), "UserID"]
    
    jobCndAppsUserIDs <- rbind(jobCndAppsUserIDs,validTestApps[which(validTestApps$JobID == input$job), "UserID"])
    
    jobCndAppsUsers <- validUsers[which(validUsers$UserID %in% unlist(jobCndAppsUserIDs)),]
    
    jobCndAppsUsers
  }, caption="Job Application Candidates")
  
  ###############################################################################
  
  output$jobRecommCandidates <- DT::renderDataTable({
    if(is.null(input$job))
      return(NULL)
    
    jobDescription <- as.character(validTrainJobs[which(validTrainJobs$JobID == input$job), "Description"])
    
    if(is.null(jobDescription))
      jobDescription <- validTestJobs[which(validTest$JobID == input$job), "Description"]
    
    jobRecommUserDets <- job_apps_recomm_users(jobDescription)
    
    jobRecommUserDets
  }, caption="Recommended Job Application Candidates")
  
  ###############################################################################
  
  output$newJobRecommCandidates <- DT::renderDataTable({
    if(is.null(input$newJob) || input$newJob == "")
      return(NULL)
    
    jobDescription <- input$newJob
    
    jobRecommUserDets <- job_apps_recomm_users(jobDescription)
    
    jobRecommUserDets
  }, caption="Recommended Job Application Candidates")
})

###############################################################################

prep_fun <-  tolower
tok_fun <-  word_tokenizer

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