jobDescription <- validTestJobs$Description[1]

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
           