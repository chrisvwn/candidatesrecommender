library(RODBC)
library(stringr)

myServer <- "candidaterecommender.database.windows.net"
myUser <- "sqladmin"
myPassword <- "Sql123*!"
myDatabase <- "mployeddb"
myDriver <- "SQL Server" # Must correspond to an entry in the Drivers tab of "ODBC Data Sources"

connectionString <- paste0(
  "Driver=", myDriver,
  ";Server=", myServer,
  ";Database=", myDatabase,
  ";Uid=", myUser,
  ";Pwd=", myPassword)
# This query simulates a table by generating a rowset with one integer column going from 1 to 1000
sqlQuery <- "SELECT * from mp_candidates;"
conn <- odbcDriverConnect(connectionString)
candidates <- sqlQuery(conn, sqlQuery)

sqlQuery <- "SELECT * from mp_jobs where post_id='357';"
conn <- odbcDriverConnect(connectionString)
jobs  <- sqlQuery(conn, sqlQuery)

close(conn) # don't leak connections !

# a <- jobs
# 
# b <- candidates

# jobs <- jobs[,c("post_id", "qualification", "industry", "cs_post_loc_latitude", "cs_post_loc_longitude")]
# 
# jobs$type <- "job"
# 
# jobs <- setNames(jobs, c("id", "qualification", "industry", "cs_post_loc_latitude", "cs_post_loc_longitude", "type"))
# 
# candidates <- candidates[,c("user_id", "education_level", "cs_specialisms", "cs_post_loc_latitude", "cs_post_loc_longitude")]
# 
# candidates$type <- "candidate"
# 
# candidates <- setNames(candidates, c("id", "qualification", "industry", "cs_post_loc_latitude", "cs_post_loc_longitude", "type"))
# 
# bin <- rbind(jobs, candidates)

results <- NULL

jobs <- data.frame(post_id=0,cs_post_loc_latitude=0,cs_post_loc_longitude=0,industry="banking, accountancy")

for (c in 1:ncol(jobs))
  jobs[,c] <- as.character(jobs[, c])

candidates <- as.data.frame(sapply(names(candidates), function(name)as.character(candidates[[name]])), stringsAsFactors = F)

searchJobs <- jobs[1,]

for(i in 1:nrow(jobs))
{
  result <- data.frame()
  
  jskills <- unlist(strsplit(as.character(jobs[i, "industry"]), ","))
  
  for(j in 1:nrow(candidates))
  {
    cskills <- unlist(strsplit(as.character(candidates[j, "cs_specialisms"]), ","))

    score <- sum(unlist(stringr::str_trim(cskills) %in% stringr::str_trim(jskills)))
    
    result <- rbind(result, cbind(candidates[j, ], as.numeric(score)))
  }
  
  names(result) <- c(names(candidates), "score")
  
  result$score <- as.numeric(result$score)
  
  result <- result[order(result$score, decreasing = T), ]
  
  # if(nrow(result) > 10)
  #  result <- result[1:10,]
  
  results <- rbind(results, cbind(jobs[i,"post_id"], result))
}

names(results)[1] <- "job_id"

results <- results[results$score > 0, ]

for (k in 1:nrow(results))
{
  jlonlat <- as.numeric(jobs[which(jobs$post_id == results[k,"job_id"]), c("cs_post_loc_longitude", "cs_post_loc_latitude")])
  clonlat <- as.numeric(candidates[which(candidates$user_id == results[k,"user_id"]), c("cs_post_loc_longitude", "cs_post_loc_latitude")])
  
  print(results[k,])
  print(jlonlat)
  print(clonlat)
  
  if(is.na(jlonlat[1]) || is.na(jlonlat[2]) || is.na(clonlat[1]) || is.na(clonlat[2]))
    results[k, "distance"] <- +Inf
  else
    results[k, "distance"] <- raster::pointDistance(sp::SpatialPoints(matrix(jlonlat, ncol=2)), sp::SpatialPoints(matrix(clonlat, ncol=2)), longlat=T)
}

results <- dplyr::arrange(results, job_id, desc(score), distance)

#results <- subset(results, results$job_id == searchJobs$post_id)

# OutputClient = results
# 
# maml.mapOutputPort("OutputClient")