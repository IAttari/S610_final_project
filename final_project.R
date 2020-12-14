library(Matrix)
library(rARPACK)
library(dplyr)

setwd("C:\\Users\\imana\\GitHub\\final_project")

# Takes the number of nodes and all the links in a network and returns the 
# adjacency matrix of the network.
# The class of links must be a data frame and it should have two columns.
# The link is from the node in the first column to the node in the second column.

adja_mat = function(n, links){
  links <- as.matrix(links)
  mat <- Matrix(0, n, n)
  mat[links] <- 1
  return(mat)
} 

# Authority score function: returns authority score of a case in a given year
# if caseid = all, the function returns authority scores of all cases

auth_score <- function(caseid, year, network, year_last_case){
  last_case <- year_last_case$CaseID[which(year_last_case$year == year)]
  ad_mat <- network[1:last_case, 1:last_case]
  auth = t(ad_mat)%*%ad_mat
  evs <- eigs_sym(auth, k = 1, which = "LM", opts = list(retvec = TRUE))
  auth_scores <- abs(evs$vectors)
  if (caseid == "all"){
    return(auth_scores)
  }
  else{
    return(auth_scores[caseid])}
}

# Hub score function: returns hub score of a case in a given year
# if caseid = all, the function returns hub scores of all cases

hub_score <- function(caseid, year, network, year_last_case){
  last_case <- year_last_case$CaseID[which(year_last_case$year == year)]
  ad_mat <- network[1:last_case, 1:last_case]
  hub = ad_mat%*%t(ad_mat)
  evs <- eigs_sym(hub, k = 1, which = "LM", opts = list(retvec = TRUE))
  hub_scores <- abs(evs$vectors)
  if (caseid == "all"){
    return(hub_scores)
  }
  else{
    return(hub_scores[caseid])}
}
