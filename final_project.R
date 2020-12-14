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

##### hub and authority scores of opinions ####

all_cites <- read.csv("allcites.txt", header = F, sep = " ")
case_info <- read.csv("judicial.csv")

# total number of cases
n = length(case_info$caseid)

# creating the adjacency matrix of the cases network
mat <- adja_mat(n, all_cites)

# finding the last case in each year. It is used in the hub and 
# authority functions to find the appropriate subset of adj. matrix based on
# the year parameter
year_last_case <- case_info %>% 
  group_by(year) %>%
  summarise(CaseID = max(caseid))


# Recreating figure 6 of the paper

# Roe v. Wade (1973)
y_1 = sapply(c(1973:2002), auth_score, caseid = 25347, network = mat, 
             year_last_case = year_last_case) 

# Brown v. Board of Educ. (1954)
y_2 = sapply(c(1954:1983), auth_score, caseid = 21109, network = mat,
             year_last_case = year_last_case) 

plot(y_1 ~ c(0:29), type = "l", col = "red", xlab = "Years after Decision",
     ylab = "Authority Scores", ylim=c(-0.001, 0.06),
     xlim=c(0, 30), main = "Rise of Brown and Roe", 
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
lines(y_2 ~ c(0:29), type = "l", col = "blue")
legend("bottomright", c("Roe v. Wade","Brown v. Board of Educ."),
       fill=c("red","blue"))




# Recreating figure 10 of the paper

years = c(1940:2002)
y_1 = sapply(years, auth_score, caseid = 18501, network = mat, 
             year_last_case = year_last_case) # Brown v. Mississippi 
y_2 = sapply(years, auth_score, caseid = 23601, network = mat,
             year_last_case = year_last_case) # Miranda v. Arizona 
y_3 = sapply(years, auth_score, caseid = 23115, network = mat,
             year_last_case = year_last_case) # Escobedo v. Illinois
y_4 = sapply(years, auth_score, caseid = 26918, network = mat, 
             year_last_case = year_last_case) # Rhode Island v. Innis

plot(y_1 ~ years, type = "l", col = "red", xlab = "Year", ylab = "Authority Scores", 
     main = "5th Amendment")
lines(y_2 ~ years, type = "l", col = "blue")
lines(y_3 ~ years, type = "l", col = "green")
lines(y_4 ~ years, type = "l", col = "black")
