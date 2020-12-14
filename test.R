context("Check adja_mat, hub and authority functions")
source("Final project.R")


all_cites <- read.csv("allcites.txt", header = F, sep = " ")
case_info <- read.csv("judicial.csv")
year_last_case <- case_info %>% 
  group_by(year) %>%
  summarise(CaseID = max(caseid))


#### Testing the adja_mat function ####

# 1)creating a simple network
A = Matrix(c(0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0), 
           nrow = 4, ncol = 4, byrow = TRUE)
# list of the network links
links <- data.frame(from = c(1, 1, 2, 3, 3, 4), to = c(2, 4, 3, 1, 4, 3))

test_that("adja_mat returns the correct adjacency matrix", {
  expect_equal(adja_mat(4, links), A)
})


# 2) We also test the output of adja_mat function with the real data
# We pick case ID 30288 and check if the 30288th row of the adjacency matrix
# is consistent with the data from all_cites

A = adja_mat(30288, all_cites)

test_that("adja_mat returns correct results for case ID 30288", {
  expect_equal(which(A[30288,] == 1), all_cites$V2[which(all_cites$V1 == 30288)])
})

#### Testing the authority and hub score functions ####

auth_scores <- auth_score("all", 2002, A, year_last_case)
hub_scores <- hub_score("all", 2002, A, year_last_case)

# 1) squares of authority and hub scores should sum to 1

test_that("squres of authority scores sum to 1", {
  expect_equal(sum(auth_scores^2), 1)
})

test_that("squres of hub scores sum to 1", {
  expect_equal(sum(hub_scores^2), 1)
})

# 2) comparing 3 highest authority scores with pre-computed scores on the website

test_that("3 highest authority scores match pre computed scores", {
  expect_equal(round(tail(sort(auth_scores),3), 3), round(tail(sort(case_info$auth),3), 3))
})

# 3) comparing 3 highest hub scores with pre-computed scores on the website

test_that("3 highest hub scores match pre computed scores", {
  expect_equal(round(tail(sort(hub_scores),3), 3), round(tail(sort(case_info$hub),3), 3))
})

# 4) comparing the authority scores of case 19238 in 2000-2002 with pre-computed
# scores on the website

# reading pre-computed auth scores
authmat <- read.csv("authmat.txt", header = TRUE, check.names=FALSE)

test_that("the function calculates authority scores of different years correctly", {
  expect_equal(round(sapply(c(2000:2002), auth_score, caseid = 19238, network = A,
                            year_last_case = year_last_case), 3)
               , round(as.numeric(authmat[19238, 202:204]), 3))
})

#5) comparing the hub scores of case 25247 in 2000-2002 with pre-computed
# scores on the website

# reading pre-computed hub scores
hubmat <- read.csv("hubmat.txt", header = TRUE, check.names=FALSE)

test_that("the function calculates hub scores of different years correctly", {
  expect_equal(round(sapply(c(2000:2002), hub_score, caseid = 25247, network = A, 
                            year_last_case = year_last_case), 3)
               , round(as.numeric(hubmat[25247, 202:204]), 3))
})
