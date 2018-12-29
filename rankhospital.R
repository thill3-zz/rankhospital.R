#Write a function called rankhospital that takes three arguments: the 
# 2-character abbreviated name of a state (state), an outcome (outcome), and the 
# ranking of a hospital in that state for that outcome (num). The function reads 
# the outcome-of-care-measures.csv file and returns a character vector with the 
# name of the hospital that has the ranking specifed by the num argument.

rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        setwd(
                "G:/My Drive/Coursera Data Science Specialization/rprog_data_ProgAssignment3-data/"
        )
        data <-
                read.csv("outcome-of-care-measures.csv",
                         stringsAsFactors = FALSE,
                         na.strings = c("Not Available")
                         )
        ## Check that state and outcome are valid
        if (!(state %in% unique(data[, 7]))) {
                stop("invalid state")
        }
        if (tolower(outcome) == "heart attack") {
                col_index <- 11
        } else if (tolower(outcome) == "heart failure") {
                col_index <- 17
        } else if (tolower(outcome) == "pneumonia") {
                col_index <- 23
        } else {
                stop("invalid outcome")
        }
        if (num == "best") {
                num <- 1
        }
        #will have to set rowNumber for "worst" just before I check it.
        
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        row_TF <- data[, 7] == state
        suppressWarnings(values <-
                data.frame(data[, 2], data[, 7], as.numeric(data[, col_index]), stringsAsFactors = FALSE)) #collect the important data
        values <- values[row_TF, ] #limit to the relevant state
        values <- na.omit(values) #remove NAs
        if (num > nrow(values)){ NA }
        order_for_values_index <- order(values[, 3], values[,1],decreasing = FALSE)
        if (num == "worst") { num <- nrow(values)}
        used_ind <- order_for_values_index[num]
        values[used_ind,1]
}

# > source("rankhospital.R")
state <- "TX"
outcome <- "heart failure"
num <- 4
rankhospital("TX", "heart failure", 4)
# [1] "DETAR HOSPITAL NAVARRO"
rankhospital("MD", "heart attack", "worst")
# [1] "HARFORD MEMORIAL HOSPITAL"
rankhospital("MN", "heart attack", 5000)
# [1] NA