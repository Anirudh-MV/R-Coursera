#best(state, outcome)
#returns hospital name with lowest mortality rate across heart attack, heart failure, pneumonia

best <- function(state, outcome){
    # browser()
    ## read csv
    outcomes <- read.csv('outcome-of-care-measures.csv', colClasses="character")
    uniquestatecode <- unique(outcomes[,7])
    ## check state and outcome are valid 
    if(sum(state == uniquestatecode) > 0){
        if(outcome == "heart attack"){
            column <- 11
        } else if(outcome == "heart failure"){
            column <- 17
        } else if(outcome == "pneumonia"){
            column <- 23
        } else stop("invalid state")
    } else {
        stop("invalid state")
    }

    ## change column to numeric
    ## because initially all columns are characters
    outcomes[,column] = suppressWarnings(as.numeric(outcomes[,column]))

    ## get only specific column
    sbst <- subset(outcomes, State==state, select=c(2,column))
    sbst <- na.omit(sbst)
    minimum <- min(sbst[,2])
    sbst <- sbst[sbst[,2]==minimum,]
    vct <- as.vector(sbst[,1])
    vct <- sort(vct)
    vct[1]
}