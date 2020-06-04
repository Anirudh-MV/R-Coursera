rankhospital <- function(state, outcome, num = "best"){
    #best(state, outcome, num)
    #returns hospital name with best/worst/colnum ranking 
    ## read csv
    outcomes <- read.csv('outcome-of-care-measures.csv', colClasses="character")
    uniquestatecode <- unique(outcomes[,7])
    ## check state and outcome are valid 
    # browser()
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

    if(num == "best" || num == "worst" || (is.numeric(num) && num > 0)){
    } else{
        stop("invalid state")
    }

    ## change column to numeric
    ## because initially all columns are characters
    outcomes[,column] = suppressWarnings(as.numeric(outcomes[,column]))

    ## get only specific column
    sbst <- subset(outcomes, State==state, select=c(2,column))
    sbst <- na.omit(sbst)

    ## if the num is more than rank, throw error
    if(is.numeric(num) && num > nrow(sbst)){
        return(NA)
    }
    ## convert best/worst to ranks
    if(num == "best"){
        num <- 1
    } else if(num == "worst"){
        num <- nrow(sbst)
    }

    if(num == 1){
        return(best(state, outcome))
    }

    #order data frame by name first then order by rank
    sortedsbst <- sbst[order(sbst[2], sbst[1]), ]
    sortedsbst[num, 1]
}