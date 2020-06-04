rankall <- function(outcome, num = "best"){
    #best(outcome)
    #returns hospital names of each state with lowest mortality in outcome with state code 
    ## read csv
    outcomes <- read.csv('outcome-of-care-measures.csv', colClasses="character")
    uniquestatecode <- unique(outcomes[,7])
    ## check state and outcome are valid 
    # browser()
    if(outcome == "heart attack"){
        column <- 11
    } else if(outcome == "heart failure"){
        column <- 17
    } else if(outcome == "pneumonia"){
        column <- 23
    } else stop("invalid state")
    if(num == "best" || num == "worst" || (is.numeric(num) && num > 0)){
    } else{
        stop("invalid state")
    }

    hospital <- c()

    state <- c()

    ## easy way, by calling existing function
    # for(statecode in uniquestatecode){
    #     besthospital <- rankhospital(statecode, outcome, num)
    #     hospital <- c(hospital, as.character(besthospital))
    #     state <- c(state, as.character(statecode))
    # }   

    # df <- data.frame(hospital, state)
    # df[order(df[2]), ]

    outcomes[,column] = suppressWarnings(as.numeric(outcomes[,column]))

    ## get only specific column
    sbst <- subset(outcomes, select=c(2,7,column))
    sbst <- na.omit(sbst)

    ## change column names for ease of calling split
    colnames(sbst) <- c("hospital", "state", "death")

    ## split df by state
    outp<-split(sbst, sbst$state)
    # browser()

    ## store the num variable for later
    numchk <- num

    ## select each state
    for(sbst in outp){
        ## assign a number to rank
        if(numchk == "best"){
            num <- 1
        } else if(numchk == "worst"){
            num <- nrow(sbst)
        }
        state <- c(state, unique(sbst$state))
        # browser()

        #sort the df and store the rank value in a vector
        if(nrow(sbst) >= num){
            sortedsbst <- sbst[order(sbst[3], sbst[1]), ]
            hospital <- c(hospital, sortedsbst[num, 1])
        } else{
            hospital <- c(hospital, NA)
        }

    }

    ## form a dataframe and return it
    df <- data.frame(hospital, state)
    df[order(df[2]), ]
}