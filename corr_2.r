#corr.R
#wrote this function initially but i was getting wrong output for test cases
#after looking on the internet, i figured the wrong output was because
#i was using id = 1:322 instead of 1:332
#but during the search, I used more built in functions
#to optimize code in corr.R
corr <- function(directory, threshold=0) {
    corrcases <- c()
    id <- 1:332
    for(i in id){
        dirname <- sprintf("%s/%03d.csv", directory, i)
        # print(dirname)
        csvdata <- read.csv(dirname, head=TRUE)
        comp <- complete.cases(csvdata$sulfate, csvdata$nitrate, csvdata$ID)
        rowlength <- length(comp[comp==TRUE])
        if(rowlength>threshold){
            sulfate <- csvdata[comp, "sulfate"]
            nitrate <- csvdata[comp, "nitrate"]
            corrcases <- c(corrcases, cor(sulfate, nitrate))
        } else {
            
        }
    }
    if(length(corrcases)==0){
        corrcases <- numeric(0)
    } 
    corrcases
}   