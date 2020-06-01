#corr.R
corr <- function(directory, threshold=0) {
    corrcases <- c()
    cases <- complete(directory, 1:332)
    checkid <- subset(cases, nobs>threshold, select=id)
    checkid <- checkid$id
    for(item in checkid){
        dirname <- sprintf("%s/%03d.csv", directory, item)
        # print(dirname)
        csvdata <- read.csv(dirname, head=TRUE)
        comp <- complete.cases(csvdata$sulfate, csvdata$nitrate)
        corrcases <- c(corrcases, cor(csvdata[comp, "sulfate"], csvdata[comp, "nitrate"]))
    }
    # if(length(corrcases)==0){
    #     corrcases = numeric(0)
    # }
    corrcases
}   