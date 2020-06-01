#complete.R
complete <- function(directory, id = 1:332) {
    completecases <- c()
    for(i in id){
        dirname <- sprintf("%s/%03d.csv", directory, i)
        # print(dirname)
        csvdata <- read.csv(dirname, head=TRUE)
        comp <- complete.cases(csvdata$sulfate, csvdata$nitrate)
        completecases <- c(completecases,length(comp[comp==TRUE]))
    }
    data.frame(id=id, nobs=completecases)
}