#pollutantmean.R
pollutantmean <- function(directory, pollutant, id = 1:332) {
    masterpollutant <- c()
    for(i in id){
        dirname <- sprintf("%s/%03d.csv", directory, i)
        # print(dirname)
        csvdata <- read.csv(dirname, head=TRUE)
        poll <- csvdata[, pollutant]
        poll2 <- poll[!is.na(poll)]
        masterpollutant <- c(masterpollutant, poll2)
    }
    mean(masterpollutant)
}