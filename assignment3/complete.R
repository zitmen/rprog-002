complete <- function(directory, id = 1:332) {
    fr <- data.frame(id=numeric(length(id)), nobs=numeric(length(id)));
    row <- 1;
    for(i in id) {
        d <- read.csv(sprintf("%s/%03d.csv",directory,i));
        fr$id[row] <- i;
        fr$nobs[row] <- sum(complete.cases(d));
        row <- row + 1;
    }
    fr
}