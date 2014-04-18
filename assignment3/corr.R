corr <- function(directory, threshold = 0) {
    v <- numeric();
    for(i in 1:332) {
        d <- read.csv(sprintf("%s/%03d.csv",directory,i));
        idx <- complete.cases(d);
        if(sum(idx) > threshold) {
            v <- c(v, cor(d[idx,"sulfate"],d[idx,"nitrate"]));
        }
    }
    v
}