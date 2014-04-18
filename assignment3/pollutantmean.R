pollutantmean <- function(directory, pollutant, id = 1:332) {
    pol <- vector();
    for(i in id) {
        data <- read.csv(sprintf("%s/%03d.csv",directory,i));
        pol <- c(pol, data[[pollutant]][!is.na(data[[pollutant]])]);
    }
    mean(pol);
}
