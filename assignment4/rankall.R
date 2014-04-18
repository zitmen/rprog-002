rankall <- function(outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character");
    
    ## Check that outcome is valid
    if(outcome == "heart attack") {
        outcome = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack";
    } else if(outcome == "heart failure") {
        outcome = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure";
    } else if(outcome == "pneumonia") {
        outcome = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia";
    } else {
        stop("invalid outcome");
    }
    
    if(num == "best") {
        rank <- 1;
    } else if(num == "worst") {
        rank <- -1;
    } else {
        rank <- as.numeric(num);
    }
    
    data[,outcome] <- suppressWarnings(as.numeric(data[,outcome]));
    data[,"State"] <- as.factor(data[,"State"]);
    
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the (abbreviated) state name
    idx <- !is.na(data[,outcome]);
    sel_state <- data[,'State'][idx];
    sel_name <- data[,'Hospital.Name'][idx];
    sel_out <- data[,outcome][idx];
    o <- order(sel_state,sel_out,sel_name);
    
    state_size <- table(sel_state);
    state_start <- cumsum(state_size) - state_size;
    res <- sapply(levels(sel_state),function(s) {
        start <- as.numeric(state_start[s]);
        last <- start + as.numeric(state_size[s]);
        pos <- start + rank;
        if(pos > last) {
            list(hospital=NA, state=s)
        } else if(pos < start) {
            list(hospital=sel_name[o[last]], state=s)
        } else {
            list(hospital=sel_name[o[pos]], state=s)
        }
    });
    
    tbl <- data.frame(cbind(res[1,],res[2,]));
    colnames(tbl) <- c("hospital","state");
    tbl
    
}