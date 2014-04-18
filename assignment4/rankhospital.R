rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character");
    
    ## Check that state and outcome are valid
    states <- data[['State']];
    state_idx = (states == state);
    if(sum(state_idx) == 0) {
        stop("invalid state");
    }
    
    if(outcome == "heart attack") {
        outcome = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack";
    } else if(outcome == "heart failure") {
        outcome = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure";
    } else if(outcome == "pneumonia") {
        outcome = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia";
    } else {
        stop("invalid outcome");
    }
    data[,outcome] <- suppressWarnings(as.numeric(data[,outcome]));
    
    ## Return hospital name in that state with the given rank 30-day death rate
    idx <- !is.na(data[state_idx,outcome]);
    sel_name <- data[state_idx,'Hospital.Name'][idx];
    sel_out <- data[state_idx,outcome][idx];
    o <- order(sel_out,sel_name);
    
    if(num == "best") {
        rank <- 1;
    } else if(num == "worst") {
        rank <- length(o);
    } else {
        rank <- as.numeric(num);
    }
    
    if(rank > length(o)) {
        NA
    } else {
        sel_name[o[rank]]
    }
}