best <- function(state, outcome) {
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
    
    ## Return hospital name in that state with lowest 30-day death rate
    selected <- data[state_idx,outcome];
    minval <- min(selected, na.rm = TRUE);
    h_idx <- ((selected == minval) & !is.na(selected));
    hospitals <- sort(data[state_idx,'Hospital.Name'][h_idx]);
    
    hospitals[1]
}