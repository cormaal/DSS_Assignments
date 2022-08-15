library(dplyr)
library(data.table)

best <- function(state, outcome) {
        state_codes <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL",
                         "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME",
                         "MD", "MA", "MI"," MN", "MS", "MO", "MT", "NE", "NV", "NH",
                         "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI",
                         "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")
        if (state %in% state_codes == FALSE) {
                stop("invalid state")
        }
        outcomes <- c("heart attack", "heart failure", "pneumonia")
        if (outcome %in% outcomes == FALSE) {
                stop("invalid outcome")
        }
        df <- fread("outcome-of-care-measures.csv", select = c(2, 7, 11, 17, 23))
        colnames(df) <- c("Hospital", "State", "heart attack", "heart failure", "pneumonia")
        df[df == "Not Available"] <- NA
        df <- df %>% mutate_at(c('heart attack', 'heart failure', 'pneumonia'), as.numeric)
        sh <- select(filter(df, State == state), c(Hospital, State, all_of(outcome)))
        good <- complete.cases(sh)
        sh_clean <- sh[good, ]
        minimum <- min(sh_clean[, 3])
        results <- select(filter(sh_clean, sh_clean[, 3] == minimum), c(Hospital, State, all_of(outcome)))
        results_ordered <- results[order(results$Hospital), ]
        final_result <- results_ordered[1, 1]
        names(final_result) <- NULL
        final_result
}

