# Day 1: Sonar Sweep

part1 <- function() {
    readings <- readLines("Day1_input.txt")

    output <- character(length(readings))

    last_reading <- strtoi(readings[1])

    for (i in seq_len(length(readings))) {
        reading <- strtoi(readings[i])

        if (reading > last_reading) {
            output[i] <- "Increased"
        } else if (reading < last_reading) {
            output[i] <- "Decreased"
        } else {
            output[i] <- "No Change"
        }

        last_reading <- reading
    }

    cat("  Depth increased ", sum(output == "Increased"), " times!")
}

part1()
