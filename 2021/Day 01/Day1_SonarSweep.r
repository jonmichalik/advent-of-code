# Day 1: Sonar Sweep

get_day1_input_path <- function() {
    paste(getwd(), "/2021/Day 01/Day1_input.txt", sep = "")
}

part1 <- function() {
    readings <- readLines(get_day1_input_path())

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

part2 <- function() {
    readings <- readLines(get_day1_input_path())

    output <- character(length(readings))

    last_reading <- get_three_reading_sum(readings, 1)

    for (i in seq_len(length(readings))) {

        # Ensure there are 3 numbers to sum
        if (length(readings) - i >= 2) {
            reading <- get_three_reading_sum(readings, i)

            if (reading > last_reading) {
                output[i] <- "Increased"
            } else if (reading < last_reading) {
                output[i] <- "Decreased"
            } else {
                output[i] <- "No Change"
            }

            last_reading <- reading
        }
    }

    cat("  Depth increased ", sum(output == "Increased"), " times!")
}

get_three_reading_sum <- function (readings, start_index) {
    sum(strtoi(readings[start_index]),
        strtoi(readings[start_index + 1]),
        strtoi(readings[start_index + 2]))
}

part1()
part2()
