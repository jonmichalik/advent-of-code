# Day 3: Binary Diagnostic

get_day3_input_path <- function() {
    paste(getwd(), "/2021/Day 3/Day3_input.txt", sep = "")
}

part1 <- function() {
    readings <- readLines(get_day3_input_path())

    total_readings <- length(readings)
    diag_len <- nchar(readings[1])

    diagnostics <- matrix(ncol = diag_len)
    diagnostics <- diagnostics[-1, ]

    gamma_rate <- ""
    epsilon_rate <- ""

    for (i in seq_len(total_readings)) {
        diagnostics <- rbind(diagnostics,
                            strsplit(readings[i], "")[[1]])
    }

    for (i in seq_len(diag_len)) {
        if (sum(diagnostics[, i] == "1") > (total_readings / 2)) {
            gamma_rate <- paste(gamma_rate, "1", sep = "")
            epsilon_rate <- paste(epsilon_rate, "0", sep = "")
        } else {
            gamma_rate <- paste(gamma_rate, "0", sep = "")
            epsilon_rate <- paste(epsilon_rate, "1", sep = "")
        }
    }

    print(strtoi(gamma_rate, base = 2) * strtoi(epsilon_rate, base = 2))
}

part2 <- function() {
    readings <- readLines(get_day3_input_path())

    total_readings <- length(readings)
    diag_len <- nchar(readings[1])

    diagnostics <- matrix(ncol = diag_len)
    diagnostics <- diagnostics[-1, ]

    for (i in seq_len(total_readings)) {
        diagnostics <- rbind(diagnostics,
                            strsplit(readings[i], "")[[1]])
    }

    # Oxygen Generator Rating
    o2_gen_rate <- diagnostics
    for (i in seq_len(diag_len)) {
        if (is.matrix(o2_gen_rate)) {
            if (sum(o2_gen_rate[, i] == "1") >= sum(o2_gen_rate[, i] == "0")) {
                o2_gen_rate <- o2_gen_rate[o2_gen_rate[, i] == "1", ]
            } else {
                o2_gen_rate <- o2_gen_rate[o2_gen_rate[, i] == "0", ]
            }
        } else {
            break
        }
    }

    o2_gen_rate_str <- paste(o2_gen_rate[1:diag_len], collapse = "")

    # CO2 Scrubber Rating
    co2_scrub_rate <- diagnostics
    for (i in seq_len(diag_len)) {
        if (is.matrix(co2_scrub_rate)) {
            if (sum(co2_scrub_rate[, i] == "1") <
                sum(co2_scrub_rate[, i] == "0")) {
                co2_scrub_rate <- co2_scrub_rate[co2_scrub_rate[, i] == "1", ]
            } else {
                co2_scrub_rate <- co2_scrub_rate[co2_scrub_rate[, i] == "0", ]
            }
        } else {
            break
        }
    }

    co2_scrub_rate_str <-  paste(co2_scrub_rate[1:diag_len], collapse = "")

    print(strtoi(o2_gen_rate_str, base = 2) *
        strtoi(co2_scrub_rate_str, base = 2))
}

part1()
part2()
