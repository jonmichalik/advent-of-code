# Day 3: Binary Diagnostic

part1 <- function() {
    readings <- readLines("Day3_input.txt")

    total_readings <- length(readings)

    diagnostics <- matrix(ncol = nchar(readings[1]))
    diagnostics <- diagnostics[-1, ]

    gamma_rate <- ""
    epsilon_rate <- ""

    for (i in seq_len(total_readings)) {
        diagnostics <- rbind(diagnostics,
                            strtoi(strsplit(readings[i], "")[[1]]))
    }

    for (i in seq_len(nchar(readings[1]))) {
        print(sum(diagnostics[, i] == 1))
        if (sum(diagnostics[, i] == 1) > (total_readings / 2)) {
            gamma_rate <- paste(gamma_rate, 1, sep = "")
            epsilon_rate <- paste(epsilon_rate, 0, sep = "")
        } else {
            gamma_rate <- paste(gamma_rate, 0, sep = "")
            epsilon_rate <- paste(epsilon_rate, 1, sep = "")
        }
    }

    print(strtoi(gamma_rate, base = 2) * strtoi(epsilon_rate, base = 2))
}

part1()
