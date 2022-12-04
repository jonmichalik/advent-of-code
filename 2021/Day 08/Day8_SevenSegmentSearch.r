# Day 8: Seven Segment Search

get_day8_input_path <- function() {
    paste(getwd(), "/2021/Day 08/Day8_input.txt", sep = "")
}

part1 <- function() {
    readings <- readLines(get_day8_input_path())

    entries <- strsplit(readings, " | ")
    output_indices <- 12:15

    num_1s <- 0
    num_4s <- 0
    num_7s <- 0
    num_8s <- 0

    for (i in seq_len(length(entries))) {
        entry <- unlist(entries[i])

        num_1s <- num_1s + sum(nchar(entry[output_indices]) == 2)
        num_4s <- num_4s + sum(nchar(entry[output_indices]) == 4)
        num_7s <- num_7s + sum(nchar(entry[output_indices]) == 3)
        num_8s <- num_8s + sum(nchar(entry[output_indices]) == 7)
    }

    cat("1:", num_1s, "; 4:", num_4s, "; 7:", num_7s, "; 8:", num_8s, "\n")
    print(sum(num_1s, num_4s, num_7s, num_8s))
}

part2 <- function() {
    readings <- readLines(get_day8_input_path())

    entries <- strsplit(readings, " | ")
    config_indices <- 1:10
    output_indices <- 12:15

    sum_of_outputs <- 0

    for (i in seq_len(length(entries))) {
        entry <- unlist(entries[i])

        wires <- decode_wires(entry[config_indices])
        output <- ""

        output_vals <- entry[output_indices]
        for (i in seq_len(length(output_vals))) {
            num <- paste(sort(unlist(strsplit(output_vals[i], ""))),
                        collapse = "")
            decoded_num <- wires[which(wires[, 1] == num), ][2]
            output <- paste(output, decoded_num, sep = "")
        }

        sum_of_outputs <- sum_of_outputs + as.numeric(output)
    }
    print(sum_of_outputs)
}

# Not proud of this monster function lol
decode_wires <- function(config) {
    wires <- matrix(ncol = 2)[-1, ]
    segments <- vector(mode = "character", length = 7)

    wires <- get_known_wires(wires, config)

    wire_7 <- unlist(strsplit(wires[which(wires[, 2] == "7"), ][1], ""))
    wire_1 <- unlist(strsplit(wires[which(wires[, 2] == "1"), ][1], ""))
    segments[1] <- setdiff(wire_7, wire_1)

    # Identify 9 based on 8 and 4
    wire_8 <- unlist(strsplit(wires[which(wires[, 2] == "8"), ][1], ""))
    wire_4 <- unlist(strsplit(wires[which(wires[, 2] == "4"), ][1], ""))

    len_6_configs <- config[nchar(config) == 6]

    for (i in seq_len(length(len_6_configs))) {
        num <- sort(unlist(strsplit(len_6_configs[i], "")))

        in_8_not_num <- setdiff(wire_8, num)
        in_4_not_num <- setdiff(wire_4, num)
        if (length(in_8_not_num == 1) && length(in_4_not_num) == 0) {
            segments[5] <- in_8_not_num
            nine_index <- which(wires[, 1] == paste(num, collapse = ""))
            wires[nine_index, 2] <- 9
            break
        }
    }

    wire_9 <- unlist(strsplit(wires[which(wires[, 2] == "9"), ][1], ""))

    # Identify 6 based on 9 and 1
    for (i in seq_len(length(len_6_configs))) {
        num <- sort(unlist(strsplit(len_6_configs[i], "")))

        in_1_not_num <- setdiff(wire_1, num)
        in_num_not_9 <- setdiff(num, wire_9)
        if (length(in_1_not_num == 1) && length(in_num_not_9) == 1
                && in_num_not_9[1] == segments[5]) {
            segments[5] <- in_num_not_9[1]
            six_index <- which(wires[, 1] == paste(num, collapse = ""))
            wires[six_index, 2] <- 6

            segments[3] <- in_1_not_num[1]
            segments[6] <- setdiff(wire_1, in_1_not_num)
            break
        }
    }

    wire_6 <- unlist(strsplit(wires[which(wires[, 2] == "6"), ][1], ""))

    # Identify 0 based on 9 and 6
    for (i in seq_len(length(len_6_configs))) {
        num <- sort(unlist(strsplit(len_6_configs[i], "")))

        in_6_not_num <- setdiff(wire_6, num)
        in_9_not_num <- setdiff(wire_9, num)
        if (length(in_9_not_num) == 1 && length(in_6_not_num) == 1 &&
                in_9_not_num == in_6_not_num) {
            segments[4] <- in_6_not_num[1]
            zero_index <- which(wires[, 1] == paste(num, collapse = ""))
            wires[zero_index, 2] <- 0
            break
        }
    }

    len_5_configs <- config[nchar(config) == 5]

    # Identify 5 based on 6
    for (i in seq_len(length(len_5_configs))) {
        num <- sort(unlist(strsplit(len_5_configs[i], "")))

        in_6_not_num <- setdiff(wire_6, num)
        if (length(in_6_not_num) == 1 && in_6_not_num[1] == segments[5]) {
            five_index <- which(wires[, 1] == paste(num, collapse = ""))
            wires[five_index, 2] <- 5
            break
        }
    }

    # Identify 2 based on 6 and 1
    for (i in seq_len(length(len_5_configs))) {
        num <- sort(unlist(strsplit(len_5_configs[i], "")))

        in_6_not_num <- setdiff(wire_6, num)
        in_1_not_6 <- setdiff(wire_1, wire_6)
        in_1_not_num <- setdiff(wire_1, num)
        if (length(in_6_not_num) == 2 &&
                length(in_1_not_6) > 0 &&
                length(in_1_not_num) > 0 &&
                in_1_not_6[1] != in_1_not_num[1]) {
            two_index <- which(wires[, 1] == paste(num, collapse = ""))
            wires[two_index, 2] <- 2
            segments[2] <- setdiff(in_6_not_num, in_1_not_num)
            break
        }
    }

    # Remainder is 3
    wires[which(wires[, 2] == "-1"), 2] <- 3

    wires
}

get_known_wires <- function(wires, config) {
    for (i in seq_len(length(config))) {
        num <- sort(unlist(strsplit(config[i], "")))

        # Starting known values
        if (length(num) == 2) {
            wires <- rbind(wires, c(paste(num, collapse = ""), 1))
        } else if (length(num) == 3) {
            wires <- rbind(wires, c(paste(num, collapse = ""), 7))
        } else if (length(num) == 4) {
            wires <- rbind(wires, c(paste(num, collapse = ""), 4))
        } else if (length(num) == 7) {
            wires <- rbind(wires, c(paste(num, collapse = ""), 8))
        } else {
            wires <- rbind(wires, c(paste(num, collapse = ""), -1))
        }
    }
    wires
}

part1()
part2()
