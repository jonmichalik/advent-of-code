# Day 12: Hot Springs
library(R.utils)
library(stringr)

get_day12_input_path <- function() {
    paste(getwd(), "/2023/Day 12/Day12_input.txt", sep = "")
}

# Takes like 10 minutes, but it works :)
part1 <- function() {
    input <- readLines(get_day12_input_path())
    total_matches <- 0

    for (i in seq_len(length(input))) {
        cat("Processing line", i, "\n")
        line <- unlist(strsplit(input[i], " "))
        line <- gsub("#", "1", line)
        line <- gsub("\\.", "0", line)

        conditions <- unlist(strsplit(line[1], ""))
        expected <- unlist(strsplit(line[2], ","))
        num_unknown <- sum(conditions == "?")
        unk_ind <- which(conditions == "?", arr.ind = TRUE)

        perms <- 2 ^ num_unknown - 1
        mask_length <- nchar(convert_to_bin(perms))

        for (p in 0:perms) {
            mask <- stringr::str_pad(convert_to_bin(p), mask_length, "left", "0")
            values <- unlist(strsplit(mask, ""))

            test <- conditions
            test[unk_ind] <- values

            str <- paste(test, collapse = "")
            grouped <- unlist(strsplit(str, "0"))
            grouped <- grouped[nzchar(grouped)]

            if (is_match(grouped, expected)) {
                total_matches <- total_matches + 1
            }
        }
    }

    print(total_matches)
}

part2 <- function() {
    print("TODO")
}

is_match <- function(groups, expected) {
    match <- TRUE

    if (length(groups) == length(expected)) {
        for (i in seq_len(length(expected))) {
            if (expected[i] != nchar(groups[i])) {
                match <- FALSE
                break
            }
        }
    } else {
        match <- FALSE
    }

    match
}

convert_to_bin <- function(num) {
    R.utils::intToBin(num)
}

part1()
part2()