# Day 1: Trebuchet?!

get_day1_input_path <- function() {
    paste(getwd(), "/2023/Day 01/Day1_input.txt", sep = "")
}

part1 <- function() {
    lines <- readLines(get_day1_input_path())
    calibrations <- c()

    for (i in seq_len(length(lines))) {
        digits <- unlist(strsplit(lines[i], ""))
        first <- get_first_num(digits)

        reversed <- rev(digits)
        last <- get_first_num(reversed)

        calibration <- paste(first, last, sep = "")
        calibrations <- append(calibrations, as.numeric(calibration))
    }

    print(sum(calibrations))
}

part2 <- function() {
    lines <- readLines(get_day1_input_path())

    nums <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
    rev_nums <- c("eno", "owt", "eerht", "ruof", "evif", "xis", "neves", "thgie", "enin")

    calibrations <- c()

    for (i in seq_len(length(lines))) {
        digits <- unlist(strsplit(lines[i], ""))
        first <- get_first_num_or_word(digits, nums)

        reversed <- rev(digits)
        last <- get_first_num_or_word(reversed, rev_nums)

        calibration <- paste(first, last, sep = "")
        calibrations <- append(calibrations, as.numeric(calibration))
    }

    print(sum(calibrations))
}

get_first_num <- function(digits) {
    num <- ""
    for (d in seq_len(length(digits))) {
        if (!is.na(as.numeric(digits[d]))) {
            num <- digits[d]
            break
        }
    }
    num
}

get_first_num_or_word <- function(digits, words) {
    num <- ""
    for (d in seq_len(length(digits))) {
        if (!is.na(as.numeric(digits[d]))) {
            num <- digits[d]
            break
        }

        num <- paste(num, digits[d], sep = "")
        if (nchar(num) > 2) {
            for (i in seq_len(length(words))) {
                if (grepl(words[i], num, fixed = TRUE)) {
                    num <- as.character(i)
                    break
                }
            }

            if (!is.na(as.numeric(num))) {
                break
            }
        }
    }
    num
}

part1()
part2()