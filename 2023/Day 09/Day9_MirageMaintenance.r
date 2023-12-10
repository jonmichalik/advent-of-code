# Day 9: Mirage Maintenance

get_day9_input_path <- function() {
    paste(getwd(), "/2023/Day 09/Day9_input.txt", sep = "")
}

part1 <- function() {
    readings <- readLines(get_day9_input_path())
    next_values <- c()

    for (i in seq_len(length(readings))) {
        reading <- as.numeric(unlist(strsplit(readings[i], " ")))
        next_val <- extrapolate_reading(reading) + rev(reading)[1]

        next_values <- append(next_values, next_val)
    }

    print(sum(next_values))
}

part2 <- function() {
    readings <- readLines(get_day9_input_path())
    prev_values <- c()

    for (i in seq_len(length(readings))) {
        reading <- as.numeric(unlist(strsplit(readings[i], " ")))
        prev_val <- reading[1] - predict_reading(reading)

        prev_values <- append(prev_values, prev_val)
    }

    print(sum(prev_values))
}

extrapolate_reading <- function(reading) {
    ex <- c()
    next_val <- 0

    for (i in seq_len(length(reading))) {
        if (i < length(reading)) {
            delta <- reading[i + 1] - reading[i]
            ex <- append(ex, delta)
        }
    }

    if (!all(ex == 0)) {
        next_val <- extrapolate_reading(ex) + rev(ex)[1]
    } else {
        next_val <- rev(ex)[1]
    }

    next_val
}

predict_reading <- function(reading) {
    ex <- c()
    prev_val <- 0

    for (i in seq_len(length(reading))) {
        if (i < length(reading)) {
            delta <- reading[i + 1] - reading[i]
            ex <- append(ex, delta)
        }
    }

    if (!all(ex == 0)) {
        prev_val <- ex[1] - predict_reading(ex)
    } else {
        prev_val <- ex[1]
    }

    prev_val
}

part1()
part2()