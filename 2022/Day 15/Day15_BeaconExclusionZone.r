# Day 15: Beacon Exclusion Zone

get_day15_input_path <- function() {
    paste(getwd(), "/2022/Day 15/Day15_input.txt", sep = "")
}

part1 <- function(target_row = 2000000) {
    input <- readLines(get_day15_input_path())

    no_beacon <- c()

    for (i in seq_len(length(input))) {
        sensor <- parse_sensor(input[i])
        beacon <- parse_beacon(input[i])

        x_delta <- abs(sensor[1] - beacon[1])
        y_delta <- abs(sensor[2] - beacon[2])
        distance <- y_delta + x_delta

        if ((target_row >= sensor[2] & target_row <= sensor[2] + distance) |
            (target_row <= sensor[2] & target_row >= sensor[2] - distance)) {

            delta <- abs(target_row - sensor[2])
            radius <- distance - delta
            min_c <- sensor[1] - radius
            max_c <- sensor[1] + radius

            no_beacon <- unique(append(no_beacon, min_c:max_c))
        }
    }

    num_no_beacon <- length(no_beacon) - 1
    print(num_no_beacon)
}

part2 <- function() {
    print("TODO")
}

parse_sensor <- function(input) {
    tokenized <- unlist(strsplit(input, " "))
    x <- strtoi(substr(tokenized[3], 3, nchar(tokenized[3]) - 1))
    y <- strtoi(substr(tokenized[4], 3, nchar(tokenized[4]) - 1))
    c(x, y)
}

parse_beacon <- function(input) {
    tokenized <- unlist(strsplit(input, " "))
    x <- strtoi(substr(tokenized[9], 3, nchar(tokenized[9]) - 1))
    y <- strtoi(substr(tokenized[10], 3, nchar(tokenized[10])))
    c(x, y)
}

part1()
part2()