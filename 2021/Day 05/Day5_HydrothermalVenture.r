# Day 5: Hydrothermal Venture

get_day5_input_path <- function() {
    paste(getwd(), "/2021/Day 5/Day5_input.txt", sep = "")
}

part1 <- function() {
    readings <- readLines(get_day5_input_path())

    plot <- matrix(0, ncol = 1000, nrow = 1000)
    for (i in seq_len(length(readings))) {
        reading <- stringr::str_replace(readings[i], " -> ", ",")

        points <- matrix(unlist(strsplit(reading, ",")),
                        ncol = 2, byrow = TRUE)
        if (points[1, 1] == points [2, 1]) {
            x <- strtoi(points[1, 1])
            y1 <- strtoi(points[1, 2])
            y2 <- strtoi(points[2, 2])
            for (i in y1:y2) {
                # Add 1 to each index because 1-base :P
                plot[x + 1, i + 1] <- plot[x + 1, i + 1] + 1
            }
        } else if (points[1, 2] == points[2, 2]) {
            y <- strtoi(points[1, 2])
            x1 <- strtoi(points[1, 1])
            x2 <- strtoi(points[2, 1])
            for (i in x1:x2) {
                # Add 1 to each index because 1-base :P
                plot[i + 1, y + 1] <- plot[i + 1, y + 1] + 1
            }
        }
    }
    print(sum(plot > 1))
}

part2 <- function() {
    readings <- readLines(get_day5_input_path())

    plot <- matrix(0, ncol = 1000, nrow = 1000)
    for (i in seq_len(length(readings))) {
        reading <- stringr::str_replace(readings[i], " -> ", ",")

        points <- matrix(unlist(strsplit(reading, ",")),
                        ncol = 2, byrow = TRUE)
        if (points[1, 1] == points [2, 1]) {
            x <- strtoi(points[1, 1])
            y1 <- strtoi(points[1, 2])
            y2 <- strtoi(points[2, 2])
            for (i in y1:y2) {
                # Add 1 to each index because 1-base :P
                plot[x + 1, i + 1] <- plot[x + 1, i + 1] + 1
            }
        } else if (points[1, 2] == points[2, 2]) {
            y <- strtoi(points[1, 2])
            x1 <- strtoi(points[1, 1])
            x2 <- strtoi(points[2, 1])
            for (i in x1:x2) {
                # Add 1 to each index because 1-base :P
                plot[i + 1, y + 1] <- plot[i + 1, y + 1] + 1
            }
        } else {
            x1 <- strtoi(points[1, 1])
            x2 <- strtoi(points[2, 1])
            y1 <- strtoi(points[1, 2])
            y2 <- strtoi(points[2, 2])
            x_range <- x1:x2
            y_range <- y1:y2
            for (i in seq_len(length(x_range))) {
                # Add 1 to each index because 1-base :P
                plot[x_range[i] + 1, y_range[i] + 1] <-
                    plot[x_range[i] + 1, y_range[i] + 1] + 1
            }
        }
    }
    print(sum(plot > 1))
}

part1()
part2()
