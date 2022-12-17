# Day 14: Regolith Reservoir

get_day14_input_path <- function() {
    paste(getwd(), "/2022/Day 14/Day14_input.txt", sep = "")
}

lowest_point <- 1

part1 <- function() {
    input <- readLines(get_day14_input_path())
    matrix <- matrix(".", nrow = 170, ncol = 610)

    points <- strsplit(input, " -> ")

    for (i in seq_len(length(points))) {
        line <- parse_line(points[[i]])
        for (p in seq_len(length(line))) {
            point <- c(line[[p]][2], line[[p]][1])
            matrix[point[1], point[2]] <- "#"
        }
    }

    num_grains <- 0
    full <- FALSE
    cur_pos <- c(1, 501)

    while (!full) {
        falling <- TRUE
        cur_pos <- c(1, 501)

        while (falling) {
            if (cur_pos[1] + 1 > lowest_point + 1) {
                full <- TRUE
                break
            }

            space_below <- matrix[cur_pos[1] + 1, cur_pos[2]]
            if (space_below == ".") {
                cur_pos <- c(cur_pos[1] + 1, cur_pos[2])
            } else {
                space_left <- matrix[cur_pos[1] + 1, cur_pos[2] - 1]
                if (space_left == ".") {
                    cur_pos <- c(cur_pos[1] + 1, cur_pos[2] - 1)
                } else {
                    space_right <- matrix[cur_pos[1] + 1, cur_pos[2] + 1]
                    if (space_right == ".") {
                        cur_pos <- c(cur_pos[1] + 1, cur_pos[2] + 1)
                    } else {
                        matrix[cur_pos[1], cur_pos[2]] <- "o"
                        falling <- FALSE
                        num_grains <- num_grains + 1
                    }
                }
            }
        }
    }

    print(num_grains)
}

part2 <- function() {
    input <- readLines(get_day14_input_path())

    # Inside baseball, knows this should be 171 from part 1
    matrix <- matrix(".", nrow = 171, ncol = 700)

    points <- strsplit(input, " -> ")

    for (i in seq_len(length(points))) {
        line <- parse_line(points[[i]])
        for (p in seq_len(length(line))) {
            point <- c(line[[p]][2], line[[p]][1])
            matrix[point[1], point[2]] <- "#"
        }
    }

    matrix[lowest_point + 2, ] <- "#"

    num_grains <- 0
    full <- FALSE
    cur_pos <- c(1, 501)

    while (!full) {
        falling <- TRUE
        cur_pos <- c(1, 501)

        while (falling) {
            space_below <- matrix[cur_pos[1] + 1, cur_pos[2]]
            if (space_below == ".") {
                cur_pos <- c(cur_pos[1] + 1, cur_pos[2])
            } else {
                space_left <- matrix[cur_pos[1] + 1, cur_pos[2] - 1]
                if (space_left == ".") {
                    cur_pos <- c(cur_pos[1] + 1, cur_pos[2] - 1)
                } else {
                    space_right <- matrix[cur_pos[1] + 1, cur_pos[2] + 1]
                    if (space_right == ".") {
                        cur_pos <- c(cur_pos[1] + 1, cur_pos[2] + 1)
                    } else {
                        matrix[cur_pos[1], cur_pos[2]] <- "o"
                        falling <- FALSE
                        num_grains <- num_grains + 1

                        if (cur_pos[1] == 1 & cur_pos[2] == 501) {
                            full <- TRUE
                            break
                        }
                    }
                }
            }
        }
    }

    print(num_grains)
}

parse_line <- function(points) {
    line <- list()

    tokenized <- strsplit(points, ",")
    for (i in seq_len(length(tokenized) - 1)) {
        left <- strtoi(tokenized[[i]]) + 1
        right <- strtoi(tokenized[[i + 1]]) + 1

        line[[length(line) + 1]] <- left

        if (left[1] == right[1]) {
            if (left[2] > right[2]) {
                for (r in (left[2] - 1):(right[2] + 1)) {
                    line[[length(line) + 1]] <- c(left[1], r)
                }
            } else {
                for (r in (right[2] - 1):(left[2] + 1)) {
                    line[[length(line) + 1]] <- c(left[1], r)
                }
            }
        } else {
            if (left[1] > right[1]) {
                for (c in (left[1] - 1):(right[1] + 1)) {
                    line[[length(line) + 1]] <- c(c, left[2])
                }
            } else {
                for (c in (right[1] - 1):(left[1] + 1)) {
                    line[[length(line) + 1]] <- c(c, left[2])
                }
            }
        }

        if (left[2] > lowest_point) {
            lowest_point <<- left[2]
        }

        if (i == length(tokenized) - 1) {
            line[[length(line) + 1]] <- right

            if (right[2] > lowest_point) {
                lowest_point <<- right[2]
            }
        }
    }

    line
}

part1()
part2()