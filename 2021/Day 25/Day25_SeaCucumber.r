# Day 25: Sea Cucumber

get_day25_input_path <- function() {
    paste(getwd(), "/2021/Day 25/Day25_input.txt", sep = "")
}

part1 <- function() {
    readings <- readLines(get_day25_input_path())

    max_col <- nchar(readings[1])
    max_row <- length(readings)

    floor <- matrix(ncol = max_col)[-1, ]
    for (i in seq_len(max_row)) {
        floor <- rbind(floor, unlist(strsplit(readings[i], "")))
    }

    has_moved <- TRUE
    num_steps <- 1

    while (has_moved) {
        moves_this_step <- 0

        # Get indexes for right moving cucumbers with empty spaces to the right
        eastward <- which(floor == ">", arr.ind = TRUE)
        east_over <- get_adj_east_spaces(eastward, max_col)
        east_moveable <- matrix(which(floor[east_over] == "."), ncol = 2)

        moves_this_step <- length(east_moveable)

        # Swap values of the above indexes with the indexes to the right of them
        floor[east_over[east_moveable, ]] <- ">"
        floor[eastward[east_moveable, ]] <- "."

        # Get indexes for down moving cucumbers with empty spaces below
        southward <- which(floor == "v", arr.ind = TRUE)
        south_below <- get_adj_south_spaces(southward, max_row)
        south_moveable <- matrix(which(floor[south_below] == "."), ncol = 2)

        moves_this_step <- moves_this_step + length(south_moveable)

        # Swap values of the above indexes with the indexes below them
        floor[south_below[south_moveable, ]] <- "v"
        floor[southward[south_moveable, ]] <- "."

        if (moves_this_step == 0) {
            has_moved <- FALSE
        } else {
            num_steps <- num_steps + 1
        }
    }

    print(num_steps)
}

part2 <- function() {
    print("TODO")
}

get_adj_east_spaces <- function(eastward, max_col) {
    east_over <- eastward
    east_over[, 2] <- east_over[, 2] + 1
    east_over[, 2][east_over[, 2] > max_col] <- 1

    east_over
}

get_adj_south_spaces <- function(southward, max_row) {
    south_below <- southward
    south_below[, 1] <- south_below[, 1] + 1
    south_below[, 1][south_below[, 1] > max_row] <- 1

    south_below
}

part1()
part2()
