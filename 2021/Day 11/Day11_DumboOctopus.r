# Day 11: Dumbo Octopus

get_day11_input_path <- function() {
    paste(getwd(), "/2021/Day 11/Day11_input.txt", sep = "")
}

part1 <- function() {
    readings <- readLines(get_day11_input_path())

    cave_rows <- length(readings)
    cave_cols <- nchar(readings[1])
    cave <- matrix(ncol = cave_cols)[-1, ]
    num_steps <- 100
    total_flashes <- 0

    for (i in seq_len(length(readings))) {
        cave <- rbind(cave, strtoi(unlist(strsplit(readings[i], ""))))
    }

    for (i in 1:num_steps) {
        cave <- cave + 1
        ready_to_flash <- which(cave > 9, arr.ind = TRUE)

        if (length(ready_to_flash) > 0) {
            total_flashes <- total_flashes + length(ready_to_flash) / 2
            cave[ready_to_flash] <- 0

            while (length(ready_to_flash) > 0) {
                adj_octs <- get_adj_points(ready_to_flash[1, 1],
                                            ready_to_flash[1, 2],
                                            cave_rows, cave_cols)

                adj_octs_to_inc <- rbind(
                                    which(cave > 0, arr.ind = TRUE),
                                    adj_octs)
                adj_octs_to_inc <- adj_octs_to_inc[duplicated(adj_octs_to_inc),
                                                    ,
                                                    drop = FALSE]

                cave[adj_octs_to_inc] <- cave[adj_octs_to_inc] + 1
                ready_to_flash <- ready_to_flash[-1, ]

                if (length(ready_to_flash) > 0) {
                    ready_to_flash <- rbind(ready_to_flash)
                }

                adj_to_flash <- which(cave > 9, arr.ind = TRUE)

                if (length(adj_to_flash) > 0) {
                    total_flashes <- total_flashes + length(adj_to_flash) / 2
                    cave[adj_to_flash] <- 0
                    ready_to_flash <- rbind(ready_to_flash, adj_to_flash)
                }
            }
        }
    }
    print(cave)
    print(total_flashes)
}

part2 <- function() {
    readings <- readLines(get_day11_input_path())

    cave_rows <- length(readings)
    cave_cols <- nchar(readings[1])
    cave <- matrix(ncol = cave_cols)[-1, ]
    total_flashes <- 0
    num_steps <- 0
    super_flash <- FALSE

    for (i in seq_len(length(readings))) {
        cave <- rbind(cave, strtoi(unlist(strsplit(readings[i], ""))))
    }

    while (!super_flash) {
        num_steps <- num_steps + 1
        cave <- cave + 1
        ready_to_flash <- which(cave > 9, arr.ind = TRUE)

        if (length(ready_to_flash) > 0) {
            total_flashes <- total_flashes + length(ready_to_flash) / 2
            cave[ready_to_flash] <- 0

            while (length(ready_to_flash) > 0) {
                adj_octs <- get_adj_points(ready_to_flash[1, 1],
                                            ready_to_flash[1, 2],
                                            cave_rows, cave_cols)

                adj_octs_to_inc <- rbind(
                                    which(cave > 0, arr.ind = TRUE),
                                    adj_octs)
                adj_octs_to_inc <- adj_octs_to_inc[duplicated(adj_octs_to_inc),
                                                    ,
                                                    drop = FALSE]

                cave[adj_octs_to_inc] <- cave[adj_octs_to_inc] + 1
                ready_to_flash <- ready_to_flash[-1, ]

                if (length(ready_to_flash) > 0) {
                    ready_to_flash <- rbind(ready_to_flash)
                }

                adj_to_flash <- which(cave > 9, arr.ind = TRUE)

                if (length(adj_to_flash) > 0) {
                    total_flashes <- total_flashes + length(adj_to_flash) / 2
                    cave[adj_to_flash] <- 0
                    ready_to_flash <- rbind(ready_to_flash, adj_to_flash)
                }
            }
        }
        super_flash <- sum(cave == 0) == cave_rows * cave_cols
    }
    print(cave)
    cat("Total flashes: ", total_flashes, "\n")
    cat("Steps til super flash: ", num_steps, "\n")
}

get_adj_points <- function(row, col, max_row, max_col) {
    adj_points <- matrix(ncol = 2)[-1, ]

    # Up Value
    if (row > 1) {
        adj_points <- rbind(adj_points, c(row - 1, col))
    }

    # Up Left Value
    if (row > 1 && col > 1) {
        adj_points <- rbind(adj_points, c(row - 1, col - 1))
    }

    # Up Right Value
    if (row > 1 && col < max_col) {
        adj_points <- rbind(adj_points, c(row - 1, col + 1))
    }

    # Down Value
    if (row < max_row) {
        adj_points <- rbind(adj_points, c(row + 1, col))
    }

    # Down Left Value
    if (row < max_row && col > 1) {
        adj_points <- rbind(adj_points, c(row + 1, col - 1))
    }

    # Down Right Value
    if (row < max_row && col < max_col) {
        adj_points <- rbind(adj_points, c(row + 1, col + 1))
    }

    # Left Value
    if (col > 1) {
        adj_points <- rbind(adj_points, c(row, col - 1))
    }

    # Right Value
    if (col < max_col) {
        adj_points <- rbind(adj_points, c(row, col + 1))
    }

    adj_points
}

part1()
part2()
