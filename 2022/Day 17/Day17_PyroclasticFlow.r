# Day 17: Pyroclastic Flow

get_day17_input_path <- function() {
    paste(getwd(), "/2022/Day 17/Day17_input.txt", sep = "")
}

part1 <- function(num_blocks = 2022, matrix_size = 100) {
    input <- readLines(get_day17_input_path())

    blocks <- c("-", "+", "j", "l", "o")
    shift_interval <- 25
    new_block <- matrix(".", ncol = 7, nrow = shift_interval, byrow = TRUE)
    jets <- unlist(strsplit(input, ""))

    tower_height <- 0
    cur_highest_row <- matrix_size
    cur_block_num <- 0

    jet_iterations <- 1

    matrix <- matrix(".", nrow = matrix_size, ncol = 7, byrow = TRUE)
    matrix[matrix_size, ] <- "#"

    while (cur_block_num < num_blocks) {
        for (b in seq_len(length(blocks))) {
            falling <- TRUE
            block <- spawn_block(cur_highest_row, blocks[b])

            while (falling) {
                # jet calc
                jet <- jets[jet_iterations]

                block <- calc_jet_coords(matrix, block, jet)

                if (jet_iterations + 1 > length(jets)) {
                    jet_iterations <- 1
                } else {
                    jet_iterations <- jet_iterations + 1
                }

                # fall calc
                if (can_fall(matrix, block)) {
                    block <- calc_fall_coords(block)
                } else {
                    matrix[block] <- "#"
                    delta <- cur_highest_row - block[1, 1]
                    if (delta > 0) {
                        tower_height <- tower_height + delta
                        cur_highest_row <- block[1, 1]
                    }

                    falling <- FALSE

                    # if matrix is getting full, shift it
                    if (cur_highest_row < shift_interval) {
                        matrix <- rbind(new_block,
                            matrix[seq_len(nrow(matrix) - shift_interval), ])
                        cur_highest_row <- cur_highest_row + shift_interval
                    }
                }
            }

            cur_block_num <- cur_block_num + 1
            if (cur_block_num == num_blocks) {
                break
            }
        }
    }

    print(tower_height)
}

part2 <- function() {
    print("TODO")
}

spawn_block <- function(highest_row, shape) {
    coords <- matrix()

    if (shape == "-") {
        coords <- get_h_line(highest_row)
    } else if (shape == "+") {
        coords <- get_plus(highest_row)
    } else if (shape == "j") {
        coords <- get_j_angle(highest_row)
    } else if (shape == "l") {
        coords <- get_v_line(highest_row)
    } else if (shape == "o") {
        coords <- get_square(highest_row)
    }

    coords
}

calc_jet_coords <- function(matrix, block, jet) {
    next_coords <- block
    if (jet == ">") {
        next_coords[, 2] <- next_coords[, 2] + 1
        if (!any(next_coords[, 2] > 7) &&
            !any(matrix[next_coords] == "#")) {
            block <- next_coords
        }
    } else {
        next_coords[, 2] <- next_coords[, 2] - 1
        if (!any(next_coords[, 2] < 1) &&
            !any(matrix[next_coords] == "#")) {
            block <- next_coords
        }
    }

    block
}

calc_fall_coords <- function(block) {
    block[, 1] <- block[, 1] + 1
    block
}

can_fall <- function(matrix, block) {
    next_coords <- block
    next_coords[, 1] <- next_coords[, 1] + 1
    !any(matrix[next_coords] == "#")
}

get_h_line <- function(highest_row) {
    one <- c(highest_row - 4, 3)
    two <- c(highest_row - 4, 4)
    three <- c(highest_row - 4, 5)
    four <- c(highest_row - 4, 6)
    matrix(c(one, two, three, four), ncol = 2, byrow = TRUE)
}

get_plus <- function(highest_row) {
    one <- c(highest_row - 6, 4)
    two <- c(highest_row - 5, 3)
    three <- c(highest_row - 5, 4)
    four <- c(highest_row - 5, 5)
    five <- c(highest_row - 4, 4)
    matrix(c(one, two, three, four, five), ncol = 2, byrow = TRUE)
}

get_j_angle <- function(highest_row) {
    one <- c(highest_row - 6, 5)
    two <- c(highest_row - 5, 5)
    three <- c(highest_row - 4, 3)
    four <- c(highest_row - 4, 4)
    five <- c(highest_row - 4, 5)
    matrix(c(one, two, three, four, five), ncol = 2, byrow = TRUE)
}

get_v_line <- function(highest_row) {
    one <- c(highest_row - 7, 3)
    two <- c(highest_row - 6, 3)
    three <- c(highest_row - 5, 3)
    four <- c(highest_row - 4, 3)
    matrix(c(one, two, three, four), ncol = 2, byrow = TRUE)
}

get_square <- function(highest_row) {
    one <- c(highest_row - 5, 3)
    two <- c(highest_row - 5, 4)
    three <- c(highest_row - 4, 3)
    four <- c(highest_row - 4, 4)
    matrix(c(one, two, three, four), ncol = 2, byrow = TRUE)
}

part1()
part2()