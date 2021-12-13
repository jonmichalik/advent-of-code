# Day 13: Transparent Origami

get_day13_input_path <- function() {
    paste(getwd(), "/2021/Day 13/Day13_input.txt", sep = "")
}

highest_x <- 0
highest_y <- 0

part1 <- function(num_folds = 1) {
    readings <- readLines(get_day13_input_path())

    dots <- readings[1:799]
    directions <- strsplit(readings[801:length(readings)], " ")
    set_dimensions(dots)

    grid <- matrix(".", ncol = highest_x, nrow = highest_y)

    for (d in seq_len(length(dots))) {
        dot <- strtoi(unlist(strsplit(dots[d], ","))) + 1
        grid[dot[2], dot[1]] <- "#"
    }

    for (f in seq_len(length(directions))) {
        grid <- fold_grid(grid, directions[f])

        if (f == num_folds) {
            break
        }
    }

    print(sum(grid == "#"))

    if (num_folds > 1) {
        print(grid)
        # EFJKZLBL
    }
}

part2 <- function() {
    part1(100)
}

set_dimensions <- function(dots) {
    for (i in seq_len(length(dots))) {
        dot <- strtoi(unlist(strsplit(dots[i], ","))) + 1
        if (highest_x < dot[1]) {
            highest_x <<- dot[1]
        }
        if (highest_y < dot[2]) {
            highest_y <<- dot[2]
        }
    }
}

fold_grid <- function(grid, direction) {
    parsed_dir <- unlist(strsplit(unlist(direction)[3], "="))

    if (parsed_dir[1] == "x") {
        grid <- x_fold(grid, strtoi(parsed_dir[2]) + 1)
    } else {
        grid <- y_fold(grid, strtoi(parsed_dir[2]) + 1)
    }

    grid
}

x_fold <- function(grid, fold_line) {
    right_fold <- grid[, (fold_line + 1):highest_x]
    right_fold_bound <- highest_x - fold_line
    right_fold_mirror <- right_fold[, c(right_fold_bound:1)]

    left_fold <- grid[, 1:(fold_line - 1)]

    for (i in seq_len(nrow(left_fold))) {
        for (j in seq_len(ncol(left_fold))) {
            if (left_fold[i, j] == "#") {
                next
            } else {
                left_fold[i, j] <- right_fold_mirror[i, j]
            }
        }
    }

    highest_x <<- ncol(left_fold)
    left_fold
}

y_fold <- function(grid, fold_line) {
    down_fold <- grid[(fold_line + 1):highest_y, ]
    down_fold_bound <- highest_y - fold_line
    down_fold_mirror <- down_fold[c(down_fold_bound:1), ]

    up_fold <- grid[1:(fold_line - 1), ]

    for (i in seq_len(nrow(up_fold))) {
        for (j in seq_len(ncol(up_fold))) {
            if (up_fold[i, j] == "#") {
                next
            } else {
                up_fold[i, j] <- down_fold_mirror[i, j]
            }
        }
    }

    highest_y <<- nrow(up_fold)
    up_fold
}

part1()
part2()
