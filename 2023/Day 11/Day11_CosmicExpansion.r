# Day 11: Cosmic Expansion

get_day11_input_path <- function() {
    paste(getwd(), "/2023/Day 11/Day11_input.txt", sep = "")
}

part1 <- function() {
    input <- readLines(get_day11_input_path())

    length <- nchar(input[1])
    height <- length(input)

    cosmos <- t(matrix(unlist(strsplit(input[1:height], "")),
                        ncol = length))

    cosmos <- expand_cosmos(cosmos, length, height)
    galaxies <- which(cosmos == "#", arr.ind = TRUE)
    num_galaxies <- length(galaxies[, 1])
    sum_of_paths <- 0

    for (g in seq_len(num_galaxies)) {
        galaxy <- galaxies[g, ]

        if (g < num_galaxies) {
            for (x in (g + 1):num_galaxies) {
                pair <- galaxies[x, ]
                steps <- abs(galaxy[1] - pair[1]) + abs(galaxy[2] - pair[2])
                sum_of_paths <- sum_of_paths + steps
            }
        }
    }

    print(sum_of_paths[[1]])
}

part2 <- function() {
    input <- readLines(get_day11_input_path())

    length <- nchar(input[1])
    height <- length(input)

    cosmos <- t(matrix(unlist(strsplit(input[1:height], "")),
                        ncol = length))

    empty_rows <- get_empty_rows(cosmos, length)
    empty_cols <- get_empty_cols(cosmos, height)

    galaxies <- which(cosmos == "#", arr.ind = TRUE)
    num_galaxies <- length(galaxies[, 1])
    sum_of_paths <- 0

    for (g in seq_len(num_galaxies)) {
        galaxy <- galaxies[g, ]

        if (g < num_galaxies) {
            for (x in (g + 1):num_galaxies) {
                pair <- galaxies[x, ]

                row_coords <- sort(c(galaxy[1], pair[1]))
                num_erow_passes <- sum(empty_rows > row_coords[1] &
                    empty_rows < row_coords[2]) * 999999

                col_coords <- sort(c(galaxy[2], pair[2]))
                num_ecol_passes <- sum(empty_cols > col_coords[1] &
                    empty_cols < col_coords[2]) * 999999

                steps <- (abs(galaxy[1] - pair[1]) + num_erow_passes) +
                    (abs(galaxy[2] - pair[2]) + num_ecol_passes)
                sum_of_paths <- sum_of_paths + steps
            }
        }
    }

    print(format(sum_of_paths[[1]], scientific = FALSE))
}

get_empty_rows <- function(cosmos, length) {
    empty_rows <- c()

    for (r in seq_len(length)) {
        row <- cosmos[r, ]
        if (all(row == ".")) {
            empty_rows <- append(empty_rows, r)
        }
    }

    empty_rows
}

get_empty_cols <- function(cosmos, height) {
    empty_cols <- c()

    for (c in seq_len(height)) {
        col <- cosmos[, c]
        if (all(col == ".")) {
            empty_cols <- append(empty_cols, c)
        }
    }

    empty_cols
}

expand_cosmos <- function(cosmos, length, height) {
    empty_rows <- get_empty_rows(cosmos, length)
    cosmos <- expand_rows(cosmos, empty_rows)

    empty_cols <- get_empty_cols(cosmos, height)
    cosmos <- expand_cols(cosmos, empty_cols)

    cosmos
}

expand_rows <- function(cosmos, rows) {
    for (r in seq_len(length(rows))) {
        height <- length(cosmos[, 1]) + 1
        length <- length(cosmos[1, ])
        offset <- r - 1

        matrix <- matrix(".", nrow = height, ncol = length)
        matrix[-(rows[r] + offset), ] <- cosmos
        cosmos <- matrix
    }
    cosmos
}

expand_cols <- function(cosmos, cols) {
    for (c in seq_len(length(cols))) {
        height <- length(cosmos[, 1])
        length <- length(cosmos[1, ]) + 1
        offset <- c - 1

        matrix <- matrix(".", nrow = height, ncol = length)
        matrix[, -(cols[c] + offset)] <- cosmos
        cosmos <- matrix
    }
    cosmos
}

part1()
part2()