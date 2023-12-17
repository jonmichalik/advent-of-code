# Day 13: Point of Incidence

get_day13_input_path <- function() {
    paste(getwd(), "/2023/Day 13/Day13_input.txt", sep = "")
}

part1 <- function(check_smudge = FALSE) {
    input <- readLines(get_day13_input_path())
    cur_matrix <- c()

    refs <- list()
    summary <- 0

    for (i in seq_len(length(input))) {
        if (nchar(input[i]) > 0) {
            cur_matrix <- append(cur_matrix, input[i])
        } else {
            refs[[length(refs) + 1]] <- check_matrix(cur_matrix, check_smudge)
            cur_matrix <- c()
        }
    }

    refs[[length(refs) + 1]] <- check_matrix(cur_matrix, check_smudge)

    for (i in seq_len(length(refs))) {
        r <- refs[[i]]

        if (r@type == "vertical") {
            summary <- summary + r@value
        } else {
            summary <- summary + (100 * r@value)
        }
    }

    print(summary)
}

part2 <- function() {
    part1(TRUE)
}

check_matrix <- function(lines, check_smudge = FALSE) {
    length <- nchar(lines[1])
    height <- length(lines)

    data <- unlist(strsplit(paste(lines, collapse = ""), ""))
    matrix <- matrix(data, nrow = height, ncol = length, byrow = TRUE)

    get_reflection(matrix, length, height, check_smudge)
}

get_reflection <- function(matrix, length, height, check_smudge) {
    r <- check_vertical(matrix, length, check_smudge)

    if (length(r@type) == 0) {
        r <- check_horizontal(matrix, height, check_smudge)
    }

    r
}

check_vertical <- function(matrix, length, check_smudge) {
    r <- new("Reflection")

    for (i in 1:(length - 1)) {
        left_set <- as.matrix(matrix[, i:1])
        right_set <- as.matrix(matrix[, (i + 1):length])

        if (length(left_set) < length(right_set)) {
            left_set <- left_set[seq_len(length(left_set))]
            right_set <- right_set[seq_len(length(left_set))]
        } else if (length(left_set) > length(right_set)) {
            left_set <- left_set[seq_len(length(right_set))]
            right_set <- right_set[seq_len(length(right_set))]
        }

        if (!check_smudge) {
            if (identical(left_set, right_set)) {
                r@type <- "vertical"
                r@value <- i
                break
            }
        } else {
            if (length(which(left_set != right_set)) == 1) {
                r@type <- "vertical"
                r@value <- i
                break
            }
        }
    }

    r
}

check_horizontal <- function(matrix, height, check_smudge) {
    r <- new("Reflection")

    for (i in 1:(height - 1)) {
        up_set <- as.matrix(matrix[i:1, ])
        down_set <- as.matrix(matrix[(i + 1):height, ])

        if (ncol(up_set) == 1) {
            up_set <- t(up_set)
        }

        if (ncol(down_set) == 1) {
            down_set <- t(down_set)
        }

        up_chars <- c()
        down_chars <- c()

        if (nrow(up_set) < nrow(down_set)) {
            up_chars <- up_set[seq_len(nrow(up_set)), ]
            down_chars <- down_set[seq_len(nrow(up_set)), ]
        } else if (nrow(up_set) > nrow(down_set)) {
            up_chars <- up_set[seq_len(nrow(down_set)), ]
            down_chars <- down_set[seq_len(nrow(down_set)), ]
        }

        if (!check_smudge) {
            if (identical(up_chars, down_chars)) {
                r@type <- "horizontal"
                r@value <- i
                break
            }
        } else {
            if (length(which(up_chars != down_chars)) == 1) {
                r@type <- "horizontal"
                r@value <- i
                break
            }
        }
    }

    r
}

setClass("Reflection", representation(
    type = "character",
    value = "numeric")
)

part1()
part2()