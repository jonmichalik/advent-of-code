# Day 14: Parabolic Reflector Dish

get_day14_input_path <- function() {
    paste(getwd(), "/2023/Day 14/Day14_input.txt", sep = "")
}

part1 <- function() {
    input <- readLines(get_day14_input_path())

    map_length <- nchar(input[1])
    map_height <- length(input)

    map <- t(matrix(unlist(strsplit(input[1:map_height], "")),
                        ncol = map_length))

    rocks <- which(map == "O", arr.ind = TRUE)
    row_magnitudes <- rev(c(seq_len(map_height)))
    total_load <- 0

    for (r in seq_len(nrow(rocks))) {
        rock <- rocks[r, ]
        row <- rock[1]
        col <- rock[2]

        map <- move_rock_up(map, row, col)
    }

    for (r in seq_len(nrow(map))) {
        total_load <- total_load + (sum(map[r, ] == "O") * row_magnitudes[r])
    }

    print(total_load)
}

part2 <- function() {
    print("TODO")
}

move_rock_up <- function(map, row, col) {
    blockers <- c("O", "#")
    moving <- TRUE
    cur_row <- row
    cur_col <- col

    while (moving) {
        up <- get_adj_up(cur_row, cur_col)

        if (!is.null(up)) {
            up_value <- map[up[1], up[2]]
            if (up_value %in% blockers) {
                moving <- FALSE
            } else {
                map[up[1], up[2]] <- "O"
                map[cur_row, cur_col] <- "."

                cur_row <- up[1]
                cur_col <- up[2]
            }
        } else {
            moving <- FALSE
        }
    }

    map
}

get_adj_up <- function(row, col) {
    if (row > 1) {
        c(row - 1, col)
    }
}

part1()
part2()