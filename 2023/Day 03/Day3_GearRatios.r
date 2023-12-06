# Day 3: Gear Ratios

get_day3_input_path <- function() {
    paste(getwd(), "/2023/Day 03/Day3_input.txt", sep = "")
}

part1 <- function() {
    lines <- readLines(get_day3_input_path())

    grid_length <- nchar(lines[1])
    grid_height <- length(lines)

    schematic <- t(matrix(unlist(strsplit(lines[1:grid_height], "")),
                        ncol = grid_length))

    part_nums <- c()

    for (i in seq_len(length(1:grid_height))) {
        cur_num_parts <- c()
        cur_num_coords <- list()

        for (j in seq_len(length(1:grid_length))) {
            cur_char <- schematic[i, j]

            if (!is.na(as.numeric(cur_char))) {
                cur_num_parts <- append(cur_num_parts, cur_char)
                cur_num_coords[[length(cur_num_coords) + 1]] <- c(i, j)

                if (j == grid_length && !is.na(cur_num_coords)) {
                    if (!is.na(cur_num_coords) &
                        is_part_num(cur_num_coords, schematic, grid_length, grid_height)) {

                        part_num <- as.numeric(
                            paste(cur_num_parts, collapse = ""))

                        part_nums <- append(part_nums, part_num)
                    }
                }
            } else if (length(cur_num_coords) > 0) {
                if (is_part_num(cur_num_coords, schematic, grid_length, grid_height)) {

                    part_num <- as.numeric(paste(cur_num_parts, collapse = ""))
                    part_nums <- append(part_nums, part_num)
                }

                cur_num_parts <- c()
                cur_num_coords <- c()
            }
        }
    }

    print(sum(part_nums))
}

part2 <- function() {
    print("TODO")
}

is_part_num <- function(num_coords, schematic, length, height) {
    is_part <- FALSE
    adj_coords <- get_adj_coords(num_coords, length, height)

    for (c in seq_len(length(adj_coords))) {
        coord <- adj_coords[[c]]
        val <- schematic[coord[1], coord[2]]
        if (val != "." && is.na(as.numeric(val))) {
            is_part <- TRUE
            break
        }
    }

    is_part
}

get_adj_coords <- function(coords, length, height) {
    adj_coords <- list()

    for (c in seq_len(length(coords))) {
        coord <- coords[[c]]

        # Adj Up
        if (coord[1] > 1) {
            adj_coords[[length(adj_coords) + 1]] <- c(coord[1] - 1, coord[2])
        }
        # Adj Down
        if (coord[1] < height) {
            adj_coords[[length(adj_coords) + 1]] <- c(coord[1] + 1, coord[2])
        }
        # Adj Left
        if (coord[2] > 1) {
            adj_coords[[length(adj_coords) + 1]] <- c(coord[1], coord[2] - 1)
        }
        # Adj Right
        if (coord[2] < length) {
            adj_coords[[length(adj_coords) + 1]] <- c(coord[1], coord[2] + 1)
        }
    }

    first <- coords[[1]]
    last <- coords[[length(coords)]]

    # Adj NW
    if (first[1] > 1 && first[2] > 1) {
        adj_coords[[length(adj_coords) + 1]] <- c(first[1] - 1, first[2] - 1)
    }
    # Adj SW
    if (first[1] < height && first[2] > 1) {
        adj_coords[[length(adj_coords) + 1]] <- c(first[1] + 1, first[2] - 1)
    }
    # Adj NE
    if (last[1] > 1 && last[2] < length) {
        adj_coords[[length(adj_coords) + 1]] <- c(last[1] - 1, last[2] + 1)
    }
    # Adj SE
    if (last[1] < height && last[2] < length) {
        adj_coords[[length(adj_coords) + 1]] <- c(last[1] + 1, last[2] + 1)
    }

    adj_coords
}

part1()
part2()