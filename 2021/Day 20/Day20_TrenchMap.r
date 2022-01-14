# Day 20: Trench Map

get_day20_input_path <- function() {
    paste(getwd(), "/2021/Day 20/Day20_input.txt", sep = "")
}

part1 <- function(num_enhancements = 2) {
    readings <- readLines(get_day20_input_path())

    padding <- num_enhancements + 1
    trench_len <- nchar(readings[3]) + padding * 2

    enhance <- unlist(strsplit(readings[1], ""))
    trench <- matrix(ncol = trench_len)[-1, ]

    trench <- map_trench(trench, readings, padding)

    # Enhancements loop
    for (i in 1:num_enhancements) {
        new_trench <- matrix(".", nrow = nrow(trench), ncol = ncol(trench))

        trench_ind <- which(trench == "#", arr.ind = TRUE)
        trench_ind <- rbind(trench_ind, which(trench == ".", arr.ind = TRUE))

        # Trench index loop
        for (l in seq_len(nrow(trench_ind))) {
            points_to_calc <- get_adj_points(trench_ind[l, 1], trench_ind[l, 2],
                                        nrow(trench), ncol(trench))
            points_to_calc <- rbind(points_to_calc, trench_ind[l, ])

            # Points around current index loop
            for (p in seq_len(nrow(points_to_calc))) {
                segment <- get_adj_points(points_to_calc[p, 1],
                                        points_to_calc[p, 2],
                                        nrow(trench), ncol(trench))
                segment <- rbind(segment, points_to_calc[p, ])

                # If we have a 9-piece segment, calculate the enhancement index
                if (nrow(segment) == 9) {
                    segment <- segment[order(segment[, 1], segment[, 2]), ]
                    values <- trench[segment]

                    enhance_index <- lights_to_dec(values) + 1
                    point <- matrix(points_to_calc[p, ], ncol = 2)
                    new_trench[point] <- enhance[enhance_index]
                } else {
                    # We don't have a 9-piece segment, so we're near a border
                    # 0 index is "on" and 511 index is "off"
                    # So if we're not near the enhancement image,
                    # We can fill in the partial segment based on loop iteration
                    if (i %% 2 == 0) {
                        new_trench[segment] <- "."
                    } else {
                        new_trench[segment] <- "#"
                    }
                }
            }
        }
        trench <- new_trench
    }

    cat("Lit pixels:", sum(trench == "#"), "\n")
}

part2 <- function() {
    part1(50)
}

map_trench <- function(trench, readings, padding) {
    trench <- pad_rows(trench, padding)

    space <- rep(".", padding)
    for (i in 3:length(readings)) {
        trench <- rbind(trench, c(space,
                                  unlist(strsplit(readings[i], "")),
                                  space))
    }

    trench <- pad_rows(trench, padding)
}

pad_rows <- function(trench, padding) {
    for (i in 1:padding) {
        trench <- rbind(trench, ".")
    }
    trench
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

lights_to_dec <- function(values) {
    values <- replace(values, values == ".", "0")
    values <- replace(values, values == "#", "1")
    bin <- paste(values, collapse = "")
    dec <- strtoi(bin, base = 2)
    dec
}


part1()
part2()
