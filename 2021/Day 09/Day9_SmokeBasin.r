# Day 9: Smoke Basin

get_day9_input_path <- function() {
    paste(getwd(), "/2021/Day 9/Day9_input.txt", sep = "")
}

part1 <- function() {
    readings <- readLines(get_day9_input_path())

    basin_rows <- length(readings)
    basin_cols <- nchar(readings[1])
    basin <- matrix(ncol = basin_cols)[-1, ]

    for (i in seq_len(length(readings))) {
        basin <- rbind(basin, strtoi(unlist(strsplit(readings[i], ""))))
    }

    total_risk_level <- 0

    for (r in seq_len(basin_rows)) {
        for (c in seq_len(basin_cols)) {
            total_risk_level <- total_risk_level +
                calc_risk_level(basin, r, c, basin_rows, basin_cols)
        }
    }

    print(total_risk_level)
}

part2 <- function() {
    readings <- readLines(get_day9_input_path())

    basin_rows <- length(readings)
    basin_cols <- nchar(readings[1])
    basin <- matrix(ncol = basin_cols)[-1, ]
    low_points <- matrix(ncol = 2)[-1, ]
    basin_sizes <- c()

    for (i in seq_len(length(readings))) {
        basin <- rbind(basin, strtoi(unlist(strsplit(readings[i], ""))))
    }

    for (r in seq_len(basin_rows)) {
        for (c in seq_len(basin_cols)) {
            if (calc_risk_level(basin, r, c, basin_rows, basin_cols) > 0) {
                low_points <- rbind(low_points, c(r, c))
            }
        }
    }

    for (i in seq_len(length(low_points) / 2)) {
        # Sprawl from low points to make basins
        ind_basin <- get_basin_from_low_point(
                        basin,
                        low_points[i, 1],
                        low_points[i, 2],
                        basin_rows,
                        basin_cols)

        # Get size of individual basin
        ind_basin_size <- length(ind_basin) / 2
        basin_sizes <- append(basin_sizes, ind_basin_size)
    }

    basin_sizes <- sort(basin_sizes, decreasing = TRUE)

    # Multiply sizes of 3 largest basins
    print(prod(basin_sizes[1:3]))
}

calc_risk_level <- function(basin, row, col, max_row, max_col) {
    point_val <- basin[row, col]
    risk_level <- 0
    adj_point_vals <- c()

    # Up Value
    if (row > 1) {
        adj_point_vals <- append(adj_point_vals, basin[row - 1, col])
    }

    # Down Value
    if (row < max_row) {
        adj_point_vals <- append(adj_point_vals, basin[row + 1, col])
    }

    # Left Value
    if (col > 1) {
        adj_point_vals <- append(adj_point_vals, basin[row, col - 1])
    }

    # Right Value
    if (col < max_col) {
        adj_point_vals <- append(adj_point_vals, basin[row, col + 1])
    }

    if (sum(adj_point_vals > point_val) == length(adj_point_vals)) {
        risk_level <- point_val + 1
    }

    risk_level
}

get_basin_from_low_point <- function(basin, row, col, max_row, max_col) {
    basin_points <- matrix(ncol = 2)[-1, ]
    new_points <- matrix(ncol = 2)[-1, ]

    basin_points <- rbind(basin_points, c(row, col))
    new_points <- rbind(new_points, c(row, col))

    while (length(new_points) > 0) {
        adj_points <- get_adj_points(basin,
                                    new_points[1, 1],
                                    new_points[1, 2],
                                    max_row, max_col)
        new_points <- new_points[-1, ]
        new_points <- rbind(new_points)

        for (i in seq_len(length(adj_points) / 2)) {
            adj_point_r <- adj_points[i, 1]
            adj_point_c <- adj_points[i, 2]
            point_val <- basin[adj_point_r, adj_point_c]
            if (point_val < 9) {
                match <- FALSE
                for (p in seq_len(length(basin_points) / 2)) {
                    match <- (basin_points[p, 1] == adj_point_r &&
                                basin_points[p, 2] == adj_point_c)
                    if (match) break
                }

                if (!match) {
                    basin_points <- rbind(basin_points,
                                        c(adj_point_r, adj_point_c))
                    new_points <- rbind(new_points,
                                        c(adj_point_r, adj_point_c))
                }
            }
        }
    }
    basin_points
}

get_adj_points <- function(basin, row, col, max_row, max_col) {
    adj_points <- matrix(ncol = 2)[-1, ]

    # Up Value
    if (row > 1) {
        adj_points <- rbind(adj_points, c(row - 1, col))
    }

    # Down Value
    if (row < max_row) {
        adj_points <- rbind(adj_points, c(row + 1, col))
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
