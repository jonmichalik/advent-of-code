# Day 17: Trick Shot

get_day17_input_path <- function() {
    paste(getwd(), "/2021/Day 17/Day17_input.txt", sep = "")
}

part1 <- function() {
    readings <- readLines(get_day17_input_path())

    area <- unlist(strsplit(readings[1], ": "))[2]
    axes <- unlist(strsplit(area, ", "))

    x_range <- parse_points(axes[1])
    y_range <- parse_points(axes[2])

    # Optimal velocity should be the number that reaches velocity of 0
    # closest to the left border of the target zone
    min_x <- min(x_range)

    opt_velocity <- 1
    while (sum(1:opt_velocity) < min_x) {
        opt_velocity <- opt_velocity + 1
    }

    # Optimal height angle should be the number where the next gravity addition
    # lands closest to the bottom border of the target zone
    min_y <- min(y_range)

    opt_angle <- abs(min_y) - 1
    target_y <- - (opt_angle + 1)

    cat("Optimal input:", opt_velocity, ",", opt_angle, "\n")
    cat("Lands at:", sum(1:opt_velocity), ",", target_y, "\n")
    cat("Heighest point:", sum(1:opt_angle), "\n")
}

part2 <- function() {
    print("TODO")
}

parse_points <- function(axis) {
    range <- unlist(strsplit(axis, "="))
    points <- as.numeric(unlist(strsplit(range[2], "\\..")))
    points[1]:points[2]
}

part1()
part2()
