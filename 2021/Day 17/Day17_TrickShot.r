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
    readings <- readLines(get_day17_input_path())

    area <- unlist(strsplit(readings[1], ": "))[2]
    axes <- unlist(strsplit(area, ", "))

    x_range <- parse_points(axes[1])
    y_range <- parse_points(axes[2])

    safe_initial_velocities <- 0

    angle_lo_bound <- min(y_range)
    angle_hi_bound <- abs(min(y_range))

    # Determined from Part 1
    velocity_lo_bound <- 17
    velocity_hi_bound <- max(x_range)

    for (x in velocity_lo_bound:velocity_hi_bound) {
        for (y in angle_lo_bound:angle_hi_bound) {
            cur_x <- x
            cur_y <- y
            velocity <- x
            gravity <- 1
            while (!passed_target_area(cur_x, cur_y, x_range, y_range)) {
                if (in_target_area(cur_x, cur_y, x_range, y_range)) {
                    safe_initial_velocities <- safe_initial_velocities + 1
                    break
                }
                if (velocity > 0) {
                    velocity <- velocity - 1
                    cur_x <- cur_x + velocity
                }
                cur_y <- cur_y + (y - gravity)
                gravity <- gravity + 1
            }
        }
    }

    cat("Safe Velocities:", safe_initial_velocities, "\n")
}

parse_points <- function(axis) {
    range <- unlist(strsplit(axis, "="))
    points <- as.numeric(unlist(strsplit(range[2], "\\..")))
    points[1]:points[2]
}

passed_target_area <- function(x, y, x_range, y_range) {
    x > max(x_range) || y < min(y_range)
}

in_target_area <- function(x, y, x_range, y_range) {
    x %in% x_range && y %in% y_range
}

part1()
part2()
