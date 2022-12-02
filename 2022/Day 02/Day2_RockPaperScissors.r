# Day 2: Rock Paper Scissors

get_day2_input_path <- function() {
    paste(getwd(), "/2022/Day 02/Day2_input.txt", sep = "")
}

part1 <- function() {
    strategies <- readLines(get_day2_input_path())

    scores <- c()

    o_shapes <- c("A", "B", "C")
    p_shapes <- c("X", "Y", "Z")

    for (i in seq_len(length(strategies))) {
        strat <- unlist(strsplit(strategies[i], " "))

        o_shape <- strat[1]
        p_shape <- strat[2]

        o_shape_points <- match(o_shape, o_shapes)
        p_shape_points <- match(p_shape, p_shapes)

        if (is_draw(o_shape_points, p_shape_points)) {
            score <- 3 + p_shape_points
            scores <- append(scores, score)
        } else if (is_win(o_shape_points, p_shape_points)) {
            score <- 6 + p_shape_points
            scores <- append(scores, score)
        } else {
            scores <- append(scores, p_shape_points)
        }
    }

    print(sum(scores))
}

part2 <- function() {
    strategies <- readLines(get_day2_input_path())

    scores <- c()

    shapes <- c("A", "B", "C")

    for (i in seq_len(length(strategies))) {
        strat <- unlist(strsplit(strategies[i], " "))

        o_shape <- strat[1]
        o_shape_points <- match(o_shape, shapes)

        p_shape <- ""
        outcome_points <- 0

        if (strat[2] == "X") {
            p_shape <- get_lose_shape(o_shape_points)
        } else if (strat[2] == "Y") {
            p_shape <- get_draw_shape(o_shape_points)
            outcome_points <- 3
        } else {
            p_shape <- get_win_shape(o_shape_points)
            outcome_points <- 6
        }

        p_shape_points <- match(p_shape, shapes)
        score <- outcome_points + p_shape_points
        scores <- append(scores, score)
    }

    print(sum(scores))
}

is_draw <- function(o_shape_points, p_shape_points) {
    (o_shape_points == p_shape_points)
}

is_win <- function(o_shape_points, p_shape_points) {
    (p_shape_points == 1 & o_shape_points == 3) |
    (p_shape_points == 2 & o_shape_points == 1) |
    (p_shape_points == 3 & o_shape_points == 2)
}

get_win_shape <- function(o_shape_points) {
    if (o_shape_points == 1) {
        "B"
    } else if (o_shape_points == 2) {
        "C"
    } else {
        "A"
    }
}

get_draw_shape <- function(o_shape_points) {
    if (o_shape_points == 1) {
        "A"
    } else if (o_shape_points == 2) {
        "B"
    } else {
        "C"
    }
}

get_lose_shape <- function(o_shape_points) {
    if (o_shape_points == 1) {
        "C"
    } else if (o_shape_points == 2) {
        "A"
    } else {
        "B"
    }
}

part1()
part2()