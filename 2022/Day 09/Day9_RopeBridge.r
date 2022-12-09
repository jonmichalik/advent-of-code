# Day 9: Rope Bridge

get_day9_input_path <- function() {
    paste(getwd(), "/2022/Day 09/Day9_input.txt", sep = "")
}

part1 <- function() {
    motions <- readLines(get_day9_input_path())

    head_pos <- c(0, 0)
    tail_pos <- c(0, 0)
    tail_visited <- c("0,0")

    for (i in seq_len(length(motions))) {
        motion <- unlist(strsplit(motions[i], " "))
        dir <- motion[1]
        steps <- motion[2]

        for (s in seq_len(length(1:steps))) {
            head_pos <- move_head(head_pos, dir)
            tail_pos <- move_tail(head_pos, tail_pos)

            tail_visited <- append(tail_visited,
                paste(tail_pos, collapse = ","))
        }
    }

    print(length(unique(tail_visited)))
}

part2 <- function() {
    motions <- readLines(get_day9_input_path())

    knots <- list(c(0, 0), c(0, 0), c(0, 0), c(0, 0), c(0, 0),
        c(0, 0), c(0, 0), c(0, 0), c(0, 0), c(0, 0))
    tail_visited <- c("0,0")

    for (i in seq_len(length(motions))) {
        motion <- unlist(strsplit(motions[i], " "))
        dir <- motion[1]
        steps <- motion[2]

        for (s in seq_len(length(1:steps))) {
            knots[[1]] <- move_head(knots[[1]], dir)

            for (k in 2:length(knots)) {
                knots[[k]] <- move_tail(knots[[k - 1]], knots[[k]])

                if (k == length(knots)) {
                    tail_visited <- append(tail_visited,
                        paste(knots[[k]], collapse = ","))
                }
            }
        }
    }

    print(length(unique(tail_visited)))
}

move_head <- function(head_pos, dir) {
    if (dir == "R") {
        c(head_pos[1], head_pos[2] + 1)
    } else if (dir == "L") {
        c(head_pos[1], head_pos[2] - 1)
    } else if (dir == "U") {
        c(head_pos[1] + 1, head_pos[2])
    } else if (dir == "D") {
        c(head_pos[1] - 1, head_pos[2])
    }
}

move_tail <- function(head_pos, tail_pos) {
    if (is_adjacent(head_pos, tail_pos)) {
        tail_pos
    } else {
        if (head_pos[1] == tail_pos[1]) {
            if (head_pos[2] < tail_pos[2]) {
                c(tail_pos[1], tail_pos[2] - 1)
            } else {
                c(tail_pos[1], tail_pos[2] + 1)
            }
        } else if (head_pos[2] == tail_pos[2]) {
            if (head_pos[1] < tail_pos[1]) {
                c(tail_pos[1] - 1, tail_pos[2])
            } else {
                c(tail_pos[1] + 1, tail_pos[2])
            }
        } else {
            calc_diag_move(head_pos, tail_pos)
        }
    }
}

is_adjacent <- function(head_pos, tail_pos) {
    (head_pos[1] <= tail_pos[1] + 1) &
    (head_pos[1] >= tail_pos[1] - 1) &
    (head_pos[2] <= tail_pos[2] + 1) &
    (head_pos[2] >= tail_pos[2] - 1)
}

calc_diag_move <- function(head_pos, tail_pos) {
    if (head_pos[1] > tail_pos[1] & head_pos[2] > tail_pos[2]) {
        c(tail_pos[1] + 1, tail_pos[2] + 1)
    } else if (head_pos[1] > tail_pos[1] & head_pos[2] < tail_pos[2]) {
        c(tail_pos[1] + 1, tail_pos[2] - 1)
    } else if (head_pos[1] < tail_pos[1] & head_pos[2] > tail_pos[2]) {
        c(tail_pos[1] - 1, tail_pos[2] + 1)
    } else if (head_pos[1] < tail_pos[1] & head_pos[2] < tail_pos[2]) {
        c(tail_pos[1] - 1, tail_pos[2] - 1)
    } else {
        tail_pos
    }
}

part1()
part2()