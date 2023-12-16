# Day 10: Pipe Maze

get_day10_input_path <- function() {
    paste(getwd(), "/2023/Day 10/Day10_input.txt", sep = "")
}

part1 <- function() {
    map <- readLines(get_day10_input_path())

    map_length <- nchar(map[1])
    map_height <- length(map)

    maze <- t(matrix(unlist(strsplit(map[1:map_height], "")),
                        ncol = map_length))

    loop <- list()

    start <- which(maze == "S", arr.ind = TRUE)
    start_pipe <- identify_self(start, map_length, map_height, maze)

    loop[[length(loop) + 1]] <- start_pipe

    closed_loop <- FALSE
    cur_pipe <- start_pipe

    while (!closed_loop) {
        next_row <- cur_pipe@to[1]
        next_col <- cur_pipe@to[2]

        next_from <- c(cur_pipe@row, cur_pipe@col)
        next_type <- maze[next_row, next_col]

        if (next_type == "S") {
            closed_loop <- TRUE
        } else {
            next_to <- get_next_to(cur_pipe@to, next_from,
                next_type, map_length, map_height)

            next_pipe <- create_pipe(next_row, next_col,
                next_type, next_from, next_to)

            loop[[length(loop) + 1]] <- next_pipe
            cur_pipe <- next_pipe
        }
    }

    print(length(loop) / 2)
}

part2 <- function() {
    print("TODO")
}

identify_self <- function(pos, length, height, map) {
    row <- pos[1]
    col <- pos[2]

    up <- get_adj_up(row, col)
    down <- get_adj_down(row, col, height)
    left <- get_adj_left(row, col)
    right <- get_adj_right(row, col, length)

    get_pipe(row, col, up, down, left, right, map)
}

get_adj_up <- function(row, col) {
    if (row > 1) {
        c(row - 1, col)
    }
}

get_adj_down <- function(row, col, height) {
    if (row < height) {
        c(row + 1, col)
    }
}

get_adj_left <- function(row, col) {
    if (col > 1) {
        c(row, col - 1)
    }
}

get_adj_right <- function(row, col, length) {
    if (col < length) {
        c(row, col + 1)
    }
}

get_next_to <- function(pos, prev_pos, type, length, height) {
    row <- pos[1]
    col <- pos[2]
    op1 <- c()
    op2 <- c()

    if (type == "|") {
        op1 <- get_adj_up(row, col)
        op2 <- get_adj_down(row, col, height)
    } else if (type == "-") {
        op1 <- get_adj_left(row, col)
        op2 <- get_adj_right(row, col, length)
    } else if (type == "L") {
        op1 <- get_adj_up(row, col)
        op2 <- get_adj_right(row, col, length)
    } else if (type == "J") {
        op1 <- get_adj_up(row, col)
        op2 <- get_adj_left(row, col)
    } else if (type == "7") {
        op1 <- get_adj_left(row, col)
        op2 <- get_adj_down(row, col, height)
    } else if (type == "F") {
        op1 <- get_adj_down(row, col, height)
        op2 <- get_adj_right(row, col, length)
    }

    choose_next_coords(prev_pos, op1, op2)
}

choose_next_coords <- function(pos, op1, op2) {
    if (pos[1] == op1[1] && pos[2] == op1[2]) {
        op2
    } else {
        op1
    }
}

pipe_goes_up <- function(up, map) {
    connects_down <- c("|", "7", "F")
    !is.null(up) && map[up[1], up[2]] %in% connects_down
}

pipe_goes_down <- function(down, map) {
    connects_up <- c("|", "L", "J")
    !is.null(down) && map[down[1], down[2]] %in% connects_up
}

pipe_goes_left <- function(left, map) {
    connects_right <- c("-", "L", "F")
    !is.null(left) && map[left[1], left[2]] %in% connects_right
}

pipe_goes_right <- function(right, map) {
    connects_left <- c("-", "J", "7")
    !is.null(right) && map[right[1], right[2]] %in% connects_left
}

get_pipe <- function(row, col, up, down, left, right, map) {
    goes_up <- pipe_goes_up(up, map)
    goes_down <- pipe_goes_down(down, map)
    goes_left <- pipe_goes_left(left, map)
    goes_right <- pipe_goes_right(right, map)

    if (goes_up && goes_down) {
        create_pipe(row, col, "|", down, up)
    } else if (goes_left && goes_right) {
        create_pipe(row, col, "-", left, right)
    } else if (goes_up && goes_right) {
        create_pipe(row, col, "L", up, right)
    } else if (goes_up && goes_left) {
        create_pipe(row, col, "J", left, up)
    } else if (goes_down && goes_left) {
        create_pipe(row, col, "7", left, down)
    } else if (goes_down && goes_right) {
        create_pipe(row, col, "F", down, right)
    }
}

create_pipe <- function(row, col, type, from, to) {
    pipe <- new("Pipe")

    pipe@row <- row
    pipe@col <- col
    pipe@type <- type
    pipe@from <- from
    pipe@to <- to

    pipe
}

setClass("Pipe", representation(
    row = "numeric",
    col = "numeric",
    type = "character",
    from = "vector",
    to = "vector")
)

part1()
part2()