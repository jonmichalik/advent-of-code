# Day 4: Giant Squid

part1 <- function() {
    readings <- readLines("Day4_input.txt")

    call_numbers <- unlist(strsplit(readings[1], ","))
    row_size <- 5
    boards <- parse_boards(readings[3:length(readings)], row_size)

    for (i in seq_len(length(call_numbers))) {
        boards <- update_boards(boards, row_size, call_numbers[i])

        if (length(boards) == 1) {
            calc_winning_board(boards[[1]], call_numbers[i])
            break
        }
    }
}

part2 <- function() {
    print("TODO")
}

parse_boards <- function(input, row_size) {
    boards <- list()

    i <- 1
    board_num <- 1
    while (i < length(input)) {
        first_row <- i
        last_row <- i + row_size - 1
        board <- matrix(unlist(strsplit(R.oo::trim(input[first_row:last_row]),
                                        "\\s+")),
                        ncol = 5)
        boards[[board_num]] <- board

        # Skip rows between boards
        i <- i + row_size + 1
        board_num <- board_num + 1
    }

    boards
}

update_boards <- function(boards, row_size, call_num) {
    for (i in seq_len(length(boards))) {
        match <- which(boards[[i]] == call_num, arr.ind = TRUE)
        if (length(match) > 0) {
            boards[[i]][match[1, 1], match[1, 2]] <- "0"
            if (is_winning_board(boards[[i]], row_size, match)) {
                boards <- list(boards[[i]])
                break
            }
        }
    }
    boards
}

is_winning_board <- function(board, row_size, last_match) {
    sum(board[last_match[1, 1], ] == "0") == row_size ||
    sum(board[, last_match[1, 2]] == "0") == row_size
}

calc_winning_board <- function(board, call_num) {
    print(board)
    print(sum(strtoi(board)) * strtoi(call_num))
}

part1()
