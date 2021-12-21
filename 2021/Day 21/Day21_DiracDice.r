# Day 21: Dirac Dice

get_day21_input_path <- function() {
    paste(getwd(), "/2021/Day 21/Day21_input.txt", sep = "")
}

part1 <- function() {
    readings <- readLines(get_day21_input_path())

    p1_start <- as.numeric(unlist(strsplit(readings[1], ": "))[2])
    p2_start <- as.numeric(unlist(strsplit(readings[2], ": "))[2])

    p1_cur_space <- p1_start
    p2_cur_space <- p2_start

    num_rolls <- 0
    p1_score <- 0
    p2_score <- 0

    turn_num <- 1
    game_won <- FALSE

    while (!game_won) {
        sum_of_rolls <- 0
        for (i in 1:3) {
            sum_of_rolls <- sum_of_rolls + roll_det_100_die(num_rolls)
            num_rolls <- num_rolls + 1
        }
        if (turn_num %% 2 == 0) {
            p2_cur_space <- board_10_move_pawn(p2_cur_space, sum_of_rolls)
            p2_score <- p2_score + p2_cur_space

            if (p2_score >= 1000) {
                game_won <- TRUE
            }
        } else {
            p1_cur_space <- board_10_move_pawn(p1_cur_space, sum_of_rolls)
            p1_score <- p1_score + p1_cur_space

            if (p1_score >= 1000) {
                game_won <- TRUE
            }
        }
        turn_num <- turn_num + 1
    }

    cat("P1 Score: ", p1_score, "\n")
    cat("P2 Score: ", p2_score, "\n")
    cat("Roll Count: ", num_rolls, "\n")
    print(min(c(p1_score, p2_score)) * num_rolls)
}

part2 <- function() {
    print("TODO")
}

roll_det_100_die <- function(times_rolled) {
    if (times_rolled %% 100 == 0) {
        1
    } else {
        times_rolled + 1
    }
}

board_10_move_pawn <- function(cur_pos, num_spaces) {
    spaces_to_move <- num_spaces %% 10

    if (cur_pos + spaces_to_move <= 10) {
        cur_pos + spaces_to_move
    } else {
        (cur_pos + spaces_to_move) %% 10
    }
}

part1()
part2()
