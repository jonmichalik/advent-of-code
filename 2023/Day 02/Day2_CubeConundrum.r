# Day 2: Cube Conundrum

get_day2_input_path <- function() {
    paste(getwd(), "/2023/Day 02/Day2_input.txt", sep = "")
}

part1 <- function() {
    games <- readLines(get_day2_input_path())
    possibles <- c()

    max_blue <- 14
    max_green <- 13
    max_red <- 12

    for (i in seq_len(length(games))) {
        game <- games[i]
        is_impossible <- FALSE
        contents <- unlist(strsplit(game, ":"))[2]

        rounds <- unlist(strsplit(contents, ";"))

        for (r in seq_len(length(rounds))) {
            marbles <- unlist(strsplit(rounds[r], ","))

            for (m in seq_len(length(marbles))) {
                marble <- trimws(marbles[m])

                marble_stats <- unlist(strsplit(marble, " "))
                count <- as.numeric(marble_stats[1])

                if (marble_stats[2] == "blue") {
                    is_impossible <- check_impossibility(count, max_blue)
                } else if (marble_stats[2] == "green") {
                    is_impossible <- check_impossibility(count, max_green)
                } else {
                    is_impossible <- check_impossibility(count, max_red)
                }

                if (is_impossible)
                    break
            }

            if (is_impossible)
                break
        }

        if (!is_impossible)
            possibles <- append(possibles, i)
    }

    print(sum(possibles))
}

part2 <- function() {
    games <- readLines(get_day2_input_path())
    powers <- c()

    for (i in seq_len(length(games))) {
        game <- games[i]

        max_blue <- 0
        max_green <- 0
        max_red <- 0

        contents <- unlist(strsplit(game, ":"))[2]
        rounds <- unlist(strsplit(contents, ";"))

        for (r in seq_len(length(rounds))) {
            marbles <- unlist(strsplit(rounds[r], ","))

            for (m in seq_len(length(marbles))) {
                marble <- trimws(marbles[m])

                marble_stats <- unlist(strsplit(marble, " "))
                count <- as.numeric(marble_stats[1])

                if (marble_stats[2] == "blue") {
                    if (is_new_max(max_blue, count)) {
                        max_blue <- count
                    }
                } else if (marble_stats[2] == "green") {
                    if (is_new_max(max_green, count)) {
                        max_green <- count
                    }
                } else {
                    if (is_new_max(max_red, count)) {
                        max_red <- count
                    }
                }
            }
        }

        power <- max_blue * max_green * max_red
        powers <- append(powers, power)
    }

    print(sum(powers))
}

check_impossibility <- function(actual, expected) {
    actual > expected
}

is_new_max <- function(current, new) {
    new > current
}

part1()
part2()