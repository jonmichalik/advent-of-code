# Day 6: Wait For It

get_day6_input_path <- function() {
    paste(getwd(), "/2023/Day 06/Day6_input.txt", sep = "")
}

part1 <- function() {
    stats <- readLines(get_day6_input_path())

    times <- unlist(strsplit(unlist(strsplit(stats[1], ":"))[2], " "))
    times <- as.numeric(times[nzchar(times)])

    distances <- unlist(strsplit(unlist(strsplit(stats[2], ":"))[2], " "))
    distances <- as.numeric(distances[nzchar(distances)])

    num_wins <- c()

    for (i in seq_len(length(times))) {
        winning_strats <- 0
        total_time <- times[i]
        dist_to_beat <- distances[i]

        for (t in 0:total_time) {
            speed <- t
            time_rem <- total_time - t

            dist <- speed * time_rem

            if (dist > dist_to_beat) {
                winning_strats <- winning_strats + 1
            }
        }

        num_wins <- append(num_wins, winning_strats)
    }

    print(prod(num_wins))
}

part2 <- function() {
    stats <- readLines(get_day6_input_path())

    times <- unlist(strsplit(unlist(strsplit(stats[1], ":"))[2], " "))
    total_time <- as.numeric(paste(times[nzchar(times)], collapse = ""))[1]

    distances <- unlist(strsplit(unlist(strsplit(stats[2], ":"))[2], " "))
    dist_to_beat <- as.numeric(paste(distances[nzchar(distances)], collapse = ""))[1]

    winning_strats <- 0

    in_win_range <- FALSE

    for (t in 0:total_time) {
        speed <- t
        time_rem <- total_time - t

        dist <- speed * time_rem

        if (dist > dist_to_beat) {
            winning_strats <- winning_strats + 1

            if (!in_win_range) {
                in_win_range <- TRUE
            }
        } else if (dist <= dist_to_beat && in_win_range) {
            break
        }
    }

    print(winning_strats)
}

part1()
part2()