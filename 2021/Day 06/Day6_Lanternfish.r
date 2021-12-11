# Day 6: Lanternfish

get_day6_input_path <- function() {
    paste(getwd(), "/2021/Day 6/Day6_input.txt", sep = "")
}

part1 <- function(num_days = 80) {
    readings <- readLines(get_day6_input_path())

    days <- num_days
    fishies <- strtoi(unlist(strsplit(readings[1], ",")))

    lanternfish <- optimize_fishies(fishies, c(1:5))

    for (i in 1:days) {
        lanternfish[, 1] <- lanternfish[, 1] - 1

        sum_new_fishies <- sum(lanternfish[which(lanternfish[, 1] == -1), 2])

        lanternfish[, 1][lanternfish[, 1] == -1] <- 6
        if (sum_new_fishies > 0) {
            lanternfish <- rbind(lanternfish, c(8, sum_new_fishies))
        }
    }
    print(sum(lanternfish[, 2]))
    print(paste(sum(lanternfish[, 2])))
}

part2 <- function() {
    part1(256)
}

optimize_fishies <- function(fishies, starting_vals) {
    lanternfish <- matrix(ncol = 2)[-1, ]

    for (i in seq_len(length(starting_vals))) {
        sum_of_i <- sum(fishies == i)
        if (sum_of_i > 0) {
            lanternfish <- rbind(lanternfish, c(i, sum_of_i))
        }
    }

    lanternfish
}

part1()
part2()
