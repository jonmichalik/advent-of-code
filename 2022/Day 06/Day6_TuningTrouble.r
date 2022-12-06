# Day 6: Tuning Trouble

get_day6_input_path <- function() {
    paste(getwd(), "/2022/Day 06/Day6_input.txt", sep = "")
}

part1 <- function(marker_size = 4) {
    message <- readLines(get_day6_input_path())
    itemized <- unlist(strsplit(message, ""))

    marker_end_pos <- 0

    for (i in seq_len(length(itemized))) {
        fragment <- itemized[i:(i + marker_size - 1)]

        if (length(unique(fragment)) == marker_size) {
            marker_end_pos <- i + marker_size - 1
            break
        }
    }

    print(marker_end_pos)
}

part2 <- function() {
    part1(14)
}

part1()
part2()