# Day 8: Seven Segment Search

get_day8_input_path <- function() {
    paste(getwd(), "/2021/Day 8/Day8_input.txt", sep = "")
}

part1 <- function() {
    readings <- readLines(get_day8_input_path())

    entries <- strsplit(readings, " | ")
    output_indices <- 12:15

    num_1s <- 0
    num_4s <- 0
    num_7s <- 0
    num_8s <- 0

    for (i in seq_len(length(entries))) {
        entry <- unlist(entries[i])

        num_1s <- num_1s + sum(nchar(entry[output_indices]) == 2)
        num_4s <- num_4s + sum(nchar(entry[output_indices]) == 4)
        num_7s <- num_7s + sum(nchar(entry[output_indices]) == 3)
        num_8s <- num_8s + sum(nchar(entry[output_indices]) == 7)
    }

    cat("1:", num_1s, "; 4:", num_4s, "; 7:", num_7s, "; 8:", num_8s, "\n")
    print(sum(num_1s, num_4s, num_7s, num_8s))
}

part2 <- function() {
    print("done")
}

part1()
part2()
