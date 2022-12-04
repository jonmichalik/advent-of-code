# Day 4: Camp Cleanup

get_day4_input_path <- function() {
    paste(getwd(), "/2022/Day 04/Day4_input.txt", sep = "")
}

part1 <- function() {
    pairs <- readLines(get_day4_input_path())

    num_subsets <- 0

    for (i in seq_len(length(pairs))) {
        pair <- pairs[i]
        assignments <- unlist(strsplit(pair, ","))

        elf_1 <- unlist(strsplit(assignments[1], "-"))
        elf_2 <- unlist(strsplit(assignments[2], "-"))

        range_1 <- c(strtoi(elf_1[1]):strtoi(elf_1[2]))
        range_2 <- c(strtoi(elf_2[1]):strtoi(elf_2[2]))

        if (all(range_1 %in% range_2) | all(range_2 %in% range_1)) {
            num_subsets <- num_subsets + 1
        }
    }

    print(num_subsets)
}

part2 <- function() {
    pairs <- readLines(get_day4_input_path())

    num_overlaps <- 0

    for (i in seq_len(length(pairs))) {
        pair <- pairs[i]
        assignments <- unlist(strsplit(pair, ","))

        elf_1 <- unlist(strsplit(assignments[1], "-"))
        elf_2 <- unlist(strsplit(assignments[2], "-"))

        range_1 <- c(strtoi(elf_1[1]):strtoi(elf_1[2]))
        range_2 <- c(strtoi(elf_2[1]):strtoi(elf_2[2]))

        overlap <- intersect(range_1, range_2)

        if (length(overlap) > 0) {
            num_overlaps <- num_overlaps + 1
        }
    }

    print(num_overlaps)
}

part1()
part2()