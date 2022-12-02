# Day 1: Calorie Counting

get_day1_input_path <- function() {
    paste(getwd(), "/2022/Day 01/Day1_input.txt", sep = "")
}

part1 <- function() {
    calories <- readLines(get_day1_input_path())

    current_elf_calories <- 0

    beeg_elf_total <- 0

    for (i in seq_len(length(calories))) {
        calorie <- strtoi(calories[i])

        if (is.na(calorie)) {
            if (current_elf_calories > beeg_elf_total) {
                beeg_elf_total <- current_elf_calories
            }

            current_elf_calories <- 0
        } else {
            current_elf_calories <- current_elf_calories + calorie
        }
    }

    print(beeg_elf_total)
}

part2 <- function() {
    calories <- readLines(get_day1_input_path())

    current_elf_calories <- 0
    all_totals <- c()

    for (i in seq_len(length(calories))) {
        calorie <- strtoi(calories[i])

        if (is.na(calorie)) {
            all_totals <- append(all_totals, current_elf_calories)
            current_elf_calories <- 0
        } else {
            current_elf_calories <- current_elf_calories + calorie
        }

        if (i == length(calories)) {
            all_totals <- append(all_totals, current_elf_calories)
        }
    }

    all_totals <- sort(all_totals, decreasing = TRUE)

    print(sum(all_totals[1:3]))
}

part1()
part2()