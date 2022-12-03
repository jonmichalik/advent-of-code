# Day 3: Rucksack Reorganization

get_day3_input_path <- function() {
    paste(getwd(), "/2022/Day 03/Day3_input.txt", sep = "")
}

part1 <- function() {
    rucksacks <- readLines(get_day3_input_path())

    priority_ref <- c(letters, LETTERS)
    priorities <- c()

    for (i in seq_len(length(rucksacks))) {
        rucksack <- rucksacks[i]
        size <- nchar(rucksack)
        midpoint <- size / 2

        compartment_1 <- unlist(strsplit(substr(
            rucksack, 1, midpoint), ""))
        compartment_2 <- unlist(strsplit(substr(
            rucksack, midpoint + 1, size), ""))

        intersection <- intersect(compartment_1, compartment_2)
        priority <- match(intersection, priority_ref)
        priorities <- append(priorities, priority)
    }

    print(sum(priorities))
}

part2 <- function() {
    rucksacks <- readLines(get_day3_input_path())

    priority_ref <- c(letters, LETTERS)
    priorities <- c()

    cur_group_items <- c()

    for (i in seq_len(length(rucksacks))) {
        rucksack <- rucksacks[i]

        itemized <- strsplit(rucksack, "")
        cur_group_items <- append(cur_group_items, itemized)

        if (i %% 3 == 0) {
            badge <- intersect(
                intersect(cur_group_items[[1]], cur_group_items[[2]]),
                cur_group_items[[3]])

            priority <- match(badge, priority_ref)
            priorities <- append(priorities, priority)

            cur_group_items <- c()
        }
    }

    print(sum(priorities))
}

part1()
part2()