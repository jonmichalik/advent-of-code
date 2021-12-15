# Day 14: Extended Polymerization

get_day14_input_path <- function() {
    paste(getwd(), "/2021/Day 14/Day14_input.txt", sep = "")
}

part1 <- function(num_steps = 10) {
    readings <- readLines(get_day14_input_path())

    polymer <- readings[1]
    rules <- parse_rules(readings[3:length(readings)])

    for (i in 1:num_steps) {
        cat("Step ", i, " of ", num_steps, "\n")
        polymer <- polymerize(polymer, rules)
    }

    polymer_arr <- unlist(strsplit(polymer, ""))
    uniq_elements <- unique(polymer_arr)

    highest_count <- 0
    lowest_count <- 2147483647

    for (e in seq_len(length(uniq_elements))) {
        sum_of_letter <- sum(polymer_arr == uniq_elements[e])

        if (e == 1) {
            highest_count <- sum_of_letter
            lowest_count <- sum_of_letter
        } else {
            if (sum_of_letter > highest_count) {
                highest_count <- sum_of_letter
            }
            if (sum_of_letter < lowest_count) {
                lowest_count <- sum_of_letter
            }
        }
    }

    print(toString(highest_count - lowest_count))
}

part2 <- function() {
    print("TODO")
}

parse_rules <- function(readings) {
    templates <- strsplit(readings, " -> ")
    rules <- list()

    for (i in seq_len(length(templates))) {
        template <- unlist(templates[i])
        rules[[template[1]]] <- append(rules[[template[1]]], template[2])
    }
    rules
}

polymerize <- function(polymer, rules) {
    polymer_arr <- unlist(strsplit(polymer, ""))
    elements_to_insert <- c()

    for (i in seq_len(length(polymer_arr) - 1)) {
        template <- paste(polymer_arr[i], polymer_arr[i + 1], sep = "")
        elements_to_insert <- append(elements_to_insert, rules[[template]])
    }

    new_polymer_arr <- c()

    for (e in seq_len(length(elements_to_insert))) {
        new_polymer_arr <- append(new_polymer_arr, polymer_arr[e])
        new_polymer_arr <- append(new_polymer_arr, elements_to_insert[e])
    }

    new_polymer_arr <- append(new_polymer_arr, polymer_arr[length(polymer_arr)])
    paste(new_polymer_arr, collapse = "")
}

part1()
part2()
