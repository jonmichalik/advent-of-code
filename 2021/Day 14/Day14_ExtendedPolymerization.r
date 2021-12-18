# Day 14: Extended Polymerization

get_day14_input_path <- function() {
    paste(getwd(), "/2021/Day 14/Day14_input.txt", sep = "")
}

chars <- matrix(ncol = 2)[-1, ]

part1 <- function(num_steps = 10) {
    readings <- readLines(get_day14_input_path())

    polymer <- readings[1]
    rules <- parse_rules(readings[3:length(readings)])

    for (i in 1:num_steps) {
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

part2 <- function(num_steps = 40) {
    readings <- readLines(get_day14_input_path())

    polymer <- readings[1]
    last_char <- rev(unlist(strsplit(polymer, "")))[1]
    rules <- parse_rules(readings[3:length(readings)])

    pairs <- optimize_pairs(polymer)

    for (i in 1:num_steps) {
        pairs <- polymerize_arr(pairs, rules, i == num_steps)
    }

    chars <<- merge_set(chars, last_char, 1)
    char_sums <- sort(as.numeric(chars[, 2]), decreasing = TRUE)

    print(chars)
    print(toString(char_sums[1] - char_sums[length(char_sums)]))
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

optimize_pairs <- function(polymer) {
    pairs <- matrix(ncol = 2)[-1, ]
    polymer_arr <- unlist(strsplit(polymer, ""))

    for (i in seq_len(length(polymer_arr) - 1)) {
        template <- paste(polymer_arr[i], polymer_arr[i + 1], sep = "")
        pairs <- merge_set(pairs, template, 1)
    }
    pairs
}

polymerize_arr <- function(pairs, rules, calc_chars) {
    pairs_to_add <- matrix(ncol = 2)[-1, ]

    for (i in seq_len(nrow(pairs))) {
        element_to_insert <- rules[[pairs[i, 1]]]

        char_arr <- unlist(strsplit(pairs[i, 1], ""))
        pair_arr <- char_arr
        pair_arr[1] <- paste(pair_arr[1], element_to_insert, sep = "")
        pair_arr[2] <- paste(element_to_insert, pair_arr[2], sep = "")

        for (p in seq_len(length(pair_arr))) {
            pairs_to_add <- merge_set(pairs_to_add, pair_arr[p], pairs[i, 2])
        }

        if (calc_chars) {
            chars <<- merge_set(chars, char_arr[1], pairs[i, 2])
            chars <<- merge_set(chars, element_to_insert, pairs[i, 2])
        }
    }
    pairs_to_add
}

merge_set <- function(set, element, num_to_add) {
    match <- which(set[, 1] == element, arr.ind = TRUE)

        if (length(match) == 0) {
            set <- rbind(set, c(element, num_to_add))
        } else {
            set[match, 2] <- as.numeric(set[match, 2]) +
                               as.numeric(num_to_add)
        }
    set
}

part1()
part2()
