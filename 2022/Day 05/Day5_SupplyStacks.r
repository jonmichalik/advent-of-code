# Day 5: Supply Stacks

get_day5_input_path <- function() {
    paste(getwd(), "/2022/Day 05/Day5_input.txt", sep = "")
}

part1 <- function() {
    input <- readLines(get_day5_input_path())

    instructions_reached <- FALSE
    num_stacks <- get_num_stacks(input)
    top_crates <- c()

    stacks <- vector("list", num_stacks)

    for (i in seq_len(length(input))) {
        line <- input[i]

        if (!instructions_reached) {
            if (is_stack_line(line)) {
                next
            }

            if (is_blank_line(line)) {
                instructions_reached <- TRUE
                for (v in seq_len(length(stacks))) {
                    stacks[[v]] <- rev(stacks[[v]])
                }
                next
            }

            pos <- 1
            stack <- 1
            while (pos < nchar(line)) {
                letter <- substr(line, pos + 1, pos + 1)

                if (!identical(letter, " ")) {
                    stacks[[stack]] <- append(stacks[[stack]], letter)
                }

                pos <- pos + 4
                stack <- stack + 1
            }
        } else {
            dirs <- parse_directions(line)
            num_to_move <- dirs[1]
            source <- dirs[2]
            target <- dirs[3]
            for (d in 1:num_to_move) {
                crate <- stacks[[source]][length(stacks[[source]])]
                stacks[[source]] <- stacks[[source]][-length(stacks[[source]])]
                stacks[[target]] <- append(stacks[[target]], crate)
            }
        }
    }

    for (i in seq_len(length(stacks))) {
        top_crates <- append(top_crates, stacks[[i]][length(stacks[[i]])])
    }

    print(paste(top_crates, collapse = ""))
}

part2 <- function() {
    input <- readLines(get_day5_input_path())

    instructions_reached <- FALSE
    num_stacks <- get_num_stacks(input)
    top_crates <- c()

    stacks <- vector("list", num_stacks)

    for (i in seq_len(length(input))) {
        line <- input[i]

        if (!instructions_reached) {
            if (is_stack_line(line)) {
                next
            }

            if (is_blank_line(line)) {
                instructions_reached <- TRUE
                for (v in seq_len(length(stacks))) {
                    stacks[[v]] <- rev(stacks[[v]])
                }
                next
            }

            pos <- 1
            stack <- 1
            while (pos < nchar(line)) {
                letter <- substr(line, pos + 1, pos + 1)

                if (!identical(letter, " ")) {
                    stacks[[stack]] <- append(stacks[[stack]], letter)
                }

                pos <- pos + 4
                stack <- stack + 1
            }
        } else {
            dirs <- parse_directions(line)
            num_to_move <- dirs[1]
            source <- dirs[2]
            target <- dirs[3]
            crates_to_move <- c()
            for (d in 1:num_to_move) {
                crate <- stacks[[source]][length(stacks[[source]])]
                stacks[[source]] <- stacks[[source]][-length(stacks[[source]])]
                crates_to_move <- append(crates_to_move, crate)
            }
            crates_to_move <- rev(crates_to_move)
            stacks[[target]] <- append(stacks[[target]], crates_to_move)
        }
    }

    for (i in seq_len(length(stacks))) {
        top_crates <- append(top_crates, stacks[[i]][length(stacks[[i]])])
    }

    print(paste(top_crates, collapse = ""))
}

get_num_stacks <- function(input) {
    num_stacks <- 0

    for (i in seq_len(length(input))) {
        line <- gsub(" ", "", input[i])

        if (grepl("1", line)) {
            numbers <- unlist(strsplit(line, ""))
            num_stacks <- strtoi(numbers[length(numbers)])
            break
        }
    }

    num_stacks
}

is_blank_line <- function(line) {
    val <- gsub(" ", "", line)
    identical(val, "")
}

is_stack_line <- function(line) {
    grepl("1", line)
}

parse_directions <- function(line) {
    words <- unlist(strsplit(line, " "))
    c(strtoi(words[2]),
        strtoi(words[4]),
        strtoi(words[6]))
}

part1()
part2()