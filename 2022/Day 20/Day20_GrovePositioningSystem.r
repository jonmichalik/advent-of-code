# Day 20: Grove Positioning System

get_day20_input_path <- function() {
    paste(getwd(), "/2022/Day 20/Day20_input.txt", sep = "")
}

part1 <- function() {
    input <- readLines(get_day20_input_path())
    ids <- c(letters)

    input <- uniqueify_input(input, ids)
    mixed <- input

    # print(mixed)

    for (i in seq_len(length(input))) {
        value <- input[i]
        number <- strtoi(unlist(strsplit(value, ":"))[1])

        if (number != 0) {
            mixed_ind <- which(mixed == value)
            new_ind <- calc_new_index(mixed_ind, number, length(mixed))

            # cat("Value: ", value, "\n")
            # cat("Current Index: ", mixed_ind, "\n")
            # cat("New Index: ", new_ind, "\n")

            mixed <- mixed[-mixed_ind]

            if (new_ind >= mixed_ind) {
                new_ind <- new_ind - 1
            }

            mixed <- append(mixed, value, after = new_ind)
            # print(mixed)
        }
    }

    zero_ind <- which(mixed == "0:a")
    grove <- get_grove_coords(zero_ind, mixed)

    print(mixed)
    print(grove)
    print(sum(grove))
    # 1644 too low
}

part2 <- function() {
    print("TODO")
}

uniqueify_input <- function(input, ids) {
    uniqueified <- c()

    for (i in seq_len(length(input))) {
        value <- unlist(strsplit(input[i], ":"))[1]

        if (length(which(uniqueified == value)) == 0) {
            value_ind <- which(input == value)

            for (v in seq_len(length(value_ind))) {
                input[value_ind[v]] <- paste(input[value_ind[v]],
                    ":", ids[v], sep = "")
            }
        }
    }

    input
}

calc_new_index <- function(cur_index, num_moves, length) {
    single_loop_moves <- abs(num_moves) %% length

    if (num_moves < 0) {
        if (cur_index - single_loop_moves > 1) {
            cur_index - single_loop_moves
        } else if (cur_index - single_loop_moves == 1) {
            length
        } else {
            remainder <- abs(cur_index - single_loop_moves)
            length - remainder - 1
        }
    } else {
        if (cur_index + single_loop_moves <= length) {
            cur_index + single_loop_moves
        } else {
            abs(length - (cur_index + single_loop_moves))
        }
    }
}

get_grove_coords <- function(zero_ind, mixed) {
    mixed_length <- length(mixed)
    checks <- c(1000, 2000, 3000)
    coords <- c()

    cat("Zero Index: ", zero_ind, "\n")

    for (i in seq_len(length(checks))) {
        index <- calc_new_index(zero_ind, checks[i], mixed_length)
        value <- strtoi(unlist(strsplit(mixed[index], ":"))[1])
        cat("Grove Index: ", index, "; Grove Value: ", value, "\n")
        coords <- append(coords, value)
    }

    coords
}

part1()
part2()