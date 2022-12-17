# Day 13: Distress Signal

get_day13_input_path <- function() {
    paste(getwd(), "/2022/Day 13/Day13_input.txt", sep = "")
}

cur_input_pos <- 1

part1 <- function() {
    input <- readLines(get_day13_input_path())

    left <- NULL
    right <- NULL

    cur_index <- 0
    correct_indexes <- c()
    num_correct_order <- 0

    for (i in seq_len(length(input))) {
        if (nchar(input[i]) == 0) {
            cur_index <- cur_index + 1
            if (compare_inputs(left, right)) {
                correct_indexes <- append(correct_indexes, cur_index)
                num_correct_order <- num_correct_order + 1
            }
            left <- NULL
            right <- NULL
        } else {
            if (is.null(left)) {
                left <- input[i]
            } else {
                right <- input[i]
            }
        }
    }

    cur_index <- cur_index + 1
    if (compare_inputs(left, right)) {
        correct_indexes <- append(correct_indexes, cur_index)
        num_correct_order <- num_correct_order + 1
    }

    print(sum(correct_indexes))
}

part2 <- function() {
    input <- readLines(get_day13_input_path())

    messages <- c("[[2]]", "[[6]]")

    for (i in seq_len(length(input))) {
        if (nchar(input[i]) > 0) {
            messages <- append(messages, input[i])
        }
    }

    sorted <- FALSE

    while (!sorted) {
        touched <- FALSE
        for (i in seq_len(length(messages) - 1)) {
            left <- messages[i]
            right <- messages[i + 1]
            if (!compare_inputs(messages[i], messages[i + 1])) {
                touched <- TRUE
                messages[i] <- right
                messages[i + 1] <- left
            }
        }

        if (!touched) {
            sorted <- TRUE
        }
    }

    div_1 <- which(messages == "[[2]]")
    div_2 <- which(messages == "[[6]]")

    print(div_1 * div_2)
}

compare_inputs <- function(left, right) {
    correct_order <- FALSE
    left_struct <- parse_input(left)
    right_struct <- parse_input(right)

    if (length(left_struct) == 0 & length(right_struct) > 0) {
        correct_order <- TRUE
    } else {
        for (i in seq_len(length(left_struct))) {
            if (length(right_struct) < i) {
                correct_order <- FALSE
                break
            }

            result <- compare_values(left_struct[[i]], right_struct[[i]])
            if (result == "y") {
                correct_order <- TRUE
                break
            } else if (result == "n") {
                correct_order <- FALSE
                break
            } else {
                if (result == "i" & i == length(left_struct) &
                    length(left_struct) < length(right_struct)) {
                    correct_order <- TRUE
                }
            }
        }
    }

    correct_order
}

compare_values <- function(lval, rval) {
    result <- "i" # inconclusive
    ltype <- typeof(lval)
    rtype <- typeof(rval)

    if (ltype == "integer" & ltype == rtype) {
        # compare integer values
        if (lval < rval) {
            result <- "y" # correct
        } else if (rval < lval) {
            result <- "n" # incorrect
        }
    } else if (ltype == "list" & ltype == rtype) {
        # compare each list value
        if (length(lval) == 0 & length(rval) > 0) {
            result <- "y"
        } else {
            for (i in seq_len(length(lval))) {
                if (length(rval) < i) {
                    result <- "n"
                    break
                }

                result <- compare_values(lval[[i]], rval[[i]])
                if (result != "i") {
                    break
                }

                if (result == "i" & i == length(lval) &
                    length(lval) < length(rval)) {
                    result <- "y"
                }
            }
        }
    } else {
        # convert both to lists and compare each list value
        if (ltype == "integer") {
            result <- compare_values(list(lval), rval)
        } else {
            result <- compare_values(lval, list(rval))
        }
    }

    result
}

parse_input <- function(input) {
    # Doing a stupid amount of string manipulation to avoid splitting 10 into 1 and 0 :P
    input <- gsub("[", "[,", input, fixed = TRUE)
    input <- gsub("]", ",],", input, fixed = TRUE)
    input <- gsub(",,", ",", input, fixed = TRUE)
    input <- substr(input, 1, nchar(input) - 1)
    tokenized <- unlist(strsplit(input, ","))
    outer_list <- listify(tokenized, 1)

    outer_list
}

listify <- function(input, start_pos) {
    list <- list()
    cur_input_pos <<- start_pos + 1

    while (cur_input_pos <= length(input)) {
        if (!is.na(strtoi(input[cur_input_pos]))) {
            list <- append(list, strtoi(input[cur_input_pos]))
        } else if (input[cur_input_pos] == "[") {
            list[[length(list) + 1]] <- listify(input, cur_input_pos)
        } else if (input[cur_input_pos] == "]") {
            break
        }

        cur_input_pos <<- cur_input_pos + 1
    }

    list
}

part1()
part2()