# Day 11: Monkey in the Middle

get_day11_input_path <- function() {
    paste(getwd(), "/2022/Day 11/Day11_input.txt", sep = "")
}

part1 <- function(num_rounds = 20, relief_val = 3, panic = NA) {
    notes <- readLines(get_day11_input_path())

    monkeys <- parse_monkeys(notes)

    for (r in seq_len(length(1:num_rounds))) {
        for (m in seq_len(length(monkeys))) {
            # Hacky, can't use empty vectors in an S4 class
            # Handles tracking of -1 placeholder
            if (length(monkeys[[m]]@items) > 1) {
                for (i in 2:length(monkeys[[m]]@items)) {
                    monkeys[[m]]@numInspections <- as.integer(
                        monkeys[[m]]@numInspections + 1)

                    cur_worry_level <- monkeys[[m]]@items[i]

                    new_worry_level <- get_worry_level(cur_worry_level,
                        monkeys[[m]]@inspectOp,
                        monkeys[[m]]@inspectVal,
                        relief_val)

                    result <- new_worry_level %% monkeys[[m]]@throwTestVal

                    if (!is.na(panic)) {
                        new_worry_level <- new_worry_level %% panic
                    }

                    if (result == 0) {
                        catch_monkey <- monkeys[[m]]@throwTestPass + 1
                        monkeys[[catch_monkey]]@items <- append(
                            monkeys[[catch_monkey]]@items, new_worry_level)
                    } else {
                        catch_monkey <- monkeys[[m]]@throwTestFail + 1
                        monkeys[[catch_monkey]]@items <- append(
                            monkeys[[catch_monkey]]@items, new_worry_level)
                    }
                }
            }

            monkeys[[m]]@items <- c(-1)
        }
    }

    inspection_counts <- sort(get_inspection_counts(monkeys), decreasing = TRUE)
    monkey_business <- prod(inspection_counts[1:2])

    print(monkey_business)
}

part2 <- function() {
    part1(10000, 1, 9699690)
}

setClass("Monkey", representation(
    items = "vector",
    inspectOp = "character",
    inspectVal = "integer",
    throwTestVal = "integer",
    throwTestPass = "integer",
    throwTestFail = "integer",
    numInspections = "integer")
)

parse_monkeys <- function(notes) {
    monkeys <- list()
    monkey <- new("Monkey")

    for (i in seq_len(length(notes))) {
        line <- unlist(strsplit(trimws(notes[i]), " "))

        if (length(line) == 0) {
            monkeys <- append(monkeys, monkey)
            next
        }
        if (line[1] == "Monkey") {
            monkey <- new("Monkey")
            monkey@numInspections <- as.integer(0)
        } else if (line[1] == "Starting") {
            items <- gsub(",", "", line[3:length(line)])
            monkey@items <- c(-1, strtoi(items))
        } else if (line[1] == "Operation:") {
            monkey@inspectOp <- line[5]
            monkey@inspectVal <- strtoi(line[6])
        } else if (line[1] == "Test:") {
            monkey@throwTestVal <- strtoi(line[4])
        } else if (line[2] == "true:") {
            monkey@throwTestPass <- strtoi(line[6])
        } else if (line[2] == "false:") {
            monkey@throwTestFail <- strtoi(line[6])
        }
    }

    monkeys <- append(monkeys, monkey)
    monkeys
}

get_worry_level <- function(old, op, op_val, relief) {
    new_level <- 0

    if (op == "+") {
        new_level <- (old + op_val) / relief
    } else if (op == "*" & is.na(op_val)) {
        new_level <- (old * old) / relief
    } else if (op == "*") {
        new_level <- (old * op_val) / relief
    }

    floor(new_level)
}

get_inspection_counts <- function(monkeys) {
    counts <- c()

    for (m in seq_len(length(monkeys))) {
        counts <- append(counts, monkeys[[m]]@numInspections)
    }

    counts
}

part1()
part2()