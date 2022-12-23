# Day 21: Monkey Math

get_day21_input_path <- function() {
    paste(getwd(), "/2022/Day 21/Day21_input.txt", sep = "")
}

part1 <- function() {
    input <- readLines(get_day21_input_path())

    monkeys <- parse_monkeys(input)
    root <- monkeys[["root"]]

    root_value <- get_monkey_value(root, monkeys)

    print(paste(root_value, sep = ""))
}

part2 <- function() {
    print("TODO")
}

setClass("Monkey", representation(
    name = "character",
    isOperation = "logical",
    value = "integer",
    monkey1 = "character",
    monkey2 = "character",
    operator = "character")
)

get_monkey_value <- function(monkey, monkeys) {
    value <- 0

    if (monkey@isOperation) {
        monkey_1_val <- get_monkey_value(monkeys[[monkey@monkey1]], monkeys)
        monkey_2_val <- get_monkey_value(monkeys[[monkey@monkey2]], monkeys)
        value <- get_operation_value(monkey_1_val,
            monkey_2_val,
            monkey@operator)
    } else {
        value <- monkey@value
    }

    value
}

get_operation_value <- function(val_1, val_2, operator) {
    if (operator == "+") {
        val_1 + val_2
    } else if (operator == "-") {
        val_1 - val_2
    } else if (operator == "*") {
        val_1 * val_2
    } else {
        val_1 / val_2
    }
}

parse_monkeys <- function(input) {
    monkeys <- list()

    for (i in seq_len(length(input))) {
        monkey <- new("Monkey")
        parts <- unlist(strsplit(input[i], " "))

        monkey@name <- gsub(":", "", parts[1])
        monkey@isOperation <- length(parts) == 4

        if (monkey@isOperation) {
            monkey@monkey1 <- parts[2]
            monkey@operator <- parts[3]
            monkey@monkey2 <- parts[4]
        } else {
            monkey@value <- strtoi(parts[2])
        }

        monkeys[[monkey@name]] <- monkey
    }

    monkeys
}

part1()
part2()