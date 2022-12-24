# Day 21: Monkey Math

get_day21_input_path <- function() {
    paste(getwd(), "/2022/Day 21/Day21_input.txt", sep = "")
}

monkey_1_name <- ""

part1 <- function() {
    input <- readLines(get_day21_input_path())

    monkeys <- parse_monkeys(input)
    root <- monkeys[["root"]]

    root_value <- get_monkey_value(root, monkeys)

    print(paste(root_value, sep = ""))
}

part2 <- function() {
    input <- readLines(get_day21_input_path())

    monkeys <- parse_monkeys(input)
    root <- monkeys[["root"]]

    monkey_1_tree <- data.tree::Node$new(root@monkey1)
    monkey_1_name <<- root@monkey1

    # Inside baseball, monkey 1 contains the humn number
    add_monkey_node(monkeys[[root@monkey1]], monkeys, monkey_1_tree)
    monkey_2_value <- get_monkey_value(monkeys[[root@monkey2]], monkeys)

    path_to_humn <- c(level = data.tree::FindNode(
        monkey_1_tree, "humn")$Get(
            "name", traversal = "ancestor"))

    operations <- get_operations(path_to_humn, monkeys)
    humn_value <- monkey_2_value

    for (i in seq_len(length(operations))) {
        op <- operations[[i]]
        humn_value <- get_inverse_operation_value(humn_value, op)
    }

    print(paste(humn_value, sep = ""))
}

setClass("Monkey", representation(
    name = "character",
    isOperation = "logical",
    value = "numeric",
    monkey1 = "character",
    monkey2 = "character",
    operator = "character")
)

setClass("Operation", representation(
    operator = "character",
    value = "numeric",
    isLeft = "logical"
))

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

add_monkey_node <- function(monkey, monkeys, parent) {
    if (monkey@name != monkey_1_name) {
        node <- parent$AddChild(monkey@name)
    } else {
        node <- parent
    }

    if (monkey@isOperation) {
        add_monkey_node(monkeys[[monkey@monkey1]], monkeys, node)
        add_monkey_node(monkeys[[monkey@monkey2]], monkeys, node)
    }
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

get_inverse_operation_value <- function(humn_val, operation) {
    value <- 0

    if (operation@operator == "+") {
        value <- humn_val - operation@value
    } else if (operation@operator == "*") {
        value <- humn_val / operation@value
    } else if (operation@operator == "-") {
        if (operation@isLeft) {
            value <- -1 * (humn_val - operation@value)
        } else {
            value <- humn_val + operation@value
        }
    } else {
        if (operation@isLeft) {
            value <- operation@value / humn_val
        } else {
            value <- humn_val * operation@value
        }
    }

    value
}

get_operations <- function(path_to_humn, monkeys) {
    operations <- list()

    for (i in seq_len(length(path_to_humn))) {
        if (path_to_humn[i] == "humn") {
            # do nothing
        } else {
            monkey <- monkeys[[path_to_humn[i]]]
            op <- new("Operation")
            op@operator <- monkey@operator

            if (length(which(path_to_humn == monkey@monkey1)) > 0) {
                op@value <- get_monkey_value(monkeys[[monkey@monkey2]], monkeys)
                op@isLeft <- FALSE
            } else {
                op@value <- get_monkey_value(monkeys[[monkey@monkey1]], monkeys)
                op@isLeft <- TRUE
            }

            operations <- append(operations, op)
        }
    }

    rev(operations)
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