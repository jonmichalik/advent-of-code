# Day 19: Aplenty

get_day19_input_path <- function() {
    paste(getwd(), "/2023/Day 19/Day19_input.txt", sep = "")
}

part1 <- function() {
    input <- readLines(get_day19_input_path())

    processing_workflows <- TRUE

    workflows <- list()
    items <- c()

    for (i in seq_len(length(input))) {
        line <- input[i]

        if (processing_workflows) {
            if (nchar(line) > 0) {
                parts <- unlist(strsplit(line, "\\{"))
                name <- parts[1]
                conditions <- unlist(strsplit(gsub("\\}", "", parts[2]), ","))
                instructions <- parse_instructions(conditions)

                workflows[[name]] <- instructions
            } else {
                processing_workflows <- FALSE
            }
        } else {
            items <- append(items, parse_item(line))
        }
    }

    accepted <- get_accepted_items(workflows, items)

    print(sum_items(accepted))
}

part2 <- function() {
    print("TODO")
}

get_accepted_items <- function(workflows, items) {
    accepted <- c()

    for (i in seq_len(length(items))) {
        processing <- TRUE
        cur_flow <- "in"
        item <- items[[i]]

        while (processing) {
            if (cur_flow == "A") {
                accepted <- append(accepted, item)
                processing <- FALSE
            } else if (cur_flow == "R") {
                processing <- FALSE
            } else {
                workflow <- workflows[[cur_flow]]
                cur_flow <- get_next_workflow(workflow, item)
            }
        }
    }

    accepted
}

get_next_workflow <- function(workflow, item) {
    next_flow <- c()

    for (w in seq_len(length(workflow))) {
        instruction <- workflow[[w]]
        operator <- instruction@operator

        if (length(operator) > 0) {
            value <- slot(item, instruction@category)
            condition <- instruction@condition
            if (operator == ">") {
                if (value > condition) {
                    next_flow <- instruction@destination
                    break
                }
            } else {
                if (value < condition) {
                    next_flow <- instruction@destination
                    break
                }
            }
        } else {
            next_flow <- instruction@destination
            break
        }
    }

    next_flow
}

sum_items <- function(items) {
    total <- 0

    for (i in seq_len(length(items))) {
        item <- items[[i]]
        subtotal <- item@x + item@m + item@a + item@s
        total <- total + subtotal
    }

    total
}

parse_instructions <- function(conditions) {
    instructions <- c()
    for (i in seq_len(length(conditions))) {
        c <- conditions[i]
        if (grepl(":", c, fixed = TRUE)) {
            parts <- unlist(strsplit(c, ":"))
            destination <- parts[2]

            chars <- unlist(strsplit(parts[1], ""))
            category <- chars[1]
            operator <- chars[2]
            condition <- as.numeric(
                paste(chars[3:length(chars)], collapse = ""))

            instructions <- append(instructions,
                create_instruction(category, operator, condition, destination))
        } else {
            instruction <- new("Instruction")
            instruction@destination <- c
            instructions <- append(instructions, instruction)
        }
    }

    instructions
}

parse_item <- function(item_string) {
    item <- new("Item")
    item_string <- gsub("\\{", "", item_string)
    item_string <- gsub("\\}", "", item_string)

    parts <- unlist(strsplit(item_string, ","))

    for (p in seq_len(length(parts))) {
        split <- unlist(strsplit(parts[p], "="))
        value <- as.numeric(split[2])

        if (split[1] == "x") {
            item@x <- value
        } else if (split[1] == "m") {
            item@m <- value
        } else if (split[1] == "a") {
            item@a <- value
        } else if (split[1] == "s") {
            item@s <- value
        }
    }

    item
}

create_instruction <- function(category, operator, condition, destination) {
    instruction <- new("Instruction")
    instruction@category <- category
    instruction@operator <- operator
    instruction@condition <- condition
    instruction@destination <- destination

    instruction
}

setClass("Instruction", representation(
    category = "character",
    operator = "character",
    condition = "numeric",
    destination = "character")
)

setClass("Item", representation(
    x = "numeric",
    m = "numeric",
    a = "numeric",
    s = "numeric")
)

part1()
part2()