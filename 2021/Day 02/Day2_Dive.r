# Day 2: Dive!

get_day2_input_path <- function() {
    paste(getwd(), "/2021/Day 02/Day2_input.txt", sep = "")
}

part1 <- function() {
    commands <- readLines(get_day2_input_path())
    position <- 0
    depth <- 0

    for (i in seq_len(length(commands))) {
        command <- strsplit(commands[i], " ")[[1]]

        if (command[1] == "forward") {
            position <- position + strtoi(command[2])
        } else if (command[1] == "down") {
            depth <- depth + strtoi(command[2])
        } else if (command[1] == "up") {
            depth <- depth - strtoi(command[2])
        }
    }

    cat("  Position = ", position, "; Depth = ", depth, ";")
    print(position * depth)
}

part2 <- function() {
    commands <- readLines(get_day2_input_path())
    aim <- 0
    position <- 0
    depth <- 0

    for (i in seq_len(length(commands))) {
        command <- strsplit(commands[i], " ")[[1]]

        if (command[1] == "forward") {
            position <- position + strtoi(command[2])
            depth <- depth + (aim * strtoi(command[2]))
        } else if (command[1] == "down") {
            aim <- aim + strtoi(command[2])
        } else if (command[1] == "up") {
            aim <- aim - strtoi(command[2])
        }
    }

    cat("  Position = ", position, "; Depth = ", depth, ";")
    print(position * depth)
}

part1()
part2()
