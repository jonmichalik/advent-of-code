# Day 2: Dive!

part1 <- function() {
    commands <- readLines("Day2_input.txt")
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

part1()
