# Day 10: Cathode-Ray Tube

get_day10_input_path <- function() {
    paste(getwd(), "/2022/Day 10/Day10_input.txt", sep = "")
}

part1 <- function(cycles = 220) {
    instructions <- readLines(get_day10_input_path())

    x <- 1
    cur_instruction <- 1
    adding_cycle <- 0
    strength_intervals <- c(20, 60, 100, 140, 180, 220)
    signal_strengths <- c()

    for (i in seq_len(length(1:cycles))) {
        instruct_parts <- unlist(strsplit(instructions[cur_instruction], " "))

        if (any(strength_intervals == i)) {
            signal_strengths <- append(signal_strengths, i * x)
        }

        if (instruct_parts[1] == "noop") {
            cur_instruction <- cur_instruction + 1
        } else {
            adding_cycle <- adding_cycle + 1

            if (adding_cycle < 2) {
                # do nothing, still calculating
            } else {
                cur_instruction <- cur_instruction + 1
                x <- x + strtoi(instruct_parts[2])
                adding_cycle <- 0
            }
        }
    }

    print(sum(signal_strengths))
}

part2 <- function(cycles = 240) {
    instructions <- readLines(get_day10_input_path())

    sprite_pos <- 1:3
    cur_instruction <- 1
    adding_cycle <- 0
    row_length <- 40

    display <- matrix(".", ncol = row_length, nrow = 6)
    cur_row <- 1

    for (i in seq_len(length(1:cycles))) {
        instruct_parts <- unlist(strsplit(instructions[cur_instruction], " "))

        pixel <- c(cur_row, i - ((cur_row - 1) * row_length))

        if (any(sprite_pos == pixel[2])) {
            display[pixel[1], pixel[2]] <- "#"
        }

        if (instruct_parts[1] == "noop") {
            cur_instruction <- cur_instruction + 1
        } else {
            adding_cycle <- adding_cycle + 1

            if (adding_cycle < 2) {
                # do nothing, still calculating
            } else {
                cur_instruction <- cur_instruction + 1
                new_pos <- sprite_pos[1] + strtoi(instruct_parts[2])
                sprite_pos <- new_pos:(new_pos + 2)
                adding_cycle <- 0
            }
        }

        if (i %% row_length == 0) {
            cur_row <- cur_row + 1
        }
    }

    print(display)
    # PBZGRAZA
}

part1()
part2()