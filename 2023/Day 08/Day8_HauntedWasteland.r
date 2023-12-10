# Day 8: Haunted Wasteland

get_day8_input_path <- function() {
    paste(getwd(), "/2023/Day 08/Day8_input.txt", sep = "")
}

dir_ind <- c("L", "R")

part1 <- function() {
    lines <- readLines(get_day8_input_path())

    directions <- unlist(strsplit(lines[1], ""))
    rooms <- list()

    for (i in 3:length(lines)) {
        room_def <- lines[i]
        room_split <- unlist(strsplit(room_def, "="))

        cur_room <- trimws(room_split[1])

        adj_rooms <- unlist(strsplit(trimws(room_split[2]), ","))
        left_room <- gsub("\\(", "", adj_rooms[1])
        right_room <- gsub("\\)", "", trimws(adj_rooms[2]))

        rooms[[cur_room]] <- c(left_room, right_room)
    }

    cur_room <- rooms[["AAA"]]
    num_steps <- 0
    traversing <- TRUE

    while (traversing) {
        for (d in seq_len(length(directions))) {
            new_room <- cur_room[match(directions[d], dir_ind)]
            num_steps <- num_steps + 1

            cur_room <- rooms[[new_room]]

            if (new_room == "ZZZ") {
                traversing <- FALSE
                break
            }
        }
    }

    print(num_steps)
}

part2 <- function() {
    print("TODO")
}

part1()
part2()