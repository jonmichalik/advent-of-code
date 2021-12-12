# Day 12: Passage Pathing

get_day12_input_path <- function() {
    paste(getwd(), "/2021/Day 12/Day12_input.txt", sep = "")
}

part_1_path_count <- 0

part1 <- function() {
    readings <- readLines(get_day12_input_path())

    cave <- parse_cave(readings)
    for (i in seq_len(length(cave[["start"]]))) {
        traverse(cave, c("start"), cave[["start"]][i], c("start"))
    }
    print(part_1_path_count)
}

part2 <- function() {
    print("done")
}

parse_cave <- function(readings) {
    rooms <- list()
    for (i in seq_len(length(readings))) {
        connections <- unlist(strsplit(readings[i], "-"))
        rooms[[connections[1]]] <- append(rooms[[connections[1]]],
                                        connections[2])
        rooms[[connections[2]]] <- append(rooms[[connections[2]]],
                                        connections[1])
    }
    rooms
}

traverse <- function(cave, path, cur_room, visited) {
    path <- append(path, cur_room)
    if (cur_room == tolower(cur_room)) {
        visited <- append(visited, cur_room)
    }
    if (cur_room != "end") {
        next_rooms <- cave[[cur_room]]
        for (i in seq_len(length(next_rooms))) {
            next_room <- next_rooms[i]
            if (next_room %in% visited) {
                next
            }
            traverse(cave, path, next_room, visited)
        }
    } else {
        # print(path)
        part_1_path_count <<- part_1_path_count + 1
    }
}

part1()
part2()
