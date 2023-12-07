# Day 5: If You Give A Seed A Fertilizer

get_day5_input_path <- function() {
    paste(getwd(), "/2023/Day 05/Day5_input.txt", sep = "")
}

part1 <- function() {
    almanac <- readLines(get_day5_input_path())
    order <- c("seed", "soil", "fertilizer", "water",
        "light", "temperature", "humidity")

    lowest_loc <- 2147483647

    seeds <- trimws(unlist(strsplit(almanac[1], ":")), "l")[2]
    seeds <- unlist(strsplit(seeds, " "))

    maps <- c()

    for (i in seq_len(length(almanac))) {
        if (grepl("-", almanac[i], fixed = TRUE)) {
            maps <- append(maps, parse_maps(almanac, i))
        }
    }

    for (s in seq_len(length(seeds))) {
        cur_resource <- as.numeric(seeds[s])

        for (o in seq_len(length(order))) {
            for (m in seq_len(length(maps))) {
                map <- maps[[m]]
                if (map@source == order[o]) {
                    if (cur_resource >= map@src_start &&
                        cur_resource <= map@src_end) {

                        src_ind <- cur_resource - map@src_start
                        cur_resource <- map@dest_start + src_ind
                        break
                    }
                }
            }
        }

        if (cur_resource < lowest_loc) {
            lowest_loc <- cur_resource
        }
    }

    print(lowest_loc)
}

part2 <- function() {
    print("TODO")
}

parse_maps <- function(almanac, line) {
    maps <- c()
    is_break <- FALSE

    dir <- almanac[line]
    res <- unlist(strsplit(dir, " "))[1]
    res <- unlist(strsplit(res, "-"))
    line <- line + 1

    source <- res[1]
    destination <- res[3]

    while (!is_break) {
        map <- new("Map")
        dir <- almanac[line]
        if (dir == "") {
            is_break <- TRUE
        } else {
            metrics <- unlist(strsplit(dir, " "))

            map@src_start <- as.numeric(metrics[2])
            map@dest_start <- as.numeric(metrics[1])
            length <- strtoi(metrics[3])

            map@src_end <- map@src_start + length - 1
            map@dest_end <- map@dest_start + length - 1
            map@source <- source
            map@destination <- destination

            maps <- append(maps, map)
        }

        line <- line + 1
    }

    maps
}

setClass("Map", representation(
    source = "character",
    destination = "character",
    src_start = "numeric",
    src_end = "numeric",
    dest_start = "numeric",
    dest_end = "numeric")
)

part1()
part2()