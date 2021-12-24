# Day 22: Reactor Reboot

get_day22_input_path <- function() {
    paste(getwd(), "/2021/Day 22/Day22_input.txt", sep = "")
}

part1 <- function() {
    readings <- readLines(get_day22_input_path())[1:20]

    reactor <- array("off", dim = c(100, 100, 100))

    for (i in seq_len(length(readings))) {
        direction <- unlist(strsplit(readings[i], " "))
        state <- direction[1]

        axes <- unlist(strsplit(direction[2], ","))
        x_range <- parse_points(axes[1])
        y_range <- parse_points(axes[2])
        z_range <- parse_points(axes[3])

        for (x in x_range) {
            x_index <- x + 50
            for (y in y_range) {
                y_index <- y + 50
                for (z in z_range) {
                    z_index <- z + 50
                    reactor[x_index, y_index, z_index] <- state
                }
            }
        }
    }

    print(sum(reactor == "on"))
}

part2 <- function() {
    print("TODO")
}

parse_points <- function(axis) {
    range <- unlist(strsplit(axis, "="))
    points <- as.numeric(unlist(strsplit(range[2], "\\..")))
    points[1]:points[2]
}

part1()
part2()
