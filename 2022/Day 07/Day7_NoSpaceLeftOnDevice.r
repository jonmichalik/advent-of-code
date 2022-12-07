# Day 7: No Space Left On Device

get_day7_input_path <- function() {
    paste(getwd(), "/2022/Day 07/Day7_input.txt", sep = "")
}

dirs <- c()
child_dirs <- list()

# size of files in directory (excludes children)
sizes <- c()

# size of directory including children
directory_sizes <- c()

part1 <- function() {
    console <- readLines(get_day7_input_path())

    dir_stack <- c()
    cur_dir <- ""

    for (i in seq_len(length(console))) {
        line <- console[i]

        if (is_command(line)) {
            command <- get_command_parts(line)

            if (command[1] == "cd") {
                if (command[2] == "..") {
                    dir_stack <- dir_stack[-length(dir_stack)]
                    cur_dir <- paste(dir_stack, collapse = ".")
                } else {
                    dir_stack <- append(dir_stack, command[2])
                    dir_name <- paste(dir_stack, collapse = ".")

                    if (is.na(match(dir_name, dirs))) {
                        dirs <<- append(dirs, dir_name)
                        cur_dir <- dir_name
                        child_dirs[[dir_name]] <<- c()
                        sizes <<- append(sizes, 0)
                    }
                }
            }
        } else {
            output <- get_output_parts(line)
            cur_dir_index <- which(dirs == cur_dir)

            if (output[1] == "dir") {
                new_child <- paste(cur_dir, output[2], sep = ".")
                child_dirs[[cur_dir]] <<- append(
                    child_dirs[[cur_dir]], new_child)
            } else {
                size <- strtoi(output[1])
                sizes[cur_dir_index] <<- sizes[cur_dir_index] + size
            }
        }
    }

    total_size <- sum_dirs_with_max(dirs, 100000)

    print(total_size)
}

part2 <- function() {
    total_space <- 70000000
    req_space <- 30000000
    used_space <- sum_dirs_with_max(c("/"))
    unused_space <- total_space - used_space
    needed_space <- req_space - unused_space

    sorted <- sort(directory_sizes)
    min_req_sizes <- sorted[which(sort(sorted) >= needed_space)]

    print(min_req_sizes[1])
}

is_command <- function(line) {
    parts <- unlist(strsplit(line, " "))
    parts[1] == "$"
}

get_command_parts <- function(line) {
    parts <- unlist(strsplit(line, " "))
    c(parts[2], parts[3])
}

get_output_parts <- function(line) {
    parts <- unlist(strsplit(line, " "))
    c(parts[1], parts[2])
}

sum_dirs_with_max <- function(dirs_to_check, max = NA) {
    dir_sizes <- c()

    for (i in seq_len(length(dirs_to_check))) {
        dir_index <- match(dirs_to_check[i], dirs)
        file_size_sum <- sizes[dir_index]

        dir_size_sum <- 0

        if (exists(dirs_to_check[i], where = child_dirs)) {
            children <- child_dirs[[dirs_to_check[i]]]
            children_sum <- sum_dirs_with_max(children)
            dir_size_sum <- dir_size_sum + children_sum
        }

        dir_sizes <- append(dir_sizes, file_size_sum + dir_size_sum)
    }

    if (is.na(max)) {
        sum(dir_sizes)
    } else {
        directory_sizes <<- dir_sizes
        sum(dir_sizes[which(dir_sizes <= max)])
    }
}

part1()
part2()