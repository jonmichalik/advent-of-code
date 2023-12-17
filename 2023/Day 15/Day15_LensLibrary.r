# Day 15: Lens Library

get_day15_input_path <- function() {
    paste(getwd(), "/2023/Day 15/Day15_input.txt", sep = "")
}

part1 <- function() {
    input <- readLines(get_day15_input_path())

    steps <- unlist(strsplit(input, ","))
    step_vals <- c()

    for (i in seq_len(length(steps))) {
        chars <- unlist(strsplit(steps[i], ""))
        cur_val <- 0

        for (c in seq_len(length(chars))) {
            val <- cur_val
            ascii <- as.numeric(charToRaw(chars[c]))
            cur_val <- ((val + ascii) * 17) %% 256
        }

        step_vals <- append(step_vals, cur_val)
    }

    print(sum(step_vals))
}

part2 <- function() {
    input <- readLines(get_day15_input_path())

    steps <- unlist(strsplit(input, ","))
    boxes <- create_boxes()

    for (i in seq_len(length(steps))) {
        chars <- unlist(strsplit(steps[i], ""))
        label <- c()
        instruction <- c()

        if ("-" %in% chars) {
            label <- chars[seq_len((length(chars) - 1))]
            instruction <- chars[length(chars)]
        } else {
            label <- chars[seq_len((length(chars) - 2))]
            instruction <- chars[(length(chars) - 1):length(chars)]
        }

        box <- 0
        for (c in seq_len(length(label))) {
            val <- box
            ascii <- as.numeric(charToRaw(label[c]))
            box <- ((val + ascii) * 17) %% 256
        }

        # Can't have 0-based lists, shift the box number
        # Also let's us skip adding one to box number later
        box <- box + 1

        str_label <- paste(label, collapse = "")
        rel_box <- boxes[[box]]
        lens_ind <- 0

        for (x in seq_len(length(rel_box))) {
            l <- rel_box[[x]][1]
            if (l == str_label) {
                lens_ind <- x
                break
            }
        }

        if (length(instruction) == 1) {
            if (lens_ind > 0) {
                rel_box <- rel_box[-lens_ind]
                boxes[[box]] <- rel_box
            }
        } else {
            focal_length <- as.numeric(instruction[2])
            if (lens_ind > 0) {
                rel_box[[lens_ind]] <- c(str_label, focal_length)
            } else {
                rel_box[[length(rel_box) + 1]] <- c(str_label, focal_length)
            }
            boxes[[box]] <- rel_box
        }
    }

    focusing_power <- calc_focusing_power(boxes)
    print(focusing_power)
}

calc_focusing_power <- function(boxes) {
    focusing_power <- 0

    for (b in seq_len(length(boxes))) {
        lenses <- boxes[[b]]
        for (l in seq_len(length(lenses))) {
            focal_length <- as.numeric(lenses[[l]][2])
            power <- b * l * focal_length
            focusing_power <- focusing_power + power
        }
    }

    focusing_power
}

create_boxes <- function() {
    boxes <- list()

    for (i in seq_len(256)) {
        boxes[[i]] <- list()
    }

    boxes
}

part1()
part2()