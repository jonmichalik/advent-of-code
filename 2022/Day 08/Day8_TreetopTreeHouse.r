# Day 8: Treetop Tree House

get_day8_input_path <- function() {
    paste(getwd(), "/2022/Day 08/Day8_input.txt", sep = "")
}

part1 <- function() {
    tree_lines <- readLines(get_day8_input_path())

    grid_length <- nchar(tree_lines[1])
    grid_height <- length(tree_lines)

    grid <- t(matrix(strtoi(unlist(strsplit(tree_lines[1:grid_height], ""))),
                        ncol = grid_length))

    visible_trees <- get_visible_trees(grid, grid_length, grid_height)

    print(visible_trees)
}

part2 <- function() {
    tree_lines <- readLines(get_day8_input_path())

    length <- nchar(tree_lines[1])
    height <- length(tree_lines)

    grid <- t(matrix(strtoi(unlist(strsplit(tree_lines[1:height], ""))),
                        ncol = length))

    best_scenic_score <- get_optimal_scenic_score(grid, length, height)

    print(best_scenic_score)
}

get_visible_trees <- function(grid, length, height) {
    visible_trees <- 0

    for (r in seq_len(length(1:height))) {
        if (r == 1 | r == height) {
            visible_trees <- visible_trees + length
            next
        }

        for (c in seq_len(length(1:length))) {
            if (c == 1 | c == length) {
                visible_trees <- visible_trees + 1
                next
            }

            tree <- grid[r, c]
            trees_left <- grid[r, 1:(c - 1)]
            trees_up <- grid[1:(r - 1), c]
            trees_right <- grid[r, (c + 1):length]
            trees_down <- grid[(r + 1):height, c]

            if (is_cardinally_visible(
                    tree, trees_left, trees_up, trees_right, trees_down)) {
                visible_trees <- visible_trees + 1
            }
        }
    }

    visible_trees
}

is_cardinally_visible <- function(tree, left, up, right, down) {
    all(left < tree) | all(up < tree) |
    all(right < tree) | all(down < tree)
}

get_optimal_scenic_score <- function(grid, length, height) {
    best_score <- 0

    for (r in seq_len(length(1:height))) {
        if (r == 1 | r == height) {
            next
        }

        for (c in seq_len(length(1:length))) {
            if (c == 1 | c == length) {
                next
            }

            tree <- grid[r, c]
            trees_left <- grid[r, (c - 1):1]
            trees_up <- grid[(r - 1):1, c]
            trees_right <- grid[r, (c + 1):length]
            trees_down <- grid[(r + 1):height, c]

            left_vd <- get_viewing_distance(tree, trees_left)
            up_vd <- get_viewing_distance(tree, trees_up)
            right_vd <- get_viewing_distance(tree, trees_right)
            down_vd <- get_viewing_distance(tree, trees_down)

            scenic_score <- left_vd * up_vd * right_vd * down_vd

            if (scenic_score > best_score) {
                best_score <- scenic_score
            }
        }
    }

    best_score
}

get_viewing_distance <- function(tree, adj_trees) {
    viewing_distance <- 0

    for (i in seq_len(length(adj_trees))) {
        viewing_distance <- viewing_distance + 1

        if (adj_trees[i] >= tree) {
            break
        }
    }

    viewing_distance
}

part1()
part2()