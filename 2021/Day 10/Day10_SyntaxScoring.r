# Day 10: Syntax Scoring

get_day10_input_path <- function() {
    paste(getwd(), "/2021/Day 10/Day10_input.txt", sep = "")
}

part1 <- function() {
    readings <- readLines(get_day10_input_path())

    open_chars <- c("(", "[", "{", "<")
    close_chars <- c(")", "]", "}", ">")
    scores <- c(3, 57, 1197, 25137)
    total_score <- 0

    for (i in seq_len(length(readings))) {
        line <- unlist(strsplit(readings[i], ""))
        expected_closes <- c()

        for (c in seq_len(length(line))) {
            op_match <- match(line[c], open_chars)
            if (!is.na(op_match)) {
                expected_closes <- append(expected_closes,
                                        close_chars[op_match])
            } else {
                if (line[c] == expected_closes[length(expected_closes)]) {
                    expected_closes <- expected_closes[-length(expected_closes)]
                } else {
                    syntax_score <- scores[match(line[c], close_chars)]
                    total_score <- total_score + syntax_score
                    break
                }
            }
        }
    }
    print(total_score)
}

part2 <- function() {
    readings <- readLines(get_day10_input_path())

    open_chars <- c("(", "[", "{", "<")
    close_chars <- c(")", "]", "}", ">")
    scores <- c(1, 2, 3, 4)
    total_scores <- c()

    for (i in seq_len(length(readings))) {
        line <- unlist(strsplit(readings[i], ""))
        is_corrupt <- FALSE
        expected_closes <- c()

        for (c in seq_len(length(line))) {
            op_match <- match(line[c], open_chars)
            if (!is.na(op_match)) {
                expected_closes <- append(expected_closes,
                                        close_chars[op_match])
            } else {
                if (line[c] == expected_closes[length(expected_closes)]) {
                    expected_closes <- expected_closes[-length(expected_closes)]
                } else {
                    is_corrupt <- TRUE
                    break
                }
            }
        }

        if (!is_corrupt) {
            expected_closes <- rev(expected_closes)
            total_score <- 0
            for (e in seq_len(length(expected_closes))) {
                char_score <- scores[match(expected_closes[e], close_chars)]
                total_score <- (total_score * 5) + char_score
            }
            total_scores <- append(total_scores, total_score)
        }
    }
    print(median(sort(total_scores)))
}

part1()
part2()
