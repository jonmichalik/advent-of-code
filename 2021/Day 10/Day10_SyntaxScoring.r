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
        actual <- c()
        expected_closes <- c()

        for (c in seq_len(length(line))) {
            op_match <- match(line[c], open_chars)
            if (!is.na(op_match)) {
                actual <- append(actual, line[c])
                expected_closes <- append(expected_closes,
                                        close_chars[op_match])
            } else {
                if (line[c] == expected_closes[length(expected_closes)]) {
                    actual <- append(actual, line[c])
                    expected_closes <- expected_closes[-length(expected_closes)]
                } else {
                    cl_match <- match(line[c], close_chars)
                    syntax_score <- scores[cl_match]
                    total_score <- total_score + syntax_score
                    break
                }
            }
        }
    }
    print(total_score)
}

part2 <- function() {
    print("done")
}

part1()
part2()
