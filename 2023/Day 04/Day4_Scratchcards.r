# Day 4: Scratchcards

get_day4_input_path <- function() {
    paste(getwd(), "/2023/Day 04/Day4_input.txt", sep = "")
}

part1 <- function() {
    cards <- readLines(get_day4_input_path())
    scores <- c()

    for (c in seq_len(length(cards))) {
        card <- unlist(strsplit(cards[c], ":"))[2]
        score <- 0

        split <- unlist(strsplit(card, "\\|"))

        winning_nums <- unlist(strsplit(split[1], " "))
        winning_nums <- winning_nums[nzchar(winning_nums)]

        pool <- unlist(strsplit(split[2], " "))
        pool <- pool[nzchar(pool)]

        for (n in seq_len(length(pool))) {
            if (pool[n] %in% winning_nums) {
                if (score == 0) {
                    score <- 1
                } else {
                    score <- score * 2
                }
            }
        }

        scores <- append(scores, score)
    }

    print(sum(scores))
}

part2 <- function() {
    cards <- readLines(get_day4_input_path())

    num_each_card <- rep(c(1), length(cards))

    for (c in seq_len(length(cards))) {
        card <- unlist(strsplit(cards[c], ":"))[2]
        card_winnings <- 0
        num_cur_card <- num_each_card[c]

        split <- unlist(strsplit(card, "\\|"))

        winning_nums <- unlist(strsplit(split[1], " "))
        winning_nums <- winning_nums[nzchar(winning_nums)]

        pool <- unlist(strsplit(split[2], " "))
        pool <- pool[nzchar(pool)]

        for (n in seq_len(length(pool))) {
            if (pool[n] %in% winning_nums) {
                card_winnings <- card_winnings + 1
                num_each_card[c + card_winnings] <-
                    num_each_card[c + card_winnings] + num_cur_card
            }
        }
    }

    print(sum(num_each_card))
}

part1()
part2()