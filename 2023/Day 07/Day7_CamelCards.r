# Day 7: Camel Cards

get_day7_input_path <- function() {
    paste(getwd(), "/2023/Day 07/Day7_input.txt", sep = "")
}

types <- c("HC", "1P", "2P", "3K", "FH", "4K", "5K")
faces <- c("2", "3", "4", "5", "6", "7", "8", "9", "T", "J", "Q", "K", "A")
joker_faces <- c("J", "2", "3", "4", "5", "6", "7", "8", "9", "T", "Q", "K", "A")

part1 <- function(joker = FALSE) {
    hands <- readLines(get_day7_input_path())

    ranked_hands <- list()

    for (h in seq_len(length(hands))) {
        hand_and_bid <- unlist(strsplit(hands[h], " "))

        ranked_hand <- new("RankedHand")

        ranked_hand@hand <- hand_and_bid[1]
        ranked_hand@bid <- as.numeric(hand_and_bid[2])
        ranked_hand@type <- get_type(ranked_hand@hand, joker)
        ranked_hand@strength <- get_strength(ranked_hand@hand, joker)

        ranked_hands[[ranked_hand@hand]] <- ranked_hand
    }

    total_rankings <- 0

    five_kind <- get_hands_of_type(ranked_hands, "5K")
    four_kind <- get_hands_of_type(ranked_hands, "4K")
    full_house <- get_hands_of_type(ranked_hands, "FH")
    three_kind <- get_hands_of_type(ranked_hands, "3K")
    two_pair <- get_hands_of_type(ranked_hands, "2P")
    one_pair <- get_hands_of_type(ranked_hands, "1P")
    high_card <- get_hands_of_type(ranked_hands, "HC")

    ranked_hands <- calc_ranking(ranked_hands, high_card, total_rankings)
    total_rankings <- total_rankings + length(high_card)

    ranked_hands <- calc_ranking(ranked_hands, one_pair, total_rankings)
    total_rankings <- total_rankings + length(one_pair)

    ranked_hands <- calc_ranking(ranked_hands, two_pair, total_rankings)
    total_rankings <- total_rankings + length(two_pair)

    ranked_hands <- calc_ranking(ranked_hands, three_kind, total_rankings)
    total_rankings <- total_rankings + length(three_kind)

    ranked_hands <- calc_ranking(ranked_hands, full_house, total_rankings)
    total_rankings <- total_rankings + length(full_house)

    ranked_hands <- calc_ranking(ranked_hands, four_kind, total_rankings)
    total_rankings <- total_rankings + length(four_kind)

    ranked_hands <- calc_ranking(ranked_hands, five_kind, total_rankings)
    total_rankings <- total_rankings + length(five_kind)

    winnings <- c()
    for (i in seq_len(length(ranked_hands))) {
        hand <- ranked_hands[[i]]
        winnings <- append(winnings, hand@bid * hand@rank)
    }

    print(sum(winnings))
}

part2 <- function() {
    part1(TRUE)
}

get_type <- function(hand, joker = FALSE) {
    cards <- unlist(strsplit(hand, ""))
    card_counts <- unlist(get_card_counts(cards, joker))

    if (is_x_of_a_kind(card_counts, 5)) {
        "5K"
    } else if (is_x_of_a_kind(card_counts, 4)) {
        "4K"
    } else if (is_full_house(card_counts, joker)) {
        "FH"
    } else if (is_x_of_a_kind(card_counts, 3)) {
        "3K"
    } else if (is_x_pair(card_counts, 2)) {
        "2P"
    } else if (is_x_pair(card_counts, 1)) {
        "1P"
    } else {
        "HC"
    }
}

is_x_of_a_kind <- function(card_counts, num_kind) {
    sort(card_counts, decreasing = TRUE)[1] == num_kind
}

is_x_pair <- function(card_counts, num_pair) {
    length(which(card_counts == 2)) == num_pair
}

is_full_house <- function(card_counts, joker = FALSE) {
    sorted_counts <- sort(card_counts, decreasing = TRUE)
    sorted_counts[1] == 3 && sorted_counts[2] == 2
}

get_card_counts <- function(cards, joker = FALSE) {
    card_counts <- list()
    sorted <- sort(cards)

    cur_val <- sorted[1]
    cur_count <- 0

    for (i in seq_len(length(sorted))) {
        if (sorted[i] == cur_val) {
            cur_count <- cur_count + 1
        } else {
            card_counts[[cur_val]] <- cur_count

            cur_val <- sorted[i]
            cur_count <- 1
        }

        if (i == length(sorted)) {
            card_counts[[cur_val]] <- cur_count
        }
    }

    if (joker) {
        joker_count <- card_counts[["J"]]

        if (length(joker_count) > 0) {
            card_counts[["J"]] <- 0
            joker_sort <- sort(unlist(card_counts), decreasing = TRUE)
            joker_sort[1] <- joker_sort[1] + joker_count
            card_counts <- joker_sort
        }
    }
    card_counts
}

get_strength <- function(hand, joker = FALSE) {
    cards <- rev(unlist(strsplit(hand, "")))
    multipliers <- c(10, 1000, 100000, 10000000, 1000000000)

    strength <- 0

    for (c in seq_len(length(cards))) {
        face_ind <- match(cards[c], faces)

        if (joker)
            face_ind <- match(cards[c], joker_faces)

        power <- face_ind * multipliers[c]
        strength <- strength + power
    }

    strength
}

get_hands_of_type <- function(hands, type) {
    hands_of_type <- c()

    for (i in seq_len(length(hands))) {
        hand <- hands[[i]]

        if (hand@type == type) {
            hands_of_type <- append(hands_of_type, hand)
        }
    }

    hands_of_type
}

calc_ranking <- function(ranked_hands, sub_hands, rank_start) {
    if (length(sub_hands) > 0) {
        strengths <- list()
        for (i in seq_len(length(sub_hands))) {
            strengths[[sub_hands[[i]]@hand]] <- sub_hands[[i]]@strength
        }

        sorted <- strengths[order(unlist(strengths))]

        for (i in seq_len(length(sorted))) {
            #cat("Rank", rank_start + i, "assigned to", names(sorted[i]), "(", ranked_hands[[names(sorted[i])]]@type, ")\n")
            ranked_hands[[names(sorted[i])]]@rank <- rank_start + i
        }
    }

    ranked_hands
}

setClass("RankedHand", representation(
    hand = "character",
    bid = "numeric",
    type = "character",
    strength = "numeric",
    rank = "numeric")
)

part1()
part2()