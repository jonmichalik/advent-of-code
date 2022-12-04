# Day 7: The Treachery of Whales

get_day7_input_path <- function() {
    paste(getwd(), "/2021/Day 07/Day7_input.txt", sep = "")
}

part1 <- function(fuel_type = "standard") {
    positions <- strtoi(unlist(strsplit(readLines(get_day7_input_path()), ",")))

    unique_crab_pos <- sort(unique(positions))
    max_crab_pos <- unique_crab_pos[length(unique_crab_pos)]
    crabs <- matrix(ncol = 2)[-1, ]

    for (i in seq_len(length(unique_crab_pos))) {
        sum_crabs_in_pos <- sum(positions == unique_crab_pos[i])
        crabs <- rbind(crabs, c(unique_crab_pos[i], sum_crabs_in_pos))
    }

    optimal_pos <- 0
    optimal_fuel <- 2147483647

    for (i in 0:max_crab_pos) {
        fuel_used <- 0
        target_pos <- i
        for (c in seq_len(length(unique_crab_pos))) {
            if (fuel_type == "standard") {
                fuel_used <- fuel_used +
                    calc_fuel_std(target_pos, crabs[c, 1], crabs[c, 2])
            } else {
                fuel_used <- fuel_used +
                    calc_fuel_crab(target_pos, crabs[c, 1], crabs[c, 2])
            }
        }

        if (fuel_used < optimal_fuel) {
            optimal_fuel <- fuel_used
            optimal_pos <- target_pos
        }
    }

    cat("Optimal Position: ", optimal_pos, "; Fuel Used: ", optimal_fuel, "\n")
}

part2 <- function() {
    part1("crab")
}

calc_fuel_std <- function(tar_pos, cur_pos, num_crabs) {
    abs(tar_pos - cur_pos) * num_crabs
}

calc_fuel_crab <- function(tar_pos, cur_pos, num_crabs) {
    delta <- abs(tar_pos - cur_pos)
    crab_adj <- 0

    if (delta > 0) {
        crab_adj <- sum(1:delta)
    }

    crab_adj * num_crabs
}

part1()
part2()
