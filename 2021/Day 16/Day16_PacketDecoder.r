# Day 16: Packet Decoder

get_day16_input_path <- function() {
    paste(getwd(), "/2021/Day 16/Day16_input.txt", sep = "")
}

packets <- matrix(ncol = 5)[-1, ]
packet_depth <- 0

part1 <- function() {
    readings <- readLines(get_day16_input_path())
    chars <- unlist(strsplit(readings[1], ""))

    binary <- R.utils::intToBin(strtoi(chars, base = 16L))
    binary_str <- paste(binary, collapse = "")
    bits <- unlist(strsplit(binary_str, ""))

    main_packet <- create_packet(bits)

    if (main_packet[2] == 4) {
        # Calculate literal value
        main_packet <- set_literal_value(bits, 7, main_packet)
        packets <<- rbind(packets, c(main_packet))
    } else {
        # Process operator
        packets <<- rbind(packets, c(main_packet))
        process_packets(bits, 7)
    }

    print(packets)
    cat("Sum of versions:", sum(packets[, 1]), "\n")
    cat("Sum of values:", sum(packets[, 3]), "\n")
}

part2 <- function() {
    print("TODO")
}

create_packet <- function(bits) {
    version <- bits[1:3]
    version_dec <- strtoi(paste(version, collapse = ""), base = 2)

    type <- bits[4:6]
    type_dec <- strtoi(paste(type, collapse = ""), base = 2)

    # version, type, value, next_index
    packet <- c(version_dec, type_dec, 0, 0, packet_depth)
    packet
}

# Process Packets assumes it's being passed an Operator packet
# Start Index is the index of the length indicator
process_packets <- function(bits, start_index) {
    packet_depth <<- packet_depth + 1
    cat("Entering packet depth:", packet_depth, "\n")
    length_ind <- bits[start_index]
    next_packet_index <- 0

    if (length_ind == "0") {
        print("15-bit Operator")
        # 15-bit length mode, parse length of all subpackets
        subpackets_len <- bits[(start_index + 1):(start_index + 15)]
        sp_len_dec <- strtoi(paste(subpackets_len, collapse = ""), base = 2)
        cat(sp_len_dec, "bits to process\n")

        sp_bound <- start_index + 15 + sp_len_dec

        # Next packet index will pass up to previous caller
        # 15-bit mode makes this easy since it tells us where the packet ends
        next_packet_index <- sp_bound + 1
        sp_bits_start <- start_index + 15 + 1

        sp_bits <- bits[sp_bits_start:sp_bound]
        sp_bits <- sp_bits[!is.na(sp_bits)]

        end_of_bits <- FALSE
        bits_processed <- 0

        # Process packets while we have bits left to consume
        while (!end_of_bits) {
            packet <- create_packet(sp_bits)
            if (packet[2] == 4) {
                # Packet is a literal value
                packet <- set_literal_value(sp_bits, 7, packet)

                # Packet[4] is the index immediately after its last bit
                bits_processed <- bits_processed + packet[4] - 1
                cat("L Proc'd", bits_processed, "of", sp_len_dec, "bits\n")
                if (bits_processed == sp_len_dec) {
                    end_of_bits <- TRUE
                } else {
                    # We have more bits to go, trim the packet we just processed
                    sp_bits <- sp_bits[packet[4]:length(sp_bits)]
                    sp_bits <- sp_bits[!is.na(sp_bits)]
                }

                # Add literal value packet to list
                packets <<- rbind(packets, c(packet))
            } else {
                # This is an operator packet, add it to the list
                packets <<- rbind(packets, c(packet))

                # Recursively inspect this operator packet
                next_subpacket_index <- process_packets(sp_bits, 7)

                # Process packet returns the bounds of this packet + 1
                # (AKA the next packet starting point)
                bits_processed <- bits_processed + next_subpacket_index - 1
                cat("O Proc'd", bits_processed, "of", sp_len_dec, "bits\n")

                if (bits_processed == sp_len_dec) {
                    end_of_bits <- TRUE
                } else {
                    # We have more bits to go, trim the packet we just processed
                    new_sp_bits_start <- sp_bits_start + bits_processed
                    sp_bits <- bits[new_sp_bits_start:sp_bound]
                    sp_bits <- sp_bits[!is.na(sp_bits)]
                }
            }
        }
    } else {
        print("11-bit operator")
        # 11-bit length mode, parse number of subpackets in this packet
        num_subpackets <- bits[(start_index + 1):(start_index + 11)]
        sp_num_dec <- strtoi(paste(num_subpackets, collapse = ""), base = 2)

        cat(sp_num_dec, "subpackets to process\n")

        # Move bits cursor up to first subpacket starting index
        sp_bits <- bits[(start_index + 11 + 1):length(bits)]
        sp_bits <- sp_bits[!is.na(sp_bits)]

        # Store running calculation of the next packet index
        # Harder than 15-bit mode since the length of this packet starts unknown
        next_packet_index <- start_index + 11 + 1
        for (i in 1:sp_num_dec) {
            packet <- create_packet(sp_bits)
            if (packet[2] == 4) {
                # Packet is a literal value
                packet <- set_literal_value(sp_bits, 7, packet)

                # Packet[4] is the index immediately after its last bit
                next_packet_index <- next_packet_index + packet[4] - 1
                cat("L Proc'd", next_packet_index - 1, "of ??? bits\n")

                # Trim the packet we just processed
                sp_bits <- sp_bits[packet[4]:length(sp_bits)]
                sp_bits <- sp_bits[!is.na(sp_bits)]

                # Add literal value packet to list
                packets <<- rbind(packets, c(packet))
            } else {
                # This is an operator packet, add it to the list
                packets <<- rbind(packets, c(packet))

                # Recursively inspect this operator packet
                next_packet_index <- next_packet_index +
                                     process_packets(sp_bits, 7) - 1
                cat("O Proc'd", next_packet_index - 1, "of ??? bits\n")

                # Trim the packet we just processed
                sp_bits <- bits[next_packet_index:length(bits)]
                sp_bits <- sp_bits[!is.na(sp_bits)]
            }
        }
    }
    cat("Exiting packet depth:", packet_depth, "\n")
    packet_depth <<- packet_depth - 1
    next_packet_index
}

set_literal_value <- function(bits, start_index, packet) {
    value <- ""
    last_num <- FALSE
    next_index <- start_index
    while (!last_num) {
        num <- bits[next_index:(next_index + 4)]
        if (num[1] == "0") {
            last_num <- TRUE
        }
        value <- paste(value, paste(num[2:5], collapse = ""), sep = "")
        next_index <- next_index + 5
    }
    packet[3] <- strtoi(value, base = 2)
    packet[4] <- next_index
    packet
}

part1()
part2()
