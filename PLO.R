library(dplyr)
# library(data.table)
library(purrr)


deck <- c("Ad",	"2d",	"3d",	"4d",	"5d",	"6d",	"7d",	"8d",	"9d",	"Td",	"Jd",	"Qd",	"Kd",	"Ac",	"2c",	"3c",	"4c",	"5c",	"6c",	"7c",	"8c",	"9c",	"Tc",	"Jc",	"Qc",	"Kc",	"Ah",	"2h",	"3h",	"4h",	"5h",	"6h",	"7h",	"8h",	"9h",	"Th",	"Jh",	"Qh",	"Kh",	"As",	"2s",	"3s",	"4s",	"5s",	"6s",	"7s",	"8s",	"9s",	"Ts",	"Js",	"Qs",	"Ks")

possible_4HoleCards <- deck %>% combn(4) %>% t() %>% data.frame()

hand_validation <- function(hand){
  uniqueness <- unique(hand) %>% length() == 5
  card_mapping <- c("A" = 1, "T" = 10, "J" = 11, "Q" = 12, "K" = 13,
                    "2" = 2, "3" = 3, "4" = 4, "5" = 5, 
                    "6" = 6, "7" = 7, "8" = 8, "9" = 9)
  rank_check <- all(substr(hand,1,1) %in% names(card_mapping))
  suit_check <- all(substr(hand,2,2) %in% c("d","c","h","s"))
  return(uniqueness & rank_check & suit_check)
}

card_value_low <- function(card) {
  card_mapping <- c("A" = 1, "T" = 10, "J" = 11, "Q" = 12, "K" = 13,
                    "2" = 2, "3" = 3, "4" = 4, "5" = 5, 
                    "6" = 6, "7" = 7, "8" = 8, "9" = 9)
  if (card %in% names(card_mapping)) {
    return(card_mapping[card])
  } else {
    return(NA)  # Return NA for unmapped cards
  }
}

card_value_high <- function(card) {
  card_mapping <- c("A" = 14, "T" = 10, "J" = 11, "Q" = 12, "K" = 13,
                    "2" = 2, "3" = 3, "4" = 4, "5" = 5, 
                    "6" = 6, "7" = 7, "8" = 8, "9" = 9)
  if (card %in% names(card_mapping)) {
    return(card_mapping[card])
  } else {
    return(NA)  # Return NA for unmapped cards
  }
}

# two_card_ngappers <- function(two_card){
#   card1 <- two_card[1] 
#   card1_low <- card_value_low(card1)
#   card1_high <- card_value_high(card1)
#   card2 <- two_card[2]
#   card2_low <- card_value_low(card2)
#   card2_high <- card_value_high(card2)
#   x <- min(abs(card1_low-card2_low)-1,
#            abs(card1_low-card2_high)-1,
#            abs(card1_high-card2_low)-1,
#            abs(card1_high-card2_high)-1)
#   if(x==-1|x>3){return(10)}else(return(x))
# }
# 
# # Connectedness of two cards to strength possibility of straights
# # ngappers: n = 0,1,2,3 or 10 only
# two_card_connectedness <- data.table(expand.grid(c("A",2,3,4,5,6,7,8,9,"T","J","Q","K"),c("A",2,3,4,5,6,7,8,9,"T","J","Q","K")))
# two_card_connectedness$connectedness <- apply(two_card_connectedness, 1, two_card_ngappers)
# colnames(two_card_connectedness) <- c("Card1", "Card2", "Connectedness_Score")
# 
# plo_connectedness <- function(hand) {
#   
#   # Extract ranks from the hand
#   ranks <- sapply(hand, function(card) substr(card, 1, nchar(card) - 1))
#   
#   # 4C2, sum up the 6 connectedness scores
#   six_combn <- combn(ranks, 2) %>% t() %>% data.frame()
#   
#   # Calculate connectedness score (smaller differences are better)
#   six_combn$connectedness <- apply(six_combn, 1, two_card_ngappers)
#   
#   return(six_combn)
# }
# 
# plo_connectedness(c("Ah","Ts","Tc","4h"))
# plo_connectedness(c("Ah","2s","3c","4h"))
# 
# possible_4HoleCards %>% head(3)

hand_rank <- data.frame("Hand" = c("Straight_Flush","Quads","Full_House","Flush","Straight","Trips","Two_Pairs","Pairs","High_Card"))

is_straight_flush <- function(hand) {
  if(hand_validation(hand) == FALSE){
    return(stop(paste("Hand",hand,"is not valid.")))
  }
  
  # Extract ranks and suits
  ranks <- substr(hand,1,1)
  suits <- substr(hand,2,2)
  
  # Check if all suits are the same
  same_suit <- length(unique(suits)) == 1
  
  strength <- NA
  
  # Check if ranks are consecutive
  consecutive_high <- all(diff(sort(sapply(ranks, card_value_high))) == 1)
  if(consecutive_high == TRUE){
    strength <- sort(sapply(ranks, card_value_high)) %>% max()
  }
  consecutive_low <- all(diff(sort(sapply(ranks, card_value_low))) == 1)
  if(consecutive_low == TRUE){
    strength <- sort(sapply(ranks, card_value_low)) %>% max()
  }
  consecutive <- consecutive_high | consecutive_low
  
  if(!(same_suit & consecutive)){
    strength <- NA
  }
  
  return(list(same_suit & consecutive, strength))
}

is_straight_flush(c("As","2s","3s","4s","5s"))
is_straight_flush(c("Ts","Js","Qs","Ks","As"))
is_straight_flush(c("Ks","As","2s","3s","4s"))
is_straight_flush(c("5s","6h","7s","8c","9d"))

is_straight <- function(hand) {
  if(hand_validation(hand) == FALSE){
    return(stop(paste("Hand",hand,"is not valid.")))
  }
  
  # Extract ranks and suits
  ranks <- substr(hand,1,1)
  suits <- substr(hand,2,2)
  
  # Check if all suits are the same, if yes, not straight
  diff_suit <- length(unique(suits)) != 1
  
  strength <- NA
  
  # Check if ranks are consecutive
  consecutive_high <- all(diff(sort(sapply(ranks, card_value_high))) == 1)
  if(consecutive_high == TRUE & diff_suit){
    strength <- sort(sapply(ranks, card_value_high)) %>% max()
  }
  consecutive_low <- all(diff(sort(sapply(ranks, card_value_low))) == 1)
  if(consecutive_low == TRUE & diff_suit){
    strength <- sort(sapply(ranks, card_value_low)) %>% max()
  }
  consecutive <- consecutive_high | consecutive_low
  
  return(list(consecutive & diff_suit, strength))
}

is_straight(c("As","2s","3s","4s","5s"))
is_straight(c("Ts","Js","Qs","Ks","As"))
is_straight(c("Ks","As","2s","3s","4s"))
is_straight(c("5s","6h","7s","8c","9d"))

is_flush <- function(hand) {
  if(hand_validation(hand) == FALSE){
    return(stop(paste("Hand",hand,"is not valid.")))
  }
  
  # Extract ranks
  ranks <- substr(hand,1,1)
  
  # Extract suits
  suits <- substr(hand,2,2)
  
  strength <- NA
  
  # Check if ranks are consecutive
  consecutive_high <- all(diff(sort(sapply(ranks, card_value_high))) == 1)
  consecutive_low <- all(diff(sort(sapply(ranks, card_value_low))) == 1)
  non_consecutive <- !(consecutive_high | consecutive_low)
  
  # Check if all suits are the same
  same_suit <- length(unique(suits)) == 1
  
  if(same_suit & non_consecutive){
    strength <- max(sapply(ranks, card_value_high))
  }
  
  return(list(same_suit & non_consecutive, strength))
}

is_flush(c("As","2s","3s","4s","5s"))
is_flush(c("Ts","Js","Qs","Ks","As"))
is_flush(c("Ks","As","2s","3s","4s"))
is_flush(c("5s","6h","7s","8c","9d"))

is_quads <- function(hand) {
  if(hand_validation(hand) == FALSE){
    return(stop(paste("Hand",hand,"is not valid.")))
  }
  
  # Extract ranks
  ranks <- substr(hand,1,1)
  ranks <- sapply(ranks, card_value_high)
  
  # Check if any ranks has appearance = 4
  count_equals_4 <- table(ranks) %>% max() == 4
  
  strength <- NA
  
  # get strength
  if(count_equals_4){
    df <- table(ranks) %>% data.frame()
    strength <- as.numeric(as.character(df$ranks))[which.max(df$Freq)] * 100
    kicker <- as.numeric(as.character(df$ranks))[which.min(df$Freq)]
    strength <- strength + kicker
  }
  
  return(list(count_equals_4, strength))
}

is_quads(c("As","Ad","Ah","Ac","5s"))
is_quads(c("5s","5d","5h","5c","As"))
# is_quads(c("As","Ad","Ah","Ac","5t"))
is_quads(c("Ts","Js","Qs","Ks","As"))
is_quads(c("Ks","As","2s","3s","4s"))
is_quads(c("5s","6h","7s","8c","9d"))

is_fullhouse <- function(hand) {
  if(hand_validation(hand) == FALSE){
    return(stop(paste("Hand",hand,"is not valid.")))
  }
  
  # Extract ranks
  ranks <- substr(hand,1,1)
  
  strength <- NA
  
  # Check if any ranks has appearance = 4
  max_count_equals_3 <- table(ranks) %>% max() == 3
  min_count_equals_2 <- table(ranks) %>% min() == 2
  
  # get strength
  if(max_count_equals_3 & min_count_equals_2){
    df <- table(ranks) %>% data.frame()
    strength <- as.numeric(as.character(df$ranks))[which.max(df$Freq)] * 100
    kicker <- as.numeric(as.character(df$ranks))[which.min(df$Freq)]
    strength <- strength + kicker
  }
  
  return(list(max_count_equals_3 & min_count_equals_2, strength))
}

is_fullhouse(c("5s","6h","5c","5h","6d"))
is_fullhouse(c("6s","5h","6c","6h","5d"))

is_trips <- function(hand) {
  if(hand_validation(hand) == FALSE){
    return(stop(paste("Hand",hand,"is not valid.")))
  }
  
  # Extract ranks
  ranks <- substr(hand,1,1)
  ranks <- sapply(ranks, card_value_high)
  
  strength <- NA
  
  # Check if any ranks has appearance = 3
  max_count_equals_3 <- table(ranks) %>% max() == 3
  min_count_equals_1 <- table(ranks) %>% min() == 1
  
  # get strength
  if(max_count_equals_3 & min_count_equals_1){
    df <- table(ranks) %>% data.frame()
    strength <- as.numeric(as.character(df$ranks))[which.max(df$Freq)] * 100
    kickers <- ranks[ranks != as.numeric(as.character(df$ranks))[which.max(df$Freq)]] %>% sum()
    strength <- strength + kickers
  }
  
  return(list(max_count_equals_3 & min_count_equals_1, strength))
}

is_trips(c("5s","6h","5c","5h","7d"))
is_trips(c("6s","5h","6c","6h","5d"))

is_two_pairs <- function(hand) {
  if(hand_validation(hand) == FALSE){
    return(stop(paste("Hand",hand,"is not valid.")))
  }
  
  # Extract ranks
  ranks <- substr(hand,1,1)
  ranks <- sapply(ranks, card_value_high)
  
  strength <- NA
  
  # Check if the shape of the hand is 1,2,2
  if(length(table(ranks) %>% sort()) != 3){
    hand_shape_122 <- FALSE
  }else{
    hand_shape_122 <- all(table(ranks) %>% sort() == c(1,2,2))
  }
  
  # get strength
  if(hand_shape_122){
    df <- table(ranks) %>% sort(decreasing = TRUE) %>% data.frame()
    strength <- as.numeric(as.character(df[1:2,"ranks"])) %>% sum() * 100
    kicker <- as.numeric(as.character(df[3,"ranks"]))
    strength <- strength + kicker
  }
  
  return(list(hand_shape_122, strength))
}

is_two_pairs(c("5s","6h","5c","5h","7d"))
is_two_pairs(c("6s","5h","6c","6h","5d"))
is_two_pairs(c("As","5h","6c","6h","5d"))

is_pair <- function(hand) {
  if(hand_validation(hand) == FALSE){
    return(stop(paste("Hand",hand,"is not valid.")))
  }
  
  # Extract ranks
  ranks <- substr(hand,1,1)
  ranks <- sapply(ranks, card_value_high)
  
  strength <- NA
  
  # Check if the hand has a pair
  pair <- (unique(ranks) %>% length()) == 4
  
  # get strength
  if(pair){
    df <- table(ranks) %>% sort(decreasing = TRUE) %>% data.frame()
    strength <- as.numeric(as.character(df[1,"ranks"])) * 100
    kickers <- as.numeric(as.character(df[2:4,"ranks"])) %>% sum()
    strength <- strength + kickers
  }
  
  return(list(pair, strength))
}

is_pair(c("5s","6h","5c","5h","7d"))
is_pair(c("6s","5h","6c","6h","5d"))
is_pair(c("As","5h","6c","6h","5d"))
is_pair(c("As","5h","6c","6h","4d"))

is_high_card <- function(hand) {
  if(hand_validation(hand) == FALSE){
    return(stop(paste("Hand",hand,"is not valid.")))
  }
  
  # Extract ranks and suits
  ranks <- substr(hand,1,1)
  ranks_value <- sapply(ranks, card_value_high)
  suits <- substr(hand,2,2)
  
  length_5 <- length(unique(ranks)) == 5
  diff_suits <- length(unique(suits)) != 1
  
  # Check if ranks are consecutive
  consecutive_high <- all(diff(sort(sapply(ranks, card_value_high))) == 1)
  consecutive_low <- all(diff(sort(sapply(ranks, card_value_low))) == 1)
  non_consecutive <- !(consecutive_high | consecutive_low)
  
  strength <- NA
  
  if(length_5 & diff_suits & non_consecutive){
    strength <- sum(ranks_value)
  }
  
  return(list(length_5 & diff_suits & non_consecutive, strength))
}

is_high_card(c("5s","6h","7c","8h","Td"))
is_high_card(c("6s","5h","6c","6h","5d"))
is_high_card(c("As","5h","6c","6h","5d"))
is_high_card(c("As","5h","6c","6h","4d"))

################################################################################################
################################################################################################
################################################################################################
################################################################################################

# Create a deck of 52 cards (assuming standard suits and ranks)
suits <- c("h", "d", "c", "s")
ranks <- c("2","3","4","5","6","7","8","9","T","J","Q","K","A")

deck <- expand.grid(rank = ranks, suit = suits)  # Generate all cards
deck <- deck[order(deck$suit), ]  # Sort for clarity

# Generate all unique 5-card combinations
hands <- t(combn(1:nrow(deck), 5))  # Indices of all possible hands

# Convert indices into actual cards
hand_table <- as.data.frame(matrix(ncol = 5, nrow = nrow(hands)))

for (i in 1:5) {
  hand_table[, i] <- paste0(deck$rank[hands[, i]], deck$suit[hands[, i]])
}

rm(hands)

# Assign column names
colnames(hand_table) <- paste0("Card_", 1:5)

################################################################################################
################################################################################################
################################################################################################
################################################################################################

# stop("stop")

tic <- Sys.time()
hand_table$straight_flush <- apply(hand_table[,1:5], 1, is_straight_flush)
hand_table$straight_flush <- sapply(hand_table$straight_flush, `[[`, 1)
hand_table$straight_flush <- sapply(hand_table$straight_flush, as.integer)
toc <- Sys.time()
print(toc - tic)

tic <- Sys.time()
hand_table$quads <- apply(hand_table[,1:5], 1, is_quads)
hand_table$quads <- sapply(hand_table$quads, `[[`, 1)
hand_table$quads <- sapply(hand_table$quads, as.integer)
toc <- Sys.time()
print(toc - tic)

tic <- Sys.time()
hand_table$full_house <- apply(hand_table[,1:5], 1, is_fullhouse)
hand_table$full_house <- sapply(hand_table$full_house, `[[`, 1)
hand_table$full_house <- sapply(hand_table$full_house, as.integer)
toc <- Sys.time()
print(toc - tic)

tic <- Sys.time()
hand_table$flush <- apply(hand_table[,1:5], 1, is_flush)
hand_table$flush <- sapply(hand_table$flush, `[[`, 1)
hand_table$flush <- sapply(hand_table$flush, as.integer)
toc <- Sys.time()
print(toc - tic)

tic <- Sys.time()
hand_table$straight <- apply(hand_table[,1:5], 1, is_straight)
hand_table$straight <- sapply(hand_table$straight, `[[`, 1)
hand_table$straight <- sapply(hand_table$straight, as.integer)
toc <- Sys.time()
print(toc - tic)

tic <- Sys.time()
hand_table$trips <- apply(hand_table[,1:5], 1, is_trips)
hand_table$trips <- sapply(hand_table$trips, `[[`, 1)
hand_table$trips <- sapply(hand_table$trips, as.integer)
toc <- Sys.time()
print(toc - tic)

tic <- Sys.time()
hand_table$two_pairs <- apply(hand_table[,1:5], 1, is_two_pairs)
hand_table$two_pairs <- sapply(hand_table$two_pairs, `[[`, 1)
hand_table$two_pairs <- sapply(hand_table$two_pairs, as.integer)
toc <- Sys.time()
print(toc - tic)

tic <- Sys.time()
hand_table$pair <- apply(hand_table[,1:5], 1, is_pair)
hand_table$pair <- sapply(hand_table$pair, `[[`, 1)
hand_table$pair <- sapply(hand_table$pair, as.integer)
toc <- Sys.time()
print(toc - tic)

tic <- Sys.time()
hand_table$high_card <- apply(hand_table[,1:5], 1, is_high_card)
hand_table$high_card <- sapply(hand_table$high_card, `[[`, 1)
hand_table$high_card <- sapply(hand_table$high_card, as.integer)
toc <- Sys.time()
print(toc - tic)

tic <- Sys.time()
hand_table$sum_check <- apply(hand_table[,6:14], 1, sum)
toc <- Sys.time()
print(toc - tic)

