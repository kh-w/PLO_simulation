numbers <- c("A","2","3","4","5","6","7","8","9","T","J","Q","K")
suits <- c("s","h","c","d")

deck <- expand.grid(numbers = numbers, suits = suits)
deck <- paste0(deck$numbers, deck$suits)

hand_p1_input <- c("As","Ah")
hand_p2 <- c("2s","2h")
hand_p3 <- c("3s","3h")
hand_p4 <- c("4s","4h")
hand_p5 <- c("5s","5h")
hand_p6 <- c("6s","6h")
hand_p7 <- c("7s","7h")
hand_p8 <- c("8s","8h")
hand_p9 <- c("9s","9h")
hand_all <- c(hand_p1_input, hand_p2, hand_p3, 
                    hand_p4, hand_p5, hand_p6,
                    hand_p7, hand_p8, hand_p9)

deck <- deck[!(deck %in% hand_all)]

flop <- sample(deck, 3, replace = FALSE)

deck <- deck[!(deck %in% flop)]

turn <- sample(deck, 1, replace = FALSE)

deck <- deck[!(deck %in% turn)]

river <- sample(deck, 1, replace = FALSE)