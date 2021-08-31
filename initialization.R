numbers <- c("A","2","3","4","5","6","7","8","9","T","J","Q","K")
suits <- c("s","h","c","d")

deck <- expand.grid(numbers = numbers, 
                    suits = suits)
deck <- split(deck, f = deck)

draw1 <- function(){
  hand <- sample(deck, 1, replace = FALSE)
  deck2 <- deck[!(deck %in% hand)]
  assign("deck", deck2, envir = .GlobalEnv)
  return(hand)
}

draw2 <- function(){
  hand <- sample(deck, 2, replace = FALSE)
  deck2 <- deck[!(deck %in% hand)]
  assign("deck", deck2, envir = .GlobalEnv)
  return(hand)
}

draw3 <- function(){
  hand <- sample(deck, 3, replace = FALSE)
  deck2 <- deck[!(deck %in% hand)]
  assign("deck", deck2, envir = .GlobalEnv)
  return(hand)
}

hand_input <- function(N1,S1,N2,S2){
  input <- data.frame(numbers = c(N1,N2), 
                      suits = c(S1,S2))
  input$numbers <- factor(input$numbers, 
                          levels = c("A","2","3","4","5","6","7","8","9","T","J","Q","K"), 
                          ordered = TRUE)
  input$suits <- factor(input$suits, 
                        levels = c("s","h","c","d"), 
                        ordered = TRUE)
  input <- split(input, f = input)
  input <- deck[deck %in% input]
  deck2 <- deck[!(deck %in% input)]
  assign("deck", deck2, envir = .GlobalEnv)
  return(input)
}

hand_p1 <- hand_input("A","s","A","h")
hand_p2 <- draw2()
hand_p3 <- draw2()
hand_p4 <- draw2()
hand_p5 <- draw2()
hand_p6 <- draw2()
hand_p7 <- draw2()
hand_p8 <- draw2()
hand_p9 <- draw2()

pf_1flop <- draw3()

pf_2turn <- draw1()

pf_3river <- draw1()

# p1_made_hand <- c(hand_p1_input, pf_board)
# 
# p1_made_hand <- p1_made_hand[order(sapply(p1_made_hand,function(x) x[[1]]))]
# p1_made_hand_n <- sapply(p1_made_hand,function(x) x[[1]])
# p1_made_hand_s <- sapply(p1_made_hand,function(x) x[[2]])

recog_hand <- function(player, flop, turn, river){
  
}
