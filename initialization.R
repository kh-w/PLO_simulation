numbers <- c("A","2","3","4","5","6","7","8","9","T","J","Q","K")
suits <- c("s","h","c","d")

deck <- expand.grid(numbers = numbers, 
                    suits = suits)
deck <- split(deck, f = deck, drop = TRUE)

draw1 <- function(){
  card <- sample(deck, 1, replace = FALSE)
  deck2 <- deck[!(deck %in% card)]
  assign("deck", deck2, envir = .GlobalEnv)
  return(card)
}

draw2 <- function(){
  card <- sample(deck, 2, replace = FALSE)
  deck2 <- deck[!(deck %in% card)]
  assign("deck", deck2, envir = .GlobalEnv)
  return(card)
}

draw3 <- function(){
  card <- sample(deck, 3, replace = FALSE)
  deck2 <- deck[!(deck %in% card)]
  assign("deck", deck2, envir = .GlobalEnv)
  return(card)
}

oneCard_input <- function(C1){
  C1 <- strsplit(C1,"")
  input <- data.frame(numbers = c(C1[[1]][1]), 
                      suits = c(C1[[1]][2]))
  input$numbers <- factor(input$numbers, 
                          levels = c("A","2","3","4","5","6","7","8","9","T","J","Q","K"), 
                          ordered = TRUE)
  input$suits <- factor(input$suits, 
                        levels = c("s","h","c","d"), 
                        ordered = TRUE)
  input <- split(input, f = input, drop = TRUE)
  input <- deck[deck %in% input]
  deck2 <- deck[!(deck %in% input)]
  assign("deck", deck2, envir = .GlobalEnv)
  return(input)
}

twoCard_input <- function(C1, C2){
  C1 <- strsplit(C1,"")
  C2 <- strsplit(C2,"")
  input <- data.frame(numbers = c(C1[[1]][1],C2[[1]][1]), 
                      suits = c(C1[[1]][2],C2[[1]][2]))
  input$numbers <- factor(input$numbers, 
                          levels = c("A","2","3","4","5","6","7","8","9","T","J","Q","K"), 
                          ordered = TRUE)
  input$suits <- factor(input$suits, 
                        levels = c("s","h","c","d"), 
                        ordered = TRUE)
  input <- split(input, f = input, drop = TRUE)
  input <- deck[deck %in% input]
  deck2 <- deck[!(deck %in% input)]
  assign("deck", deck2, envir = .GlobalEnv)
  return(input)
}

threeCard_input <- function(C1, C2, C3){
  C1 <- strsplit(C1, "")
  C2 <- strsplit(C2, "")
  C3 <- strsplit(C3, "")
  input <- data.frame(numbers = c(C1[[1]][1], C2[[1]][1], C3[[1]][1]), 
                      suits = c(C1[[1]][2], C2[[1]][2], C3[[1]][2]))
  input$numbers <- factor(input$numbers, 
                          levels = c("A","2","3","4","5","6","7","8","9","T","J","Q","K"), 
                          ordered = TRUE)
  input$suits <- factor(input$suits, 
                        levels = c("s","h","c","d"), 
                        ordered = TRUE)
  input <- split(input, f = input, drop = TRUE)
  input <- deck[deck %in% input]
  deck2 <- deck[!(deck %in% input)]
  assign("deck", deck2, envir = .GlobalEnv)
  return(input)
}

holeCards_p1 <- twoCard_input("Ts", "Js")
# holeCards_p2 <- draw2()
# holeCards_p3 <- draw2()
# holeCards_p4 <- draw2()
# holeCards_p5 <- draw2()
# holeCards_p6 <- draw2()
# holeCards_p7 <- draw2()
# holeCards_p8 <- draw2()
# holeCards_p9 <- draw2()

pf_1flop <- threeCard_input("Qs", "Ks", "As")

pf_2turn <- oneCard_input("9c")

pf_3river <- oneCard_input("8s")

recog_straight <- function(hand) {
  hand <- hand[order(sapply(hand,function(x) {x[[1]]}))]
  # straight 
  h_num <- sapply(hand,function(x) {x[[1]]})
  all_comb <- combn(h_num, 5, simplify=FALSE)
  L <- length(all_comb)
  str8_check <- data.frame(str8 = "", str8_size = "")
  for (i in 1:L) {
    sumIfOne <- 
      ifelse(as.numeric(all_comb[[i]][5]) - as.numeric(all_comb[[i]][4]) == 1, 1, 0) + 
      ifelse(as.numeric(all_comb[[i]][4]) - as.numeric(all_comb[[i]][3]) == 1, 1, 0) + 
      ifelse(as.numeric(all_comb[[i]][3]) - as.numeric(all_comb[[i]][2]) == 1, 1, 0) + 
      ifelse(as.numeric(all_comb[[i]][2]) - as.numeric(all_comb[[i]][1]) == 1|
             as.numeric(all_comb[[i]][2]) - as.numeric(all_comb[[i]][1]) == 9, 1, 0)
    str8_check[i,1] <- ifelse(sumIfOne == 4,
                      TRUE,
                      ifelse(sum(all_comb[[i]] == c("A","T","J","Q","K")) == 5, TRUE, FALSE))
    str8_check[i,2] <- ifelse(sumIfOne == 4 & sum(all_comb[[i]] == c("A","T","J","Q","K")) != 5, 
                      sum(as.numeric(all_comb[[i]])), 
                      ifelse(sum(all_comb[[i]] == c("A","T","J","Q","K")) == 5, 60, 0))
  }
  size <- as.numeric(str8_check[,2][which.max(str8_check[,2])])
  return(size)
}

recog_flush <- function(hand) {
  # flush 
  h_num <- sapply(hand,function(x) {x[[1]]})
  h_suit <- sapply(hand,function(x) {x[[2]]})
  tbl <- data.frame(table(h_suit))
  max_suit <- tbl$h_suit[which.max(tbl$Freq)]
  num_suited <- h_num[h_suit == max_suit]
  max_num <- ifelse("A" %in% num_suited, 14, max(as.numeric(num_suited)))
  size <- ifelse(max(tbl$Freq) < 5, 0, max_num)
  return(size)
}

recof_str8flush <- function(hand){
  # straight flush
  h_num <- sapply(hand,function(x) {x[[1]]})
  h_suit <- sapply(hand,function(x) {x[[2]]})
  tbl <- data.frame(table(h_suit))
  max_suit <- tbl$h_suit[which.max(tbl$Freq)]
  num_suited <- h_num[h_suit == max_suit]
  str8check <- recog_straight(num_suited)
  max_num <- ifelse("A" %in% num_suited, 14, max(as.numeric(num_suited)))
  size <- ifelse(max(tbl$Freq) > 4 & str8check != 0, str8check, 0)
  return(size)
}

hand <- c(holeCards_p1, pf_1flop, pf_2turn, pf_3river)

recog_straight(hand)
recog_flush(hand)
recof_str8flush(hand)

