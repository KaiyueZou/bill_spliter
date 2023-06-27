#===================================
# R Code for Bills to split  
# Author: Kaiyue Zou
# Date: 6/26/2023 - 6/27/2023
#
# Content
#   - Type in all the transactions 
#   - Character spliter 
#   - Use the function to calculate 
#===================================


# Load the packages 
library(stringr)
library(combinat)


# Type in all the transactions 
## Transaction type-in rules: 
## - always start from the lender to the borrower 
## - all possible direct transactions
## - no need to do pre-calculations to simplify 
payment <- 
  "
  K -> L: 364.35, 61.50; 
  K -> Y: 364.35; 
  K -> R: 364.35; 
  L -> K: 525.18; 
  L -> Y: 525.18; 
  L -> R: 525.18; 
  Y -> K: 39.50; 
  Y -> L: 39.50; 
  Y -> R: 39.50; 
  R -> K: 120.75; 
  R -> L: 120.75; 
  R -> Y: 120.75; 
"


# Split the sentence and make a matrix 
## Clean up 
ppl_all <- unique(unlist(stringr::str_extract_all(payment, "[a-zA-Z]+")))
sen_wise <- unlist(stringr::str_split(payment, ";"))

sen_cln <- stringr::str_replace_all(sen_wise, "\n", "")
sen_cln <- stringr::str_replace_all(sen_cln, " ", "")
sen_cln <- sen_cln[sen_cln != ""]

ppl_order <- as.list(ppl_all)

## Get a matrix
mtr <- matrix(, nrow = length(ppl_all), ncol = length(ppl_all))

mtr_filler <- function(mtr, sen_cln){
  for (i in seq_len(dim(mtr)[1])) {
    ppl_lend <- ppl_order[[i]]
    mny_lend <- sen_cln[grepl(paste0(ppl_lend, "->"), sen_cln)]
    if(length(mny_lend) == 0){next}
    
    for (j in seq_len(dim(mtr)[2])) {
      if(i == j){next}
      ppl_borrow <- ppl_order[[j]]
      ppl_l_b <- mny_lend[grepl(paste0("->", ppl_borrow), mny_lend)]
      if(length(ppl_l_b) == 0){next}
      money <- as.numeric(unlist(stringr::str_extract_all(ppl_l_b, 
                                                          "[0-9]*\\.[0-9]*")))
      ttl <- sum(money)
      mtr[i, j] <- ttl
    }
    
  }
  
  mtr[is.na(mtr)] <- 0  #remove all the NAs
  mtr <- t(mtr)         #transpose: from lending to borrowing 
  return(mtr)
}
mtr <- mtr_filler(mtr, sen_cln)
mtr_full <- mtr

## Simplify the matrix 
mtr_simplify <- function(mtr){
  cmb_unique <- combinat::combn(seq_len(dim(mtr)[1]), 2)
  for (j in seq_len(dim(cmb_unique)[2])) {
    cmb <- cmb_unique[, j]
    mtr_12 <- ifelse(mtr[cmb[1], cmb[2]] >= mtr[cmb[2], cmb[1]], 
                                  (mtr[cmb[1], cmb[2]] - mtr[cmb[2], cmb[1]]), 
                                  0)
    mtr_21 <- ifelse(mtr[cmb[2], cmb[1]] >= mtr[cmb[1], cmb[2]], 
                                  (mtr[cmb[2], cmb[1]] - mtr[cmb[1], cmb[2]]), 
                                  0)
    mtr[cmb[1], cmb[2]] <- mtr_12
    mtr[cmb[2], cmb[1]] <- mtr_21
  }
  return(mtr)
}
mtr <- mtr_simplify(mtr)
mtr_simple <- mtr


# Simplify the final transactions even further 
one_dim_bill_maker <- function(mtr){
  max_in_search <- max(mtr)
  
  while(max_in_search > min(mtr)){
    max_idx <- which(mtr == max_in_search, arr.ind = TRUE)
    
    for (i in seq_len(nrow(max_idx))) {
      first_idx <- max_idx[i, ]
      snd_vector <- mtr[first_idx[2], ]
      second_max_col <- which(snd_vector == 
                                max(snd_vector[snd_vector <= max_in_search]))[1]
      if(length(second_max_col) == 0){next}
      second_max_idx <- c(first_idx[2], second_max_col)
      
      mtr[first_idx[1], second_max_idx[2]] <- 
        (mtr[first_idx[1], second_max_idx[2]]
          + mtr[second_max_idx[1], second_max_idx[2]])
      mtr[first_idx[1], first_idx[2]] <- 
        (mtr[first_idx[1], first_idx[2]] 
          - mtr[second_max_idx[1], second_max_idx[2]])
      mtr[second_max_idx[1], second_max_idx[2]] <- 0
    }
    
    max_in_search <- max(mtr[mtr < max_in_search], na.rm = T)
  }
  
  return(mtr)
}
mtr <- one_dim_bill_maker(mtr)
mtr_one_dim <- mtr


# Summary 
rownames(mtr_full) <- ppl_order
colnames(mtr_full) <- ppl_order
rownames(mtr_simple) <- ppl_order
colnames(mtr_simple) <- ppl_order
rownames(mtr_one_dim) <- ppl_order
colnames(mtr_one_dim) <- ppl_order


# Output file 
sink("bill.txt")
cat("From row names to column names: the flow of money to return\n")
cat("\n")
cat("The first matrix is the original one: \n")
mtr_full
cat("\n")
cat("The second matrix is the simplified one: \n")
mtr_simple
cat("\n")
cat("The third matrix simplified the transactions even further: \n")
mtr_one_dim
cat("\n")
cat("To summarize: \n")
for (i in seq_len(nrow(mtr_one_dim))) {
  for (j in seq_len(ncol(mtr_one_dim))) {
    if(mtr_one_dim[i, j] != 0){
      cat(paste(
        ppl_order[i], "should give back", mtr_one_dim[i, j], "to", ppl_order[j],
        "\n"
      ))
    }
  }
}
sink()
