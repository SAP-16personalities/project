color <- function(personality) {
  ifelse(personality %in% c("INTJ", "INTP", "ENTJ", "ENTP"), "Analysts",
         ifelse(personality %in% c("INFJ", "INFP", "ENFJ", "ENFP"), "Diplomats",
                ifelse(personality %in% c("ISTJ", "ISFJ", "ESTJ", "ESFJ"), "Sentinels", "Explorers")))
}

check_expected <- function(table) {
  rows_total <- rowSums(table)
  cols_total <- colSums(table)
  total <- sum(rows_total)
  
  for(i in rows_total) {
    for(j in cols_total) {
      if(i*j/total < 5) return("Table doesn't meet the expected values criteria")
    }
  }
  return("Table meets the expected values criteria")
}
