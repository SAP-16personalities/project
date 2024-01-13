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
      if(i*j/total < 5) return(FALSE)
    }
  }
  TRUE
}

height_category <- function(height) {
  ifelse(height <= 160, "Short", 
         ifelse(height <= 175, "Medium height", "Tall"))
}

weight_category <- function(weight) {
  ifelse(weight <= 60, "Light", 
         ifelse(weight <= 90, "Medium weight", "Heavy"))
}
