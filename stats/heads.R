#install.packages("dplyr")
#install.packages("tidyverse")
#install.packages("xtable")
#install.packages("lm_test")

library(dplyr)
library(tidyverse)
library(xtable)
library(lmtest)

in_path <- "C:/Users/wojte/OneDrive/Documents/Dependency Structure of Coordination/corpora"
out_path <- "C:/Users/wojte/OneDrive/Documents/Dependency Structure of Coordination/thesis/results"

languages <- c("Czech", "English", "German", "Icelandic", "Italian", "Korean", "Latin",
               "Polish", "Portuguese", "Romanian", "Russian", "Spanish", "Turkish")
jezyki <- c('czeski', 'angielski', 'niemiecki', 'islandzki', 'włoski', 'koreański', 'łaciński',
            'polski', 'portugalski', 'rumuński', 'rosyjski', 'hiszpański', 'turecki')

lang_dict <- setNames(jezyki, languages)

heads <- function(data, language){
  L <- data$L.head.rel.pos
  R <- data$R.head.rel.pos
  testL <- t.test(L, mu = 0.5)
  testR <- t.test(L, mu = 0.5)
  
  tab <- data.frame(
    sum(!is.na(L)), format(mean(L, na.rm = T), digits=2), format(testL$stat, digits = 2), format(testL$p.val, digits = 3),
    sum(!is.na(R)), format(mean(R, na.rm = T), digits=2), format(testR$stat, digits = 2), format(testR$p.val, digits = 3),
    row.names = lang_dict[[language]]
  )
  names(tab) <- c("N", "średnia","t", "p", "N", "średnia","t", "p")
  return(tab)
}


# Data

extract_data <- function(path, language){
  
  language_path <- paste(path, "/", language, "/heads", sep = '')
  setwd(language_path)
  corpora <- list.files()
  
  data <- NULL
  for (corpus in corpora) {
    d <- read_csv(corpus, show_col_types = FALSE)[,c(3,4,5,6,7,8)]
    data <- rbind(data, d)
  }
  
  data$L.tokens <- data$L.last.id - data$L.first.id
  data$L.head.pos <- data$L.head.id - data$L.first.id
  data$L.head.rel.pos <- data$L.head.pos/data$L.tokens
  
  data$R.tokens <- data$R.last.id - data$R.first.id
  data$R.head.pos <- data$R.head.id - data$R.first.id
  data$R.head.rel.pos <- data$R.head.pos/data$R.tokens
  
  return(data)
  
}


# Main

main <- function(){
  
  setwd(in_path)
  languages <- list.files()
  table <- data.frame()

  
  for(language in languages){
    table <- rbind(table, heads(extract_data(in_path, language), language))
  }
  
  print(table)
  
  setwd(out_path)
  print(xtable(table, type = "latex"), file = "head-pos-tmp.tex")
}

main()
