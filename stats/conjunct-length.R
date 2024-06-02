#install.packages("dplyr")
#install.packages("tidyverse")
#install.packages("xtable")
#install.packages("lm_test")

library(dplyr)
library(tidyverse)
library(xtable)
library(lmtest)

save <- TRUE

in_path <- "C:/Users/wojte/OneDrive/Documents/Dependency Structure of Coordination/corpora"
out_path <- "C:/Users/wojte/OneDrive/Documents/Dependency Structure of Coordination/thesis/results"

# Table 1 - Governor position

tab1 <- function(data, language){
  
  All <-  nrow(data)
  
  Lgov <- as.numeric(data$governor.position == "L")
  LgovN <- sum(Lgov)
  LgovP <- round(LgovN / All, 2)
  
  Rgov <- as.numeric(data$governor.position == "R")
  RgovN <- sum(Rgov)
  RgovP <- round(RgovN / All, 2)
  
  Ogov <- as.numeric(data$governor.position == "0")
  OgovN <- sum(Ogov)
  OgovP <- round(OgovN / All, 2)
  
  tab = data.frame(All, LgovN, LgovP, RgovN, RgovP, OgovN, OgovP,
                   row.names = language)
  names(tab) <- c("All", "Lgov", "%", "Rgov", "%", "Nogov", "%")
  
  return(tab)
}

# Table 2 - Governor position - stats

tab2 <- function(data, language){
  
  All <-  nrow(data)
  
  Lgov <- as.numeric(data$governor.position == "L")
  Rgov <- as.numeric(data$governor.position == "R")
  Ogov <- as.numeric(data$governor.position == "0")
  
  LvsR <- wilcox.test(Lgov, Rgov)
  LvsRW <- sprintf("%.2e", LvsR$stat)
  LvsRp <- sprintf("%.3g", LvsR$p.val)
  
  LvsO <- wilcox.test(Lgov, Ogov)
  LvsOW <- sprintf("%.2e", LvsO$stat)
  LvsOp <- sprintf("%.3g", LvsO$p.val)
  
  OvsR <- wilcox.test(Ogov, Rgov)
  OvsRW <- sprintf("%.2e", OvsR$stat)
  OvsRp <- sprintf("%.3g", OvsR$p.val)
  
  tab = data.frame(LvsRW, LvsRp, LvsOW, LvsOp, OvsRW, OvsRp, 
                   row.names = language)
  names(tab) <- c("L-R", "p", "L-0", "p", "0-R", "p")
  
  return(tab)
}

# Language

extract_data <- function(path, language){

  language_path <- paste(path, "/", language, "/output", sep = '')
  setwd(language_path)
  corpora <- list.files()
  
  data <- NULL
  for (corpus in corpora) {
    
    d <- read_csv(corpus, show_col_types = FALSE)[,c(1, 17, 19, 20, 27, 29, 30)]
    data <- rbind(data, d)
  }
  
  # shorter conjunct
  
  data$Lshorter.words = as.integer(data$L.words < data$R.words)
  data$Lshorter.syllables = as.integer(data$L.syllables < data$R.syllables)
  data$Lshorter.chars = as.integer(data$L.chars < data$R.chars)
  
  # lenght difference
  
  data$diff.words = abs(data$L.words - data$R.words)
  data$diff.syllables = abs(data$L.syllables - data$R.syllables) 
  data$diff.chars = abs(data$L.chars - data$R.chars)
  
  return(data)
  
}

# Main

main <- function(){
  
  setwd(in_path)
  languages <- list.files()
  
  table1 <- data.frame()
  table2 <- data.frame()
  
  for(language in languages){
    
    data <- extract_data(in_path, language)
    
    table1 <- rbind(table1, tab1(data, language))
    table2 <- rbind(table2, tab2(data, language))
  }
  
  setwd(out_path)
  print(table1)
  print(table2)
  print(xtable(table1, type = "latex"), file = "conjunct-length-tmp.tex")
  print(xtable(table1, type = "latex"), file = "conjunct-length-stats-tmp.tex")
}

main()

