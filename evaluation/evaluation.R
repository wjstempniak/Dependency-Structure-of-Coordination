#install.packages("dplyr")
#install.packages("tidyverse")
#install.packages("psych")
library(dplyr)
library(tidyverse)
library(psych)

path <- 'C:/Users/wojte/OneDrive/Documents/Dependency Structure of Coordination'
language <- 'English'
rat <- c("WS", "OP") # raters' initials

g = c("L","R","0")   # governor positions
n = c(100, 100, 100) # number of coords per governor position

sample1 <- paste('sample_', rat[1],'.csv', sep='')
sample2 <- paste('sample_', rat[2],'.csv', sep='')

### Create files with random sample of coords ###

setwd(paste(path, 'corpora', language, 'output', sep = "/"))

files <- list.files()
coords <- NULL
for (file in files) {
  c <- read_csv(file, show_col_types = FALSE)
  coords <- rbind(coords, c)
  print(file)
}

print(paste("N =", nrow(coords)))
round(prop.table(table(coords$governor.position)), 3)


sample <- NULL
for (i in 1:3){
  s <- coords %>%
      filter(governor.position == g[i]) %>%
      sample_n(size = n[i], replace = TRUE) %>%
      select(sent.id, text, L.conjunct, R.conjunct, governor.position)
    sample <- rbind(sample,s)
    print(g[i])}

sample <- sample %>%
  mutate(correct = "", comment = "")

setwd(paste(path, 'evaluation', language, sep = '/'))
write_csv(sample,"sample.csv")
write_csv(sample,sample1)
write_csv(sample,sample2)

### Generate 'conflict.csv' after individual evaluation ###

setwd(paste(path, 'evaluation', language, sep = '/'))

R1_id = read_csv(sample1, show_col_types = FALSE)$sent.id
R2_id = read_csv(sample2, show_col_types = FALSE)$sent.id
sum(R1_id == R2_id)

R1_correct = read_csv(sample1, show_col_types = FALSE)$correct
R2_correct = read_csv(sample2, show_col_types = FALSE)$correct
agreement = (R1_correct == R2_correct)
sum(agreement, na.rm=TRUE)

conflicts <- read_csv("sample.csv", show_col_types = FALSE) %>%
  select(sent.id, text, L.conjunct, R.conjunct, governor.position) %>%
  mutate(R1 = R1_correct,
         R2 = R2_correct,
         resolved = ifelse(R1_correct == R2_correct, R1_correct, ""),
         comment = "")
write_csv(conflicts,"conflicts.csv")

### Calculate accuracy and inter-rater agreement after resolving conflicts###

setwd(paste(path, 'evaluation', language, sep = '/'))
resolved <- read_csv("resolved.csv", show_col_types = FALSE)
accuracy <- data.frame(
  All = mean(resolved$resolved),
  L = mean(resolved[resolved$governor.position == "L",]$resolved),
  O = mean(resolved[resolved$governor.position == "0",]$resolved),
  R = mean(resolved[resolved$governor.position == "R",]$resolved)
)
  
agreement <- cohen.kappa(x=cbind(resolved$R1, resolved$R2))

accuracy
agreement


