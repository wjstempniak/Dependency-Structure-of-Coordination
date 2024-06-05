#install.packages("dplyr")
#install.packages("tidyverse")
library(dplyr)
library(tidyverse)

path <- 'path/to/folder'
language <- 'Turkish'

n = c(60, 0)

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

coords$h = list_to_vector(coords$heuristics.used)

sample1 <- coords %>%
  sample_n(size = n[1], replace = TRUE) %>%
  select(sent.id, text, L.conjunct, R.conjunct, governor.position)

 sample2 <- coords %>%
   filter(grepl("H3", heuristics.used, fixed = TRUE) |
          grepl("H4", heuristics.used, fixed = TRUE) |
          grepl("H5", heuristics.used, fixed = TRUE) |
          grepl("H6", heuristics.used, fixed = TRUE) |
          grepl("H7", heuristics.used, fixed = TRUE) ) %>%
   sample_n(size = n[2], replace = TRUE) %>%
   select(sent.id, text, L.conjunct, R.conjunct, governor.position)

sample <- rbind(sample1, sample2)

sample <- sample %>%
  mutate(correct = "", comment = "")

setwd(paste(path, 'evaluation', language, sep = '/'))
write_csv(sample,"sample.csv")