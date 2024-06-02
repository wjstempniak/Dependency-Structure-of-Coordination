################################################################################
## This code is strongly based on code used in the analysis for Przepiórkowski 
## and Woźniak (2023) paper and was mostly written by prof. Adam Przepiórkowski. 
################################################################################

rm(list=ls(all.names=TRUE))

library(car)
library(effects)
library(dplyr)
library(lattice)
library(gridExtra)

library(xtable)
options(
  xtable.table.placement="H",
  xtable.sanitize.text.function=function(x){x},
  xtable.booktabs=TRUE,
  xtable.scalebox="0.8") # 0.8 to fit in a column

in_path = "C:/Users/wojte/OneDrive/Documents/Dependency Structure of Coordination/corpora"
out_path = "C:/Users/wojte/OneDrive/Documents/Dependency Structure of Coordination/thesis/results"

setwd(in_path)

languages <- c("Czech", "English", "German", "Icelandic", "Italian", "Korean", "Latin",
               "Polish", "Portuguese", "Romanian", "Russian", "Spanish", "Turkish")
jezyki <- c('czeski', 'angielski', 'niemiecki', 'islandzki', 'włoski', 'koreański', 'łaciński',
            'polski', 'portugalski', 'rumuński', 'rosyjski', 'hiszpański', 'turecki')

lang_dict <- setNames(jezyki, languages)

for(language in list.files()){

cat(paste("\n\n----------------------------", language, "----------------------------\n"))
  
lang_path = paste(in_path, language, "output", sep="/")
setwd(lang_path)
corpora <- list.files()

xx <- NULL
for (corpus in corpora) {
  x <- read.csv(file=corpus, header=TRUE)
  xx <- rbind(xx, x)
}

xx$governor.position <- factor(xx$governor.position, levels=c("L", "0", "R", "M"))  ## order of levels: L, 0, R

xx$chars.diff <- xx$R.chars-xx$L.chars  ## right–left
xx$syllables.diff <- xx$R.syllables-xx$L.syllables  ## right–left
xx$words.diff <- xx$R.words-xx$L.words  ## right–left

oo <- xx[xx$governor.position=="0",]
ll <- xx[xx$governor.position=="L",]
rr <- xx[xx$governor.position=="R",]


###############################################################################
## Generating the LaTeX code of tables with coordinations conjunct length
###############################################################################

tabs_path <- paste(out_path, "tabs", sep="/")
setwd(tabs_path)

populations <- list("Wszystkie koordynacje"=xx,
                    "Brak nadrzędnika"=oo,
                    "Nadrzędnik po lewej"=ll,
                    "Nadrzędnik po prawej"=rr)
no.populations <- length(populations)

## Create data frames and LaTeX tables for each of the above four populations of coordinations:

tabs <- list()  #

addtorow <- list()
addtorow$pos <- list(0, 0, 0, 0)
addtorow$command <- c("& \\multicolumn{2}{c}{mediana} & \\multicolumn{2}{c}{średnia}  \\\\\n",
                      "& \\multicolumn{1}{c}{lewy} & \\multicolumn{1}{c}{\\hspace*{-1ex}prawy\\hspace*{-1ex}} & \\multicolumn{1}{c}{lewy} & \\multicolumn{1}{c}{prawy} & \\multicolumn{1}{c}{V} & \\multicolumn{1}{c}{$p$} \\\\\n",
                      "\\midrule\n",
                      paste("\\multicolumn{7}{c}{\\textbf{Język ", lang_dict[[language]],"}} \\\\\n", sep = ""))

for (i in 1:no.populations) {
  
  yy <- populations[[i]]
  
  tests <- list(
    wilcox.test(yy$L.chars, yy$R.chars, alternative="less", paired=TRUE, exact=FALSE, correct=FALSE),
    wilcox.test(yy$L.syllables, yy$R.syllables, alternative="less", paired=TRUE, exact=FALSE, correct=FALSE),
    wilcox.test(yy$L.words, yy$R.words, alternative="less", paired=TRUE, exact=FALSE, correct=FALSE))
  tabs[[i]] <- data.frame(measure = c('znaki','sylaby','słowa'),
                          med.1 = c(median(yy$L.chars), median(yy$L.syllables), median(yy$L.words)),
                          med.2 = c(median(yy$R.chars), median(yy$R.syllables), median(yy$R.words)),
                          mean.1 = c(mean(yy$L.chars), mean(yy$L.syllables), mean(yy$L.words)),
                          mean.2 = c(mean(yy$R.chars), mean(yy$R.syllables), mean(yy$R.words)),
                          V = c(tests[[1]]$statistic, tests[[2]]$statistic, tests[[3]]$statistic),
                          p = c(tests[[1]]$p.value, tests[[2]]$p.value, tests[[3]]$p.value))
  
  addtorow$pos <- append(addtorow$pos, rep(3*(i-1), 3))
  subtitle <- paste("\\multicolumn{7}{c}{", names(populations)[i], " (N = ", format(nrow(yy),big.mark=",",scientific=FALSE), ")} \\\\\n", sep="")
  addtorow$command <- append(addtorow$command, c("\\midrule\n", subtitle, ifelse(i>1, "\\midrule\n", "")))
}

## Combine these data frames and LaTeX tables into one data frame and one LaTeX table:

tab <- tabs[[1]]
for (i in 2:no.populations) tab <- rbind(tab,tabs[[i]])

print(
   xtable(tab,
          align="llrrrrrr",
          display=c("s","s","f","f","f","f","g","g"),
          digits=c(0,0,0,0,2,2,2,2),
          #caption=paste("Średnie i mediany długości prawego i lewego członu w języku", language),
          label="tab:basic"),
   floating = FALSE, # only tabular
   size="\\setlength{\\tabcolsep}{4pt}",
   include.colnames=FALSE,
   include.rownames=FALSE,
   add.to.row=addtorow,
   file=paste(language, "tex", sep="."))

######################################################################
## Code for generating the PDF of Figure showing tendencies
## and for the multifactorial binary logistic regression analysis
######################################################################

plots_path <- paste(out_path, "plots", sep="/")
setwd(plots_path)

multipliers <- c(6, 2, 1)
buckets <- c(0,1,2,3,6,18) # each bucket: ( buckets[i], buckets[i+1] ]
buckets.ch <- buckets*multipliers[1]
buckets.sy <- buckets*multipliers[2]
buckets.wo <- buckets*multipliers[3]


xx.ch <- xx[0<abs(xx$chars.diff) & abs(xx$chars.diff)<=buckets.ch[length(buckets.ch)], c("governor.position","chars.diff")]
colnames(xx.ch) <- c("governor.position", "diff")
xx.sy <- xx[0<abs(xx$syllables.diff) & abs(xx$syllables.diff)<=buckets.sy[length(buckets.sy)], c("governor.position","syllables.diff")]
colnames(xx.sy) <- c("governor.position", "diff")
xx.wo <- xx[0<abs(xx$words.diff) & abs(xx$words.diff)<=buckets.wo[length(buckets.wo)], c("governor.position","words.diff")]
colnames(xx.wo) <- c("governor.position", "diff")

xx.ch$shorter <- factor(ifelse(xx.ch$diff>0,"L","R"))
xx.ch$diff <- abs(xx.ch$diff)
for (i in 2:length(buckets.ch)) {
  xx.ch$diff[xx.ch$diff > buckets.ch[i-1] & xx.ch$diff <= buckets.ch[i]] <- buckets.ch[i]
}
###print(table(xx.ch)) # checking that each bucket is sufficiently large

xx.sy$shorter <- factor(ifelse(xx.sy$diff>0,"L","R"))
xx.sy$diff <- abs(xx.sy$diff)
for (i in 2:length(buckets.sy)) {
  xx.sy$diff[xx.sy$diff > buckets.sy[i-1] & xx.sy$diff <= buckets.sy[i]] <- buckets.sy[i]
}
###print(table(xx.sy)) # checking that each bucket is sufficiently large

xx.wo$shorter <- factor(ifelse(xx.wo$diff>0,"L","R"))
xx.wo$diff <- abs(xx.wo$diff)
for (i in 2:length(buckets.wo)) {
  xx.wo$diff[xx.wo$diff > buckets.wo[i-1] & xx.wo$diff <= buckets.wo[i]] <- buckets.wo[i]
}
###print(table(xx.wo)) # checking that each bucket is sufficiently large

governors <- c("0", "L", "R")
gov.texts <- c("BRAK nadrzędnika", "Nadrzędnik po LEWEJ", "Nadrzędnik po PRAWEJ")
differences <- list("znakach"=xx.ch, "sylabach"=xx.sy, "słowach"=xx.wo)

plots <- list()
for (d in 1:length(differences)) {
  d.name <- names(differences)[d]
  ###cat(paste("\n---------- DIFFERENCES (BUCKETS) OF LENGTHS measured in ", d.name, " ----------\n", sep=""))
  D <- differences[[d]]
  D$shorter <- factor(D$shorter, levels=levels(D$shorter)[c(2,1)])
  
  # multifactorial analysis for a given measure (length counted in d.name):
  
  mm <- glm(shorter ~ 1 + diff * governor.position, data=D, family=binomial, na.action=na.exclude)
  ###print(summary(mm))
  ###print(drop1(mm, test="Chisq"))
  ###print(pairs(emmeans::emtrends(mm, ~ governor.position, var="diff"), adjust="none"))
  
  for (g in 1:length(governors)) {
    ###cat("\n", paste(rep("-",40), collapse=""), governors[g], paste(rep("-",40), collapse=""), "\n")
    g.name <- governors[g]
    
    # monofactorial analysis for a given measure (d.name) and governor presence/position (g.name):
    
    m <- glm(shorter ~ 1 + diff, data=D[D$governor.position==g.name,], family=binomial, na.action=na.exclude)
    ###print(summary(m))
    ###print(drop1(m, test="Chisq"))
    ph <- data.frame(lend <- effect("diff", m))
    ###print(ph)
    
    plot.name <- paste(g.name,d.name,sep="/")
    
    m.slope <- sprintf("%.2e", summary(m)$coefficients[2,1])
    m.prob <- sprintf("%.3g", summary(m)$coefficients[2,4])
    cat(paste("\n", g.name, "w", d.name, "\t p =", m.prob, "\t wsp. =", m.slope))
    if (summary(m)$coefficients[2,4] <= 0.05) colors = c("gray10","gray40")
    else colors = c("gray10","red")
    
    p <- plot(lend, type="response", ylim=c(0.4, 1), grid=TRUE,
              key=list(corner = c(0.01, 0.98), cex=.8, col=colors,
                       text=list(lab=c(paste("współczynnik: ", m.slope, sep=""), paste("p: ", m.prob, sep="")))),
              main=paste(gov.texts[g], " (długość w ", toupper(d.name), ")", sep=""),
              xlab=paste("bezwzględna różnica w",d.name),
              ylab="odsetek krótszych lewych członów"); p
    
    plots[[plot.name]] <- p
  }
}

# Visualize the results as Figure:

library(Cairo)
file_name = paste(language, "pdf", sep=".")
CairoPDF(file = file_name, width = 15, height = 11)
tps <- list(par.xlab.text = list(cex=0.8),
            par.ylab.text = list(cex=0.75),
            par.main.text = list(cex=.95))
plots <- plots[c(2,5,8,1,4,7,3,6,9)]
do.call(grid.arrange, c(lapply(plots, update, par.settings=tps), list(ncol=length(differences))))
dev.off()

}

