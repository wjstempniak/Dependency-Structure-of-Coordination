rm(list=ls(all.names=TRUE))

library(car)
library(effects)
library(dplyr)
library(lattice)
library(gridExtra)

library(xtable)
options(
    xtable.table.placement="t!",
    xtable.sanitize.text.function=function(x){x},
    xtable.booktabs=TRUE,
    xtable.scalebox="0.8")

in_path = "path/to/folder/PW23/ptb_coordinations_acl2023_data"
out_path = "path/to/folder/thesis/results"

setwd(in_path)

xx <- read.table(file="ptb_coordinations_acl2023.csv", header=TRUE, sep="\t", quote="", comment.char="", stringsAsFactors=TRUE)
xx$governor <- factor(xx$governor, levels=levels(xx$governor)[c(2,1,3)])  ## order of levels: L, 0, R

xx$chars.diff <- xx$chars.2-xx$chars.1  ## right–left
xx$syllables.diff <- xx$syllables.2-xx$syllables.1  ## right–left
xx$words.diff <- xx$words.2-xx$words.1  ## right–left

oo <- xx[xx$governor=="0",]
ll <- xx[xx$governor=="L",]
rr <- xx[xx$governor=="R",]



######################################################################
## Code for generating (the LaTeX sources of) Table 1 on p.3
######################################################################

populations <- list("All coordinations"=xx,
                    "No governor"=oo,
                    "Governor on the left"=ll,
                    "Governor on the right"=rr)
no.populations <- length(populations)

## Create data frames and LaTeX tables for each of the above four populations of coordinations:

tabs <- list()  #

addtorow <- list()
addtorow$pos <- list(0, 0)
addtorow$command <- c("& \\multicolumn{2}{c}{\\textls[50]{\\,median}} & \\multicolumn{2}{c}{\\textls[500]{mean}}  \\\\\n",
                      "& \\multicolumn{1}{c}{left} & \\multicolumn{1}{c}{\\hspace*{-1ex}right\\hspace*{-1ex}} & \\multicolumn{1}{c}{left} & \\multicolumn{1}{c}{right} & \\multicolumn{1}{c}{V} & \\multicolumn{1}{c}{$p$} \\\\\n")

for (i in 1:no.populations) {

    yy <- populations[[i]]

    tests <- list(
        wilcox.test(yy$chars.1, yy$chars.2, alternative="less", paired=TRUE, exact=FALSE, correct=FALSE),
        wilcox.test(yy$syllables.1, yy$syllables.2, alternative="less", paired=TRUE, exact=FALSE, correct=FALSE),
        wilcox.test(yy$words.1, yy$words.2, alternative="less", paired=TRUE, exact=FALSE, correct=FALSE))
    tabs[[i]] <- data.frame(measure = c('characters','syllables','words'),
                            med.1 = c(median(yy$chars.1), median(yy$syllables.1), median(yy$words.1)),
                            med.2 = c(median(yy$chars.2), median(yy$syllables.2), median(yy$words.2)),
                            mean.1 = c(mean(yy$chars.1), mean(yy$syllables.1), mean(yy$words.1)),
                            mean.2 = c(mean(yy$chars.2), mean(yy$syllables.2), mean(yy$words.2)),
                            V = c(tests[[1]]$statistic, tests[[2]]$statistic, tests[[3]]$statistic),
                            p = c(tests[[1]]$p.value, tests[[2]]$p.value, tests[[3]]$p.value))

    addtorow$pos <- append(addtorow$pos, rep(3*(i-1), 3))
    subtitle <- paste("\\multicolumn{7}{c}{\\emph{", names(populations)[i], " (N = ", format(nrow(yy),big.mark=",",scientific=FALSE), ")}} \\\\\n", sep="")
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
           caption="Medians and means of lengths of left and right conjuncts in binary coordinations in \\ptbc",
           label="tab:basic"),
    size="\\setlength{\\tabcolsep}{4pt}",
    include.colnames=FALSE,
    include.rownames=FALSE,
    add.to.row=addtorow,
    file="tab_basic.tex")



######################################################################
## Code for generating (the LaTeX sources of) Table 2 on p.3
######################################################################

tests <- list(
    prop.test(x = c(nrow(ll[ll$chars.diff>0,]), nrow(rr[rr$chars.diff>0,])),
              n = c(nrow(ll[ll$chars.diff!=0,]), nrow(rr[rr$chars.diff!=0,])),
              correct=FALSE),
    prop.test(x = c(nrow(ll[ll$syllables.diff>0,]), nrow(rr[rr$syllables.diff>0,])),
              n = c(nrow(ll[ll$syllables.diff!=0,]), nrow(rr[rr$syllables.diff!=0,])),
              correct=FALSE),
    prop.test(x = c(nrow(ll[ll$words.diff>0,]), nrow(rr[rr$words.diff>0,])),
              n = c(nrow(ll[ll$words.diff!=0,]), nrow(rr[rr$words.diff!=0,])),
              correct=FALSE))

tab <- matrix(c(
    tests[[1]]$estimate[1], tests[[2]]$estimate[1], tests[[3]]$estimate[1], 
    nrow(ll[ll$chars.diff!=0,]), nrow(ll[ll$syllables.diff!=0,]), nrow(ll[ll$words.diff!=0,]),
    tests[[1]]$estimate[2], tests[[2]]$estimate[2], tests[[3]]$estimate[2], 
    nrow(rr[rr$chars.diff!=0,]), nrow(rr[rr$syllables.diff!=0,]), nrow(rr[rr$words.diff!=0,]),
    tests[[1]]$statistic,   tests[[2]]$statistic,   tests[[3]]$statistic,
    tests[[1]]$p.value,     tests[[2]]$p.value,     tests[[3]]$p.value),
    ncol=6)

addtorow <- list()
addtorow$pos <- list(0, 0, 0)
addtorow$command <- c("& \\multicolumn{4}{c}{\\textls[1000]{\\hspace{-2pt}governor}} & & \\\\\n",
                      "& \\multicolumn{2}{c}{\\textls[100]{on the left}} & \\multicolumn{2}{c}{on the right} \\\\\n",
                      "& \\multicolumn{1}{c}{prop} & \\multicolumn{1}{c}{$N$} & \\multicolumn{1}{r}{prop} & \\multicolumn{1}{c}{$N$} & \\multicolumn{1}{c}{$\\chi^2(1)$} & \\multicolumn{1}{c}{$p$} \\\\\n")
rownames(tab) <- c('characters','syllables','words')
print(
    xtable(tab,
           align="lrrrrrr",
           display=c("s","f","f","f","f","f","g"),
           digits=c(0,3,0,3,0,1,2),
           caption="Proportions of shorter conjuncts occurring on the left (vs.~right) depending on the position of the governor, in coordinations with conjuncts of different lengths",
           label="tab:basic:props:lr"),
    size="\\setlength{\\tabcolsep}{4pt}",
    include.colnames=FALSE,
    add.to.row=addtorow,
    file="tab_basic_props_lr.tex")



######################################################################
## Code for generating (the LaTeX sources of) Table 3 on p.3
######################################################################

tests <- list(
    prop.test(x = c(nrow(ll[ll$chars.diff>0,]), nrow(oo[oo$chars.diff>0,])),
              n = c(nrow(ll[ll$chars.diff!=0,]), nrow(oo[oo$chars.diff!=0,])),
              correct=FALSE),
    prop.test(x = c(nrow(ll[ll$syllables.diff>0,]), nrow(oo[oo$syllables.diff>0,])),
              n = c(nrow(ll[ll$syllables.diff!=0,]), nrow(oo[oo$syllables.diff!=0,])),
              correct=FALSE),
    prop.test(x = c(nrow(ll[ll$words.diff>0,]), nrow(oo[oo$words.diff>0,])),
              n = c(nrow(ll[ll$words.diff!=0,]), nrow(oo[oo$words.diff!=0,])),
              correct=FALSE),
    prop.test(x = c(nrow(oo[oo$chars.diff>0,]), nrow(rr[rr$chars.diff>0,])),
              n = c(nrow(oo[oo$chars.diff!=0,]), nrow(rr[rr$chars.diff!=0,])),
              correct=FALSE),
    prop.test(x = c(nrow(oo[oo$syllables.diff>0,]), nrow(rr[rr$syllables.diff>0,])),
              n = c(nrow(oo[oo$syllables.diff!=0,]), nrow(rr[rr$syllables.diff!=0,])),
              correct=FALSE),
    prop.test(x = c(nrow(oo[oo$words.diff>0,]), nrow(rr[rr$words.diff>0,])),
              n = c(nrow(oo[oo$words.diff!=0,]), nrow(rr[rr$words.diff!=0,])),
              correct=FALSE))

tab <- matrix(c(
    tests[[1]]$estimate[2], tests[[2]]$estimate[2], tests[[3]]$estimate[2], 
    nrow(oo[oo$chars.diff!=0,]), nrow(oo[oo$syllables.diff!=0,]), nrow(oo[oo$words.diff!=0,]),
    tests[[1]]$statistic,   tests[[2]]$statistic,   tests[[3]]$statistic,
    tests[[1]]$p.value,     tests[[2]]$p.value,     tests[[3]]$p.value,
    tests[[4]]$statistic,   tests[[5]]$statistic,   tests[[6]]$statistic,
    tests[[4]]$p.value,     tests[[5]]$p.value,     tests[[6]]$p.value),
    ncol=6)

addtorow <- list()
addtorow$pos <- list(0, 0)
addtorow$command <- c("& \\multicolumn{2}{l}{no governor} & \\multicolumn{2}{c}{\\textls[50]{vs.~on the left}} & \\multicolumn{2}{c}{vs.~on the right} \\\\\n",
                      "& \\multicolumn{1}{c}{prop} & \\multicolumn{1}{c}{$N$} & \\multicolumn{1}{c}{$\\chi^2(1)$} & \\multicolumn{1}{c}{$p$} & \\multicolumn{1}{c}{$\\chi^2(1)$} & \\multicolumn{1}{c}{$p$} \\\\\n")
rownames(tab) <- c('characters','syllables','words')
print(
    xtable(tab,
           align="lrrrrrr",
           display=c("s","f","f","f","g","f","g"),
           digits=c(0,3,0,1,2,1,2),
           caption="Proportions of shorter conjuncts occurring on the left (vs.~right) in the absence of governor, in coordinations with conjuncts of different lengths",
           label="tab:basic:props:o"),
    size="\\setlength{\\tabcolsep}{4pt}",
    include.colnames=FALSE,
    add.to.row=addtorow,
    file="tab_basic_props_o.tex")



######################################################################
## Code for generating (the LaTeX sources of) Table 4 on p.11
## and (the PDF of) Figure 2 on p.11
######################################################################

cats <- c("ADJP","ADVP","NP","NX","PP","QP","S","SBAR","UCP","VP") # in both tables (L and R) occur at least 20 times; for all others, in *all three tables* fewer than 10 occurrences

ll.cats <- summary(droplevels(ll$coordination.category[ll$coordination.category %in% cats]))
rr.cats <- summary(droplevels(rr$coordination.category[rr$coordination.category %in% cats]))
oo.cats <- summary(droplevels(oo$coordination.category[oo$coordination.category %in% cats]))

ll.cats.perc <- 100*prop.table(table(ll$coordination.category))[cats]
rr.cats.perc <- 100*prop.table(table(rr$coordination.category))[cats]
oo.cats.perc <- 100*prop.table(table(oo$coordination.category))[cats]

empty <- rep("~~~~",length(cats))

tab <- data.frame(empty, oo.cats, oo.cats.perc, empty, ll.cats, ll.cats.perc, empty, rr.cats, rr.cats.perc)[,c(2,4,6,8,10,12)]
rownames(tab) <- cats
addtorow <- list()
addtorow$pos <- list(0, 0, 0)
addtorow$command <- c("& \\multicolumn{6}{c}{\\textls[1000]{governor}}  \\\\\n",
                      "& \\multicolumn{2}{r}{\\textls[600]{none}} & \\multicolumn{2}{r}{\\textls[100]{on the left}} & \\multicolumn{2}{r}{\\textls[100]{on the right}}  \\\\\n",
                      "& \\# & \\% & \\# & \\% & \\# & \\% \\\\\n")
print(
    xtable(tab,
           align="lrrrrrr",
           display=c("s","d","f","d","f","d","f"),
           digits=c(0,0,2,0,2,0,2),
           caption="Numbers (\\#) and percentages (\\%) of coordinations of different categories depending on the presence and position of the governor",
           label="tab:gov:cats"),
    include.colnames=FALSE,
    add.to.row=addtorow,
    file="tab_cats.tex")

library(Cairo)
CairoPDF(file = "cat-gov.pdf", width = 5, height = 8)

lro <- rbind(oo.cats, ll.cats, rr.cats)
rownames(lro) <- c("none","on the left","on the right")
cols <- c("gray72","gray85","gray55","gray85","gray85","gray85","gray37","gray85","gray85","gray15")
mosaicplot(lro, main="", col=cols, xlab="governor", ylab="category", las=1, border=NA, cex.axis=.5)

dev.off()



########################################################################
## Code for generating (the PDF of) Figure 1 on p.5 and for the
## multifactorial binary logistic regression analysis referred to on p.4
## Plot description translated to Polish by WS.
########################################################################

multipliers <- c(6, 2, 1)
buckets <- c(0,1,2,3,6,25) # each bucket: ( buckets[i], buckets[i+1] ]
buckets.ch <- buckets*multipliers[1]
buckets.sy <- buckets*multipliers[2]
buckets.wo <- buckets*multipliers[3]


xx.ch <- xx[0<abs(xx$chars.diff) & abs(xx$chars.diff)<=buckets.ch[length(buckets.ch)], c("governor","chars.diff")]
colnames(xx.ch) <- c("governor", "diff")
xx.sy <- xx[0<abs(xx$syllables.diff) & abs(xx$syllables.diff)<=buckets.sy[length(buckets.sy)], c("governor","syllables.diff")]
colnames(xx.sy) <- c("governor", "diff")
xx.wo <- xx[0<abs(xx$words.diff) & abs(xx$words.diff)<=buckets.wo[length(buckets.wo)], c("governor","words.diff")]
colnames(xx.wo) <- c("governor", "diff")

xx.ch$shorter <- factor(ifelse(xx.ch$diff>0,"L","R"))
xx.ch$diff <- abs(xx.ch$diff)
for (i in 2:length(buckets.ch)) {
    xx.ch$diff[xx.ch$diff > buckets.ch[i-1] & xx.ch$diff <= buckets.ch[i]] <- buckets.ch[i]
}
table(xx.ch) # checking that each bucket is sufficiently large

xx.sy$shorter <- factor(ifelse(xx.sy$diff>0,"L","R"))
xx.sy$diff <- abs(xx.sy$diff)
for (i in 2:length(buckets.sy)) {
    xx.sy$diff[xx.sy$diff > buckets.sy[i-1] & xx.sy$diff <= buckets.sy[i]] <- buckets.sy[i]
}
table(xx.sy) # checking that each bucket is sufficiently large

xx.wo$shorter <- factor(ifelse(xx.wo$diff>0,"L","R"))
xx.wo$diff <- abs(xx.wo$diff)
for (i in 2:length(buckets.wo)) {
    xx.wo$diff[xx.wo$diff > buckets.wo[i-1] & xx.wo$diff <= buckets.wo[i]] <- buckets.wo[i]
}
table(xx.wo) # checking that each bucket is sufficiently large

governors <- c("0", "L", "R")
gov.texts <- c("BRAK nadrzędnika", "Nadrzędnik po LEWEJ", "Nadrzędnik po PRAWEJ")
differences <- list("znakach"=xx.ch, "sylabach"=xx.sy, "słowach"=xx.wo)

plots <- list()
for (d in 1:length(differences)) {
    d.name <- names(differences)[d]
    cat(paste("\n---------- DIFFERENCES (BUCKETS) OF LENGTHS measured in ", d.name, " ----------\n", sep=""))
    D <- differences[[d]]
    D$shorter <- factor(D$shorter, levels=levels(D$shorter)[c(2,1)])

    # multifactorial analysis for a given measure (length counted in d.name):
    
    print(summary(mm <- glm(shorter ~ 1 + diff * governor, data=D, family=binomial, na.action=na.exclude)))
    print(drop1(mm, test="Chisq"))
    print(pairs(emmeans::emtrends(mm, ~ governor, var="diff"), adjust="none"))

    for (g in 1:length(governors)) {
        cat("\n", paste(rep("-",40), collapse=""), governors[g], paste(rep("-",40), collapse=""), "\n")
        g.name <- governors[g]

        # monofactorial analysis for a given measure (d.name) and governor presence/position (g.name):

        print(summary(m <- glm(shorter ~ 1 + diff, data=D[D$governor==g.name,], family=binomial, na.action=na.exclude)))
        print(drop1(m, test="Chisq"))
        ph <- data.frame(lend <- effect("diff", m))
        print(ph)

        plot.name <- paste(g.name,d.name,sep="/")

        m.slope <- sprintf("%.2e", summary(m)$coefficients[2,1])
        m.prob <- sprintf("%.3g", summary(m)$coefficients[2,4])
        
        if (summary(m)$coefficients[2,4] <= 0.05) colors = c("gray10","gray40")
        else colors = c("gray10","red")
        
        # In GLM variable coefficient is NOT the same as slope.
        # The regression line does not follow f(x) = ax + b, but f(x) = g(ax+b), where g(x) is a link function.
        # In case of binomial regression the link function is the sigmoid.
        # The relation is not linear, so there is no slope
        

        p <- plot(lend, type="response", ylim=c(0.4, 1), grid=TRUE,
                  key=list(corner = c(0.01, 0.98), cex=.8, col=colors,
                           text=list(lab=c(paste("współczynnik: ", m.slope, sep=""), paste("p: ", m.prob, sep="")))),
                  main=paste(gov.texts[g], " (długość w ", toupper(d.name), ")", sep=""),
                  xlab=paste("bezwzględna różnica w",d.name),
                  ylab="odsetek krótszych lewych członów"); p

        plots[[plot.name]] <- p
    }
}

## Visualize the results as Figure 1:

setwd(out_path)

library(Cairo)
CairoPDF(file = "PW23.pdf", width = 15, height = 11)
tps <- list(par.xlab.text = list(cex=0.8),
            par.ylab.text = list(cex=0.75),
            par.main.text = list(cex=.95))
plots <- plots[c(2,5,8,1,4,7,3,6,9)]
do.call(grid.arrange, c(lapply(plots, update, par.settings=tps), list(ncol=length(differences))))
dev.off()
