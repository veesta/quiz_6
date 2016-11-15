library(tidyverse)
library(apaTables)
library(haven)

my.data <- read_csv("reg_quiz2_data.csv")

glimpse(my.data)

#correlation matrix

apa.cor.table(my.data)

apa.cor.table(my.data, filename="Table_1.doc", table.number=1)

#check curvilinear relationships 

psych::pairs.panels(as.data.frame(my.data))

#as of now, best predictor may be assessment centre ratings, r = .37

#examine single regressions - does DV2 contribute to prediction of job perf beyond DV1
#IV = jobperf
#DV1 = gma
#DV2 = con, ac, graph

#gma alone
regression.1 <- lm(aSuc ~ selfEsteem, data=my.data)
summary(regression.1)
apa.reg.table(regression.1)





regression.2 <- lm(aSuc ~ selfEsteem + PAS, data=my.data)
summary(regression.2)
apa.reg.table(regression.2)

apa.reg.table(regression.2, filename="Table_2.doc", table.number=2)






regression.3 <- lm(aSuc ~ selfEsteem + NAS, data=my.data)
summary(regression.3)
apa.reg.table(regression.3)

apa.reg.table(regression.3, filename="Table_3.doc", table.number=3)



##USE BLOCKS! 


##PART 2: Using two block regressions 

block1 = lm(aSuc ~ NAS + PAS, data=my.data)


block2 = lm(aSuc ~ NAS + PAS + selfEsteem, data=my.data)

apa.reg.table(block1,block2)

apa.reg.table(block1,block2, filename="Table_4.doc", table.number=4)

