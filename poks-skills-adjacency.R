library(CDM)
library(sna)
str(data.fraction2)
source('lib-poks.R')

ks <- ks.init(as.matrix(data.fraction2$data), p.min=0.8)
str(ks)
ks$m
vnames<-seq(1,11,1)
#vcoord <- matrix( ,nrow = ncol(ks$m), ncol = 2)
gplot(ks$m, label = vnames, label.pos = 5)

item2<-data.fraction2$data$H02
item3<-data.fraction2$data$H03

table(item2,item3)
chisq.test(item2,item3)


ks$odds.t
ks$odds.f
cor(c(ks$odds.t),c(t(ks$odds.f)))

cor(colMeans(data.fraction1$data), rowSums(ks$m))
cor(colMeans(data.fraction1$data), colSums(ks$m))
(data.fraction1)$q.matrix

(adj1 <- cbind(ks$m, data.fraction1$q.matrix))
(adj2 <- rbind(adj1,cbind(t((data.fraction1)$q.matrix), matrix(0,5,5))))
gplot(adj2, label.pos = 5)

colnames(adj2) <- rownames(adj2)
adj2
adj2 %*% adj2

qm <- grepl('Q',colnames(adj2))
adj3 <- rowSums(adj2[,!qm]) * adj2[,qm]
