rm(list = ls())
source('packages.R')
#----data----
fraction <- data.fraction1
head(fraction$data)
tail(fraction$data)

#----poks----
source('lib-poks-3.R')
ks <- ks.init(as.matrix(fraction$data), p.min=0.8)
ks$m

#----poks-graph----
vnames<-seq(1, nrow(ks$m) ,1)
gplot(ks$m, label = vnames, label.pos = 5)

#----poks-worked-ftable----
item5<-fraction$data$T05
item4<-fraction$data$T04
table(item4,item5)


#----poks-worked-chisquare----
chisq.test(item4,item5)

#----qmatrix----
fraction$q.matrix

#----skill-mastery-matrix----
head(as.matrix(fraction$data) %*% as.matrix(fraction$q.matrix))
tail(as.matrix(fraction$data) %*% as.matrix(fraction$q.matrix))

#----skill-mastery-matrix-norm----
skill.mast.data0 <- ((as.matrix(fraction$data) %*% (as.matrix(fraction$q.matrix))))/((matrix(1, nrow(fraction$data), ncol(fraction$data)) %*% fraction$q.matrix))
head(skill.mast.data0) %>% round(2)
tail(skill.mast.data0) %>% round(2)

#----poks-skills----
skill.mast.data.round <- skill.mast.data0 %>% round
ks.skills <- ks.init(as.matrix(skill.mast.data.round), p.min=0.99)
ks.skills$m


#----poks-skills-graph----
vnames<-seq(1, nrow(ks.skills$m) ,1)
gplot(ks.skills$m, label = vnames, label.pos = 5)
