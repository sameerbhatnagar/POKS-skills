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

#----replace-missing-values-with-zeros----
fraction$data[is.na(fraction$data)] <-0
fraction$data <- as.matrix(fraction$data)

#----skill-mastery-success-fail----
skill.succ <- as.matrix(fraction$data) %*% as.matrix(fraction$q.matrix)
skill.fail <- as.matrix(1-fraction$data) %*% as.matrix(fraction$q.matrix)

#----skill-mastery-matrix----
skill.mast.data <- skill.succ/(skill.succ+skill.fail)

head(skill.mast.data)
tail(skill.mast.data)


#----poks-skills----
ks.skills <- ks.init(as.matrix(skill.mast.data), p.min=0.9)
ks.skills$m

#----poks-skills-graph----
vnames<-seq(1, nrow(ks.skills$m) ,1)
gplot(ks.skills$m, label = vnames, label.pos = 5)
