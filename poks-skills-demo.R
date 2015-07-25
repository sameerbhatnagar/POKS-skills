
#----data----
frac.data <- data.fraction2
str(frac.data$data)

#----poks----
source('lib-poks.R')
ks <- ks.init(as.matrix(frac.data$data), p.min=0.8)
ks.table <- xtable(ks$m)
digits(ks.table) <- 0
print(ks.table,floating=FALSE)

#----poks-graph----
vnames<-seq(1, nrow(ks$m) ,1)
gplot(ks$m, label = vnames, label.pos = 5)

#----poks-worked-example1----
item2<-frac.data$data$H02
item3<-frac.data$data$H03
table(item2,item3)

# cont.table <- xtable(table(item2,item3), digits = 0)
# #digits(cont.table) <-0 
# print(cont.table,floating=FALSE)

#----poks-worked-example2----
chisq.test(item2,item3)

#----qmatrix----
frac.data$q.matrix1 %>% xtable(digits = 0) %>% print



