rm(list = ls())

q <- t(matrix(c(1,1,1,0,0,0,0,0,0,0,0,0,
              0,0,0,1,1,1,0,0,0,0,0,0,
              0,0,0,0,0,0,1,1,1,0,0,0,
              0,0,0,0,0,0,0,0,0,1,1,1),
            nrow = 12, ncol = 4))

skill.mast.synth.proto <- matrix(c(1,1,1,0,
                                   1,1,0,0,
                                   1,0,1,1,
                                   1,0,0,1),
                                 nrow = 4, ncol = 4)

skill.mast <- do.call(rbind, replicate(10, skill.mast.synth.proto, simplify=FALSE))

data.syn <- skill.mast %*% q

#----poks----
source('lib-poks-3.R')
ks <- ks.init(as.matrix(data.syn), p.min=0.5)
ks$m

#----poks-graph----
vnames<-seq(1, nrow(ks$m) ,1)
gplot(ks$m, label = vnames, label.pos = 5)

#----skill-mastery-success-fail----
skill.succ <- data.syn %*% t(q)
skill.fail <- as.matrix(1-data.syn) %*% t(q)

#----skill-mastery-matrix----
skill.mast.data <- skill.succ/(skill.succ+skill.fail)

#----poks-skills----
ks.skills <- ks.init(as.matrix(skill.mast.data), p.min=0.5)
ks.skills$m

#----poks-skills-graph----
vnames<-seq(1, nrow(ks.skills$m) ,1)
gplot(ks.skills$m, label = vnames, label.pos = 5)

