rm(list = ls())
source('packages.R')
##----generate-synth-data-with-defined-prereq-skills----

#each row in the code below is a skill, and there are 12 items 
q <- t(matrix(c(1,1,1,0,0,0,0,0,0,0,0,0,
              0,0,0,1,1,1,0,0,0,0,0,0,
              0,0,0,0,0,0,1,1,1,0,0,0,
              0,0,0,0,0,0,0,0,0,1,1,1),
            nrow = 12, ncol = 4))

#each row in the code below is a student, and there are 4 skills
# -these proto students are meant to represent a skill structure where
#   1->2, 1->3, 3->4
skill.mast.synth.proto <- t(matrix(c(1,1,1,1,
                                   1,1,0,0,
                                   1,0,1,0,
                                   0,0,1,1,
                                   0,0,0,1,
                                   0,1,1,1,
                                   0,1,0,1),
                                 nrow = 4, ncol = 7))

# take prototypical students and replicate to achieve statistical significance in POKS structure induction
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

