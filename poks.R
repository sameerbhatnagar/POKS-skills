source('~/Kit/Simulation/lib.R')
source('~/Kit/Simulation/lib-poks.R')

#############################################################################
# Data loading
#############################################################################
raw <- matrix(scan('~/Kit/Simulation/Data/unix48.matrix'),48,34,byrow=T); dataset='Unix'
# must do rounding because pbinom will choke on non integers in this file (could also fix this by rounding the crossprod in init.ks
raw <- matrix(round(scan('~/Kit/Simulation/Data/Vomlel/vomlel.matrix')),149,20,byrow=T); dataset='Arithmetic'
raw <- matrix(scan('~/Kit/Simulation/Data/flc.matrix'),42,160,byrow=T); dataset='Arithmetic'

# testing
(ks <- ks.init(raw, alpha.c=.15, alpha.p=.2))$nlinks
(ks0 <- ks.init(raw, alpha.c=.15, alpha.p=.2))$nlinks
(ks2 <- ks.init.o(raw, alpha.c=.15, alpha.p=.2))$nlinks
ks0$nlinks/ks2$nlinks
ks2$nlinks/ks0$nlinks
library(utils)
Rprof(filename='temp')
system.time(sapply(1:10,function(i)ks.init(raw, alpha.c=.15, alpha.p=.2)))
system.time(sapply(1:10,function(i)ks.init.o(raw, alpha.c=.15, alpha.p=.2)))
Rprof(NULL)
summaryRprof('temp')
ks.update.vector(c(rep(1,33),NA), ks$state, ks)
ks.update.vector(c(NA,rep(1,33)), ks$state, ks)
sample(c(1,0,NA), 34, replace=T)
ks.update.vector(sample(c(1,0,NA), 34, replace=T), ks$state, ks)
0
#############################################################################
## 2013 : Estimate by convergence
#############################################################################
dim(raw)
raw
ks.v <- lapply(1:nrow(raw), function(i) ks.init(raw[-i,], alpha.c=.15, alpha.p=.2))
set.na <- function(v, i) { v[i] <- NA; return(v) }
estimate.item.outcome <- function(obs, ks, i) ks.update.vector(set.na(obs, i), obs, ks)[i]
sapply(1:ncol(raw), function(i) estimate.item.outcome(raw[1,], ks, i))
r <- t(sapply(1:nrow(raw), function(j) sapply(1:ncol(raw), function(i) estimate.item.outcome(raw[j,], ks.v[[j]], i))))
mean(round(r>1) == raw)
r2 <- t(sapply(1:nrow(r), function(j) sapply(1:ncol(r), function(i) estimate.item.outcome(r[j,], ks.v[[j]], i))))
mean((r2>1) == (r>1))
r3 <- t(sapply(1:nrow(r2), function(j) sapply(1:ncol(r2), function(i) estimate.item.outcome(r2[j,], ks.v[[j]], i))))
mean((r2>1) == (r3>1))
r4 <- t(sapply(1:nrow(r3), function(j) sapply(1:ncol(r3), function(i) estimate.item.outcome(r3[j,], ks.v[[j]], i))))
mean((r3>1) == (r4>1))
r5 <- t(sapply(1:nrow(r4), function(j) sapply(1:ncol(r4), function(i) estimate.item.outcome(r4[j,], ks.v[[j]], i))))
mean((r4>1) == (r5>1))
r6 <- t(sapply(1:nrow(r5), function(j) sapply(1:ncol(r5), function(i) estimate.item.outcome(r5[j,], ks.v[[j]], i))))
mean((r5>1) == (r6>1))
mean((r2>1) == (raw))
## doesn't converge
library(NMF)
raw.nmf <- nmf(raw[-1,], 9)
(raw.nmf@fit@H) %*% raw[1,]
dim(raw.nmf@fit@H)
summary(raw.nmf)
((raw.nmf@fit@W) %*% raw.nmf@fit@H)
norm(((raw.nmf@fit@W) %*% raw.nmf@fit@H) - (raw[-1,]), 'F')
norm(((raw.nmf@fit@W) %*% raw.nmf@fit@H) - (raw[-1,]), 'F')/prod(dim(raw))
sum((((raw.nmf@fit@W) %*% raw.nmf@fit@H) - (raw[-1,]))^2)
0
#############################################################################
# 2008
r <- simulation(raw, biased=T, alpha.c=.25, alpha.p=.5, keep.na=T, choice.fn=getMostConnectedAndUncertainItem)
r2 <- simulation(raw, biased=T, alpha.c=.25, alpha.p=.5, keep.na=T, choice.fn=getHighestHeuristicEntropy)
r2 <- simulation(raw, biased=T, alpha.c=.25, alpha.p=.5, keep.na=T, choice.fn=getLowestExpEntropy)
r3 <- simulation(raw, biased=T, alpha.c=.25, alpha.p=.5, keep.na=T, choice.fn=getLowestExpEntropy.corrected)

par(mfrow=c(1,2))
make.graphi(list(replace(r,is.na(r),1),r),c('NA=1','NA=NA'),title=paste('Items - ',dataset,sep=''),ylim=c(.7,1),col=c('red','blue'))
plot.sim.results(list(r,r2),lab=c('r','r2'))
lines(0:(dim(rs)[1]),rowMeans(colMeans(rs,na.rm=T)),col='orange2',lwd=2)
lines(0:(dim(rs)[1]),rowMeans(colMeans(replace(rs,is.na(rs),1))),col='blue2',lwd=2)

# temp
sum(c(ks$m[1,],ks$m[,1]))
ks$nlinks[1]

tempa <- ks$odds
tempa[sample(1:34,5)] <- NA
odds.entropy.vector(tempa)
temp <- t(ks$m) * odds.entropy.vector(tempa)    # forward, T
temp
cor(colSums(temp,na.rm=T),abs(.5-ks$p))
colSums(temp,na.rm=T) * tempa
temp[,1:2]
getHeuristicEntropy(tempa, ks)
tempa
cor(getHeuristicEntropy(tempa, ks),ks$p)
getHighestHeuristicEntropy(tempa, ks)
(ks$m[,1:2]) * entropy.vector(ks$p)     # backward, F
0
# simulation with splitted data and n-fold
rs <- rs.u <- simulation.split.n(10, t(matrix(scan('Data/unix48.matrix'),34,48)), n.train=38, choice.fn=getMostConnectedAndUncertainItem, keep.na=T)
rs <- rs.r.u <- simulation.split.n(10, t(matrix(scan('Data/unix48.matrix'),34,48)), n.train=38, choice.fn=getRandom, keep.na=T)
rs <- rs.v <- simulation.split.n(10, t(matrix(scan('Data/vomlel.matrix'),20,149)), n.train=120, choice.fn=getMostConnectedAndUncertainItem, keep.na=T)
rs <- rs.f <- simulation.split.n(10, t(matrix(scan('Data/flc.matrix'),160,42)), n.train=32, choice.fn=getMostConnectedAndUncertainItem, keep.na=T)
rs <- rs.m02 <- simulation.split.n(2, as.matrix(read.table('Data/poly-juin2000.Rtable')), n.train=400, choice.fn=getMostConnectedAndUncertainItem, keep.na=T)
# using log-odds metric
postscript(file="temp.eps",width=6,height=6,paper='special',pointsize=14,horizontal=F)
plot((0:(dim(rs.u)[1]))/(dim(rs.u)[1]),entropy.vector(rowMeans(colMeans(replace(rs.u,is.na(rs.u),1)))),xlab='Items observed proportion',ylab='Entropy',ylim=c(0,.7),col='blue2',lwd=2,pch=1,type='l',lty=1)
lines((0:(dim(rs.u)[1]))/(dim(rs.u)[1]),entropy.vector(rowMeans(colMeans(rs.u,na.rm=T))),col='blue2',lwd=2,pch=1,type='b',lty=1)
lines((0:(dim(rs.r.u)[1]))/(dim(rs.r.u)[1]),entropy.vector(rowMeans(colMeans(rs.r.u,na.rm=T))),col='blue2',lwd=2,pch=1,type='l',lty=4)
lines((0:(dim(rs.v)[1]))/(dim(rs.v)[1]),entropy.vector(rowMeans(colMeans(rs.v,na.rm=T))),col='orange3',lwd=2,pch=1,type='b',lty=1)
lines((0:(dim(rs.v)[1]))/(dim(rs.v)[1]),entropy.vector(rowMeans(colMeans(replace(rs.v,is.na(rs.v),1)))),col='orange3',lwd=2,pch=1,type='l',lty=1)
lines((0:(dim(rs.f)[1]))/(dim(rs.f)[1]),entropy.vector(rowMeans(colMeans(rs.f,na.rm=T))),col='orange4',lwd=2,pch=1,type='b',lty=1)
lines((0:(dim(rs.m02)[1]))/(dim(rs.m02)[1]),entropy.vector(rowMeans(colMeans(rs.m02,na.rm=T))),col='blue4',lwd=2,pch=1,type='b',lty=1)
lines((0:(dim(rs.m02)[1]))/(dim(rs.m02)[1]),entropy.vector(rowMeans(colMeans(replace(rs.m02,is.na(rs.m02),1)))),col='blue4',lwd=2,pch=1,type='l',lty=1)
  abline(h=axTicks(side=2),col=gray(0.7))
  abline(v=axTicks(side=1),col=gray(0.7))
dev.off()


#############################################################################
# test with fully connected net
#############################################################################
# initialization: replace ks.m <- ks.list[[2]] by:
m <- matrix(1,ncol(raw),ncol(raw))
diag(ks.m) <- 0

#############################################################################
## Exploratory analysis
#############################################################################
system.time(r <- simulation(raw, biased=T))

r <- simulation(raw, biased=T, choice.fn=getLowestExpEntropy.corrected, alpha.c=.15, alpha.p=.2)
r <- simulation(raw, biased=F, choice.fn=getHighestEntropyItem, alpha.c=.15, alpha.p=.2)
r <- simulation(raw, biased=F, no.propag=T, choice.fn=getRandom)
r <- simulation(raw, biased=F)
r <- simulation(raw, biased=F, alpha.c=.15, alpha.p=.2)
r2 <- simulation(raw, biased=F, alpha.c=.15, alpha.p=2)
r3 <- simulation(raw, biased=F, choice.fn=getHighestEntropyItem, alpha.c=.15, alpha.p=.2)
r4 <- simulation(raw, biased=F, choice.fn=getHighestEntropyItem, alpha.c=.15, alpha.p=2)

r <- simulation(raw, biased=F, alpha.c=.25, alpha.p=2)
r2 <- simulation(raw[sample(1:floor(ncol(raw)*.75)),], biased=F, alpha.c=.25, alpha.p=2)
r3 <- simulation(raw[sample(1:floor(ncol(raw)*.5)),], biased=F, alpha.c=.25, alpha.p=2)
r4 <- simulation(raw, biased=F, alpha.c=.25, alpha.p=.2)

r <- simulation(raw, biased=F, alpha.c=.25, alpha.p=2)
r2 <- simulation(raw, biased=F, alpha.c=.35, alpha.p=2)
r3 <- simulation(raw, biased=F, alpha.c=.15, alpha.p=2)
r4 <- simulation(raw, biased=F, alpha.c=.10, alpha.p=2)

r <- simulation(raw, biased=T, choice.fn=getLowestExpEntropy.corrected, alpha.c=.15, alpha.p=.2)
system.time(r2 <- simulation(raw, biased=T, alpha.c=.15, alpha.p=.2))
r3 <- simulation(raw, biased=T, choice.fn=getMostConnectedAndUncertainItem, alpha.c=.15, alpha.p=.2)
r4 <- simulation(raw, biased=F, choice.fn=getMostConnectedAndUncertainItem, alpha.c=.15, alpha.p=.2)

#############################################################################
# 2006

r <- r2 <- r3 <- r4
lines(0:(dim(r))[1],colMeans(colMeans(r3)), type='l', col='orange2',lwd=3)
make.graphi(list(r,r2,r3,r4),c('r','r2','r3','r4'),title=paste('Items - ',dataset,sep=''), ,ylim=c(.7,1),col=c('red','blue','black','magenta'))
lines(c(0,(dim(r))[1]),c(colMeans(colMeans(r))[1],1), col='black') # diagonal
dev.off()

postscript(file="temp.eps",width=6,height=6,paper='special',pointsize=14,horizontal=F)
make.graphi(list(r,r2,r4),c('r','r2','r4'),title=paste('Items - ',dataset,sep=''), ,ylim=c(.5,1),col=c('red','blue','black','magenta'))
make.graphi(list(r,r2,r4),c('random 102','random 75','Unix'),title=paste('Items - ',dataset,sep=''), ,ylim=c(.5,1),col=c('red','blue','black','magenta'))

# FLC
# The processing for FLC is different from the others because it uses different train/test data sets
# see also generate FLC data below
# ks <- ks.init(raw.train, alpha.c=.5, alpha.p=.5)
dset.size=9
simulation.cross.files <- function(i, nsubj.train, nsubj.test, nitems, ...) {
  raw.test <- matrix(scan(paste('~/Kit/Simulation/Data/Flc/Flc.test/flc',i,'.test.matrix',sep='')),nsubj.test,nitems,byrow=T)
  raw.train <- matrix(scan(paste('~/Kit/Simulation/Data/Flc/Flc.train/flc',i,'.train.matrix',sep='')),nsubj.train,nitems,byrow=T)
  simulation.cross(raw.train, raw.test, ...)
}
system.time(simulation.cross.files(1, 30, 12, 30)
r1 = array(sapply(1:dset.size, function(i) simulation.cross.files(i, 30, 12, 30, choice.fn=getHighestEntropyItem)),c(30,12,31,9))
r.compatible = aperm(r1, c(1,3,2,4))
lines(0:(dim(r1))[1],sapply(1:31,function(i)mean((r.compatible)[,i,,])),col='orange2') # when other stuff is displayed
lines(c(0,30),c(mean(r.compatible[,1,,]),1),col='white')
plot.ci.norm(r.compatible,offset=.1)

make.graphi(list(r),c('r','r2','r3','r4'),title=paste('Items - ',dataset,sep=''), ylim=c(.6,1),col=c('red','blue','black','magenta'))

# unused
r.cum=array(0,c(30,31,12))
for (i in 1:dset.size) {
  r.temp <- (array(scan(paste('BN/Results/K2_Learned/Span1/flc',i,'.dat',sep='')),c(30,31,12)))
  r.cum = r.cum + r.temp
}
r2 = r.cum/dset.size

#############################################################################
## Code for presentation in Boston
#############################################################################
postscript(file="temp.eps",width=12,height=6,paper='special',pointsize=14,horizontal=F)
par(mfrow=c(1,2))

raw <- matrix(scan('~/Kit/Simulation/Data/unix48.matrix'),48,34,byrow=T)
detach(ks.param)
ks.param <- ks.init(raw) # Unix
attach(ks.param)
bias=F
unix.poks <- simulation(raw, biased=bias, alpha.c=.15, alpha.p=.2, choice.fn=getMostConnectedAndUncertainItem)
unix.rand <- simulation(raw, biased=bias, choice.fn=getRandom)
unix.ent.nopropag <- simulation(raw, biased=bias, choice.fn=getHighestEntropyItem, no.propag=T)
make.graphi(list(unix.poks,unix.rand,unix.ent.nopropag),c('POKS','Aléatoire','Sans inférence'),title='Items - Unix',ylim=c(.6,1),col=c('red','blue','black'))
lines(0:34,colMeans(colMeans(unix.poks)),col='black')

            X11()
raw <- matrix(scan('~/Kit/Simulation/Data/Vomlel/vomlel.matrix'),149,20,byrow=T)
detach(ks.param)
ks.param <- ks.init(raw, alpha.c=.25, alpha.p=.5, p.min=.5)
attach(ks.param)
vomlel.poks <- simulation(raw, biased=bias, alpha.c=.25, alpha.p=.5, p.min=.5, choice.fn=getMostConnectedAndUncertainItem)
vomlel.rand <- simulation(raw, biased=bias, choice.fn=getRandom)
vomlel.ent.nopropag <- simulation(raw, biased=bias, choice.fn=getHighestEntropyItem, no.propag=T)
make.graphi(list(vomlel.poks,vomlel.rand,vomlel.ent.nopropag),c('POKS','Aléatoire','Sans inférence'),title='Items - arithmetique',ylim=c(.6,1),col=c('red','blue','black'))
dev.off()
lines(0:20,colMeans(colMeans(vomlel.poks)),col='black')

lines(0:20,colMeans(f1(r3[,21,],r3)),lwd=3,col='blue')

#############################################################################
# Naive Bayes version
#############################################################################
ks.seq <- apply(raw,1,function(subj) (simulation.subj(subj,state)[1]))
ks.seq[[24]]

prod(c(condp.t[c(4,15),1], 1-condp.t[c(2),1])) > prod(c(1-condp.t[c(4,15),1], condp.t[c(2),1]))
naive.obs <- function (subj.ans, obs.vec) {
  success <- (condp.t[which(subj.ans & obs.vec),])
  failure <- (condp.t[which(!subj.ans & obs.vec),])
  isSuccess <- prodColumn(success) * prodColumn(1-failure)
  isFailure <- prodColumn(1-success) * prodColumn(failure)
  result <- rbind(isSuccess,isFailure)
  result[,obs.vec] <- NA
  result
}
obs.vec <- function(index) {m <- matrix(F,1,34); m[index] <- T; m}
obs.vec(c(2,4,5))
naive.obs(raw[24,],obs.vec(c(2,4,15)))

result.naive <- array(
      sapply(1:s, function(i) {
        s=(ks.seq[[i]])
        cbind(round(p)==raw[i,],
              sapply(1:q, function (j) {
                apply(naive.obs(raw[i,], obs.vec(s[1:j,])), 2, function(x) x[1]>x[2]) == raw[i,]}))}),
      c(q,q+1,s))
colMeans(replace(result.naive,is.na(result.naive),1))
lines(rowMeans(colMeans(replace(result.naive,is.na(result.naive),1))))
lines(c(1,35),c(mean(colMeans(round(p)==t(raw))),1),col='red')

apply(raw, 1, function (subj) apply(naive.obs(subj,obs.vec(c(2,4,15))),2,function(x) x[1]>x[2]) == subj)

apply(naive.obs(raw[24,],obs.vec(c(2,4,15))),2,function(x) x[1]>x[2]) == raw[24,]

sapply(1:48, function(i) {
  obs = (ks.seq[[i]])[1:i,]
  apply(raw, 1, function (subj) apply(naive.obs(subj,obs.vec(ks.seq)),2,function(x) x[1]>x[2]) == subj)
})


r=sapply(1:34,function(i){
  s=raw[2,];
  apply(naive.obs(s,obs.vec(1:i)),2,function(x) x[1]>x[2])==s})
foo=colMeans(replace(r,is.na(r),1));foo
naive.simul <- function(raw) {
  sapply(1:nrow(raw), function (subj) {
    r <- sapply(1:ncol(raw),function(i){s=raw[2,];apply(naive.obs(s,obs.vec(1:i)),2,function(x) x[1]>x[2])==s})
    compar <- colMeans(replace(r,is.na(r),1))
  })
}
naive.simul(raw)
0

#############################################################################
# identify the differences between inferred structures
temp <- matrix(scan("temp"),531,8,byrow=T) # temp contains ks with and->1 1 and impl->1 0
for (i in 1:dim(foo)[1]) if(length(temp[(temp[,1]==foo[i,1]&temp[,2]==foo[i,2]),]) == 0) tempa <- cbind(tempa,(foo[i,]))

# vomlel
# (subject.data in poks.build is not in the right dimension)

attach(vomlel.full)
foo <- subject.data[,1:10]
r <- poks.build(foo)
subject.data <- read.table("Data/unix.table",header=T)
impl(ftable(u[,1],u[,12]),alpha.c=.5)

sapply(concepts["CD"][[1]],function(i) impl(ftable(cbind(vomlel.full["CD"],vomlel.full[i]))))
x <- sapply(concepts.names,function(j) sapply(concepts[j][[1]],function(i) {r <- impl(ftable(cbind(vomlel.full[j],vomlel.full[i])));print(r);return(r)}))
concepts.names

attach(vomlel.full)
ft <- ftable(ACL,CL)
impl(ft)

poks.propagate.forward(ft,mean(CL))
mean(CL)


#############################################################################
## Not used
#############################################################################
poks.propagate.forward <- function(ft, p.initial)  OddsToP(PToOdds(p.initial) * (ft[4]*(ft[1]+ft[2])) / (ft[2]*(ft[3]+ft[4])))
poks.propagate.odds.forward <- function(ft, o.initial)  o.initial * (ft[4]*(ft[1]+ft[2])) / (ft[2]*(ft[3]+ft[4]))


#############################################################################
#############################################################################
# pour verification
foo <- matrix(scan('Data/unix48.relmatrix'),589,2,byrow=T) # unix48.cmd sous forme matricielle
u.m <- matrix(0,34,34)
for (i in 1:589) u.m[foo[i,1],foo[i,2]] <- 1
sum(abs(m[1,] - u.m[1,]))            # aucune diff pour 1->x
sum(abs(m - u.m))                    # 34 de differents
u <- array(scan('Data/unix48.results.p'), c(34,(34+1),48))
u[,1,1]-p
u[,1,34]-p
# subject 1 with node 4 = F
u.m[,4]
(u[,2,1]-p)
(u[,2,1]-p)[m[,4]==0]
(u[,2,1]-p)[m[,4]==1]
which(m[,4]==0)
u[,2,1]
p
X11()
par(mfrow=c(2,1))
plot((u[,2,1]-otp(update(4,0,state))),type='h',col='red',lwd=2)
lines((which(m[4,]==0))+.2,(u[,2,1]-otp(update(4,0,state)))[m[4,]==0],type='h',lwd=2,col='orange')
lines((which(m[4,]==1))+.4,(u[,2,1]-otp(update(4,0,state)))[m[4,]==1],type='h',lwd=2,col='green')
plot(log(ks.update(4,1,state)/odds(u[,2,3])),type='h',col='red',lwd=2,ylim=c(-3,3))
lines(1:34+.2,log(ks.update(4,0,state)/odds(u[,2,1])),type='h',col='blue',lwd=2)
# subject 1 with node 4 = F
u.m[4,]
u[,2,3]
(u[,2,3]-p)
(u[,2,3]-p)[m[4,]==0]
u.m[4,]-m[4,]
(u[,2,3]-p)[m[4,]==0]
(u[,2,3]-p)[m[4,]==1]
which(m[4,]==0)
p
u[,2,3]
par(mfrow=c(2,1))
plot((u[,2,3]-otp(ks.update(4,1,state))),type='h',col='red',lwd=2)
lines((which(m[,4]==0))+.2,(u[,2,3]-otp(ks.update(4,1,state)))[m[,4]==0],type='h',lwd=2,col='orange')
lines((which(m[,4]==1))+.4,(u[,2,3]-otp(ks.update(4,1,state)))[m[,4]==1],type='h',lwd=2,col='green')
plot(log(ks.update(4,1,state)/odds(u[,2,3])),type='h',col='red',lwd=2,ylim=c(-3,3))
lines(1:34+.2,log(ks.update(4,0,state)/odds(u[,2,1])),type='h',col='blue',lwd=2)

plot(log(ks.update(4,1,state)/odds(u[,2,3])),type='h',col='red',lwd=2,ylim=c(-3,3))
lines(1:34+.2,log(ks.update(25,0,ks.update(4,1,state))/odds(u[,3,3])),type='h',col='blue',lwd=2)
lines(1:34+.4,log(ks.update(22,1,ks.update(25,0,ks.update(4,1,state)))/odds(u[,4,3])),type='h',col='green',lwd=2)
# fin des verif

#############################################################################
## generate FLC data
flc.samples = matrix(scan('Data/flc-samples.dat'),30,10)
flc.data =  matrix(scan('Data/flc.matrix'),160,42)
dim(flc.samples)
dim(flc.data)

set.seed(333)
for (i in 1:10) {
  v.train = vector('logical',42)
  v.train[sample(42,30)] = T
  flc.train = flc.data[flc.samples[,i],v.train]
  flc.test = flc.data[flc.samples[,i],!v.train]
  write(flc.test, file=paste('Data/Flc/Flc.test/flc',i,'.test.matrix',sep=''), ncolumns=30)
  write(flc.train, file=paste('Data/Flc/Flc.train/flc',i,'.train.matrix',sep=''), ncolumns=30)
}
0

#############################################################################
## old stuff
bar <- apply(raw,1,function(subj) (simulation.subj(subj,state,choice.fn=getHighestEntropyItemUpTo)[,2:(q+2)] > 1)==subj)
bar <- apply(raw,1,function(subj) (simulation.subj(subj,state,choice.fn=getRandom)[,2:(q+2)] > 1)==subj)
bar <- apply(raw,1,function(subj) (simulation.subj(subj,state,choice.fn=getRandom,no.propag=T)[,2:(q+2)] > 1)==subj)
bar <- apply(raw,1,function(subj) (simulation.subj(subj,state,choice.fn=getHighestEntropyItem,no.propag=T)[,2:(q+2)] > 1)==subj)
bar <- apply(raw,1,function(subj) (simulation.subj(subj,state,choice.fn=getHighestEntropyItem)[,2:(q+2)] > 1)==subj)
bar <- apply(raw,1,function(subj) (simulation.subj(subj,state)[,2:(q+2)] > 1)==subj)
foo <- array(replace(bar,is.na(bar),1),c(q,(q+1),nrow(raw)))
bar2 <- sapply(1:nrow(raw),function(i) colMeans(foo[,,i]))
plot(rowMeans(bar2),type='b',col='blue')
lines(colMeans(f1(u[,q+1,],u)),col='red')
lines(rowMeans(bar2),col='red')
lines(c(1,35),c(mean(colMeans(round(p)==t(raw))),1),col='black')
abline(h=axTicks(side=2),col=gray(0.7))
abline(v=axTicks(side=1),col=gray(0.7))

foo <- (simulation.subj(raw[3,],state)[,2:36] > 1)==raw[3,]
par(mfrow=c(1,1))
plot(colMeans(replace(foo,is.na(foo),1)),type='b')

#############################################################################
## Messing around with odds ratio
#############################################################################

m.r <- round(matrix(runif(99)>.5,ncol=3))
m.r[m.r[,2]==1,1] <- sample(c(1,1,1,1,1,1,1,0),sum(m.r[,2]==1),replace=T)
head(m.r)
m.r.o <- m.r
m.r[sample(99,10)] <- NA
r <- ks.init(m.r)
str(r)
m.r[,1:2]
(f <- ftable(a=m.r[,1],b=m.r[,2])+1)
(fo <- ftable(a=m.r.o[,1],b=m.r.o[,2])+1)
(f <- ftable(b=m.r[,2],c=m.r[,3]))+1
(f <- ftable(a=m.r[,1],c=m.r[,3])+1)
fisher.test(f)$p.value
2 * pnorm(-abs(log((f[1,1]*f[2,2])/(f[1,2]*f[2,1]))/sqrt(sum(1/c(f)))))
foo <- round(array(runif(60)>.5,c(5,3,4)))
sqrt(1/(foo[,,1] + foo[,,2] + foo[,,3] + foo[,,4]))

m.r
raw <- m.r
  s.less.na <- colSums(!is.na(raw))
  rels <- ks.list[[1]]                  # renamed from 'ks'
  m <- ks.list[[2]]
m
  raw.sum <- colSums(raw,na.rm=T)
  p <- (raw.sum+1)/(s.less.na+2)
  odds <- p/(1-p)
  ans.cp.t <- replace(raw, is.na(raw), 0) # answers for crossprod computations of success
  ans.cp.f <- round(replace(!raw, is.na(raw), 0)) # answers for crossprod computations of failures
ans.cp.t
crossprod(m.r)
  ft <- array(c(crossprod(ans.cp.t,ans.cp.t), # frequency table of TT, TF, FT, and FF
                 crossprod(ans.cp.t,ans.cp.f),
                 crossprod(ans.cp.f,ans.cp.t),
                 crossprod(ans.cp.f,ans.cp.f)),
               c(ncol(ans.cp.t), ncol(ans.cp.t), 4)) + 1 # Laplace correction of + 1
ft
  condp.t <- (ft[,,1]) / (ft[,,1]+ft[,,3]) # P(row|col)
  condp.f <- (ft[,,2]) / (ft[,,2]+ft[,,4]) # P(row|!col)
  odds.t <- Odds(condp.t)
  odds.f <- Odds(condp.f)
  state=odds
odds.t
m
  or <- list(t=t(m) * odds.t/odds,
                f=m * odds.f/odds)
or
  or$t[or$t==0] <- 1                # neutral evidence effect
  or$f[or$f==0] <- 1                # neutral evidence effect
#  or <- list(t=odds.t/odds, f=odds.f/odds) # something to try (doesn't get exactly same result)
  # Start computing interaction test based on approximation of SE of log.odds.ratio : \sqrt(\sum_i 1/n_i)
or
ft[,,1]
  log.odds.ratio <- log((ft[,,1] * ft[,,4])/(ft[,,2] * ft[,,3]))
log.odds.ratio
1/(ft[,,1])
  log.odds.se <- sqrt((1/ft[,,1] + 1/ft[,,2] + 1/ft[,,3] + 1/ft[,,4]))
log.odds.se
ft[1,1,]
  log.odds.p <- 2 * pnorm(- abs(log.odds.ratio) / log.odds.se) # two-tail test for a normal distribution
log.odds.p
sapply(1:3,function(j)apply(ft[j,,], 1, function(i) chisq.test(i)$p.value))
log.odds.p/sapply(1:3,function(j)apply(ft[j,,], 1, function(i) chisq.test(i)$p.value))
  (log.odds.interaction <- (log.odds.p < .25))

ft
2*(choose(10,2)+choose(10,1) +choose(10,0))*.5^10
2*sum(sapply(0:2,function(k)choose(10,k))*.5^10)
choose(matrix(10:18,3),matrix(0:8,3))
choose(1000,500)
n <- (ft[,,1]+ft[,,2])
k <- ft[,,1]
n <- (ft[,,4]+ft[,,2])
k <- ft[,,4]
k
n
binom.test(11,18,.5)
lower.tri(ft[,,1])
!diag(1,4)
(ft[,,1])[c(2,5,7)]
matrix(apply(cbind(c(n),c(k)), 1, function(n.k) binom.test(n.k[2], n.k[1], .5)$p.valu), 3)
m
choose(n,k)*(0.5^k)*(0.5^(n-k))
sapply(0:k, function(i) choose(n,k)*(0.5^k)*(0.5^(n-k)))
  log.odds.interaction <- (log.odds.p < .25)
# note: variation qui devrait en theorie etre meilleure mais, en fait, n'apporte aucune difference
# condp.t <- t(apply(raw,2,function(c) (colSumsRobust(raw[c==1,])+(2*p))/(raw.sum+2) )) # Kononenko (1991) citant Chestnik (1990)
log.odds.interaction
  nlinks = colSums(m,na.rm=T) + rowSums(m,na.rm=T)
  log.nlinks = 0.6931472 / (log((nlinks+1)) + 0.6931472) # 0.6931472 is the entropy of 0.5
  list(rels=rels,m=m, p=p, q=q, odds=odds, condp.t=condp.t, or=or, state=state, alpha.c=alpha.c, alpha.p=alpha.p, p.min=p.min, nlinks=nlinks, log.nlinks=log.nlinks)

p.min=.5
alpha.p=.25
  # Compute P(B=1|A=1)
ft <- ft+1
ft
(   a1 <- (ft[,,1]+ft[,,3])-2 )            # substract Laplace correction because binom.test is exact )
(   b1a1 <- (ft[,,1])-1        )           # substract Laplace correction because binom.test is exact )
   # apply binom.test to slots that passed the interaction test )
(m.rel <- log.odds.interaction)
(   p.b1a1.v <- apply(cbind(b1a1[m.rel], a1[m.rel]), 
                     1,              # by row 
                     function(n.k) pbinom(n.k[1], n.k[2], p.min)))
(   p.b1a1.v <- apply(cbind(b1a1[m.rel], a1[m.rel]), 
                     1,              # by row 
                     function(n.k) print(n.k)))
   # p.b1a1.v is a vector and now we need a matrix  )
p.b1a1.v
(   p.b1a1 <- matrix(F, ncol(m.rel), ncol(m.rel)) )
(   p.b1a1[m.rel] <- p.b1a1.v < alpha.p                 # matrix is re-indexed by m )
   # Repeat for p.a0b0 (P(A=0|B=0) )
   # Compute P(A=0|B=0) )
(   a0 <- (ft[,,4]+ft[,,3])-2  )         # substract Laplace correction because binom.test is exact )
(   a0b0 <- (ft[,,4])-1         )        # substract Laplace correction because binom.test is exact )
(   p.a0b0.v <- apply(cbind(a0b0[m.rel], a0[m.rel]), 
                     1,              # by row )
                     function(n.k) pbinom(n.k[1], n.k[2], p.min)) )
   # p.a0b0.v is a vector and now we need a matrix  )
(   p.a0b0 <- matrix(F, ncol(m.rel), ncol(m.rel)) )
(   p.a0b0[m.rel] <- p.a0b0.v  > alpha.p )               # matrix is re-indexed by m )
   # The relation matrix is the combination of both tests (given that the interaction test is )
   # already taken into account) and we put it in integer format for backward compatibility. )
   # Transpose is also for backward compatibility )
   (m.rel <- t(round(p.a0b0 & p.b1a1))) 
 # note: variation qui devrait en theorie etre meilleure mais, en fait, n'apporte aucune difference )
# condp.t <- t(apply(raw,2,function(c) (colSumsRobust(raw[c==1,])+(2*p))/(raw.sum+2) )) # Kononenko (1991) citant Chestnik (1990)
  or <- list(t=t(m.rel) * odds.t/odds,      # We only retain odds ratios of links and in the next
                                        # instructions we set the others to 1 such that it has
                                        # not influence in the computation of new evidence
                f=m.rel * odds.f/odds)
  or$t[or$t==0] <- 1                # neutral evidence effect
  or$f[or$f==0] <- 1                # neutral evidence effect
  nlinks = colSums(m.rel, na.rm=T) + rowSums(m.rel, na.rm=T)
  log.nlinks = 0.6931472 / (log((nlinks+1)) + 0.6931472) # 0.6931472 is the entropy of 0.5
  list(m=m.rel, p=p, odds=odds, condp.t=condp.t, condp.f=condp.f, or=or, state=state,
       alpha.c=alpha.c, alpha.p=alpha.p, odds.t=odds.t, odds.f=odds.f, or=or, p.min=p.min, nlinks=nlinks, log.nlinks=log.nlinks)


source('lib-poks.R')
ks.init(raw)
fisher.test(ftable(raw[,1],raw[,2]))
ks.init.o(raw)$m
ks.init.o(raw)$m == ks.init(raw)$m
ks <- ks.init(raw)
ks2 <- ks.init(raw)
table(ks$m - ks2$m)

ks.init.o(raw)$state == ks.init(raw)$state
ks.init.o(raw)$nlinks == ks.init(raw)$nlinks
ks.init.o(raw)$condp.t == ks.init(raw)$condp.t
ks.init.o(raw)$condp.f == ks.init(raw)$condp.f
ks.init.o(raw)$odds == ks.init(raw)$odds
ks.init.o(raw)$odds.t == ks.init(raw)$odds.t
ks.init.o(raw)$or$t == ks.init(raw)$or$t
alpha.c=alpha.p=.25
p.min=.5
ft
ftable(a=raw[,1],b=raw[,2])
