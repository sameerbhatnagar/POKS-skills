source('~/Kit/utility.R')
source('~/Kit/Simulation/lib-poks.R')
raw <- matrix(scan('~/Kit/Simulation/Data/unix48.matrix'),48,34,byrow=T); dataset='Unix'
ks <- ks.init(raw)
ks <- ks.init.o(raw)
colSums(ks$m)
rowSums(ks$m)
colSums(raw)
m <- ks$m
a=24;b=2
c(m[a,b],m[b,a])                        # a implies b
table(a=raw[,a],b=raw[,2])
data.frame("ks.odds[a,b]"=c(ks$odds.t[a,b], ks$odds.t[b,a], ks$odds.f[a,b], ks$odds.f[b,a]), calc=c(Odds((25+1)/(40+2)), Odds((25+1)/(27+2)), Odds((2+1)/(8+2)), Odds((15+1)/(21+2))))
ks$odds.t[a,b]/ks$odds.f[a,b]
ks$odds.t[b,a]/ks$odds.f[b,a]
all.equal(ks$odds.t/ks$odds.f, t(ks$odds.t/ks$odds.f))
0
#cbind(a=raw[,a],b=raw[,b])
#table(a=m[34,],b=m[2,])            # symmetry of relations
0

*** output flushed ***
> dim(raw)
[1] 48 34
> getLowestExpEntropy(ks)
Erreur dans 1 + o : argument non numérique pour un opérateur binaire
> getLowestExpEntropy(raw[,1],ks)
Erreur dans ks.update(i, T, state, ks) : indice hors limites
De plus : Il y a eu 34 avis (utilisez warnings() pour les visionner)
> getLowestExpEntropy(raw[1,],ks)
[1] 10
> getLowestExpEntropy(raw[3,],ks)
[1] 4
> ks
*** output flushed ***
> raw[1,]
 [1] 1 1 1 0 0 1 0 0 0 1 1 0 0 0 1 0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0
> ks$state
 [1]  3.16666667  4.55555556 11.50000000  1.63157895  3.16666667  6.14285714
 [7]  2.84615385  0.92307692  1.94117647  2.12500000  6.14285714  2.12500000
[13]  1.77777778  1.63157895  1.77777778  1.94117647  0.35135135  0.47058824
[19]  0.31578947  2.84615385  1.94117647  1.77777778  1.38095238  1.27272727
[25]  0.85185185  0.61290323  0.13636364  2.33333333  0.28205128  0.31578947
[31]  0.04166667  0.31578947  0.28205128  0.08695652
> odds.to.p(ks$state)
Erreur : impossible de trouver la fonction "odds.to.p"
> OddsToP(ks$state)
 [1] 0.76 0.82 0.92 0.62 0.76 0.86 0.74 0.48 0.66 0.68 0.86 0.68 0.64 0.62 0.64
[16] 0.66 0.26 0.32 0.24 0.74 0.66 0.64 0.58 0.56 0.46 0.38 0.12 0.70 0.22 0.24
[31] 0.04 0.24 0.22 0.08
> ks.update(1,1,ks$state,ks)
 [1]          NA 12.00000000 38.00000000  1.63157895  8.75000000 18.50000000
 [7]  6.80000000  0.92307692  1.94117647  5.50000000 18.50000000  2.12500000
[13]  1.77777778  1.63157895  1.77777778  1.94117647  0.35135135  0.47058824
[19]  0.31578947  6.80000000  1.94117647  1.77777778  1.38095238  1.27272727
[25]  0.85185185  0.61290323  0.13636364  2.33333333  0.28205128  0.31578947
[31]  0.04166667  0.31578947  0.28205128  0.08695652
> OddsToP(ks.update(1,1,ks$state,ks))
 [1]        NA 0.9230769 0.9743590 0.6200000 0.8974359 0.9487179 0.8717949
 [8] 0.4800000 0.6600000 0.8461538 0.9487179 0.6800000 0.6400000 0.6200000
[15] 0.6400000 0.6600000 0.2600000 0.3200000 0.2400000 0.8717949 0.6600000
[22] 0.6400000 0.5800000 0.5600000 0.4600000 0.3800000 0.1200000 0.7000000
[29] 0.2200000 0.2400000 0.0400000 0.2400000 0.2200000 0.0800000
> sum(abs(OddsToP(ks.update(1,1,ks$state,ks))-raw[1,]))
[1] NA
> sum(abs(OddsToP(ks.update(1,1,ks$state,ks))-raw[1,]),na.rm=T)
[1] 12.81641
> sum(abs(OddsToP(ks$state,ks)-raw[1,]),na.rm=T)
Erreur dans OddsToP(ks$state, ks) : argument(s) inutilisé(s) ( ...)
> sum(abs(OddsToP(ks$state)-raw[1,]),na.rm=T)
[1] 13.42
> sum(abs(OddsToP(ks$state)-raw[1,])[2:34],na.rm=T)
[1] 13.18
> 
