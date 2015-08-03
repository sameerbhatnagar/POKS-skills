#############################################################################
# Optimized version of 2010.02.24
# 2015.07.28 change: !raw to 1-raw in "ans.cp.f <- round(replace(!raw, is.na(raw), 0))"

# Author: Michel Desmarais
#############################################################################

# Adjacency matrix m is directed row->col
ks.init <- function(raw, alpha.c=.25, alpha.p=.25, p.min=.5) {
  s.less.na <- colSums(!is.na(raw))
  raw.sum <- colSums(raw, na.rm=T)
  p <- (raw.sum+1)/(s.less.na+2)
  odds <- p/(1-p)
  ans.cp.t <- replace(raw, is.na(raw), 0) # answers for crossprod computations of success
  ans.cp.f <- round(replace(1-raw, is.na(raw), 0)) # answers for crossprod computations of failures
  ft <- array(c(crossprod(ans.cp.t,ans.cp.t), # frequency table of TT, TF, FT, and FF
                # f11, f21, f12, f22
                crossprod(ans.cp.t,ans.cp.f),
                crossprod(ans.cp.f,ans.cp.t),
                crossprod(ans.cp.f,ans.cp.f)),
              c(ncol(ans.cp.t), ncol(ans.cp.t), 4)) + 1 # Laplace correction of + 1
  condp.t <- (ft[,,1]) / (ft[,,1]+ft[,,3]) # P(row|col)
  condp.f <- (ft[,,2]) / (ft[,,2]+ft[,,4]) # P(row|!col)
  odds.t <- Odds(condp.t)                  # O(row|col)
  odds.f <- Odds(condp.f)                  # O(row|!col)
  state=odds
  #  or <- list(t=odds.t/odds, f=odds.f/odds) # something to try (doesn't get exactly same result)
  # Start computing interaction test based on approximation of SE of log.odds.ratio : \sqrt(\sum_i 1/n_i)
  log.odds.ratio <- log((ft[,,1] * ft[,,4])/(ft[,,2] * ft[,,3]))
  log.odds.se <- sqrt((1/ft[,,1] + 1/ft[,,2] + 1/ft[,,3] + 1/ft[,,4]))
  log.odds.p <- 2 * pnorm(- abs(log.odds.ratio) / log.odds.se) # two-tail test for a normal distribution
  # log.odds.interaction is a matrix of the pairs that passed the interaction test
  log.odds.interaction <- (log.odds.p < alpha.c)
  m.rel <- log.odds.interaction
  diag(m.rel) <- F
  # Compute P(B=1|A=1)
  a1 <- (ft[,,1]+ft[,,3])-2             # substract Laplace correction
  b1a1 <- (ft[,,1])-1                   # substract Laplace correction
  # apply binom.test to slots that passed the interaction test
  p.b1a1.v <- apply(cbind(b1a1[m.rel], a1[m.rel]),
                    1,              # by row
                    function(n.k) pbinom(n.k[1], n.k[2], p.min))
  # p.b1a1.v is a vector and now we need a matrix 
  p.b1a1 <- matrix(F, ncol(m.rel), ncol(m.rel))
  # Why is this '>' here and below??  Should be corrected by inverting the ratio.
  p.b1a1[m.rel] <- p.b1a1.v > alpha.p                 # matrix is re-indexed by m
  # Repeat for p.a0b0 (P(A=0|B=0)
  # Compute P(A=0|B=0)
  a0 <- (ft[,,4]+ft[,,3])-2           # substract Laplace correction
  a0b0 <- (ft[,,4])-1                 # substract Laplace correction
  p.a0b0.v <- apply(cbind(a0b0[m.rel], a0[m.rel]),
                    1,              # by row
                    function(n.k) pbinom(n.k[1], n.k[2], p.min))
  # p.a0b0.v is a vector and now we need a matrix 
  p.a0b0 <- matrix(F, ncol(m.rel), ncol(m.rel))
  p.a0b0[m.rel] <- p.a0b0.v  > alpha.p               # matrix is re-indexed by m
  # The relation matrix is the combination of both tests (given that the interaction test is
  # already taken into account) and we put it in integer format for backward compatibility.
  # Transpose is also for backward compatibility
  m.rel <- t(round(p.a0b0 & p.b1a1))
  # note: variation qui devrait en theorie etre meilleure mais, en fait, n'apporte aucune difference
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
       alpha.c=alpha.c, alpha.p=alpha.p, odds.t=odds.t, odds.f=odds.f, or=or, p.min=p.min, nlinks=nlinks, log.nlinks=log.nlinks, nl.for=rowSums(m.rel, na.rm=T), nl.bak=colSums(m.rel, na.rm=T))
}

# convert probability to odds
Odds <- function (p) { p / (1 - p); }

