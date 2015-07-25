# lib-poks.R
PToOdds <- function(p) p/(1-p)
OddsToP <- function(o) o/(1+o)
entropy.system <- function(p) {q=(1-p); -( sum((p * log(p)), na.rm=T) + sum(q * log(q), na.rm=T)); }
entropy.vector <- function(p) {q=(1-p); r=-( p * log(p) + q * log(q) ); r[is.na(r)]=0;return(r); }
odds.entropy <- function (o) - sum(((o*log(o)+log(1))/(1+o) - log(1+o)),na.rm=T)
odds.entropy.vector <- function (o) {
  r = - ((o*log(o)+log(1))/(1+o) - log(1+o))
  r[is.na(r)]=0
  return(r);
}
Odds.cmd <- function(d) {if(is.null(dim(d)))return(rep(1,length(d)));p <- (colSums(d,na.rm=T)+1)/(nrow(d)+2); p/(1-p);}
Odds <- function (p) { p / (1 - p); }
prodColumn <- function(m) if(is.matrix(m)) apply(m,2,prod) else { if(is.vector(m)) m else 1}
colSumsRobust <- function(x)  if(is.matrix(x)) colSums(x,na.rm=T) else {if(is.vector(x)) x else 0}
colMeans2 <- function(x,...) colMeans(colMeans(x,...),...)

missing.item <- -1    # no data available for a student over this item
item.presented <- NA       # item has been presented to student
not.answered <- -2    # item has been presented in data but student did not answer

# example use: 
# raw <- matrix(scan('~/Kit/Simulation/Data/unix48.matrix'),48,34,byrow=T); dataset='Unix'
# (ks <- ks.init(raw, alpha.c=.15, alpha.p=.2))
# ks.update(1,F,ks$state,ks)
# Note that item.presented in current.state indicates that these items were answered (observed) and they are no more estimated
ks.update <- function(item, val, current.state, ks) {
  if(is.na(val) || is.na(current.state[item]) || item.missing(current.state[item]) || item.missing(val)) 
#    { multiplier <- 1 }                 # leave state unchanged for item.presented observed or item.presented in current state
    {
      current.state[item]=item.presented
      return(current.state)
    } # leave state unchanged for item.presented observed or item.presented in current state
  else if(val > 0.5)
    { multiplier <- (ks$or)$t[,item] } 
  else
    { multiplier <- (ks$or)$f[,item] } 

  r <- current.state * multiplier
  if(is.null(dim(r)))
    r[item] <- item.presented
  else
    r[item,] <- item.presented
  return(r)
}

#############################################################################
# API functions
#############################################################################

# history: vector of answers before current question
# q.id: name of current item in response vector
# q.outcome: 0:failure, 1:success of q.no
# ksarg: should normally not be necessary and will inherit the value given by config file
# For example: POKSapi.new.answer(list(q22=1,q3=0,q1=1), 'q59',1,ks)
# For example: POKSapi.new.answer(list(dinde2b=1,elflo7a=0), 'innut4',1,ks)
# precompute: compute the items after the next, one for a success and one for a failure

poks.api0 <- function(history, q.id, q.outcome, ksarg=ks, precompute=F) {
  QuestionNames <- names(ks$state)
  answers <- rep(item.presented, length(ks$state))
  state.v <- unlist(history)
  state.v <- state.v[intersect(names(history), names(ks$state))]
  names(answers) <- QuestionNames
  if(!is.null(state.v))
    answers[names(state.v)] <- state.v
  cur.state <- ks.update.vector(answers, ks$state, ks)
  new.state <- ks.update(which(QuestionNames==q.id), q.outcome, cur.state, ks)
  # traitement des doublons de racines
  history.rac <- extract.racines(names(history))
  if(!exists('QuestionNames.rac')) {
    QuestionNames.rac <- extract.racines(QuestionNames)
  }
  Q.dup <- duplicated(c(history.rac, QuestionNames.rac))[(length(history.rac)+1):(length(history.rac)+length(QuestionNames.rac))]
  new.state[Q.dup] <- NA
  # fin racines
  item.next <- getMostConnectedAndUncertainItem(new.state, ks)
  if(precompute) {
    next.stateT <- ks.update(item.next, 1, new.state, ks)
    next.stateF <- ks.update(item.next, 0, new.state, ks)
    item.next2T <- getMostConnectedAndUncertainItem(next.stateT, ks)
    item.next2F <- getMostConnectedAndUncertainItem(next.stateF, ks)
    return(c(item.next, item.next2T, item.next2F))
  }
  return(names(item.next))
}


# Wrapper to log into a file
poks.api.wrap <- function(history, q.id, q.outcome, ksarg=ks, precompute=F, log.file=NULL, seed=NULL) {
  if(! is.null(seed))
    set.seed(seed)
#  else
#    set.seed(as.integer(format(Sys.time(), "%s")))
  # Add q.id in history if not there
  if(!is.element(q.id, names(history))) {
    q.pair <- q.outcome
    names(q.pair) <- q.id
    history <- append(history, q.pair)
  }
  r <- poks.api1(history, q.id, q.outcome, ks, precompute)
  if(!is.null(log.file) & is.character(log.file)) {
    cat('>>', format(Sys.time(), '%s'), names(history), ';', q.id, ';', unlist(q.outcome), ';', r, '\n', file=log.file, append=T)
    if(is.element(r, append(names(history), q.id))) {
      cat(format(Sys.time(), '%s'), 'doublon : ', r, '\n', file=log.file, append=T)
      browser()
    }
  }
  return(r)
}

# version with fixed number of initial questions as determined by the length of nouvelles2010
# 'history' must contain q.id already
poks.api1 <- function(history, q.id, q.outcome, ksarg=ks, precompute=F) {
  if(length(history) < (length(nouvelles2010) + length(first21)))
    tryCatch(
             {r <- poks.api.fixe(history, q.id, q.outcome, ksarg, precompute)},
             error = function(e) {print(e);browser()}
             )
  else
    tryCatch(
             {
               if(is.na(names(ks$state[q.id]))) { # like initial state
                 q.id <- NA
                 q.outcome <- NA
               }
               r <- poks.api0(history, q.id, q.outcome, ksarg, precompute)
             },
             error = function(e) {print(e);browser()}
             )
  return(r)
}

poks.api.fixe <- function(history, q.id, q.outcome, ksarg=ks, precompute=F) {
  if(precompute) 
    n.items = 3
  else
    n.items = 1
  if(length(history) < 30) {            # fixed item 
    if(length(history) < length(first21)) { # old items
      not.asked <- setdiff(first21, names(history))
      not.asked.prob <- first21.prob[not.asked]
    } else {                            # new items
      not.asked <- setdiff(nouvelles2010, names(history))
      not.asked.prob <- nouvelles2010.prob[not.asked]
    }
    if(any(sapply(not.asked.prob, is.na)))
      {cat('NA in prob', '\n');browser(); }
    if(length(not.asked) == 0)
      {cat('no more items in fixed section', '\n');browser(); }
    tryCatch(
             {
               if(length(not.asked) < n.items)
                 replace.flag <- TRUE
               else
                 replace.flag <- FALSE
               r <- sample(not.asked, n.items, replace=replace.flag, prob=not.asked.prob)
             },
             error = function(e) {e;browser()}
             )
  } else {                              # adaptive after 31
    stop('should not get here: fixed items api')
  }
  return(r)
}  

POKSapi.new.answer <- poks.api.wrap

POKSapi.sequence.test <- function(nquest=60) {
  sr <- sample(0:1, ncol(responses)+length(nouvelles2010), replace=T)
  names(sr) <- c(colnames(responses), nouvelles2010)
  sim.new.sequence(1, sr, ks)
}

# for testing: 
sim.new.sequence <- function(nruns, subj.real.resp, ks=NULL, precompute=F, ...) {
  if(is.null(ks))
    ks <- ks.init(responses, alpha.c=.25, alpha.p=.5)
  q.new <- POKSapi.new.answer(list(), NA, NA, ks, precompute)
  q.outcome <- subj.real.resp[q.new]
  q.pair <- q.outcome
  names(q.pair) <- q.new                # q.pair will be first in history
  q.new <- POKSapi.new.answer(list(), q.new, q.outcome, ks, precompute)
  history <- as.list(q.pair)
  q.pair <- q.outcome
  q.outcome <- subj.real.resp[q.new]
  q.id <- q.new
  # now we are ready to start the loop
  for(i in 1:59) {
    q.new <- POKSapi.new.answer(history, q.id, q.outcome, ks, precompute, ...)
    # store q.id and its outcome in history
    q.pair <- q.outcome
    names(q.pair) <- q.id
    history <- append(history, as.list(q.pair))
    ## new outcome
    q.outcome <- subj.real.resp[q.new]
    if(is.na(q.outcome)) {               # maybe this fix should not be used
      q.outcome <- sample(c(0,1),1 , prob=c(q.prob[q.new], 1-q.prob[q.new]))
    }
    q.id <- q.new
    if(is.na(q.id)) {cat('q.id == NA', '\n');browser(); }
    if(is.na(q.outcome))  {cat('q.outcome == NA', '\n'); browser(); }
  }
  return(unlist(history))
}

# Extracts Item types of Poly tests as defined by their names: first sequence of letters a single integer the rest are variants
extract.racines <- function(lnoms) sapply(lnoms, function(nom) sub('([adegimtv][a-z]+[0-9]+).*','\\1', nom))


# 'vector' is the response vector where:
# 0: wrong answer, 1: good answer, item.presented, not.answered, missing.item
# Note that item.presented in current.state indicates that these items were answered (observed) and they are no more estimated
ks.update.vector <- function(vector, current.state, ks) {
  if(length(vector) != length(current.state)) {
    stop('ks.update.vector: length of response and state vector should be the same')
  }
  state <- current.state
  count = 1
  for(i in vector) {
    if(!is.na(match(i, c(0,1))))
      state <- ks.update(count, i, state, ks)
    count = 1 + count
  }
  return(state)
}

# Combine observed values with predicted and return probabilities instead of odds
ks.update.vector2 <- function(vector, current.state, ks) {
  r <- OddsToP(ks.update.vector(vector, current.state, ks))
  r[!is.na(vector)] <- vector[!is.na(vector)]
  return(r)
}

# rawScores must be a respondant-item matrix; it can correspond to a snapshot of a
# student assessment after x items; or it can be computed from raw (all responses).
# Q-matrix names must be correct (Concepts X Items)
# In the Q-matrix, missing values are considered 0.
# Compute concept mastery based on a Q-matrix aggregation
topic.score <- function(rawScores, QMatrix, verbose=F) {
  if(is.list(QMatrix)) {
    r1 <- topic.score(rawScores, QMatrix[[1]], verbose)
    QMatrix.rest <- QMatrix[-1]
    if(length(QMatrix.rest) == 1)
      return(topic.score(r1, QMatrix.rest[[1]], verbose))
    else
      return(topic.score(r1, QMatrix.rest, verbose))
  }
  if(is.null(dim(rawScores)))
    rawScores <- t(rawScores)
  commonItems <- intersect(colnames(rawScores), rownames(QMatrix))
  if(verbose==T & length(commonItems) < length(colnames(rawScores)))
    warning(cat('Missing items from Q-matrix:', setdiff(colnames(rawScores), commonItems), '\n'))
  if(verbose==T & length(commonItems) < length(rownames(QMatrix)))
    warning(cat('Missing items from scores:', setdiff(rownames(QMatrix), commonItems), '\n'))
  rawScores[is.na(rawScores)] <- 0
  QMatrix[is.na(QMatrix)] <- 0
  return(rawScores[,commonItems] %*% QMatrix[commonItems,])
}

# Scores by all topics based on IRT
ltm.topic.scores <- function(global.ltm.model, topic.ltm.models, v.response, qmatrix3) {
  if(!is.list(v.response))
     v.response <- as.list(v.response)
  r.t <- sapply(1:ncol(qmatrix3),
              function(i) ltm.score(topic.ltm.models[[i]], v.response))
  r <- c(ltm.score(global.ltm.model, as.list(v.response)), r.t)
  names(r) <- c('Glob', colnames(qmatrix3))
  return(r)
}

# Score based on an IRT model
# ltm.model: result from ltm()
# v.respon: either a vector congruent with ltm.model, or a list
# correction: adjustment to make the average around 0.5 according to statistics 2010

ltm.score <- function(ltm.model, v.respon, correction=0.9) {
  if(is.list(v.respon)) {
    respon <- matrix(NA, nrow=1, ncol=nrow(ltm.model$coefficients))
    names(respon) <- names(ltm.model$coefficients[,1])
    ind <- intersect(names(v.respon), names(ltm.model$coefficients[,1]))
    if(length(ind) == 0) {
      return(NA)
    }
    respon[ind] <- unlist(v.respon)[ind]
    respon <- matrix(respon, nrow=1)
  }
  else
    respon <- matrix(v.respon, nrow=1)
  r <- plogis(as.numeric(ltm::factor.scores(ltm.model, respon)$score.dat['z1'])+correction)
  return(r)
}

# Use ltm model
POKSapi.topics <- function(scores, QMatrix=list(qmatrix, qmatrix2), matricule=NULL, write.to.file=NULL, verbose=F, robust=T, totalNItems=60, doNotKnow=0.25) {
  ## responses.ltm and topic.ltm.models are ltm models loaded from ltm-models.R 
  r0 <- ltm.topic.scores(responses.ltm, topic.ltm.models, scores, qmatrix3)
  r <- rep(doNotKnow, length(r0))*(totalNItems-length(scores))/totalNItems + r0*length(scores)/totalNItems
  if(!is.null(write.to.file)) {
    vscores <- matrix(NA, nrow=1, ncol=nrow(responses.ltm$coefficients))
    colnames(vscores) <- rownames(responses.ltm$coefficients)
    snames <- names(unlist(scores))
    if(robust==T) {
      subset.snames <- intersect(snames, colnames(vscores))
      vscores[,subset.snames] <- (unlist(scores))[subset.snames]
      if(length(subset.snames) != length(scores))
        warning(cat('Score questions do not match with q-matrix\nNumber of matches: ',length(subset.snames), '/', length(scores), '\n'))
    } else {
      vscores[,snames] <- unlist(scores)
    }
    cat(matricule, format(Sys.time(), '%s'), vscores[,snames], snames, r, '\n', file=write.to.file, append=T)
  }
  return(r)
}

# Initialize q-matrices (very specific to Poly pretest data)
POKSapi.get.qmatrix <- function(file='q-matrix.csv') as.matrix(read.csv(file,header=T,row.names=1))
POKSapi.qmatrix2 <- function(qmatrix) {
  # First letter is category and we further collapse V(ector) and M(atrices)
  collapsed.topics <- sub('V','M',sub('([A-Z]).*','\\1',colnames(qmatrix)))
  unique.topics <- unique(collapsed.topics)
  qmatrix2 <- sapply(unique.topics, function(i) (i == collapsed.topics))
  rownames(qmatrix2) <- colnames(qmatrix)
  colnames(qmatrix2) <- unique.topics
  return(as.matrix(qmatrix2))
}

# eg. POKSapi.topics(list(elnde1a=1, elnde1b=1, ints2a=1,ditpr1=1,gadro8a=1,tutri2b=1,ditpr1=1,veups3=1,mauin1=1))
# Scores is a list of the form <qname>=[0,1]
# QMatrix is a single q-matrix or a list for which the transfer model is sequentially applied
POKSapi.topics.0 <- function(scores, QMatrix=list(qmatrix, qmatrix2), matricule=NULL, write.to.file=NULL, verbose=F, robust=T) {
  vscores <- matrix(NA, nrow=1, ncol=nrow(qmatrix))
  colnames(vscores) <- rownames(qmatrix)
  snames <- names(unlist(scores))
  if(robust==T) {
    subset.snames <- intersect(snames, colnames(vscores))
    vscores[,subset.snames] <- (unlist(scores))[subset.snames]
    if(length(subset.snames) != length(scores))
      warning(cat('Score questions do not match with q-matrix\nNumber of matches: ',length(subset.snames), '/', length(scores), '\n'))
  }
  else {
    vscores[,snames] <- unlist(scores)
  }
  r <- competence.matrix.score(vscores, QMatrix, verbose)
  if(!is.null(write.to.file))
    cat(matricule, format(Sys.time(), '%s'), vscores[,snames], snames, r, '\n', file=write.to.file, append=T)
  return(r)
}

# Assumes missing values represent items not administered and non
# answered items are score 0
# QMatrix can be a list which represents successive transfer models
competence.matrix.score <- function(rawScores, QMatrix, verbose=F) {
  topic.score(rawScores, QMatrix, verbose=F) / topic.score(!is.na(rawScores), QMatrix, verbose=F)
}

getExpEntropy <- function(state, ks) {
  ks.q = length(state)
  state.p <- OddsToP(state)
  state.t <- sapply(1:ks.q, function(i) ks.update(i, 1, state, ks))
  ent.t <- apply((state.t), 2, odds.entropy)
  state.f <- sapply(1:ks.q, function(i) ks.update(i, 0, state, ks))
  ent.f <- apply((state.f), 2, odds.entropy)
  (ent.t * state.p) + (ent.f * (1 - state.p))
}

# state must be Odds
getHeuristicEntropy <- function(state, ks) {
  state <- OddsToP(state)
  state.ent <- entropy.vector(state)
  q.ent.sum.t <- colSums(t(ks$m) * state.ent, na.rm=T)
  q.ent.sum.f <- colSums(ks$m * state.ent, na.rm=T)
  q.ent.exp <- (q.ent.sum.t * state) + (q.ent.sum.f * (1-state)) + sum(is.na(state))*state
  return(q.ent.exp)
}

# state must be Odds
getHighestHeuristicEntropy <- function(state, ks) {
  which.max(getHeuristicEntropy(state,ks))
}

getLowestExpEntropy <- function(state, ks) {
  which.min(getExpEntropy(state, ks))
}

# Correction is based on this graph:
# plot(log(((0:1000))/sqrt(1000)),sapply(((0:1000)+0)/sqrt(1000),function(x)odds.entropy(exp(log(x)-2))),type='l')
getExpEntropy.corrected <- function(state, ks) {
  state.p <- OddsToP(state)
  entropy.cor = log(PToOdds(mean(state.p,na.rm=T)))
  odds.ent.fn = function (x,correct=0) odds.entropy(exp(log(x) - correct))
  ks.q = length(state)
  state.t <- sapply(1:ks.q, function(i) ks.update(i, T, state, ks))
  ent.t <- apply((state.t), 2, odds.ent.fn, correct=entropy.cor)
  state.f <- sapply(1:ks.q, function(i) ks.update(i, F, state, ks))
  ent.f <- apply((state.f), 2, odds.ent.fn, correct=entropy.cor)
  ((ent.t * state.p) + (ent.f * (1 - state.p)))
}

getLowestExpEntropy.corrected <- function(state, ks) {
  which.min(getExpEntropy.corrected(state, ks))
}

resample <- function(x, size, ...) {
  if(length(x) <= 1) { if(!missing(size) && size == 0) x[FALSE] else x
                     } else sample(x, size, ...) }

getRandom <- function(state,..) {
  resample(which(!is.na(state)),1)
}

getMostConnectedAndUncertainItem <- function(state, ks) {
  which.min(getConnectedAndUncertainItem(state, ks))
}

getHighestEntropyItem <- function(state, ...) {
  which.min(abs(state-1))
}

getHighestEntropyItemUpTo <- function(state, UpTo=length(state)/2, ks) {
  numberOfNotAns = length(which(state == item.presented))
  if(numberOfNotAns > UpTo)
    return(getLowestExpEntropy(state, ks))
  return(getHighestEntropyItem(state, ks))
}

# state is in Odds
# returns a vector instead of saclar like getMostConnectedAndUncertainItem
getConnectedAndUncertainItem <- function(state, ks) {
  ((abs(log(state))+.5) * ks$log.nlinks)
}

getNextItem <- function(choice.fn, state, ks, ...) {
  (choice.fn)(state, ks, ...)
}

# The option 'by' allows to bypass intermediate points but the performance degrades so it probably should not be used
simulation.subj <- function(subj.resp, state, ks, choice.fn, no.propag=F, by=F, verbose=F, estimate.missing=F) {
  if(verbose) {
    print('simulation.subj')
#    if(all(is.na(state))) {
#      print('all NA in state; entering browser')
#      browser()
#    }
  }
  n.items <- length(state)
  result <- state
  state[is.na(subj.resp)] <- NA
  ql <- c(0)
  for(i in 1:n.items) {
    item <- (choice.fn)(state, ks)        # choose next item according to item selection strategy
    if(by != F && ((i-1)%%by)!=0) { # simulation by steps of value of 'by'
      state[item] = item.presented
    }
    else if(no.propag || is.na(subj.resp[item]) || item.missing(subj.resp[item]) || length(item)==0)  # third OR clause happens when choice cannot be determined
      state[item] = item.presented
    else
      state <- ks.update(item, subj.resp[item], state, ks)
    result <- cbind(result,state)
    ql <- c(ql,item)
  }
  # a (logic) bug forces the coercion of the ql to the right size (happens with missing answers),
  # but given that we do not use the ql it is not important
  return(data.frame((1:n.items)[1:n.items],result))
}

# By convention, items less than 0 are missing values
item.missing <- function(i) i < 0
rm.missing <- function(m) { m[item.missing(m)] = NA ; return(m)}

# raw is a matrix [subj,item]
simulation <- function(raw, biased=F, p.min=0.5, alpha.c=.25, alpha.p=.5, no.propag=F, choice.fn=getLowestExpEntropy, keep.na=F, ...) {
  r = NULL
  ks <- ks.init(raw, p.min=p.min, alpha.c=alpha.c, alpha.p=alpha.p)
  for (i in 1:nrow(raw)) {
    if(biased == F && no.propag == F)
      ks <- ks.init(raw[-i,], p.min=p.min, alpha.c=alpha.c, alpha.p=alpha.p)
    r <- rbind(r,(simulation.subj(raw[i,], ks$state, ks, choice.fn, no.propag, ...)[,2:(ncol(raw)+2)] > 1)==raw[i,])
  }
  r2 <- array(r,c(ncol(raw),nrow(raw),ncol(raw)+1))
  if(keep.na==F)
    replace(r2, is.na(r2), 1)
  else
    r2
}

# n: number of runs
# raw is a matrix [subj,item]
# n.train : number of training cases (not used with predefined.sampling)
# predefined.sampling : matrix (items.no, runs)
# Note : old version had predefined sampling as a vector only
simulation.split.n <- function(n, raw, n.train=F, seed=333, predefined.sampling=F, ...) {
  if(!n.train)
    n.train = round(nrow(raw) * 0.66)
  seed.i = seed
  nsubjs = nrow(raw) - n.train
  nitems = ncol(raw)
  if(is.vector(predefined.sampling))
    aperm(array(sapply(1:n, function(i) simulation.split(raw, n.train, predefined.sampling=predefined.sampling, ...)), c(nitems,nsubjs,nitems+1,n)),c(1,3,2,4))
  else if(is.matrix(predefined.sampling))
    aperm(array(sapply(1:n, function(i) simulation.split(raw, n.train, predefined.sampling=predefined.sampling[,i], ...)), c(nitems,nsubjs,nitems+1,n)),c(1,3,2,4))
  else if(! (predefined.sampling==F))
    stop(simpleError('predefined.sampling must either be an Nx2 matrix or F'))
  else {
    aperm(array(sapply(1:n, function(i) simulation.split(raw, n.train, predefined.sampling=F, ...)), c(nitems,nsubjs,nitems+1,n)),c(1,3,2,4))
    seed.i = seed.i + 1
  }
}

# under development
# n: number of runs
# raw.f: matrix without missing values
# raw.m: matrix with missing values
# sampling.prob.dist can be a segment of a normal distribution ordered according to the most uncertain items, for eg.:
# sampling.prob.dist <- dnorm(seq(0,2.5,len=60))[rank(1/abs(colMeans(m2)-.5), ties.method='first')]
simulation.split.n.with.missing <- function(n, raw, train.set.size, number.of.missing, sampling.prob.dist=F, ...) {
  if(sampling.prob.dist==F) {
    sampling.prob.dist <- NULL;
  }
  sampling.prob.dist <- dnorm(seq(0,2.5,len=number.of.missing))[rank(1/abs(colMeans(m2)-.5), ties.method='first')]
  m2.half.w <- t(apply(m2,1,function(x) {x[sample(1:60,50,prob=sampling.prob.dist)] <- item.presented; return(x)})) # weighted probability of sampling

  train.index <- sample(1:nrow(raw.f), train.set.size, prob=sampling.prob.dist, ties.method='first')
  test.index <- (1:nrow(raw.f))[-train.index]
  data.test <- raw.f[test.index,]       # test without missing values
  data.train <- raw.m[train.index,]     # train with missing values
  aperm(array(sapply(1:n, function(i)
                     simulation.cross(data.train, n.train, ...)),
              c(ncol(raw.f),nrow(raw.f),ncol(raw.f)+1,n)),
        c(n,3,2,4))
}

# raw is a matrix [subj,item]
simulation.split <- function(raw, n.train, predefined.sampling=NULL, ...) {
  index.test <- rep(F, nrow(raw))
  if(is.null(predefined.sampling)) {
    print('simulation.split')
    n.test = nrow(raw) - n.train
    index.test[sample(nrow(raw),n.test)] <- T
  }
  else {
    print('simulation.split w predefined sampling')
    index.test[predefined.sampling] <- T
  }
  index.train <- !index.test
  raw.train <- raw[index.train,]
  raw.test <- raw[index.test,]
  simulation.cross(raw.train, raw.test, ...)
}

# seed is not used?
##' <description>
##'
##' <details>
##' @title Perform a cross simulation
##' @param raw.train Training data
##' @param raw.test Test data
##' @param state Allow for providing the state when dealing with missing values
##' @param p.min minimal probability (ks.init)
##' @param alpha.c conditional independence tests error (ks.init)
##' @param alpha.p minimal probability (ks.init)
##' @param no.propag For benchmarking: no inference
##' @param choice.fn Passed to ks.update
##' @param seed used?
##' @param keep.na If TRUE, will not replace NAs with 1 (allows for correct calculation of accuracy with missing values)
##' @param answers Can be 'odds' (keeps odds estimate), 'bool' (equivalent to returning odds > 1), or 'valence' (equivalent to (odds > 1) == item outcome)
##' @param ... 
##' @return An array of [n.items, n.subjs in raw.test, n.items+1 states]
##' or$t,or$f: odds ratio to compute posterior odds of row given column True,False
##' m: adjacency matrix where row implies column
##' @author Michel Desmarais
simulation.cross <- function(raw.train, raw.test, state=F, p.min=0.5, alpha.c=.25, alpha.p=.5, no.propag=F, choice.fn=getLowestExpEntropy, seed=NA, keep.na=T, answers='valence', ...) {
  r = NULL
  ks <- ks.init(raw.train, p.min=p.min, alpha.c=alpha.c, alpha.p=alpha.p)
  if(length(state)==1 && state==F)      # allow for providing the state when dealing with missing values
    state <- ks$state # this can introduce a bias with some types of sampling of question 
  for (i in 1:nrow(raw.test)) {
    if(answers=='valence') {
      r0 <- as.matrix((simulation.subj(raw.test[i,], state, ks, choice.fn, no.propag, ...)[,2:(ncol(raw.test)+2)] > 1)==raw.test[i,])
    } else if(answers=='odds') {
      r0 <- as.matrix(simulation.subj(raw.test[i,], state, ks, choice.fn, no.propag, ...)[,2:(ncol(raw.test)+2)])
    } else if(answers=='bool') {
      r0 <- as.matrix(simulation.subj(raw.test[i,], state, ks, choice.fn, no.propag, ...)[,2:(ncol(raw.test)+2)] > 1)
    } else {
      warning('invalid value for answers; using valence')
      r0 <- as.matrix(simulation.subj(raw.test[i,], state, ks, choice.fn, no.propag, ...)[,2:(ncol(raw.test)+2)] > 1)
    }
    r0[is.na(raw.test[i,])] <- missing.item
    r <- rbind(r, r0)
  }
  r2 <- array(as.matrix(r),c(ncol(raw.test),nrow(raw.test),ncol(raw.test)+1))
  if(keep.na==F)
    replace(r2, is.na(r2), 1)
  else
    r2
}

#############################################################################
# Optimized version of 2010.02.24
#############################################################################
# Adjacency matrix m is directed row->col
ks.init.o <- function(raw, alpha.c=.25, alpha.p=.25, p.min=.5) {
  s.less.na <- colSums(!is.na(raw))
  raw.sum <- colSums(raw, na.rm=T)
  p <- (raw.sum+1)/(s.less.na+2)
  odds <- p/(1-p)
  ans.cp.t <- replace(raw, is.na(raw), 0) # answers for crossprod computations of success
  ans.cp.f <- round(replace(!raw, is.na(raw), 0)) # answers for crossprod computations of failures
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

# Optimized version used in place of original (currently in poks2-lib.R)
ks.init <- ks.init.o


#############################################################################
## Avoid the problem with variable number of missing values per
## subject and per item by normalizing
#############################################################################

sim.normalized.performance <- function(r, ...) {
 if(length(dim(r)) != 3)
    stop('incorrect argument r (must be [q, nsubjs, q+1]')
  return(sapply(1:dim(r)[2], function(i) subj.normalized.performance(r[,i,], ...)))
}
  
## subj.normalized.performance <- function(r, ...) {
##   non.missing <- sum(!is.na(r[,1]))      # non missing will be progressively counted as correct
##   non.missing.vector <- c(0:non.missing, rep(0, length(r[,1]) - non.missing))
##   r2 <- (non.missing.vector + colSums(r, na.rm=T))/(non.missing.vector + colSums(!is.na(r)))
##   approx(r2[!is.nan(r2)], ...)$y
## }
# Should be the same as above but simpler
##' <description>
##' Normalizes answers by returning an interpolated
##' vector and by taking into account the missing values.  It asssumes
##' that  the NA at 0 observation are missing questions and  that all
##' non missing questions are asked.
##' <details>
##' @title Normalized performance of a subject performance
##' @param r An array that contains correcteness of guesses and having dimension
##' [n.questions, n.subjects, n.questions+1] which corresponds to
##' [State 1..n.questions, Subject 1..nsubj, Guess 0..n.questions]
##' @param ... 
##' @return A vector of length n.questions+1 that averages subject performances
##' @author Michel Desmarais
subj.normalized.performance <- function(r, ...) {
  q.asked <- !is.na(r[,1])
  r2 <- r[q.asked,]
  r2[is.na(r2)] <- 1                    # NA are observed, they are considered correctly classified
  approx(colMeans(r2)[1:sum(q.asked)], ...)$y
}


#############################################################################
## Graph
#############################################################################
##' <description>
##'
##' <details>
##' @title 
##' @param r Result from simulation.split or simulation.split.n
##' @param lab Labels
##' @param col Colors
##' @param pch Point types
##' @param title ...
##' @param linesym ...
##' @param diag If 'mult', each curve has its own diagonal
##' @param x.step 
##' @param ystart Defaults to 0.5
##' @param neg.transform Values of -1 are used for indicating observed and are distinct from missing (NA).  For plotting, they need to be transformed to NA
##' @param ... Additional parameters passed to 'plot'
##' @return 
##' @author Michel Desmarais
plot.sim.results <- function(r, lab=NULL, col = c('blue','magenta','red','green','orange'), pch=c(1,2,8,17,20,18), title='', linesym=F, diag=T, x.step=0, ystart=0.5, neg.transform=TRUE, ...) {
  if(!is.list(r))
    r = list(r)
  r.dim = dim(r[[1]])
  if(is.null(lab))
    lab = letters[1:length(r)]
  ncurves = length(r)
  if(is.logical(neg.transform) & neg.transform==T & any(sapply(1:ncurves, function(i) any(r[[i]]<0, na.rm=T)))) {
    for(i in 1:length(r)) {
      r[[i]][r[[i]]<0] <- NA
    }
  }
  lty = rep(1,length(r))
  pt.bg = rep(1,length(r))
  if(!is.vector(linesym) || linesym == F)
    linesym = rep('b',length(r))
  # plot item results
  plotRange = c(ystart,1)
  x <- seq(0,100,2)
  plot(x,rep(0,51),type='l',xlab='% Items observed',ylab='Accuracy',main=title,cex=4, ylim=plotRange, ...)
  abline(h=axTicks(side=2),col=gray(0.7))
  abline(v=axTicks(side=1),col=gray(0.7))
  cm <- NULL
  for(i in ncurves:1) {
    i.cm <- rowMeans(sim.normalized.performance(r[[i]], n=51))
    lines(x, i.cm, col=col[i],lty=lty[i],pch=pch[i],type=linesym[i])
    cm <- cbind(i.cm, cm)
  }
  if(diag=='mult')
    sapply(1:ncurves, function(i) lines(c(0,100),c(mean((r[[i]])[,,1],na.rm=T),1), col='black'))
  else if(diag)
    lines(c(0,100),c(i.cm[1],1), col='black')
  legend(median(axTicks(side=1)),.8,lab,col=col,bg=gray(.95),lty=lty,pch=pch)
  colnames(cm) <- lab
  return(cm)
}  

# Performance as relative gain over baseline
# r: curves of each simulation line in columns
# summary: returns details at each interval
sim.gain <- function(r, summary=T) {
  intervals <- nrow(r)
  startv.endv <- cbind(r[1,],r[nrow(r),])
  baseline <- apply(startv.endv, 1, function(i) approx(i, n=intervals)$y)
  rel.gain <- (r-baseline)/(1-baseline)
  rel.gain[is.nan(rel.gain)] <- 1
  if(summary==T)
    return(colMeans(rel.gain[-1,]))
  else
    return(rel.gain)
}


make.graphi <- function(r,lab, col = c('blue','magenta','red','green','grey'),pch = c(1,2,8,18),title='', linesym=F, diag=T, x.step=0, ...) {
  nquest = sum(!is.na((r[[1]])[,1,1]))       # assumes all subjects have same number of questions asked
  ncurves = length(r)
  lty = rep(1,length(r))
  pt.bg = rep(1,length(r))
  if(!is.vector(linesym) || linesym == F)
    linesym = rep('b',length(r))
                                        # plot item results
#  plotRange = c(ystart,1)
  plot(0:nquest,rep(0,nquest+1),type='b',xlab='Item',ylab='Accuracy',main=title,cex=4, ...)
  abline(h=axTicks(side=2),col=gray(0.7))
  abline(v=axTicks(side=1),col=gray(0.7))
  if(x.step != 0)
    x.ind = (((0:nquest) %% x.step)==0)
  else
    x.ind = rep(T,nquest)
  for(i in ncurves:1) {
    i.cm <- colMeans2(r[[i]],na.rm=T)[1:(nquest+1)]
    lines((0:nquest)[x.ind],(i.cm)[x.ind],col=col[i],lty=lty[i],pch=pch[i],type=linesym[i])
  }
  if(diag)
    lines(c(0,nquest),c(i.cm[1],1), col='black')
  legend(median(axTicks(side=1)),.8,lab,col=col,bg=gray(.95),lty=lty,pch=pch)
}

## Version that assumes NAs in r.w.na[,,1] indicate missing items; also
## allows more flexibility for other options

make.graphi.na <- function(r.w.na,lab, col = NULL, pch = NULL,title='', linesym=F, diag=T, x.step=0, draw.0r=F, ...) {
  r <- sapply(r.w.na, filter.na)
  nquest = sum(!is.na((r[[1]])[,1,1]))
  if(any(apply((r[[1]])[,,1], 2, function(i) sum(!is.na(i))) != nquest))
    stop('make.graphi.na: assumes all subjects have same number of questions asked')
  ncurves = length(r)
  lty = rep(1,length(r))
  pt.bg = rep(1,length(r))
  if(!is.vector(linesym) || linesym == F)
    linesym = rep('b',length(r))
                                        # plot item results
#  plotRange = c(ystart,1)
  plot(0:nquest,rep(0,nquest+1),type='b',xlab='Item',ylab='Accuracy',main=title,cex=4, ...)
  abline(h=axTicks(side=2),col=gray(0.7))
  abline(v=axTicks(side=1),col=gray(0.7))
  if(x.step != 0)
    x.ind = (((0:nquest) %% x.step)==0)
  else
    x.ind = rep(T,nquest)
  if(is.null(col))
    col = c('blue','magenta','red','green','grey','blue2','magenta2','red2','green2','grey2')[1:ncurves]
  if(is.null(pch))
    pch = c(1,2,8,3,4,5,6,7,9,1,2,8,3,4,5,6,7,9)[1:ncurves]
  for(i in ncurves:1) {
    i.cm <- colMeans2(r[[i]],na.rm=T)[1:(nquest+1)]
    lines((0:nquest)[x.ind],(i.cm)[x.ind],col=col[i],lty=lty[i],pch=pch[i],type=linesym[i])
  }
  if(diag)
    lines(c(0,nquest),c(i.cm[1],1), col='black')
  if(draw.0r) {
    r0 <- array((r[[1]])[,,1], dim(r[[1]]))
    r0[is.na(r.w.na[[1]])] <- NA
    lines((0:nquest)[x.ind],colMeans2(filter.na(r0)[[1]],na.rm=T)[x.ind],col='black',lty=1,pch=0)
    lab <- c(lab, '0r'); col <- c(col, 'black'); lty <- c(lty, 1); pch <- c(pch, -1)
  }
  legend(median(axTicks(side=1)),.8,lab,col=col,bg=gray(.95),lty=lty,pch=pch)
}

#############################################################################
# Standard deviation routines
#############################################################################
score <- function(a) {
  r.exp.cor = round(a)
  for (i in 1:ncol(a)) r.exp.cor[,i,,] = r.exp.cor[,ncol(a),,]
  round(r.exp.cor == round(a))
}

# r is a matrix [item,subj,sequence,run] that has been already scored: score(a)
plot.ci.boot <- function(r) {
  nnodes = nrow(r)
  r.subj=(sapply(1:(nnodes+1),function(i)colMeans(r[,i,,])))
  r.subj.sd = sd(r.subj)
  if(any(r.subj.sd == 0))
    r.subj = r.subj[,1:(which(r.subj.sd == 0)[1]-1)]
  # compute confidence intervals
  r.ci = sapply(1:ncol(r.subj),function(i) boot.ci(boot(r.subj[,i],function(x,i)mean(x[i]),R=1000),conf=.95,type=c('perc'))$percent)[4:5,]
  if(any(r.subj.sd == 0))
    r.ci = cbind(r.ci,matrix(0,2,(nnodes-(which(r.subj.sd == 0)[1] - 2))))
  arrows(0:(nnodes-1), r.ci[1,], 0:(nnodes-1), r.ci[2,], code=3, angle=90, length=.02)
}

# r is a matrix [item,sequence,subj,run] that has been already scored: score(a)
sd.norm <- function(r, alpha=.9, na.rm=F) {
  sd(t(sapply(1:dim(r)[3],function(j) rowMeans(sapply(1:dim(r)[4],function(i)colMeans(r[,,j,i],, na.rm=na.rm)))))) * qnorm(alpha)
}

# not used and unverified
sd.norm.btw.runs <- function(r, alpha=.9) {
  sd(colMeans(aperm(sumRuns(r),c(3,1,2)))) * qnorm(alpha)
}

sumRuns <- function(r) {
  accum = r[,,,1]
  for(i in 2:(dim(r)[4])) {
    accum = accum + r[,,,i]
  }
  accum / (dim(r)[4])
}

plot.ci.norm <- function(r, alpha=.9, offset=0, na.rm=F, ...) {
  curve = rowMeans(colMeans(score(r)),na.rm=na.rm)
  sd = sd.norm(r, alpha, na.rm=na.rm)
  nnodes = length(sd)
  high = curve+sd
  low = curve-sd
  high[which(high>1)] = trunc(high[which(high>1)])
  r.ci = rbind(high, low)
  arrows((0:(nnodes-1))+offset, r.ci[1,], (0:(nnodes-1))+offset, r.ci[2,], code=3, angle=90, length=.02, ...)
}

# assume r is scored
# plot.ci.per.sim <- function(r, alpha=.9, offset=0, ...) {
#   curves = colMeans(aperm(colMeans(r),c(2,3,1)))
#   mean.curve = colMeans(curves)
#   sd = sd(curves) * qnorm(alpha)
#   nnodes = length(sd)
#   high = mean.curve+sd
#   low = mean.curve-sd
#   high[which(high>1)] = trunc(high[which(high>1)])
#   r.ci = rbind(high, low)
#   arrows((0:(nnodes-1))+offset, r.ci[1,], (0:(nnodes-1))+offset, r.ci[2,], code=3, angle=90, length=.02, ...)
# }

plot.ci.per.sim <- function(r, alpha=.9, offset=0, step=1, ...) {
  curves = colMeans(aperm(colMeans(r),c(2,3,1)))
  mean.curve = colMeans(curves)
  sd = sd(curves) * qnorm(alpha)
  nnodes = length(sd)
  high = mean.curve+sd
  low = mean.curve-sd
  high[which(high>1)] = trunc(high[which(high>1)])
  r.ci = rbind(high, low)
  seq.step <- seq(1,(nnodes),step)
  arrows(((0:(nnodes-1))+offset)[seq.step], (r.ci[1,])[seq.step], ((0:(nnodes-1))+offset)[seq.step], (r.ci[2,])[seq.step], code=3, angle=90, length=.02, ...)
}

p.to.star <- function(p.vec) {r = rep('-',length(p.vec)); r[p.vec<.05]='*'; r[p.vec<.01]='**'; r[p.vec<.001]='***';return(r);}

#############################################################################
## Generation of simulated data
#############################################################################
tf.sample <- function(n,p) {
    sapply(p, function(pi) sample(c(T,F),n,replace=T,prob=c(pi,1-pi)))
}

## Returns a [Subject X Concept] mastery matrix.
## NAs are removed for computing mastery.
## Consider u.topic.mastery as a skill specific slip parameter
topic.mastery <- function(data, qm, no.correction=F) {
  n.items <- ncol(data)
  data2 <- data
  data2[is.na(data)] <- 0
  ## Todo: to compensate for NA : must build a matrix instead of colSums
  ## where incidences of NAs are deducted for each subject in data
  mastery <- t((data2 %*% qm)/topics.na.rm(data, qm))
  if(no.correction)
    { return(mastery) }
  else
    { return((t(data2 %*% qm)+rowMeans(mastery))/(c(rep(1,n.items) %*% qm)+1)) } 
}

## For each NA in a subject row that affects concept computation,
## deduct the number of occurences.  Allows the computation of concept
## mastery while removing NAs.
topics.na.rm <- function(data, qm) {
  qm.cs <- colSums(qm)
  na.incidence <- is.na(data) %*% qm
  r <- qm.cs - na.incidence
  r[r==0] <- -1                         # to avoid 0 division
  return(r)
}

identity <- function(x) x

## Generate data based on topic mastery
## 'data': matrix: Subjs X Item;  and can contain NA
## 'qm' : matrix: Item X Topic (created with topic.mastery())
gen.simulated.data <- function(data, qm) {
  topic.mastery <- topic.mastery(data, qm)
  q.prob <- sapply(1:ncol(data), function(i) apply(matrix(topic.mastery[qm[i,]>0,], ncol=nrow(data)), 2, min))
  data.na.ind <- is.na(data)
  sim.qm.seq <- sample(which(!data.na.ind), trunc(mean(data,na.rm=T) * sum(!data.na.ind)), prob=q.prob[!data.na.ind])
  data.sim <- matrix(0, nrow(data), ncol(data))
  data.sim[data.na.ind] <- NA
  data.sim[sim.qm.seq] <- 1
  return(data.sim)
}

# Same as gen.simulated.data but withouth concepts
gen.simulated.expected <- function(data) {
  rp <- (rowMeans(data,na.rm=T)+1)/(ncol(data)+2)
  cp <- (colMeans(data,na.rm=T)+1)/(nrow(data)+2)
  ex <- (rp %o% cp) * sum(data,na.rm=T) # expected probability
  data.na.ind <- !is.na(data)
  sim.seq <- sample(which(data.na.ind), trunc(mean(data,na.rm=T) * sum(data.na.ind)), prob=c(ex)[data.na.ind])
  data.sim <- matrix(0, nrow(data), ncol(data))
  data.sim[!data.na.ind] <- NA
  data.sim[sim.seq] <- 1
  return(data.sim)
}
# Same as gen.simulated.data keeping exact row distribution (transpose for obtaning exact same column dist)
gen.simulated.expected.r <- function(data) {
  cp <- (colMeans(data,na.rm=T)+1)/(nrow(data)+2)
  rs <- rowSums(data,na.rm=T)
  data.na.ind <- !is.na(data)
  r <- sapply(1:nrow(data), function(i) {
    ro.seq <- sample(which(data.na.ind[i,]), rs[i], prob=cp[data.na.ind[i,]])
    ro.sim <- rep(0, ncol(data))
    ro.sim[!data.na.ind[i,]] <- NA
    ro.sim[ro.seq] <- 1
    return(ro.sim)
  })
  return(t(r))
}

#plot(dnorm((-40:40)/10))
#lines(dlogis(1.7*(-40:40)/10)*.4/.25)
#lines(dlogis(1*(-40:40)/10)*.4/.25)
#############################################################################
## IRT simulated data
# d.examinee: vector of examinee params (thetas)
# d.items: matrix of item params (difficulty, discrimination)
# To test with UNIX data: sim.compare(gen.simulated.irt(u.irt.e, u.irt.i), u.m, u.qm)
# (should yield very row and column correlations)
gen.simulated.irt <- function(d.examinee, d.items) {
  t(round(sapply(d.examinee, function(j) apply(d.items, 1, function(i) rlogis(1, j-i[2], 1/i[1]))) > 0))
}

#############################################################################
# Generate simulated data with 
# data is a {0,1} matrix Examinee X Item
covariance.sim.data <- function(data, fit.rows=F) {
  data.cv <- cov(data)                  # Covariance matrix
  # Upper triangular factor of the Cholesky decomposition
  L <- chol(data.cv)                    
  # Normalized simulated sample
  temp.data.sim <- matrix(rnorm(ncol(data)*nrow(data)), nrow=nrow(data)) %*% L
  # Add item average success rate and perform discrete transformation
  data.sim  <- sweep(temp.data.sim, 2, colMeans(data)-0.5, '+')
  if(fit.rows==T)
    data.sim  <- sweep(data.sim, 1, rowMeans(data)-0.5, '+')
  return(data.sim > 0)
}

#############################################################################
## Simulated data routines with POKS

# Generate a single sample of data from the predefined 'ks'.
# I believe this corresponds to a Gibbs sampler.

# 'burn' indicates the number of initial values to set without taking
# into account observed nodes
GenSamplePOKS <- function(ks, burn=2) {
  n.q <- ncol(ks$m)
  qi <- 1:n.q
  s=rep(NA,n.q)
  n.t <- trunc(burn/2)
  n.f <- trunc(burn/2)
#  s[sample(qi, n.t, prob = ks$p)] <- T
#  s[sample(qi, n.f, prob = 1 - ks$p)] <- F
  s[sample(qi, n.t)] <- T
  s[sample(qi, n.f)] <- F
  for(ignore in 1:(n.q-burn)) {
    na.i <- is.na(s)
    if(sum(na.i)==1)                    # otherwise sample behaves the wrong way
      i=qi[na.i]
    else
      i=sample(qi[na.i],1)
    if(!is.na(s[i])) stop(paste('i NA?? Step', ignore))
    p=ProbGObs(i, s, ks)
    s[i] = sample(c(T,F), 1, prob=c(p,1-p))
  }
  return(s)
}
## Returns the probability of 'node=T' given a set of existing 'obs' and the predefined 'ks'
ProbGObs <- function(node, obs, ks) {
  OddsToP(prod(ks$state[node],
               ks$or$t[node,(!is.na(obs) & obs)],
               ks$or$f[node,(!is.na(obs) & !obs)],
               na.rm=T))
}

sim.compare <- function(data,sim, qm=F) {
  print('general means, Correlation row means, Correlation col means, Correlation concept means on respondant basis')
  if(!is.matrix(qm))
    qm <- matrix(1,ncol(data),2)        # make a dummy Q-matrix
  data.frame('Mean data'=mean(data,na.rm=T),'Mean sim'=mean(sim,na.rm=T),
    'Cor Row'=cor(rowMeans(data,na.rm=T),rowMeans(sim,na.rm=T)),
    'Cor Col'=cor(colMeans(data,na.rm=T),colMeans(sim,na.rm=T),use='complete.obs'),
    'Col Concepts'=cor(c(topic.mastery(data, qm)), c(topic.mastery(sim, qm))),
    '%diff'= mean(data==sim)
             )
}

## Transform missing values which show up at 0 observations as NA into
## -1 in the results.  This allows to distinguish them from observed
## items which are marked with NA after 0 observations
missing.to.neg <- function(results) {
  r <- results
  ## Results[,,1] is state after 0 observations and thus contains NA
  ## where missing data is.  This pattern is repeated over all
  ## observations and thus repeated n-1 times the last dimension in the array.
  r[rep(is.na(results[,,1]), (dim(results)[3]-1))] <- -1
  return(r)
}
## Follows the transformation of missing.to.neg: NA are considered
## observed and set to 1, and -1 are put to NA
na.to.obs <- function(results) {
  r <- results
  r[is.na(results)] <- 1
  r[results<0] <- NA
  return(r)
}

## Combines both routines na.to.obs and missing.to.neg
filter.na <- function(...) sapply(list(...), function(m) list(na.to.obs(missing.to.neg(m))))

## Extracts the position where each item is asked in a simulation run
## based on the first appearance of NA
extract.item.pos <- function(r) {
  t(sapply(1:(dim(r)[2]),
         function(i) apply(r[,i,], 1, function(i) match(NA,i)))) - 1
}

## Normalize results to equal length by interpolation.
normalize.res <- function(r, len=100) {
  rn <- aggregate.res.per.item(r)
  apply(rn, 2, function(i) { n.valid=(length(i)-sum(is.nan(i))); interpolate.vector(i[1:n.valid], len) })
}  

## Aggregate results of simulation data with missing items. 
aggregate.res.per.item <- function (r) apply(r, 2, function(i) { r.mis=is.na(i[,1]); colMeans(i, na.rm=T) })

## Aggregate results of simulation data with missing items, counting
## observed for correct. It allows for subjects to have different
## numbers of items, which implies a normalization step to obtain a
## matrix of equal sequenced observations.
## Eg. plot(rowMeans(aggregate.res.per.item.obsT(r)))
aggregate.res.per.item.obsT <- function (r, step=100) {
  apply(r, 2,
        function(i) {
          r.mis=is.na(i[,1]);
          i2=i[!r.mis,];
          subj.r <- apply(i2, 2, function(j) {
            j[is.na(j)] <- 1
            mean(j)
          })
          n.valid=match(T,apply(i2,2,function(i) all(is.na(i))))
          interpolate.vector(subj.r[1:n.valid], step)
        })
}

## Interpolate a vector to a new vector of given length (assumes constant spacing)
interpolate.vector <- function (v, len)   approx(1:length(v), v, seq(1,length(v),length.out=len))$y

## Returns residual score of variable length according to missing values; need
## to turn it into normalized lenght and replace residual with score
## that consider oberved as correct.  Could also use normalized length
## as a good score.




#############################################################################
## IRT simulation with 'ltm' package
#############################################################################

## Replicate a simulation run (by POKS) with IRT from 'ltm' package
## DOES NOT GENERATE FAITHFUL DATA as does gen.simulated.irt
## m is the response matrix
## res.repl is the output of 'simulation' or 'simulation.split' (keeping NA?)
## If 'model' is specified, it must have been specified as (<data> ~ z1)
## require(multicore)
irt.sim.repl <- function(m, res.repl, model=NULL, no.bias=F, cap.coefs=T, no.multicore=F) {
  require(ltm)
  if(is.null(model) & no.bias==F)
    model <- ltm(m ~ z1)
  rp <- array(t(m), dim(res.repl))
  rp[!is.na(res.repl)] <- NA
  if(no.multicore == T)
    mclapply <- lapply
  r <- mclapply(1:dim(res.repl)[3], function(i) {
    if(no.bias == T)
      model = ltm(m[-i,] ~ z1)
    fs <- factor.scores(model, resp.pattern=t(rp[,,i]))
    if(cap.coefs == T) {
      disc = cap(coef(model)[,2], 0, 4)
      diffc = cap(coef(model)[,1], -4, 4)
    } else {
      disc = coef(model)[,2]
      diffc = coef(model)[,1]
    }
    sapply(1:dim(res.repl)[1], function(j)
           icc.p(fs$score.dat$z1,disc[j],diffc[j]))
  })
  return(irt.r.filter(unlist(r), m, res.repl))
}
  
cap <- function(x,min,max) pmax(pmin(x,max),min)

## Utility function of irt.sim.repl to process the intermediate result data
irt.r.filter <- function(r, r.m, r.repl) {
  nquest <- dim(r.repl)[1]
  nsubj <- dim(r.repl)[2]
  r2 <- ((r>.5) == rep(c(r.m), nquest+1))
  r3 <- aperm(array(r2, c(nsubj,nquest,nquest+1)),c(2,1,3))
  r3[is.na(r.repl)] <- NA
  return(r3)
}

set <- function(v, val, ind) {v[ind] <- val; v}
