
#library(countreg)



get.one.ll = function(obs.array, Ro, disp.k, is.trunc = T){
  #  obs.array = c(offspring.array)
  if(is.trunc){
    obs.array = obs.array[which(obs.array > 0)]
  }
  obs.tab = as.data.frame(table(obs.array)); colnames(obs.tab) = c('offspring', 'freq')
  #
  ll.base.array = dnbinom(x = as.numeric(as.character(obs.tab$offspring)), mu = Ro, size = disp.k, log = T)
  ll.base.sum = sum(ll.base.array *obs.tab$freq, na.rm = T)
  ll.adj = ifelse(is.trunc, pnbinom(q = 0, mu = Ro, size = disp.k, lower.tail = F, log = T), 0)
  #sum(dztnbinom(x = c(1:4), mu = 1, size = 2, log = T))
  ll.sum = ll.base.sum -ll.adj *length(obs.array)
  return(ll.sum)
}





get.crude.ll.profile.of.k = function(obs.array, Ro, k.min = 0.01, k.max = 100, is.trunc = T){
  k.array = exp(seq(from = log(k.min), to = log(k.max), length.out = 101))
  ll.array = NULL
  for(k.i in 1:length(k.array)){
    temp.k = k.array[k.i]
    temp.ll = get.one.ll(obs.array = c(obs.array), Ro = Ro, disp.k = temp.k, is.trunc = is.trunc)
    ll.array = c(ll.array, temp.ll)
  }
  ll.profile = data.frame(
    k = k.array,
    ll = ll.array
  )
  return(ll.profile)
}




get.detailed.ll.profile.of.k = function(obs.array, Ro, k.min = 0.01, k.max = 100, is.trunc = T){
  ll.profile = get.crude.ll.profile.of.k(obs.array = obs.array, Ro = Ro, k.min = k.min, k.max = k.max, is.trunc = is.trunc)
  k.range = range(ll.profile$k[ll.profile$ll > (max(ll.profile$ll) -5)])
  improved.ll.profile = get.crude.ll.profile.of.k(obs.array = obs.array, Ro = Ro, k.min = k.range[1] /2, k.max = k.range[2] *2, is.trunc = is.trunc)
  return(improved.ll.profile)
}




get.ll.profile = function(obs.array, Ro.min = 0.01, Ro.max = 5, k.min = 0.01, k.max = 10, is.trunc = T){
  Ro.array = exp(seq(from = log(Ro.min), to = log(Ro.max), length.out = 101))
  ll.mat = NULL
  for(Ro.i in 1:length(Ro.array)){#        Ro.i = 1
    temp.Ro = Ro.array[Ro.i]
    temp.ll.profile.of.k = get.crude.ll.profile.of.k(is.trunc = is.trunc, obs.array = obs.array, k.min = k.min, k.max = k.max, Ro = temp.Ro)
    temp.ll.profile.of.k$Ro = temp.Ro
    ll.mat = rbind(ll.mat, temp.ll.profile.of.k)
  }
  return(ll.mat)
}




get.Ro.est = function(ll.profile){
  max.ll.index = which.max(ll.profile$ll); max.ll.index = median(max.ll.index)
  best.Ro = ll.profile$Ro[max.ll.index]; best.k = ll.profile$k[max.ll.index]
  #
  sel.index.array = which(ll.profile$k == best.k); sel.ll.profile = ll.profile[sel.index.array,]
  #  plot(sel.ll.profile$Ro, sel.ll.profile$ll, log = 'x')
  Ro.ci = range(sel.ll.profile$Ro[sel.ll.profile$ll > (max(sel.ll.profile$ll) -3.84)])
  Ro.est.array = c(best.Ro, Ro.ci)
  return(Ro.est.array)
}




get.k.est = function(ll.profile){
  max.ll.index = which.max(ll.profile$ll); max.ll.index = median(max.ll.index)
  best.Ro = ll.profile$Ro[max.ll.index]; best.k = ll.profile$k[max.ll.index]
  #
  sel.index.array = which(ll.profile$Ro == best.Ro); sel.ll.profile = ll.profile[sel.index.array,]
  #  plot(sel.ll.profile$k, sel.ll.profile$ll, log = 'x')
  k.ci = range(sel.ll.profile$k[sel.ll.profile$ll > (max(sel.ll.profile$ll) -3.84)])
  k.est.array = c(best.k, k.ci)
  return(k.est.array)
}







