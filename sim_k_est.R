
source('functions_for_est.R')






sim.runs = 51
zero.missing.percentage.array = c(0.4, 0.6, 0.8)
fix.k.array = c(0.1, 0.3, 0.9)
#

par(mfrow = c(3,3), oma = c(2,2,0,0))
letter.rank = 0
for(missing.i in 1:length(zero.missing.percentage.array)){#          missing.i = 1
  temp.zero.missing.percentage = zero.missing.percentage.array[missing.i]
  #
  for(k.i in 1:length(fix.k.array)){#         k.i = 1
    temp.k = fix.k.array[k.i]
    #
    letter.rank = letter.rank +1
    #
    par(las = 1, mar = c(3,3,2,1))
    plot(1,1, type = 'n', xlim = c(0.01,20), ylim = c(-800,-0), xaxs = 'i', log = 'x', xlab = '', ylab = '', ann = F, axe = F, frame = T)#
    axis(2); axis(1, at = c(0.01, 0.1,1,10))
    abline(v = temp.k, lwd = 4, lty = 1, col = 'green3')
    mtext(side = 3, adj = 0, cex = 1.1, paste0('(', LETTERS[letter.rank], ') w = ', round(temp.zero.missing.percentage *100, 2), '%; k = ', round(temp.k, 2)))
    for(sim.i in 1:sim.runs){
      random.Ro = 2^runif(n = 1, min = -1, max = 1)
      temp.offspring.array = rnbinom(n = 333, mu = random.Ro, size = temp.k)
      #  hist(temp.offspring.array, breaks = c(0:33))
      temp.0.index = which(temp.offspring.array == 0)
      del.index = temp.0.index[1:as.integer(length(temp.0.index) *temp.zero.missing.percentage +1)]
      temp.ll.profile.of.k = get.crude.ll.profile.of.k(is.trunc = F, obs.array = temp.offspring.array[-del.index], k.min = 0.01, k.max = 100, Ro = random.Ro)
      lines(temp.ll.profile.of.k$k, temp.ll.profile.of.k$ll, col = '#0000FF33')
      temp.mle.index = which.max(temp.ll.profile.of.k$ll); temp.mle.index = median(temp.mle.index)
      points(temp.ll.profile.of.k$k[temp.mle.index], temp.ll.profile.of.k$ll[temp.mle.index], pch = 24, bg = 'royalblue')
      #  hist(temp.offspring.array[-del.index], breaks = c(0:33))
      temp.ll.profile.of.k = get.crude.ll.profile.of.k(is.trunc = T, obs.array = temp.offspring.array, k.min = 0.01, k.max = 100, Ro = random.Ro)
      lines(temp.ll.profile.of.k$k, temp.ll.profile.of.k$ll, col = '#FF000033')
      temp.mle.index = which.max(temp.ll.profile.of.k$ll); temp.mle.index = median(temp.mle.index)
      points(temp.ll.profile.of.k$k[temp.mle.index], temp.ll.profile.of.k$ll[temp.mle.index], pch = 25, bg = 'red')
    }
    #
  }
}
mtext(side = 1, outer = T, 'dispersion parameter, k (in log scale)', cex = 1.5)
mtext(side = 2, outer = T, 'log-likelihood', cex = 1.5, las = 0)


# range(temp.ll.profile.of.k$k[temp.ll.profile.of.k$ll > (max(temp.ll.profile.of.k$ll -3.84))])






