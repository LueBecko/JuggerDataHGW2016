## monte carlo approach to topurnament ranking

generateRandTournament <- function() {
  # generate game data
  n.teams <- 5
  pv <- choose(c(5:9, 9:5), c(0:4, 4:0))
  pv <- pv / sum(pv)
  S1P.Mat <- matrix(data = sample(c(-5:-1,1:5), size = n.teams^2, prob = pv, replace = TRUE), ncol = n.teams, nrow = n.teams)
  diag(S1P.Mat) <- 0
  S1P.Mat[lower.tri(S1P.Mat)] <- - S1P.Mat[upper.tri(S1P.Mat)]
  S2P.Mat <- matrix(data = sample(c(-5:-1,1:5), size = n.teams^2, prob = pv, replace = TRUE), ncol = n.teams, nrow = n.teams)
  diag(S2P.Mat) <- 0
  S2P.Mat[lower.tri(S2P.Mat)] <- - S2P.Mat[upper.tri(S2P.Mat)]
  S3P.Mat <- matrix(data = sample(c(-5:-1,1:5), size = n.teams^2, prob = pv, replace = TRUE), ncol = n.teams, nrow = n.teams)
  diag(S3P.Mat) <- 0
  S3P.Mat[lower.tri(S3P.Mat)] <- - S3P.Mat[upper.tri(S3P.Mat)]
  S3P.Mat[sign(S1P.Mat) == sign(S2P.Mat)] <- 0
  # aggregate game data
  S12P.Mat <- S1P.Mat + S2P.Mat
  S123P.Mat <- S12P.Mat + S3P.Mat
  W.Mat <- sign(S1P.Mat) + sign(S2P.Mat) + sign(S3P.Mat)
  # generate matrix
  V.Mat  <- cbind(Wins     = rowSums(x = sign(W.Mat)),
                  S1.Win   = rowSums(x = sign(S1P.Mat)),
                  S2.Win   = rowSums(x = sign(S2P.Mat)),
                  S12.Win  = rowSums(x = sign(S12P.Mat)),
                  S3.Win   = rowSums(x = sign(S3P.Mat)),
                  #S123.Win = rowSums(x = sign(S123P.Mat)),
                  S1.Differece   = rowSums(x = S1P.Mat),
                  S2.Differece   = rowSums(x = S2P.Mat),
                  S12.Differece  = rowSums(x = S12P.Mat),
                  S3.Differece   = rowSums(x = S3P.Mat),
                  S123.Differece = rowSums(x = S123P.Mat)
  )
  return(cor(V.Mat, method = "kendall"))
}

n.tests <- 100000 # number of tests to run
system.time(mc.values <- t(sapply(1:n.tests, function(ri) { as.vector(generateRandTournament()) })))
mc.taumat <- matrix(colMeans(mc.values, na.rm = TRUE), nrow = 10, ncol = 10, dimnames = list(rownames(gc.tau),colnames(gc.tau))) # removing NAs neccessary, since sets without any variance can be created

mc.taumat[upper.tri(mc.taumat)] <- NA
ggplot(melt(mc.taumat, na.rm = TRUE), aes(Var1, Var2, fill = value)) + geom_tile(colour = "white") + 
  geom_text(aes(Var1, Var2, label = round(x = value, digits = 2)), color = "black", size = 4) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), axis.title.x = element_blank(),
        axis.title.y = element_blank(), panel.grid.major = element_blank(), panel.border = element_blank(), panel.background = element_blank(), axis.ticks = element_blank()) +
  scale_fill_gradient(low = "white", high = "steelblue", name = "Kendall's Tau")


generateRandTournament2 <- function() {
  # generate game data
  n.teams <- 5
  pv <- choose(c(5:9, 9:5), c(0:4, 4:0))
  pv <- pv / sum(pv)
  S1P.Mat <- matrix(data = sample(c(-5:-1,1:5), size = n.teams^2, prob = pv, replace = TRUE), ncol = n.teams, nrow = n.teams)
  diag(S1P.Mat) <- 0
  S1P.Mat[lower.tri(S1P.Mat)] <- - S1P.Mat[upper.tri(S1P.Mat)]
  S2P.Mat <- matrix(data = sample(c(-5:-1,1:5), size = n.teams^2, prob = pv, replace = TRUE), ncol = n.teams, nrow = n.teams)
  diag(S2P.Mat) <- 0
  S2P.Mat[lower.tri(S2P.Mat)] <- - S2P.Mat[upper.tri(S2P.Mat)]
  S3P.Mat <- matrix(data = sample(c(-5:-1,1:5), size = n.teams^2, prob = pv, replace = TRUE), ncol = n.teams, nrow = n.teams)
  diag(S3P.Mat) <- 0
  S3P.Mat[lower.tri(S3P.Mat)] <- - S3P.Mat[upper.tri(S3P.Mat)]
  S3P.Mat[sign(S1P.Mat) == sign(S2P.Mat)] <- 0
  # aggregate game data
  S12P.Mat <- S1P.Mat + S2P.Mat
  S123P.Mat <- S12P.Mat + S3P.Mat
  W.Mat <- sign(S1P.Mat) + sign(S2P.Mat) + sign(S3P.Mat)
  # generate matrix
  V.Mat  <- cbind(Wins     = rowSums(x = sign(W.Mat)),
                  S1.Win   = rowSums(x = sign(S1P.Mat)),
                  S2.Win   = rowSums(x = sign(S2P.Mat)),
                  S12.Win  = rowSums(x = sign(S12P.Mat)),
                  S3.Win   = rowSums(x = sign(S3P.Mat)),
                  #S123.Win = rowSums(x = sign(S123P.Mat)),
                  S1.Differece   = rowSums(x = S1P.Mat),
                  S2.Differece   = rowSums(x = S2P.Mat),
                  S12.Differece  = rowSums(x = S12P.Mat),
                  S3.Differece   = rowSums(x = S3P.Mat),
                  S123.Differece = rowSums(x = S123P.Mat)
  )
  return(cor(V.Mat, method = "kendall"))
}

system.time(mc.values2 <- t(sapply(1:n.tests, function(ri) { as.vector(generateRandTournament2()) })))
mc.taumat2 <- matrix(colMeans(mc.values2, na.rm = TRUE), nrow = 10, ncol = 10, dimnames = list(rownames(gc.tau),colnames(gc.tau))) # removing NAs neccessary, since sets without any variance can be created

mc.taumat2[upper.tri(mc.taumat2)] <- NA
ggplot(melt(mc.taumat2, na.rm = TRUE), aes(Var1, Var2, fill = value)) + geom_tile(colour = "white") + 
  geom_text(aes(Var1, Var2, label = round(x = value, digits = 2)), color = "black", size = 4) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), axis.title.x = element_blank(),
        axis.title.y = element_blank(), panel.grid.major = element_blank(), panel.border = element_blank(), panel.background = element_blank(), axis.ticks = element_blank()) +
  scale_fill_gradient(low = "white", high = "steelblue", name = "Kendall's Tau")
