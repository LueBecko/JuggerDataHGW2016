## visualise game data
library(ggplot2)
library(Matrix)
library(grid)
library(reshape2)

# first turn frame into skew symetric matrices
teams <- sort(levels(game.data$Team1))
game.S1D.mat <- sparseMatrix(i = match(game.data$Team1, teams), j = match(game.data$Team2, teams), x = game.data$S1D, dimnames = list(teams, teams), dims = rep(length(teams),2))
game.S1D.mat <- game.S1D.mat - t(game.S1D.mat)
game.S2D.mat <- sparseMatrix(i = match(game.data$Team1, teams), j = match(game.data$Team2, teams), x = game.data$S2D, dimnames = list(teams, teams), dims = rep(length(teams),2))
game.S2D.mat <- game.S2D.mat - t(game.S2D.mat)
game.S3D.mat <- sparseMatrix(i = match(game.data$Team1, teams), j = match(game.data$Team2, teams), x = game.data$S3D, dimnames = list(teams, teams), dims = rep(length(teams),2))
game.S3D.mat <- game.S3D.mat - t(game.S3D.mat)
game.S12D.mat <- sparseMatrix(i = match(game.data$Team1, teams), j = match(game.data$Team2, teams), x = game.data$S12D, dimnames = list(teams, teams), dims = rep(length(teams),2))
game.S12D.mat <- game.S12D.mat - t(game.S12D.mat)
game.S123D.mat <- sparseMatrix(i = match(game.data$Team1, teams), j = match(game.data$Team2, teams), x = game.data$S123D, dimnames = list(teams, teams), dims = rep(length(teams),2))
game.S123D.mat <- game.S123D.mat - t(game.S123D.mat)
game.S1W.mat <- sparseMatrix(i = match(game.data$Team1, teams), j = match(game.data$Team2, teams), x = game.data$S1W, dimnames = list(teams, teams), dims = rep(length(teams),2))
game.S1W.mat <- game.S1W.mat - t(game.S1W.mat)
game.S2W.mat <- sparseMatrix(i = match(game.data$Team1, teams), j = match(game.data$Team2, teams), x = game.data$S2W, dimnames = list(teams, teams), dims = rep(length(teams),2))
game.S2W.mat <- game.S2W.mat - t(game.S2W.mat)
game.S3W.mat <- sparseMatrix(i = match(game.data$Team1, teams), j = match(game.data$Team2, teams), x = game.data$S3W, dimnames = list(teams, teams), dims = rep(length(teams),2))
game.S3W.mat <- game.S3W.mat - t(game.S3W.mat)
game.S12W.mat <- sparseMatrix(i = match(game.data$Team1, teams), j = match(game.data$Team2, teams), x = game.data$S12W, dimnames = list(teams, teams), dims = rep(length(teams),2))
game.S12W.mat <- game.S12W.mat - t(game.S12W.mat)
game.W.mat <- sparseMatrix(i = match(game.data$Team1, teams), j = match(game.data$Team2, teams), x = game.data$W, dimnames = list(teams, teams), dims = rep(length(teams),2))
game.W.mat <- game.W.mat - t(game.W.mat)

game.compilation <- data.frame(Teams = teams, Wins = rowSums(game.W.mat), S1.Win = rowSums(game.S1W.mat), S2.Win = rowSums(game.S2W.mat), S12.Wins = rowSums(game.S12W.mat), S3.Win = rowSums(game.S3W.mat, na.rm = TRUE),
                               S1.Difference = rowSums(game.S1D.mat), S2.Difference = rowSums(game.S2D.mat), S12.Difference = rowSums(game.S12D.mat), S3.Difference = rowSums(game.S3D.mat, na.rm = TRUE), S123.Difference = rowSums(game.S123D.mat))

gc.tau <- cor(game.compilation[,-1], method = "kendall")
gc.tau[upper.tri(gc.tau)] <- NA
ggplot(melt(gc.tau, na.rm = TRUE), aes(Var1, Var2, fill = value)) + geom_tile(colour = "white") + 
    geom_text(aes(Var1, Var2, label = round(x = value, digits = 2)), color = "black", size = 4) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), axis.title.x = element_blank(),
          axis.title.y = element_blank(), panel.grid.major = element_blank(), panel.border = element_blank(), panel.background = element_blank(), axis.ticks = element_blank()) +
    scale_fill_gradient(low = "white", high = "steelblue", name = "Kendall's Tau")

# start visualisation
ggplot(game.compilation, aes(Teams, Wins)) + geom_bar(stat = "identity")
pw   <- ggplot(game.compilation, aes(reorder(Teams, Wins), Wins)) + geom_bar(stat = "identity")
pw1  <- ggplot(game.compilation, aes(reorder(Teams, Wins), S1.Win)) + geom_bar(stat = "identity")
pw2  <- ggplot(game.compilation, aes(reorder(Teams, Wins), S2.Win)) + geom_bar(stat = "identity")
pw12 <- ggplot(game.compilation, aes(reorder(Teams, Wins), S12.Wins)) + geom_bar(stat = "identity")
pushViewport(viewport(layout = grid.layout(2, 2)))
print(pw,   vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(pw1,  vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(pw2,  vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(pw12, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))

pd1   <- ggplot(game.compilation, aes(reorder(Teams, Wins), S1.Difference)) + geom_bar(stat = "identity")
pd2   <- ggplot(game.compilation, aes(reorder(Teams, Wins), S2.Difference)) + geom_bar(stat = "identity")
pd12  <- ggplot(game.compilation, aes(reorder(Teams, Wins), S12.Difference)) + geom_bar(stat = "identity")
pd123 <- ggplot(game.compilation, aes(reorder(Teams, Wins), S123.Difference)) + geom_bar(stat = "identity")
pushViewport(viewport(layout = grid.layout(2, 2)))
print(pd1,   vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(pd2,  vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(pd12,  vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(pd123, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
