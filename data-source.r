## typing in the game results - by hand... like a caveman...

game.data <- data.frame(game = numeric(), Team1 = character(), Team2 = character(), S1P1 = numeric(),
                        S1P2 = numeric(), S2P1 = numeric(), S2P2 = numeric(), S3P1 = numeric(), S3P2 = numeric())

game.data <- rbind(game.data, data.frame(game = 1, Team1 = "Torpedo Flauschi",              Team2 = "Rasenschach",                   S1P1 = 5, S1P2 = 3, S2P1 = 5, S2P2 = 2, S3P1 = NA, S3P2 = NA))
game.data <- rbind(game.data, data.frame(game = 2, Team1 = "Die leere Menge",               Team2 = "Blutgrätsche",                  S1P1 = 5, S1P2 = 1, S2P1 = 5, S2P2 = 0, S3P1 = NA, S3P2 = NA))
game.data <- rbind(game.data, data.frame(game = 3, Team1 = "Maximal konsitente Teilklasse", Team2 = "Torpedo Flauschi",              S1P1 = 1, S1P2 = 5, S2P1 = 0, S2P2 = 5, S3P1 = NA, S3P2 = NA))
game.data <- rbind(game.data, data.frame(game = 4, Team1 = "Rasenschach",                   Team2 = "Die leere Menge",               S1P1 = 3, S1P2 = 5, S2P1 = 2, S2P2 = 5, S3P1 = NA, S3P2 = NA))
game.data <- rbind(game.data, data.frame(game = 5, Team1 = "Blutgrätsche",                  Team2 = "Maximal konsitente Teilklasse", S1P1 = 5, S1P2 = 0, S2P1 = 5, S2P2 = 1, S3P1 = NA, S3P2 = NA))
game.data <- rbind(game.data, data.frame(game = 6, Team1 = "Torpedo Flauschi",              Team2 = "Die leere Menge",               S1P1 = 2, S1P2 = 5, S2P1 = 5, S2P2 = 2, S3P1 = 4,  S3P2 = 5))
game.data <- rbind(game.data, data.frame(game = 7, Team1 = "Rasenschach",                   Team2 = "Blutgrätsche",                  S1P1 = 4, S1P2 = 5, S2P1 = 5, S2P2 = 4, S3P1 = 4,  S3P2 = 5))
game.data <- rbind(game.data, data.frame(game = 8, Team1 = "Maximal konsitente Teilklasse", Team2 = "Die leere Menge",               S1P1 = 0, S1P2 = 5, S2P1 = 4, S2P2 = 5, S3P1 = NA, S3P2 = NA))
game.data <- rbind(game.data, data.frame(game = 9, Team1 = "Torpedo Flauschi",              Team2 = "Blutgrätsche",                  S1P1 = 5, S1P2 = 3, S2P1 = 5, S2P2 = 0, S3P1 = NA, S3P2 = NA))
game.data <- rbind(game.data, data.frame(game = 10,Team1 = "Rasenschach",                   Team2 = "Maximal konsitente Teilklasse", S1P1 = 5, S1P2 = 1, S2P1 = 5, S2P2 = 1, S3P1 = NA, S3P2 = NA))

## additional scores
game.data$S1D <- game.data$S1P1 - game.data$S1P2
game.data$S2D <- game.data$S2P1 - game.data$S2P2
game.data$S3D <- game.data$S3P1 - game.data$S3P2
game.data$S12D <- game.data$S1D + game.data$S2D
game.data$S123D <- ifelse(!is.na(game.data$S3D), game.data$S12D + game.data$S3D, game.data$S12D)
game.data$S1W <- sign(game.data$S1D)
game.data$S2W <- sign(game.data$S2D)
game.data$S3W <- sign(game.data$S3D)
game.data$S12W <- game.data$S1W + game.data$S2W
game.data$W <- sign(rowSums(cbind(game.data$S1W,game.data$S2W,game.data$S3W), na.rm = TRUE))
