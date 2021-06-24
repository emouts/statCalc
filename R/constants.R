nature.names <- c("Hardy", "Lonely", "Adamant", "Naughty", "Brave",
                  "Bold", "Docile", "Impish", "Lax", "Relaxed",
                  "Modest", "Mild", "Bashful", "Rash", "Quiet",
                  "Calm", "Gentle", "Careful", "Quirky", "Sassy",
                  "Timid", "Hasty", "Jolly", "Naive", "Serious")
pluses <- rep(1:5, each = 5); minuses <- rep(1:5, times = 5)
nature.effects <- matrix(1, nrow = 5, ncol = 25)
for(i in 1:5){
  nature.effects[i, pluses == i] <- nature.effects[i, pluses == i] * 1.1
  nature.effects[i, minuses == i] <- nature.effects[i, minuses == i] / 1.1
}

stat.names.short <- c("hp", "atk", "def", "spatk", "spdef", "speed")
stat.names.long <- c("HP", "Attack", "Defense", "Sp.Attack", "Sp.Defense", "Speed")
