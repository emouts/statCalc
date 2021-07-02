# parameters for Eevee starter
params.list <-list(
  eevee = list(base.stats = c(65, 75, 70, 65, 85, 75),
               initial.level = 5)
)

#set columns to center alignment by default
column <- function(width, ..., offset = 0){
  shiny::column(width = width, align = "center", ..., offset = offset)
}

# initialise the effect of each nature (columns) on each stat (rows)
nature.names <- c(
  "Hardy (Neutral)", "Lonely (+ATK | -DEF)", "Adamant (+ATK | -SP.ATK)", 
  "Naughty (+ATK | -SP.DEF)", "Brave (+ATK | -SPEED)",
  "Bold (+DEF | -ATK)", "Docile (Neutral)", "Impish (+DEF | -SP.ATK)", 
  "Lax (+DEF | -SP.DEF)", "Relaxed (+DEF | -SPEED)",
  "Modest (+SP.ATK | -ATK)", "Mild (+SP.ATK | -DEF)", "Bashful (Neutral)", 
  "Rash (+SP.ATK | -SP.DEF)", "Quiet (+SP.ATK | -SPEED)",
  "Calm (+SP.DEF | -ATK)", "Gentle (+SP.DEF | -DEF)", "Careful (+SP.DEF | -SP.ATK)", 
  "Quirky (Neutral)", "Sassy (+SP.DEF | -SPEED)",
  "Timid (+SPEED | -ATK)", "Hasty (+SPEED | -DEF)", "Jolly (+SPEED | -SP.ATK)", 
  "Naive (+SPEED | -SP.DEF)", "Serious (Neutral)")
nature.types <- c("+ATK", "+DEF", "+SP.ATK", "+SP.DEF", "+SPEED")
nature.named.list <- sapply(X = nature.types, simplify = FALSE, FUN = function(type){
  i <- match(type, nature.types)
  nature.names[1:5 + 5 * (i - 1)]
})
pluses <- rep(1:5, each = 5); minuses <- rep(1:5, times = 5)
nature.effects <- matrix(1, nrow = 5, ncol = 25)
for(i in 1:5){
  nature.effects[i, pluses == i] <- nature.effects[i, pluses == i] * 1.1
  nature.effects[i, minuses == i] <- nature.effects[i, minuses == i] / 1.1
}

# names for input ids
stat.names.short <- c("hp", "atk", "def", "spatk", "spdef", "speed")
# names for labels
stat.names.long <- c("HP", "Attack", "Defense", "Sp.Attack", "Sp.Defense", "Speed")
# constant for starters having max IVs
ivStarter <- 31
