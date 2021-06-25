#' @title Stat calculation formula for LGPE
#' @param stat stat id (1-6)
#' @param iv IV for the stat
#' @param av number of AVs gained in the stat
#' @param level level of the pokemon
#' @param nature nature id (1-25); see nature.names
#' @param happiness happiness modifier (1 + (1-10)%)
statCalcFun = function(
  stat = 1, 
  iv = 31, 
  av = 0, 
  level = 5, 
  nature = 1, 
  happiness = 1){
  base = base.stats[stat]
  if(stat == 1){
    val <- floor((2 * base + iv) * level / 100) + level + 10 + av
  }else{
    nature.effect <- nature.effects[stat - 1, nature]
    val <- floor(floor(floor((2 * base + iv) * level / 100 + 5) * nature.effect) * happiness) + av
  }
  return(val)
}