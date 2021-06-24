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