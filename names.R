get.IV.names <- function(){
  tmp = c("n.cat","s.size","i.diff","i.disc","n.item","reli")
  return(tmp)
}

get.DV.names <- function(){
  tmp = c("type1")
  return(tmp)
}

get.IV.length <- function(){
  return(length(get.IV.names()))
}

get.DV.length <- function(){
  return(length(get.DV.names()))
}