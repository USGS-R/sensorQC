get_block_flag_names <- function(sqc){
  
  prefix <- 'block'
  n_flags <- length(sqc$block_stats)
  
  flag_names <- vector('character', n_flags)
  for (i in 1:n_flags){
    expr <- sqc$block_stats[[i]]$alias # is expression human readable
    type <- sqc$block_stats[[i]]$type
    flag_names[i] <- squeeze_names(c(prefix, type, expr))

  }
  return(flag_names)
}

squeeze_names <- function(strings){
  str_out <- paste(strings, collapse = '_')
  str_out <- gsub(" ","", str_out , fixed=TRUE)
  return(str_out)
}

flag_out_squeeze <- function(flag_names, flag_mat){
  
  flag_out <- vector("character", nrow(flag_mat))
  for (j in 1:nrow(flag_mat)){
    flag_out[j] <- paste(flag_names[flag_mat[j,]], collapse = '&')
  }
  return(flag_out)
}