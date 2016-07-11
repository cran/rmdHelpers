myFrac <-
function(num, denom = NULL, format = "markdown"){
  if(is.null(denom)){
    temp <- strsplit(as.character(num),"/")
    num <- lapply(temp,function(x){x[1]})
    denom <- lapply(temp,function(x){x[2]})
  }
  if(format == "markdown"){
    paste0("^",num,"^/~",denom,"~")  
  } else if(format == "latex"){
    paste0("$\\frac{",num,"}{",denom,"}$")
  } else{
    stop("argument `format` must be one of 'markdown' or 'latex'")
  }
  
}
