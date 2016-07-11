formatEffectTable <- 
  function(object, level = 0.95
           , estDigits = 2
           , pDigits = 4
           , cleanFactors = TRUE){

    cis <- 
      stats::confint(object, level = level) %>%
      round(estDigits) %>%
      as.data.frame() %>%
      stats::setNames(c("Lower","Upper")) %>%
      mutate(Parameter = row.names(.)) 
    
    if(cleanFactors){
      logicals <- attr(summary(object)$terms,"dataClasses")[attr(summary(object)$terms,"dataClasses") == "logical"]
      
      toAddLogicals <- paste0(names(logicals), ": TRUE")
      names(toAddLogicals) <- paste0(names(logicals),"TRUE")
      
      # Add a separator for the factor levels
      namesToChange <-
        lapply(names(object$xlevels), function(thisX){
          out <- paste(thisX, object$xlevels[[thisX]], sep = ": ")
          names(out) <- paste(thisX, object$xlevels[[thisX]], sep = "")
          return(out)
        }) %>%
        unlist %>%
        c(toAddLogicals)
      
    } else{
      namesToChange <- NULL
    }
    
    
    
    summary(object)$coefficients %>%
      as.data.frame() %>%
      mutate(Parameter = row.names(.)) %>%
      mutate(Estimate = round(Estimate, estDigits)
             , `P-value` = round(.[,4], pDigits)) %>%
      inner_join(cis, by = "Parameter") %>%
      select(Parameter
             , Estimate
             , Lower
             , Upper
             , `P-value`) %>%
      mutate(Parameter = ifelse(Parameter %in% names(namesToChange)
                                , namesToChange[Parameter]
                                , Parameter))
  }