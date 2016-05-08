# function for plotting barplots according to results of an aggreate function
# input is the same as aggregate()
# returns a function, where the axis labels can be changed through the parameters xlab, ylab, main
barplot0 <- function(...){
  df <- aggregate(...)
  return(function(main=NULL, xlab=NULL, ylab=NULL){
    xlab <- ifelse(is.null(xlab), names(df)[1], xlab)
    ylab <- ifelse(is.null(ylab), names(df)[2], ylab)
    main <- ifelse(is.null(main), NA, main)
    barplot(height = df[,2], names.arg = df[,1]%>%as.character(),  xlab = xlab, ylab=ylab)
  })
}


multinom0 <- function(formula, data, k, ...){
  set.seed(1337)
  nObs <- nrow(data)
  Obs <- 1:nObs
  
  for(i in 1:k){
    if(i == k) tmp_samples <- Obs
    else tmp_samples <- sample(Obs, round(nObs/k,0))
    
    if(!exists("sets")) sets <- list(data[tmp_samples,])
    else sets <- append(sets, list(data[tmp_samples,]))
    
    Obs <- Obs[match(Obs,tmp_samples) %>% is.na]
  }
  
  outPut <- function(predictor, ...){
    for(i in 1:length(sets)){
      train_tmp <- rbindlist(sets[-i])
      test_tmp <- sets[[i]]
      model <- multinom(formula=formula, data = train_tmp, ...)
      #model <- step(model, trace = 0) # this doent work here
      newY <- predict(model, newdata = test_tmp)
      
      TMP <- substitute(select(test_tmp, X), list(X = as.name(predictor))) %>% eval %>% unlist
      confMat <- table(TMP, newY)
      
      if(!exists("results")) results <- sum(diag(confMat)/sum(confMat))
      else results <- append(results, sum(diag(confMat)/sum(confMat)))
    }
    
    return(results) # return vector with correct predictions
  }
  return(outPut)
}
