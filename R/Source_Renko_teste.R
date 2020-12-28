krenko = function(Ativo, size, threshold = 1)
{
  ## JAN/2019
  ## Guilherme Kinzel
  
  ## This code was only possible by RomanAbashin. Data.table manipulation and ggplot
  ## package used: rrenko package, https://github.com/RomanAbashin/rrenko
  
  ## 'Ativo' need to be a xts, with one of columns named 'close'. Or, it will be used the last column (as a OHLC)
  
  # require(data.table)
  # require(xts)
  
  if (!is.xts(Ativo)) {
    stop("X should be a xts")
  }
  
  whereclose <- which(tolower(names(Ativo)) == "close")
  
  if (length(whereclose) > 0) {
    price <- Ativo[, whereclose]
  } else{
    warning("Column 'Close' was not found. Using the last column")
    price <- Ativo[, ncol(Ativo)]
  }
  
  data <- data.table(date = index(Ativo), close = as.numeric(price))
  names(data) <- c("date", "close")
  
  data$corridor_bottom <- size * floor((data$close - 1E-6) / size)
  # browser()
  data <- data[, head(.SD, 1), by = .(corridor_bottom, rleid(corridor_bottom))]
  
  data$corridor_top <- rep(NA, length.out = dim(data)[1])
  data$direction <- rep(NA, length.out = dim(data)[1])
  data$base <- rep(NA, length.out = dim(data)[1])
  
  data$direction[1] <- "up"
  data$base[1] <- data$corridor_bottom[1]
  j <- 1
  
  if (dim(data)[1] <= 1) {
    stop("size too big")
  }

  for (i in 2:nrow(data)) {
    fDif <- (data$corridor_bottom[j] - data$corridor_bottom[i]) / size
    # if(i == nrow(data))
    # {
    #   browser()
    # }
    ## avoid floating point error
    if (round(abs(fDif), 1) <= threshold)next
    iJump = abs(fDif)-1
    # browser()
    if (fDif < 0) {
      data$direction[i] <- "up"
      data$corridor_bottom[i] <- data$corridor_bottom[i] - size
      data$corridor_top[i] <- data$corridor_bottom[i] + size
      j <- i
      
    } else if (fDif > 0) {
      data$direction[i] <- "down"
      data$corridor_bottom[i] <- data$corridor_bottom[i] + size
      data$corridor_top[i] <- data$corridor_bottom[i] + size
      j <- i
    }
  }
# browser()
  data <- data[!is.na(direction)]
  data$base <- data$corridor_bottom
  data <- tail(data, -1)
}
