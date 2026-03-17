

# probit with min/max margins
mprobit <- function(p, x) {
  
  # p is a vector of parameters
  # the first two are the regular parameters of the normal distribution:
  # p[1] is the mean
  # p[2] is the standard deviation
  
  # the second two parameters set the bounds:
  # p[3] is the lower margin
  # p[4] is the upper margin
  
  # if only 3 parameters are given,
  # it is taken to be both the upper and lower margin
  
  if (length(p) == 3) {
    p[4] <- p[3]
  }
  
  # p[3] + p[4] has to be lower than 1:
  if (p[3] + p[4] >= 1) {
    cat('warning: impossible margins (returning zeroes)\n')
    return(rep(0, length(x))) # might need to have a better value to return in case if invalid bounds
F  }
  
  # x is a vector of values
  
  # probit is implemented in R as the pnorm function,
  # but we add bounds:
  # print(p[1])
  # print(p[2])
  # print(x)
  # print(pnorm(x, mean=p[1], sd=p[2] ))
  
  return( p[3] + ((1 - p[3] - p[4]) * pnorm(x, mean=p[1], sd=p[2], lower.tail=TRUE, log.p=FALSE)) )
  
}

mprobit.nll <- function(p, x, y) {
  
  # p is a vector of parameters
  # the first two are the regular parameters of the normal distribution:
  # p[1] is the mean
  # p[2] is the standard deviation
  
  # the second two parameters set the bounds:
  # p[3] is the lower margin
  # p[4] is the upper margin
  
  # x is a vector of values
  # y is a vector of values (0 or 1)
  
  # probit is implemented in R as the pnorm function,
  # but we add bounds:
  
  prob <- mprobit(p, x)
  
  # small step away from what log() can handle:
  prob[which((prob-1) == 0)] <- 1-.Machine$double.eps
  y[which(y == 0)] <- .Machine$double.eps
  
  # this is the negative log likelihood,
  # which (when minimized) should give the maximum likelihood estimate
  nll <- -sum(y * log(prob) + (1 - y) * log(1 - prob))
  # print(nll)
  if (!is.finite(nll)) {
    # if the nll is not finite, return a large number
    # this will make optim() stop trying to fit the model
    prob <- rep(.Machine$double.eps, length(x)) # flat function close to zero
    nll <- -sum(y * log(prob) + (1 - y) * log(1 - prob))
    # cat(sprintf('replace nll with %f\n', nll))
  }

  return(nll)
  
}

#' @title Probit regression with min/max margins
#' @description 
#' This function fits a probit regression model with min/max margins to the data.
#' It uses the `mprobit` function to calculate the probit function with margins.
#' It also uses the `mprobit.nll` function to calculate the negative log-likelihood 
#' for optimization.
#' If IDs and data are specified, x and y are ignored.
#' @param IDs A vector of observation IDs (any repetitions are used).
#' @param data A data frame with the predictor and response variables ('x' and 'y')
#' as well as an ID column.
#' @param x A vector of predictor variables.
#' @param y A vector of response variables (0 or 1).
#' @param start A vector of starting values for the parameters.
#' @param lower A vector of lower bounds for the parameters.
#' @param upper A vector of upper bounds for the parameters.
#' @param maxit The maximum number of iterations for the optimization.
#' @param FUN The function to use for aggregation (default is `mean`).
#' @export
fit.mprobit <- function(IDs=NULL, data=NULL, x=NULL, y=NULL, start, lower, upper, maxit=1000, FUN=mean) {
  
  # this function will fit a probit function with margins to the data
  # data can be specified in two ways:
  # - using IDs and a data frame data (useful in bootstrap procedures)
  # - using x and y vectors (useful for quick fits)
  
  # IDs is a vector of observation IDs (any repetitions are used)
  # data is a data frame with the predictor and response variables ('x' and 'y') as well an ID column
  
  # x is a vector of predictor variables
  # y is a vector of response variables (values between 0 or 1)
  

  if (!is.null(IDs) & !is.null(data)) {
    # cat('creating data based on IDs\n')
    newdat <- NA
    for (ID in IDs) {
      if (is.data.frame(newdat)) {
        newdat <- rbind(newdat, data[data$ID == ID, ])
      } else {
        newdat <- data[data$ID == ID, ]
      }
    }
    aggdat <- aggregate(y ~ x, data=newdat, FUN=FUN)
    x <- aggdat$x
    y <- aggdat$y
  } else {
    if (is.null(x) | is.null(y)) {
      stop('x and y must be specified if IDs and data are not provided')
    }
  }
  
  # start is a vector of starting values for the parameters
  # lower is a vector of lower bounds for the parameters
  # upper is a vector of upper bounds for the parameters
  
  # maxit is the maximum number of iterations for the optimization
  
  # fit the model using optim
  fit <- optim(par=start,
               fn=mprobit.nll,
               x=x,
               y=y,
               method="L-BFGS-B",
               lower=lower,
               upper=upper,
               control=list(maxit=maxit))
  
  return(fit)
  
}

descr.mprobit <- function(p, prob=0.5, hs=0.0000001) {
  
  # p is a vector of parameters, in order:
  # mean, sd, lower margin, upper margin
  
  # if p has only 3 parameters,
  # the third is both the lower and upper margin:
  if (length(p) == 3) {
    p[4] <- p[3]
    PSE <- p[1]
  } else {
    # depending on the margins, the point where the probit function
    # crosses 50% (or another probability) is not the mean of the distribution
    prob <- ((prob - p[3]) / (1 - p[3] - p[4]))
    PSE <- qnorm(prob, mean=p[1], sd=p[2])
  }
  
  # we also need to calculate the slope of the probit function at this point
  # slope <- (diff(mprobit(p=p, x=PSE+c(-hs, hs))) / (2*hs))
  
  # slope of pnorm is dnorm
  slope <- ((1 - p[3] - p[4]) / 1) * dnorm(PSE, mean=p[1], sd=p[2])
  
  return(list(PSE=PSE, slope=slope))
  
}

CI.mprobit <- function(data, cluster=NULL, start, lower, upper, maxit=1000, iterations=1000, n=100, from=NULL, to=NULL, interval=c(0.025, 0.975)) {
  
  # data is a data frame with the predictor and response variables ('x' and 'y')
  #      as well as the observation ID ('ID')
  #      if there is a weights column, it will be used to aggregate the data
  # start is a vector of starting values for the parameters
  # lower is a vector of lower bounds for the parameters
  # upper is a vector of upper bounds for the parameters
  
  # maxit is the maximum number of iterations for the optimization
  # bootstraps is the number of bootstrap samples to generate
  # n is the number of X values to estimate the CI at
  # from is the minimum value of X (defaults to the lowest value in data)
  # to is the maximum value of X (defaults to the highest value in data)
  # interval is the confidence interval
  
  close_cluster <- FALSE
  if (is.null(cluster)) {
    ncores  <- parallel::detectCores()
    cluster   <- parallel::makeCluster(max(c(1,floor(ncores*0.5))))
    close_cluster <- TRUE
  }
  
  participants <- unique(data$ID)
  BSparticipants <- sample(participants, size=iterations*length(participants), replace=TRUE)
  BSparticipants <- matrix(BSparticipants, nrow=iterations)
  

  # fit the model using optim
  a <- parallel::parApply(cl = cluster,
                          X = BSparticipants,
                          MARGIN = 1,
                          FUN = fit.mprobit,
                          data = data,
                          start = start,
                          lower = lower,
                          upper = upper,
                          maxit=1000
                          )
  
  if (close_cluster) {stopCluster(cluster)}
  
  # all_fits <- as.data.frame(t(t(lapply(a, "[[", "par"))))
  all_fits <- matrix(unlist(lapply(a, "[[", "par")),nrow=iterations, byrow=TRUE)
  
  # evaluate all fits at the specified range / resolution:
  if (is.null(from)) {
    from <- min(data$x)
  }
  if (is.null(to)) {
    to <- max(data$x)
  }
  newX <- seq(from, to, length.out=n)
  
  
  # calculate the probit function for each fit
  # and for each newX value:
  # this is a matrix with the probit function values
  # for each fit (rows) and each newX value (columns)
  probit_values <- matrix(NA, nrow=iterations, ncol=n)
  for (i in seq_len(iterations)) {
    probit_values[i, ] <- mprobit(p = all_fits[i, ], x = newX)
  }
  # calculate the mean and CI for each newX value
  # this is a matrix with the mean and CI values
  # for each newX value (rows) and each CI value (columns)
  probit_CI <- matrix(NA, nrow=n, ncol=length(interval))
  for (i in seq_len(n)) {
    probit_CI[i, ] <- quantile(probit_values[, i], probs=interval)
  }
  # calculate the mean for each newX value
  probit_mean <- apply(probit_values, 2, mean)
  
  # create a data frame with the newX values and the mean and CI values
  probit_CI <- as.data.frame(probit_CI)
  colnames(probit_CI) <- c('lo','hi')
  probit_CI <- cbind(newX, probit_mean, probit_CI)
  colnames(probit_CI)[1] <- 'X'
  colnames(probit_CI)[2] <- 'mean'
  
  return(probit_CI)
  
}


CI.mprobit.serial <- function(data, start, lower, upper, maxit=1000, iterations=1000, n=100, from=NULL, to=NULL, interval=c(0.025, 0.975)) {
  
  # data is a data frame with the predictor and response variables ('x' and 'y')
  #      as well as the observation ID ('ID')
  #      if there is a weights column, it will be used to aggregate the data
  # start is a vector of starting values for the parameters
  # lower is a vector of lower bounds for the parameters
  # upper is a vector of upper bounds for the parameters
  
  # maxit is the maximum number of iterations for the optimization
  # bootstraps is the number of bootstrap samples to generate
  # n is the number of X values to estimate the CI at
  # from is the minimum value of X (defaults to the lowest value in data)
  # to is the maximum value of X (defaults to the highest value in data)
  # interval is the confidence interval
  
  participants <- unique(data$ID)
  BSparticipants <- sample(participants, size=iterations*length(participants), replace=TRUE)
  BSparticipants <- matrix(BSparticipants, nrow=iterations)
  
  
  # fit the model using optim
  a <- apply( X = BSparticipants,
              MARGIN = 1,
              FUN = fit.mprobit,
              data = data,
              start = start,
              lower = lower,
              upper = upper,
              maxit=1000
  )
  
  # all_fits <- as.data.frame(t(t(lapply(a, "[[", "par"))))
  all_fits <- matrix(unlist(lapply(a, "[[", "par")),nrow=iterations, byrow=TRUE)
  
  # evaluate all fits at the specified range / resolution:
  if (is.null(from)) {
    from <- min(data$x)
  }
  if (is.null(to)) {
    to <- max(data$x)
  }
  newX <- seq(from, to, length.out=n)
  
  
  # calculate the probit function for each fit
  # and for each newX value:
  # this is a matrix with the probit function values
  # for each fit (rows) and each newX value (columns)
  probit_values <- matrix(NA, nrow=iterations, ncol=n)
  for (i in seq_len(iterations)) {
    probit_values[i, ] <- mprobit(p = all_fits[i, ], x = newX)
  }
  # calculate the mean and CI for each newX value
  # this is a matrix with the mean and CI values
  # for each newX value (rows) and each CI value (columns)
  probit_CI <- matrix(NA, nrow=n, ncol=length(interval))
  for (i in seq_len(n)) {
    probit_CI[i, ] <- quantile(probit_values[, i], probs=interval)
  }
  # calculate the mean for each newX value
  probit_mean <- apply(probit_values, 2, mean)
  
  # create a data frame with the newX values and the mean and CI values
  probit_CI <- as.data.frame(probit_CI)
  colnames(probit_CI) <- c('lo','hi')
  probit_CI <- cbind(newX, probit_mean, probit_CI)
  colnames(probit_CI)[1] <- 'X'
  colnames(probit_CI)[2] <- 'mean'
  
  return(probit_CI)
  
}