#' weigth t-Distributed Stochastic Neighbor Embedding
#'
#' @param X matrix; Data matrix (each row is an observation, each column is a variable)
#' @param ... Other arguments that can be passed to Rtsne
#'
#' @return List with the following elements:
#' \item{Y}{Matrix containing the new representations for the objects}
#' \item{N}{Number of objects}
#' \item{origD}{Original Dimensionality before TSNE (only when \code{X} is a data matrix)}
#' \item{perplexity}{See above}
#' \item{theta}{See above}
#' \item{costs}{The cost for every object after the final iteration}
#' \item{itercosts}{The total costs (KL-divergence) for all objects in every 50th + the last iteration}
#' \item{stop_lying_iter}{Iteration after which the perplexities are no longer exaggerated}
#' \item{mom_switch_iter}{Iteration after which the final momentum is used}
#' \item{momentum}{Momentum used in the first part of the optimization}
#' \item{final_momentum}{Momentum used in the final part of the optimization}
#' \item{eta}{Learning rate}
#' \item{exaggeration_factor}{Exaggeration factor used to multiply the P matrix in the first part of the optimization}
#' \item{weight}{the weight for cost}
#'
#' @importFrom stats model.matrix na.fail prcomp rnorm dist
#' @importFrom magrittr %>%
#'
#' @export
WTSNE <- function (X, ...) {
  UseMethod("WTSNE", X)
}





#' @export
WTSNE.default <- function(X, dims = 2, initial_dims = 50,
                          perplexity = 30, check_duplicates = TRUE,
                          pca = TRUE, partial_pca = FALSE, max_iter = 1000,
                          verbose = getOption("verbose", FALSE), is_distance = FALSE,
                          Y_init = NULL, pca_center = TRUE, pca_scale = FALSE,
                          normalize = TRUE, stop_lying_iter = ifelse(is.null(Y_init), 250L,
                                                                     0L), mom_switch_iter = ifelse(is.null(Y_init), 250L, 0L),
                          momentum = 0.5, final_momentum = 0.8, eta = 200,
                          exaggeration_factor = 12,weight = rep(1,NROW(X)),...){

  if (!is.logical(is_distance)) { stop("is_distance should be a logical variable")}
  if (!is.matrix(X)) { stop("Input X is not a matrix")}
  if (is_distance & !(is.matrix(X) & (nrow(X)==ncol(X)))) { stop("Input is not an accepted distance matrix") }
  if (!(is.logical(pca_center) && is.logical(pca_scale)) ) { stop("pca_center and pca_scale should be TRUE or FALSE")}
  if (!is.wholenumber(initial_dims) || initial_dims<=0) { stop("Incorrect initial dimensionality.")}
  tsne.args <- .check_tsne_params(nrow(X), dims=dims, perplexity=perplexity, max_iter=max_iter, verbose=verbose,
                                  Y_init=Y_init, stop_lying_iter=stop_lying_iter, mom_switch_iter=mom_switch_iter,
                                  momentum=momentum, final_momentum=final_momentum, eta=eta, exaggeration_factor=exaggeration_factor,weight = weight)

  # Check for missing values
  X <- na.fail(X)

  if (!is_distance) {
    if (pca) {
      if(verbose) cat("Performing PCA\n")
      if(partial_pca){
        if (!requireNamespace("irlba", quietly = TRUE)) {stop("Package \"irlba\" is required for partial PCA. Please install it.", call. = FALSE)}
        X <- irlba::prcomp_irlba(X, n = initial_dims, center = pca_center, scale = pca_scale)$x
      }else{
        if(verbose & min(dim(X))>2500) cat("Consider setting partial_pca=TRUE for large matrices\n")
        X <- prcomp(X, retx=TRUE, center = pca_center, scale. = pca_scale, rank. = initial_dims)$x
      }
    }
    if (check_duplicates) {
      if (any(duplicated(X))) { stop("Remove duplicates before running TSNE.") }
    }
    if (normalize) {
      X <- normalize_input(X)
    }
  }else{
    X <- X^2
  }

  out <- RTsne_R(X=X,is_distance=is_distance,args=tsne.args)
  info <- list(N=nrow(X))
  if (!is_distance) { out$origD <- ncol(X) } # 'origD' is unknown for distance matrices.
  out <- c(info, out, .clear_unwanted_params(tsne.args))
  class(out) <- c("WTSNE","list")
  out
}



#' @export
WTSNE.dist <- function(X,...,is_distance=TRUE) {
  X <- as.matrix(na.fail(X))
  WTSNE(X, ..., is_distance=is_distance)
}


#' @export
WTSNE.data.frame <- function(X,...) {
  X <- model.matrix(~.-1,na.fail(X))
  WTSNE(X, ...)
}


RTsne_R <- function(X,is_distance,args){
  verbose <- args$verbose
  if(args$init){
    Y <- args$Y_in
    if (verbose) cat("Using user supplied starting positions\n");
  } else{
    Y <- matrix(rnorm(nrow(X)*args$no_dims,sd = 10^-2),ncol = args$no_dims)
  }
  if (verbose) cat("Using no_dims =", args$no_dims, ", perplexity =",args$perplexity,"\n");
  if (verbose) cat("Computing input similarities...\n");
  start <- Sys.time()
  if (verbose) cat("Symmetrizing...\n");
  P <- computeGaussianPerplexity(X,is_distance,args$perplexity) %>%
    Symmetrize()
  end <- Sys.time()
  if (verbose) cat("Done in ",(end - start),"\nLearning embedding...\n");
  trainIterations(P,Y,args)
}




trainIterations <- function(P,Y,args){
  verbose <- args$verbose
  P <- P*args$exaggeration_factor
  start <- Sys.time()
  total_time <- 0
  momentum <- args$momentum
  uY <- matrix(0,nrow = nrow(Y),ncol = ncol(Y))
  for(i in 1:args$max_iter){
    if(i==args$stop_lying_iter) P <- P/args$exaggeration_factor
    if(i == args$mom_switch_iter) momentum = args$final_momentum;
    DY <- computeExactGradient(P,Y,args)
    uY <- momentum*uY - args$eta*DY
    Y <- Y+uY
    Y <- zeroMean(Y)
    if((i>1&i%%50==0)|i==args$max_iter){
      end <- Sys.time()
      C <- evaluateError(P,Y,args)
      if(i == 1) {
        if (verbose) cat("Iteration ",i,": error is ",C,"\n");
      }
      else {
        total_time <-  total_time + end - start
        if (verbose) cat("Iteration ",i,": error is ",C," (50 iterations ",end - start,")\n")
      }
      start <- Sys.time()
    }
  }
  end <- Sys.time()
  total_time <-  total_time + end - start
  cost <- getCost(P,Y,args)
  if (verbose) cat("Fitting performed in ",total_time,"\n")
  out <- list(Y = Y,costs = cost)

}



computeExactGradient <- function(P,Y,args){
  dc <- matrix(0,nrow = nrow(Y),ncol = ncol(Y))
  DD <- dist(Y) %>% as.matrix()
  DD <- DD^2
  Q <- 1/(1+DD)
  diag(Q) <- 0
  sum_Q <- sum(Q)
  for(i in 1:nrow(Y)){
    for(j in 1:nrow(Y)){
      if(i!=j){
        mult <- (P[i,j]-Q[i,j]/sum_Q)*Q[i,j]*args$weight[i]
        for(k in 1:ncol(Y)){
          dc[i,] <- dc[i,] + (Y[i,] - Y[j,])*mult
        }
      }
    }
  }
  return(dc)
}

zeroMean <- function(Y){
  Y - matrix(colMeans(Y),nrow = nrow(Y),ncol = ncol(Y),byrow = T)
}



evaluateError <- function(P,Y,args){
  w <- matrix(args$weight,nrow = nrow(Y),ncol = nrow(Y))
  DD <- dist(Y) %>% as.matrix()
  DD <- DD^2
  Q <- 1/(1+DD)
  diag(Q) <- 0
  Q <- Q/sum(Q)
  C <- (w*P*log((P+1e-9)/(Q+1e-9))) %>% sum
  return(C)
}


getCost <- function(P,Y,args){
  w <- matrix(args$weight,nrow = nrow(Y),ncol = nrow(Y))
  DD <- dist(Y) %>% as.matrix()
  DD <- DD^2
  Q <- 1/(1+DD)
  diag(Q) <- 0
  Q <- Q/sum(Q)
  Cost <- (w*P*log((P+1e-9)/(Q+1e-9))) %>% rowSums()
  return(Cost)
}

computeGaussianPerplexity <- function(X,is_distance,perplexity){
  if(!is_distance){
    D <- dist(X) %>% as.matrix()
    D <- D^2
  }
  else{
    D <- X
  }
  diag(D) <- NA
  apply(D, 1, function(x){
    found <- FALSE
    beta <- 1.0
    min_beta <- -Inf
    max_beta <- Inf
    tol <- 1e-5
    sum_p <- 0
    iter <- 0
    while(!found&iter<200){
      p <- exp(-beta*x)
      p[is.na(p)] <- 0
      sum_p <- sum(p)
      H <- sum(beta*x*p)/sum_p+log(sum_p)
      Hdiff <- H-log(perplexity)
      if(abs(Hdiff) < tol){
        found <- TRUE
      }else{
        if(Hdiff > 0){
          min_beta <- beta
          if(max_beta == Inf){
            beta <- beta*2
          }else{
            beta <- (beta + max_beta)/2
          }
        }else{
          max_beta <- beta
          if(min_beta == Inf){
            beta <- beta/2
          }else{
            beta <- (beta + min_beta)/2
          }
        }
      }
      iter <- iter+1
    }
    p/sum_p
  }) %>% t()
}


Symmetrize <- function(X){
  n <- nrow(X)
  for(i in 1:(n-1)){
    for(j in (i+1):n){
      X[i,j] <- X[j,i] <- X[i,j] + X[j,i]
    }
  }
  X/sum(X)
}



