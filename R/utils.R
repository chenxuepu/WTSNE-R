


#' Normalize input data matrix
#'
#' Mean centers each column of an input data matrix so that it has a mean of zero.
#' Scales the entire matrix so that the largest absolute of the centered matrix is equal to unity.
#'
#' @param X matrix; Input data matrix with rows as observations and columns as variables/dimensions.
#'
#' @details
#' Normalization avoids numerical problems when the coordinates (and thus the distances between observations) are very large.
#' Directly computing distances on this scale may lead to underflow when computing the probabilities in the t-SNE algorithm.
#' Rescaling the input values mitigates these problems to some extent.
#'
#' @return A numeric matrix of the same dimensions as \code{X} but centred by column and scaled to have a maximum deviation of 1.
#'
#' @author
#' Aaron Lun
#'
#' @examples
#' iris_unique <- unique(iris) # Remove duplicates
#' iris_matrix <- as.matrix(iris_unique[,1:4])
#' X <- normalize_input(iris_matrix)
#' colMeans(X)
#' range(X)
#' @export
normalize_input <- function(X) {
  apply(X,2,function(x){
    x <- x-mean(x)
    x/max(abs(x))
  })
}


is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)
  # Checks if the input is a whole number.
{
  abs(x - round(x)) < tol
}

.check_tsne_params <- function(nsamples, dims, perplexity, max_iter, verbose, Y_init, stop_lying_iter, mom_switch_iter,
                               momentum, final_momentum, eta, exaggeration_factor,weight)
  # Checks parameters for the t-SNE algorithm that are independent of
  # the format of the input data (e.g., distance matrix or neighbors).
{
  if (!is.wholenumber(dims) || dims < 1 || dims > 3) { stop("dims should be either 1, 2 or 3") }
  if (!is.wholenumber(max_iter) || !(max_iter>0)) { stop("number of iterations should be a positive integer")}
  if (!is.null(Y_init) && (nsamples!=nrow(Y_init) || ncol(Y_init)!=dims)) { stop("incorrect format for Y_init") }

  if (!is.numeric(perplexity) || perplexity <= 0) { stop("perplexity should be a positive number") }
  if (!is.wholenumber(stop_lying_iter) || stop_lying_iter<0) { stop("stop_lying_iter should be a positive integer")}
  if (!is.wholenumber(mom_switch_iter) || mom_switch_iter<0) { stop("mom_switch_iter should be a positive integer")}
  if (!is.numeric(momentum) || momentum < 0) { stop("momentum should be a positive number") }
  if (!is.numeric(final_momentum) || final_momentum < 0) { stop("final momentum should be a positive number") }
  if (!is.numeric(eta) || eta <= 0) { stop("eta should be a positive number") }
  if (!is.numeric(exaggeration_factor) || exaggeration_factor <= 0) { stop("exaggeration_factor should be a positive number")}
  if (sum(weight)!=nsamples) weight <- (weight/sum(weight))*nsamples

  if (nsamples - 1 < 3 * perplexity) { stop("perplexity is too large for the number of samples")}

  init <- !is.null(Y_init)
  if (init) {
    Y_init <- Y_init # transposing for rapid column-major access.
  } else {
    Y_init <- matrix()
  }

  list(no_dims=dims, perplexity=perplexity, max_iter=max_iter, verbose=verbose,
       init=init, Y_in=Y_init,
       stop_lying_iter=stop_lying_iter, mom_switch_iter=mom_switch_iter,
       momentum=momentum, final_momentum=final_momentum,
       eta=eta, exaggeration_factor=exaggeration_factor,weight=weight)
}

.clear_unwanted_params <- function(args)
  # Removing parameters that we do not want to store in the output.
{
  args$Y_in <- args$init <- args$no_dims <- args$verbose <- NULL
  args
}
