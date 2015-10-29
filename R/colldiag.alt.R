#' Collinearity evaluation for more general models
#'
#' Calculates condition indexes and variance decomposition proportions
#' of a model
#'
#' @import perturb
#' @param mod model to evaluate for collinearity
#' @export
colldiag.alt <- function (mod) {
  # written by Beau Bruce (2010)
  # based on the collingenmodv9c.sas by Mathew Zack and modified by Jim Singleton and
  # colldiag from John Hendrickx's perturb package
  result <- NULL
  vcv <- vcov(mod)
  ivcv <- solve(vcv)
  divcv = diag(, ncol(vcv)) * diag(ivcv)
  scale <- solve(sqrt(divcv))
  R <- scale %*% ivcv %*% scale
  svdR <- svd(R)
  val <- svdR$d
  vec <- svdR$v
  condindx <- sqrt(val[1] / val)
  Phi = t((vec ^ 2) %*% (diag(, length(val)) * (1 / val)))
  pi <- prop.table(Phi, 2)
  dim(condindx) <- c(length(condindx), 1)
  colnames(condindx) <- "cond.index"
  rownames(condindx) <- 1:nrow(condindx)
  colnames(pi) <- colnames(vcv)
  result$condindx <- condindx
  result$pi <- pi
  class(result) <- "colldiag"
  result
}
