#' moyenne d’un vecteur
#' Une fonction pour faire une moyenne en enlevant les valeurs manquantes
#'
#' @param x un vecteur numerique
#'
#' @return la fonction renvoie la moyenne d'un vecteur
#' @importFrom stats na.omit
#' @export
moyenne <- function(x){
  # x <-  na.omit(x)
  # sum(x)/length(x)
  stop("The function moyenne has been depreciated and will be deleted from the Epiuf package.\nPlease use base function mean()")
}
