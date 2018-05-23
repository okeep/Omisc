#' endparallel
#'
#' @param clust dummy variable so that the function executes
#'
#' @return NA
#' @export
#'
#' @examples print("NA")
endparallel<-function(clust){
  return(parallel::stopCluster(clust))
}
