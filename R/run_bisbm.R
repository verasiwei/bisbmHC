#' Run graph-tool based bisbm parallelly
#'
#' \code{run_bisbm} returns lists of bisbm results
#'
#' @param ntimes The number of times for running sbm
#' @param ncore The number of cores required
#' @param dat The data which satisfy the requirements of the format of runnning sbm
#' @return Lists of sbm results, 
#'         each element of the list is a dataframe of biSBM cluster results
#'         
#' @import future dplyr pheSBMR
#' 
#' @export
#'
#' @examples
#' #run bisbm twp times
#' data("phenome_dat_long")
#' library(dplyr,quietly=TRUE)
#' 
#' head(phenome_dat_long)
#' 
#' bisbmresults <- run_bisbm(2,2,phenome_dat_long)
#' head(bisbmresults[[1]])


run_bisbm <- function(ntimes,ncore,dat,...){

  phenome_dat_long_list <- lapply(1:ntimes,function(x) dat)

  plan(multisession, workers = ncore)
  
  bisbm_result <- furrr::future_map(phenome_dat_long_list, run_bisbm_docker)

  #check missing
  miss <- which(sapply(bisbm_result,function(x) nrow(x)==0))
  print("biSBM failed to capture the structure")

  while(length(miss)!=0){
    for (i in 1:length(miss)) {
      bisbm_result[[miss[i]]] <- run_sbm(dat)
    }
    miss <- which(sapply(bisbm_result,function(x) nrow(x)==0))
  }

  return(bisbm_result)
}
