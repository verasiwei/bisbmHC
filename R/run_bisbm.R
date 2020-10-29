#' Run graph-tool based bisbm parallelly
#'
#' \code{run_bisbm} returns lists of sbm results
#'
#' @param ntimes The number of times for running sbm
#' @param ncore The number of cores required
#' @param dat The data which satisfy the requirements of the formate of runnning sbm
#' @return Lists of sbm results
#'
#' @export
#'
#' @examples
#' sbmresults <- run_bisbm(1000,10,pheno_dat)

run_bisbm <- function(ntimes,ncore,dat){

  phenome_dat_long_list <- lapply(1:ntimes,function(x) dat)

  plan(multisession, workers = ncore)
  bisbm_result <- furrr::future_map(phenome_dat_long_list, run_sbm)

  #check missing
  miss <- which(sapply(bisbm_result,function(x) nrow(x)==0))

  while(length(miss)!=0){
    for (i in 1:length(miss)) {
      bisbm_result[[miss[i]]] <- run_sbm(dat)
    }
    miss <- which(sapply(bisbm_result,function(x) nrow(x)==0))
  }

  return(bisbm_result)
}
