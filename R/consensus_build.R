#' Build the consensus graph
#'
#' \code{consensus_build} returns the long format of consensus matrix (node to node connection weight)
#'
#' @param id_label The lable of nodes
#' @param id_name column name of the variable you want to cluster
#' @param bisbm The list of bisbm results from run_bisbm
#' @param parallel Whether parallel computing
#' @param workers The number of cores
#' @return The long format of consensus matrix (node to node connection weight)
#'
#' @import future dplyr furrr purrr tidyr
#' @export
#'
#' @examples
#' data("bisbmresults")
#' library(dplyr,quietly=TRUE,warn.conflicts=FALSE)
#' 
#' id_label <- bisbmresults[[1]] %>%
#'          filter(type=="id") %>%
#'          filter(level==0) %>%
#'          dplyr::select(node) %>%
#'          arrange(node)
#' id_label <- id_label$node
#' 
#' consensus_dat <- consensus_build(id_label,"id",bisbmresults,parallel=TRUE,workers=10)
#' head(consensus_dat)
 
consensus_build <- function(id_label,id_name,bisbm,parallel=TRUE,workers=10,...) {
  #build the empty long format of consensus matrix
  consensus_dat <- tidyr::expand_grid(from=id_label,to=id_label)
  # consensus_dat <- consensus_dat[!duplicated(data.frame(t(apply(consensus_dat,1,sort)))),]
  weight <- rep(0,nrow(consensus_dat))
  
  #aggregation of the adjacency matrix
  for(i in 1:length(bisbm)){
    affinity_dat <- bisbm[[i]] %>%
      filter(type==id_name) %>%
      filter(level==0) %>%
      arrange(cluster)
    
  #calculate the times two nodes connected (in the same cluster in each iteration)
  if(parallel) {
    plan(multisession, workers = workers)
    affinity_dat <- furrr::future_map_dfr(unique(affinity_dat$cluster), function(x){
      
      dat <- tidyr::expand_grid(from=affinity_dat$node[affinity_dat$cluster==x],
                         to=affinity_dat$node[affinity_dat$cluster==x])
      # dat <- dat[!duplicated(data.frame(t(apply(dat,1,sort)))),]
      dat$weight <- rep(1,nrow(dat))
      return(dat)
      
    })
    # affinity_dat <- affinity_dat %>%
    #   relocate(.,to,from) %>%
    #   dplyr::rename(from=to,to=from) %>%
    #   bind_rows(affinity_dat) %>%
    #   distinct(from,to,.keep_all = TRUE)
    
  } else {
    affinity_dat <- purrr::map_dfr(unique(affinity_dat$cluster), function(x){
      
      dat <- tidyr::expand_grid(from=affinity_dat$node[affinity_dat$cluster==x],
                         to=affinity_dat$node[affinity_dat$cluster==x])
      # dat <- dat[!duplicated(data.frame(t(apply(dat,1,sort)))),]
      dat$weight <- rep(1,nrow(dat))
      return(dat)
      
    })
    # affinity_dat <- affinity_dat %>%
    #   relocate(.,to,from) %>%
    #   dplyr::rename(from=to,to=from) %>%
    #   bind_rows(affinity_dat) %>%
    #   distinct(from,to,.keep_all = TRUE)

  }
    
  #reorder the nodes to match the empty consensus matrix
  affinity_dat <- consensus_dat %>%
      left_join(.,affinity_dat,by=c("from","to")) %>%
      mutate(weight=ifelse(is.na(weight),0,1)) 
  #weight
  weight <- weight + affinity_dat$weight
  }
  
  #calculate the fraction of times two nodes connected
  weight <- weight/length(bisbm)
  consensus_dat <- data.frame(from=consensus_dat$from,
                              to=consensus_dat$to,
                              weight=weight)
  
  return(consensus_dat)
}








