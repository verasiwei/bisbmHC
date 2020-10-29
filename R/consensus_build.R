#' Build the consensus graph
#'
#' \code{consensus_build} returns the long format of consensus matrix (node to node connection weight)
#'
#' @param id_label The lable of nodes
#' @param bisbm The list of bisbm results from run_bisbm
#' @param 
#' @return The long format of consensus matrix (node to node connection weight)
#'
#' @export
#'
#' @examples
 
consensus_build <- function(id_label,bisbm) {
  #build the empty long format of consensus matrix
  consensus_dat <- expand.grid(from=id_label,to=id_label)
  consensus_dat <- consensus_dat[!duplicated(data.frame(t(apply(consensus_dat,1,sort)))),]
  weight <- rep(0,nrow(consensus_dat))
  
  #aggregation of the adjacency matrix
  for(i in 1:length(bisbm)){
    affinity_dat <- bisbm[[i]] %>%
      filter(type=="grid") %>%
      filter(level==0) %>%
      arrange(cluster)
    
  #calculate the times two nodes connected (in the same cluster in each iteration)
  affinity_dat <- purrr::map_dfr(unique(affinity_dat$cluster), function(x){
      
      dat <- expand.grid(from=affinity_dat$node[affinity_dat$cluster==x],
                         to=affinity_dat$node[affinity_dat$cluster==x])
      dat <- dat[!duplicated(data.frame(t(apply(dat,1,sort)))),]
      dat$weight <- rep(1,nrow(dat))
      return(dat)
      
  })
    
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








