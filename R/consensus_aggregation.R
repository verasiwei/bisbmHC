#' Create Index of randomly sampling in aggregation
#'
#' Given the number of M affinity matrix from bisbm results, randomly select N results to construct the
#' consensus graph and compare with the consensus graph which constructed by another N results
#' that randomly selected in the rest of M-first selected N results.
#'
#'
#' \code{consensus_aggregation} returns the list of a pair of independent consensus graph with the same aggregation times
#' with the same number of aggregation times
#'
#' @param N Aggregation times
#' @param M The number of total sbm results
#' @param seed A vector of the seed you want to set for each aggregation time
#' @param rep The number of pairs for comparison
#' @param bisbmresults The results from run_bisbm
#' @param id_label The label of the nodes
#' @return the list of a pair of independent consensus graph with the same aggregation times
#' 
#' @examples
#' consensus_matrix_list <- consensus_aggregation(20,500,c(1:50),50,bisbmresults,id_label)


#function to create index of aggregation
consensus_aggregation <- function(N,M,seed,rep,bisbmresults,id_label){
  
  consensus_dat <- expand.grid(from=id_label,to=id_label)
  consensus_dat <- consensus_dat[!duplicated(data.frame(t(apply(consensus_dat,1,sort)))),]
  
  consensus_dat_list <- purrr::map(1:rep, function(x){
    #first
    set.seed(seed[x])
    index_list <- sample(1:M,N,replace = FALSE)
    bisbm.result.1 <- bisbmresults[index_list]
    weight <- rep(0,nrow(consensus_dat))
    for(i in 1:length(bisbm.result.1)){
      affinity_dat <- bisbm.result.1[[i]] %>%
        filter(type=="grid") %>%
        filter(level==0) %>%
        arrange(cluster)
      affinity_dat <- purrr::map_dfr(unique(affinity_dat$cluster), function(x){
        dat <- expand.grid(from=affinity_dat$node[affinity_dat$cluster==x],to=affinity_dat$node[affinity_dat$cluster==x])
        dat <- dat[!duplicated(data.frame(t(apply(dat,1,sort)))),]
        dat$weight <- rep(1,nrow(dat))
        return(dat)
      })
      affinity_dat <- consensus_dat %>%
        left_join(.,affinity_dat,by=c("from","to")) %>%
        mutate(weight=ifelse(is.na(weight),0,1)) 
      weight <- weight + affinity_dat$weight
    }
    weight <- weight/N
    consensus_dat.1 <- data.frame(from=consensus_dat$from,to=consensus_dat$to,weight=weight)
    
    
    #rest
    index_rest <- c(1:M)[!(c(1:M) %in% index_list)]
    set.seed(seed[x])
    index_rest_list <- sample(index_rest,N,replace = FALSE)
    
    bisbm.result.2 <- bisbmresults[index_rest_list]
    weight2 <- rep(0,nrow(consensus_dat))
    for(i in 1:length(bisbm.result.2)){
      affinity_dat <- bisbm.result.2[[i]] %>%
        filter(type=="grid") %>%
        filter(level==0) %>%
        arrange(cluster)
      affinity_dat <- purrr::map_dfr(unique(affinity_dat$cluster), function(x){
        dat <- expand.grid(from=affinity_dat$node[affinity_dat$cluster==x],to=affinity_dat$node[affinity_dat$cluster==x])
        dat <- dat[!duplicated(data.frame(t(apply(dat,1,sort)))),]
        dat$weight <- rep(1,nrow(dat))
        return(dat)
      })
      affinity_dat <- consensus_dat %>%
        left_join(.,affinity_dat,by=c("from","to")) %>%
        mutate(weight=ifelse(is.na(weight),0,1)) 
      weight2 <- weight2 + affinity_dat$weight
    }
    weight2 <- weight2/N
    consensus_dat.2 <- data.frame(from=consensus_dat$from,to=consensus_dat$to,weight=weight2)
    
    return(consensus_dat_list <- list(consensus_dat.1,consensus_dat.2))
  })
  
  return(consensus_dat_list <- consensus_dat_list)
}
