#' NMI measurement to check the convergence of aggregation times
#'
#' \code{consensus_spec} returns a pair of consensus graphs
#' with the same number of aggregation times
#'
#' @param consensus_matrix_list the list of a pair of independent consensus graph with the same aggregation times
#' @param method The partition method in spectral clustering
#' @return a list of NMI measurement for a pair of independent consensus graph with the same aggregation times
#' 
#' @export
#'
#' @examples
#' consensus_spec <- consensus_spectral(consensus_matrix_list,)


consensus_spectral <- function(consensus_matrix_list,method){
  #for specific aggregation times, 50 comparisons(20 etc...)
  level_group_list <- purrr::map(1:50, function(x){
    #consensus graph 1
    g <- graph_from_data_frame(consensus_matrix_list[[x]][[1]],directed = F,vertices = NULL)
    g <- simplify(g)
    adj <- get.adjacency(g,type = "both",attr = "weight")
    cluster.result <- recursive_biTree(adj,cluster.labels.list=list(), ncluster=0,
                                       cluster.labels=c(1:nrow(adj)),n.min=10,method=method)
    comm_member <- community_membership(cluster.result$tree.path,cluster.labels=c(1:nrow(adj)))
    comm_member <- comm_member %>%
      filter(level==1) %>%
      arrange(id) %>%
      mutate(node=as.character(node),id=as.character(id))

    #consensus graph 2
    g <- graph_from_data_frame(consensus_matrix_list[[x]][[2]],directed = F,vertices = NULL)
    g <- simplify(g)
    adj <- get.adjacency(g,type = "both",attr = "weight")
    cluster.result <- recursive_biTree(adj,cluster.labels.list=list(), ncluster=0,
                                       cluster.labels=c(1:nrow(adj)),n.min=10,method=method)
    comm_member2 <- community_membership(cluster.result$tree.path,cluster.labels=c(1:nrow(adj)))
    comm_member2 <- comm_member2 %>%
      filter(level==1) %>%
      arrange(id) %>%
      mutate(node=as.character(node),id=as.character(id))

    nmi1 <- aricode::NMI(comm_member$node,comm_member2$node)
    comm_member$node <- ifelse(comm_member$node=="1-0","1-1","1-0")
    nmi2 <- aricode::NMI(comm_member$node,comm_member2$node)
    if(nmi1 >= nmi2){
      return(nmi1)
    } else {return(nmi2)}
  })

  mean_nmi <- mean(unlist(level_group_list))
  max_nmi <- max(unlist(level_group_list))
  min_nmi <- min(unlist(level_group_list))

  return(consensus_spec <- data.frame(mean_nmi=mean_nmi,
                                      max_nmi=max_nmi,
                                      min_nmi=min_nmi))
}


