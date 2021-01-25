#' Construct community membership dataframe
#'
#' \code{community_membership} returns a long-format dataframe
#'
#' @param tree.path results from recursive_biTree
#' @param nlabels The length of a vector of labels for all the input nodes, should be the order of A
#' @return returns a long-format community membership dataframe
#' 
#' @import stringr dplyr stringi
#' @export
#' 
#' @examples
#' data("cluster.result")
#' 
#' comm_member <- community_membership(cluster.result$tree.path,
#' nlabels=sum(sapply(cluster.result$cluster.labels.list,length)))
#' head(comm_member)


community_membership <- function(tree.path,nlabels){
  
  tree.path <- stringr::str_replace_all(tree.path,"l","0")
  tree.path <- stringr::str_replace_all(tree.path,"r","1")
  nlevels <- max(stringr::str_count(tree.path,"/"))
    
    comm_mem <- purrr::map_dfr(1:nlevels,function(i){
      level <- rep(i,nlabels)
      last_pat <- regexpr("\\/[^\\/]*$", tree.path) # find the last "/" in each node
      id <- stringr::str_sub(tree.path,start <- last_pat+1) # extract node label, after the last "/"
      node <- paste0(i,"-",stringr::str_remove_all(stringr::str_sub(tree.path[id!=""],1,2*i),"/"))
      node[stringr::str_count(tree.path[id!=""],"/")<i] <- NA
      id <- stringi::stri_remove_empty(id)
      node <- node[!duplicated(id)]
      id <- id[!duplicated(id)]
      comm_mem_level <- data.frame(id=id,level=level,node=node)
      return(comm_mem_level)
    })
    
}
