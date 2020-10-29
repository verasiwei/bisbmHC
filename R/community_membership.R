#' Construct community membership dataframe
#'
#' \code{community_membership} returns a long-format dataframe
#'
#' @param tree.path results from recursive_biTree
#' @param nlabels The length of a vector of labels for all the input nodes, should be the order of A
#' @return returns a long-format community membership dataframe
#' 
#' @export
#' 
#' @examples
#' comm_member <- community_membership(cluster.result$tree.path,nlabels=378)


community_membership <- function(tree.path,nlabels){
  
  tree.path <- str_replace_all(tree.path,"l","0")
  tree.path <- str_replace_all(tree.path,"r","1")
  nlevels <- max(str_count(tree.path,"/"))
    
    comm_mem <- purrr::map_dfr(1:nlevels,function(i){
      level <- rep(i,nlabels)
      last_pat <- regexpr("\\/[^\\/]*$", tree.path) # find the last "/" in each node
      id <- str_sub(tree.path,start <- last_pat+1) # extract node label, after the last "/"
      node <- paste0(i,"-",str_remove_all(str_sub(tree.path[id!=""],1,2*i),"/"))
      node[str_count(tree.path[id!=""],"/")<i] <- NA
      id <- stri_remove_empty(id)
      node <- node[!duplicated(id)]
      id <- id[!duplicated(id)]
      comm_mem_level <- data.frame(id=id,level=level,node=node)
      return(comm_mem_level)
    })
    
}
