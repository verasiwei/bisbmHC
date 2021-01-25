#' Generate json format data for binary tree visualization
#'
#' \code{data_for_tree} returns a json file for binary tree
#'
#' @param comm_member Long format community membership file from `comm_member` function

#' @return a json file for binary tree
#'
#' @import future dplyr furrr purrr d3r
#' @export
#'
#' @examples
#' data("cluster.result")
#' 
#' comm_member <- community_membership(cluster.result$tree.path,
#' nlabels=sum(sapply(cluster.result$cluster.labels.list,length)))
#' data_for_tree <- data_for_tree(comm_member)

data_for_tree <- function(comm_member){
  
  comm_member_wide <- comm_member %>%
    spread(level,node,sep="") %>%
    select(-id) %>%
    distinct(.)
  
  data_for_tree <- comm_member_wide %>%
    arrange(.dots=colnames(comm_member_wide)) %>%
    d3_nest()
  
  #write file
  # write(data_for_tree,file = paste0(path,"data_for_tree.json"))
  
  data_for_tree
  
}


