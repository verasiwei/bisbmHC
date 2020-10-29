#' Set up recursive bi-partition tree
#'
#' \code{recursive_biTree} returns a list with the number of communities, tree path of each node,a list of community labels for each community
#'
#' @param A It is the consensus graph (the long format of consensus matrix (node to node connection weight))
#' @param cluster.labels.list To store a list of labels for each cluster during the process
#' @param ncluster To store the number of clusters during the process
#' @param cluster.labels A vector of labels for all the input nodes, the order should be consistent with the order of A
#' @param n.min The minimum number of nodes for further partition
#' @param method The partition method in spectral clustering
#' @return a list with the number of communities, tree path of each node,a list of community labels for each community
#'
#' @export
#'
#' @examples
#' cluster_result <- recursive_biTree(A,cluster.labels.list=list(), ncluster=0, cluster.labels=c(1:200),n.min=10,method="kmeans")


recursive_biTree <- function(A,cluster.labels.list,ncluster,cluster.labels,n.min,method){

  nodes.connect <- which(rowSums(A) > 0)
  nodes.isolate <- which(rowSums(A) == 0)
  cluster.labels.full <- cluster.labels
  all.deg = rowSums(A)

  ##do not start if there are too many isolated nodes
  if((length(nodes.connect)<=8)||(length(nodes.isolate)>=5*length(nodes.connect))||(length(nodes.connect)<2*n.min)){
    ncluster <- ncluster + 1 #from bottom to top
    cluster.labels.list[[ncluster]] <- cluster.labels #store the label of each community
    tree.path <- c("",as.character(cluster.labels))
    mod.path <- c(0,rep(0,length(cluster.labels)))
    # if(length(nodes.isolate)>0) tree.path <- c(tree.path,as.character(cluster.labels.full[nodes.isolate]))
    # if(length(nodes.isolate)>0) nodes.isolate.list <- c(nodes.isolate.list,nodes.isolate)
    print('Too few connected nodes, not even started!')
    return(list(cluster.labels.list = cluster.labels.list, ncluster = ncluster,tree.path=tree.path,mod.path=mod.path))
  }

  ### only focus on non-isolated nodes
  A <- A[nodes.connect, nodes.connect]
  cluster.labels <- cluster.labels[nodes.connect]
  all.deg <- all.deg[nodes.connect]
  nparent <- dim(A)[1]

  #non-backtracking stopping criteria
  K = 2
  n <- nrow(A)
  I <- as(diag(rep(1,n)),"dgCMatrix")
  D <- as(diag(colSums(A)),"dgCMatrix")
  r <- sqrt(sum((colSums(A))^2)/sum(colSums(A))-1)

  B <- as(matrix(0,nrow=2*n,ncol=2*n),"dgCMatrix")
  B[(n+1):(2*n),1:n] <- -I
  B[1:n,(n+1):(2*n)] <- D-I
  B[(n+1):(2*n),(n+1):(2*n)] <- A
  ss <- Re(eigs(B,k=2,which="LM")$values)
  split.flag <- sum(abs(ss)>r)>=2

  #begin binary tree
  if(split.flag) {

    if(method=="eigen"){
      # # #eigen A
      l_matrix <- A
      eval <- eigs(l_matrix,10,which = "LM")
      clustering <- ifelse(eval$vectors[,2]<0,1,2)
    } else if(method=="fiedler"){
      # # #eigen unnormalized L
      l_matrix <- D-A
      eval <- eigs(l_matrix,10,which = "SM")
      # f <- function(x,extra = NULL){
      #   as.vector(l_matrix%*%x)
      # }
      # eval <- arpack(f,sym = TRUE,options=list(n = ncol(l_matrix),nev = 2,ncv = 8,
      #                                          which = "SM",maxiter = 1000))
      clustering <- ifelse(eval$vectors[,2]<0,1,2)

    } else if(method=="kmeans"){
      #kmeans
      deg.adj <- D+diag(rep(0.1,n))
      l_matrix <- solve(sqrtm(deg.adj))%*%A%*%solve(sqrtm(deg.adj))
      deg.adj <- NULL
      eval <- eigs(l_matrix,10,which = "LM")
      eval.adj <- eval$vectors%*%diag(abs(eval$values))
      embed.slist <- abs(eval$values[1])
      m=1
      while(embed.slist<as.numeric(0.95)*sum(abs(eval$values))){
        m=m+1
        embed.slist <- embed.slist+abs(eval$values[m])
      }
      embed.Y <- data.frame(eval.adj[,1:m])
      row.names(embed.Y) <-  A@Dimnames[[1]]
      clustering <- pam(embed.Y,k=2)$clustering
    } else if(method=="hcluster"){
      #hierarchical cluster
      deg.adj <- D+diag(rep(0.1,n))
      l_matrix <- solve(sqrtm(deg.adj))%*%A%*%solve(sqrtm(deg.adj))
      deg.adj <- NULL
      eval <- eigs(l_matrix,10,which = "LM")
      eval.adj <- eval$vectors%*%diag(abs(eval$values))
      embed.slist <- abs(eval$values[1])
      m=1
      while(embed.slist<as.numeric(0.95)*sum(abs(eval$values))){
        m=m+1
        embed.slist <- embed.slist+abs(eval$values[m])
      }
      embed.Y <- data.frame(eval.adj[,1:m])
      row.names(embed.Y) <-  A@Dimnames[[1]]
      embed.hc_result <- hclust(dist(embed.Y), method="ward.D2")
      clustering <- cutree(embed.hc_result,k=2)
    }


    #small cluster
    xi.cluster.labels = lapply(1:2, function(x){which(clustering == x)}) #index list
    smaller.cluster <- xi.cluster.labels[[which.min(sapply(xi.cluster.labels,length))]] #index small
    A.small <- A[smaller.cluster,smaller.cluster]
    small.labels <- cluster.labels[smaller.cluster] #cluster labels

    if(length(dim(A.small))>0) {
      nparent <- dim(A.small)[1]
    } else if(length(A.small)>0){
      nparent <- 1
    } else {
      nparent <- 0
    }

    if(nparent > 2*n.min) { ## only do further clustering on cluster larger than 2*n.min
      recursive <- recursive_biTree(A.small,cluster.labels.list,ncluster,small.labels,n.min,method=method)
      cluster.labels.list <- recursive$cluster.labels.list
      ncluster <- recursive$ncluster
      L.tree.path <- recursive$tree.path
      # L.mod.path <- recursive$mod.path
      # if(length(nodes.isolate)>0){
      #   cluster.labels.list[[ncluster]] = c(cluster.labels.list[[ncluster]],cluster.labels.full[nodes.isolate]) ### attached the isolated nodes in this level with the clusters under the smaller split
      #   path.head <- L.tree.path[length(L.tree.path)]
      #   path.head <- gsub('[[:digit:]]+', '', path.head) #remove all numbers
      #   iso.path <- paste(path.head,cluster.labels.full[nodes.isolate],sep="")
      #   L.tree.path <- c(L.tree.path,iso.path)
      # }
      L.mod.path <- recursive$mod.path

    } else {
      ncluster <- ncluster + 1
      cluster.labels.list[[ncluster]] <- small.labels
      # if(length(nodes.isolate)>0) cluster.labels.list[[ncluster]] = c(cluster.labels.full[nodes.isolate],cluster.labels.list[[ncluster]])
      L.tree.path <- as.character(cluster.labels.list[[ncluster]])
      L.mod.path <- rep(0,length(cluster.labels.list[[ncluster]]))
      #print('Too small left cluster, Branch End')
    }

    #large cluster
    A.large <- A[-smaller.cluster, -smaller.cluster]
    large.labels <- cluster.labels[-smaller.cluster]

    if(length(dim(A.large))>0) {
      nparent <- dim(A.large)[1]
    } else if(length(A.large)>0){
      nparent <- 1
    } else {
      nparent <- 0
    }

    if(nparent > 2*n.min) {
      recursive <- recursive_biTree(A.large,cluster.labels.list,ncluster,large.labels,n.min,method=method)
      cluster.labels.list <- recursive$cluster.labels.list
      R.tree.path <- recursive$tree.path
      R.mod.path <- recursive$mod.path
      ncluster <- recursive$ncluster

    } else {
      ncluster <- ncluster + 1
      cluster.labels.list[[ncluster]] <- large.labels
      R.tree.path <- as.character(large.labels)
      R.mod.path <- rep(0,length(large.labels))
      #print('Too small right cluster, Branch End')
    }

    L.tree.path <- paste("l",L.tree.path,sep="/")
    R.tree.path <- paste("r",R.tree.path,sep="/")
    tree.path <- c("",L.tree.path,R.tree.path)

  } else { #split flag==FALSE

    ncluster <- ncluster + 1 #from bottom to top
    cluster.labels.list[[ncluster]] <- cluster.labels # store the index label of each community
    # if(length(nodes.isolate)>0) cluster.labels.list[[ncluster]] <- c(cluster.labels.full[nodes.isolate],cluster.labels)
    tree.path <- c("",as.character(cluster.labels))
    # if(length(nodes.isolate)>0) tree.path <- c(tree.path,as.character(cluster.labels.full[nodes.isolate]))
    # if(length(nodes.isolate)>0) nodes.isolate.list <- c(nodes.isolate.list,nodes.isolate)
    #print('One cluster, Branch End, not even started!')
  }
  return(list(cluster.labels.list = cluster.labels.list, ncluster = ncluster,tree.path=tree.path))

}




