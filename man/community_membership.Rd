% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/community_membership.R
\name{community_membership}
\alias{community_membership}
\title{Construct community membership dataframe}
\usage{
community_membership(tree.path, nlabels)
}
\arguments{
\item{tree.path}{results from recursive_biTree}

\item{nlabels}{The length of a vector of labels for all the input nodes, should be the order of A}
}
\value{
returns a long-format community membership dataframe
}
\description{
\code{community_membership} returns a long-format dataframe
}
\examples{
data("cluster.result")

comm_member <- community_membership(cluster.result$tree.path,
nlabels=sum(sapply(cluster.result$cluster.labels.list,length)))
head(comm_member)
}
