# bisbmHC

This is the bipartite network clustering framework in Electronic Medical Records for unsupervised learning of hierarchical community structure of patient subgroups and disease heterogeneity.

### Introduction

It is becoming more important to discover latent disease clusters and patient subgroups using large scale Electronic Medical Records (EMR). In EMR, subjects are only grouped into communities when sharing co-occurrence diagnosis codes and diagnosis codes are clustered when they are presented in the same group of patients. We assume EMR data can be represented as a bipartite network and community structure can be inferred through model based community detection methods. However, the problems in practice of real-world network are the consistency of community membership on patients or diagnosis codes and the appropriate approach to discover the hierarchy communities. 

We build up a framework of adapting bipartite stochastic block model (biSBM) to EMR and track all possible fitted biSBM generated network structures. Then we generate an ensemble of partitions with the fraction of times two nodes are connected in all possible biSBM generated networks, which will be used in consensus clustering. Spectral clustering techniques with a top-down recursive partitioning algorithm is then applied to learn the latent hierarchical community structure. We show that the consistency of finding the communities improves under the scheme of consensus clustering and eigensplit or the kmeans algorithm applied to the top eigenvectors of the normalized Laplacian matrix when compared with adapting biSBM singularly. 

### Basic Structure

We mainly aim to capture a more stable latent structure of patient or disease clusters in the real-world EHR dataset.  

- Implement biSBM in EHR and tracked the fraction of times two nodes are connected from iteratively cluster.

- Build Consensus Graph according to patient-diagnosis code relationship.

- Learn the hierarchical community structure through spectral clustering with a top-down recursive partitioning.


### Differences?

What are the difference from other clustering approaches? Like centroid-based clustering of k-means clustering, connectivity-based clustering of hierarchical clustering...?

Hierarchical clustering is sensitive to noise and outliers. However, the real-world EMR data is always very sparse and noisy. And the hierarchical clustering is difficult to handle different sized clusters and sometimes breaks the large clusters. Kmeans has to define the number of clusters in prior, but sometimes it is difficult to identify the correct number of clusters.


### Development

The package is still under development has many deficiencies. The next step is to improve the efficiency of recursive partitioning when handling big data. The function of calculating eigenvalues will be recoded in Cpp and implemented by Rcpp in this package later. 

It is based on the algorithm that the nested stochastic block model is fitted by minimizing its description length using an agglomerative heuristic. And the collection of nodes connection is happened after the MCMC converge. A better approach is to keep track of how connected each node is with all other nodes during the MCMC sweeps to the convergence. 










