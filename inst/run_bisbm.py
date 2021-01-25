import os
import numpy as np
import pandas as pd
import pickle
import warnings
from collections import defaultdict
import graph_tool.all as gt


def load_edges_to_graph(tidy_edges, verbose = True):
    # Grab lists of unique nodes for both types
    node_a_names = tidy_edges['node_a'].unique()
    n_node_a = len(node_a_names)

    node_b_names = tidy_edges['node_b'].unique()
    n_node_b = len(node_b_names)

    # create a graph-tool graph
    g = gt.Graph(directed=False)

    # Setup and pull out node property vectors for easy setting
    name = g.vp["name"] = g.new_vp("string")
    kind = g.vp["kind"] = g.new_vp("int")

    # Dictionaries for adding and grabbing nodes from either class
    node_a_add = defaultdict(lambda: g.add_vertex())
    node_b_add = defaultdict(lambda: g.add_vertex())

    num_edges, _ = tidy_edges.shape

    # Start by adding all unique node_a's to make sure nodes are not jumbled by type
    if(verbose): print(f'Adding all unique node As to model nodes')
    for node_a_name in node_a_names:
        node_a = node_a_add[node_a_name]
        name[node_a] = node_a_name
        kind[node_a] = 0

    # Next do node b's
    if(verbose): print(f'Adding all unique node Bs to model nodes')
    for node_b_name in node_b_names:
        node_b = node_b_add[node_b_name]
        name[node_b] = node_b_name
        kind[node_b] = 1

    # Now go edge by edge and add the model.
    five_percent = np.ceil(num_edges/20)
    for edge_num in range(num_edges):

        if((edge_num%five_percent == 0) and verbose):
            print(f'{np.round((edge_num/num_edges)*100, decimals=1)}% done | {edge_num}/{num_edges}')

        node_a_name, node_b_name = tidy_edges.iloc[edge_num,:]

        # Grab the nodes for each end of the edge.
        node_a = node_a_add[node_a_name]
        node_b = node_b_add[node_b_name]

        # Add the edge to the model
        edge = g.add_edge(node_a, node_b)

    return g


# Load network edges (aka phenome vectors)
tidy_edges = pd.read_csv("data/model_data.csv", dtype = 'str')

# Next we load this edge dataframe into a `graphtools` graph object
# using a function from the helper script `load_edges_to_graph.py`.
graph_cache_loc = 'data/cached_graph.gt'

print('Building graph')
g = load_edges_to_graph(tidy_edges, verbose = True)
# Cache graph

## Next we start setting up the model
# Grab node types vertex property for all nodes
node_types = g.vp['kind']

# Make a state arguments dictionary to pass to the fitting object that
# tells the model which nodes should not be clustered together.
state_args = {'clabel': node_types, 'pclabel': node_types,}

print('Fitting the model')
state = gt.minimize_nested_blockmodel_dl(
        g,
        deg_corr = True,
        overlap = False,
        state_args = state_args
)

print('Model fitting finished')

# Now the model is fit we can start extracting info from it
Num_Hierarchies = len(state.levels) - 2

print(f'Model found {Num_Hierarchies} levels of hierarchy')


print('Starting model cluster membership extraction')

# 'Projects' the partition of the graph at level and returns a new corresponding state.
# Extract the cluster labels pairs for each edge at this level.
all_level_edges = [
    state
        .project_level(level)
        .copy(overlap=True)
        .get_edge_blocks()
    for level in range(Num_Hierarchies)
]

node_to_clust = []

n_edges = tidy_edges.shape[0]
ten_percent = int(n_edges*0.1)

print('Extracting cluster membership for all nodes')
# Iterate over all edge tuples (node_a_id -> node_b_id)
# This is actually not efficient at all because i go over
# all edges rather than just each node individually.
# It's not a big bottleneck though so I won't optimize it yet.
for i, edge in enumerate(g.edges()):
    # Extract the nodes that the edge connects
    node_a, node_b = edge
    node_a_name, node_b_name = g.vp.name[node_a],g.vp.name[node_b]

    if(i % ten_percent == 1):
        print(f'{int((i/n_edges)*100)}% done')

    for level, level_edges in enumerate(all_level_edges):
        # Extract the clusters that the connected nodes belong to
        node_a_cluster_id, node_b_cluster_id = level_edges[edge]

        # Append tuples of node id to cluster membership
        node_to_clust.append((node_a_name, f'{level}-{node_a_cluster_id}', 'node_a', level))
        node_to_clust.append((node_b_name, f'{level}-{node_b_cluster_id}', 'node_b', level))


print('Writing results to folder')

# Write results to a dataframe and save to data folder
pd.DataFrame(
    node_to_clust,
    columns=['node', 'cluster', 'type', 'level']
).drop_duplicates().to_csv('data/node_to_clust.csv', index = False)
