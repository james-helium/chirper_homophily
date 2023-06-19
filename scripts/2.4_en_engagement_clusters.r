setwd("/Users/james_k_he/Desktop/chirper_echo_chamber")

library(igraph)

# read in data
chirper_data <- read.csv("data/chirper_data.csv")

time <- 3
load(paste0("data/networks/engagement_graph_time", time, ".RData"))

# filter for english chirpers
english_chirpers <- chirper_data[
    chirper_data$lang == "en", "chirper"
]
engagements_graph <- induced_subgraph(
    engagements_graph,
    which(V(engagements_graph)$name %in% english_chirpers)
)
length(V(engagements_graph)$name)


# or, build graph from the full english network at 2023-05-16
en_engagements_edges <- read.csv(
    "data/networks/en_engagement_summary.csv"
)[, -1]
# remove repeated edges disregarding direction
edge_pairs <- data.frame(
    t(apply(en_engagements_edges, 1, function(x) sort(x[1:2])))
)
colnames(edge_pairs) <- c("source", "target")
repeated_pairs <- edge_pairs[duplicated(edge_pairs), ]
en_engagements_edges <- en_engagements_edges[
    !paste(en_engagements_edges$source, en_engagements_edges$target) %in%
        paste(repeated_pairs$source, repeated_pairs$target),
]

# get a df of chirper data that is in the edges
en_chirper_data <- chirper_data[chirper_data$lang == "en", ]
en_chirper_data <- en_chirper_data[
    en_chirper_data$chirper %in% en_engagements_edges$source |
        en_chirper_data$chirper %in% en_engagements_edges$target,
]

engagements_graph <- graph_from_data_frame(
    en_engagements_edges,
    vertices = en_chirper_data,
    directed = FALSE
)

##############
# Plot Graph #
##############

clustering <- cluster_fast_greedy(engagements_graph)
max(clustering$membership)
table(clustering$membership)

# graph plotting parameters
size <- 2000

V(engagements_graph)$color <- sample(rainbow(max(membership(clustering))))[
    membership(clustering)
]
V(engagements_graph)$size <- size / 2000
V(engagements_graph)$label <- membership(clustering)

# remove nodes in clusters less than 1% of the graph
cluster_frequency <- data.frame(table(clustering$membership))
colnames(cluster_frequency) <- c("cluster", "frequency")
cluster_frequency <- cluster_frequency[
    cluster_frequency$frequency >= 0.01 * vcount(engagements_graph),
]
engagements_graph <- induced_subgraph(
    engagements_graph,
    which(clustering$membership %in% cluster_frequency$cluster)
)

# remove Nodes with less than k edges
# reason: nodes with none or 1 edge are structurally redundant
# run this several times to remove cleanly
k <- 2
nodes_to_keep <- which(
    degree(engagements_graph) >= k
)
engagements_graph <- induced_subgraph(
    engagements_graph,
    nodes_to_keep
)
length(nodes_to_keep)



layout <- layout_with_fr(engagements_graph)

png(
    paste(
        "sna_results/time", time, "_en_engagement_clusters.png",
        sep = ""
    ),
    width = size, height = size
)
# or when running the full graph
png(
    "sna_results/full_en_engagement_clusters.png",
    width = size, height = size
)
plot(
    engagements_graph,
    layout = layout,
    vertex.label.cex = 0, vertex.label.color = "black",
    vertex.label = NA,
    vertex.frame.color = NA
)
dev.off()


modularity(
    engagements_graph, V(engagements_graph)$label
)

# t1: 0.7438
# t2: 0.4737
# t3: 0.3324
# full: 0.4984

assortativity(engagements_graph, V(engagements_graph)$label)
# t1: 0.8257
# t2: 0.5575
# t3: 0.4425
# t4: 0.7414

# save the graph
save(
    engagements_graph,
    file = paste0("data/networks/en_engagement_graph_time", time, ".RData")
)
# or when running the full graph
save(
    engagements_graph,
    file = "data/networks/en_engagement_graph_full.RData"
)


####################
# Process to DF    #
####################

time <- 3
load(paste0("data/networks/en_engagement_graph_time", time, ".RData"))

# turn the graph into a dataframe, keep just the vertices
engagements_df <- as_data_frame(engagements_graph, "vertices")

write.csv(
    engagements_df,
    paste0("data/networks/en_engagement_clusters_time", time, ".csv"),
    row.names = FALSE
)
# or when running the full graph
write.csv(
    engagements_df,
    "data/networks/en_engagement_clusters_full.csv",
    row.names = FALSE
)
