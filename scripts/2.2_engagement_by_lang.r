library(igraph)
library(bibliometrix)

chirper_data <- read.csv("data/chirper_data.csv")


####################
# Clean Data for Graph Plotting #
####################


time <- 3
engagements_edges <- read.csv(
    paste(
        "data/engagement_summary_time", time, ".csv",
        sep = ""
    )
)[, -1]

# remove repeated edges disregarding direction
edge_pairs <- data.frame(
    t(apply(engagements_edges, 1, function(x) sort(x[1:2])))
)
colnames(edge_pairs) <- c("source", "target")
repeated_pairs <- edge_pairs[duplicated(edge_pairs), ]
engagements_edges <- engagements_edges[
    !paste(engagements_edges$source, engagements_edges$target) %in%
        paste(repeated_pairs$source, repeated_pairs$target), ]

# get a df of chirper data that is in the edges
chirper_data_this_time <- chirper_data[
    chirper_data$chirper %in% engagements_edges$source |
        chirper_data$chirper %in% engagements_edges$target,
]


####################
# Plot Graph #
####################


engagements_graph <- graph_from_data_frame(
    engagements_edges,
    vertices = chirper_data_this_time, directed = FALSE
)
clustering <- cluster_label_prop(engagements_graph)
table(clustering$membership)

# graph plotting parameters
size <- 2000
language <- chirper_data_this_time$lang
language <- ifelse(
    language != "en" & language != "zh" & language != "ja",
    "", language
)
lang_colour <- c(
    adjustcolor("red", alpha.f = 0.8),
    adjustcolor("blue", alpha.f = 0.8),
    adjustcolor("black", alpha.f = 0.8),
    adjustcolor("grey", alpha.f = 0.4)
)
V(engagements_graph)$color <- lang_colour[match(
    language, c("zh", "en", "ja", "")
)]
E(engagements_graph)$weight <- engagements_edges$engagement_count
V(engagements_graph)$size <- size / 2000
E(engagements_graph)$width <- size / 3000 * (E(engagements_graph)$weight**0.7)
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


# make an identical graph but coloured by cluster
clustered_graph <- engagements_graph
V(clustered_graph)$color <- sample(rainbow(max(membership(clustering))))[
    V(engagements_graph)$label
]

layout <- layout_with_fr(engagements_graph)

png(
    paste(
        "sna_results/time", time, "_engagement_graph_by_lang.png",
        sep = ""
    ),
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
png(
    paste(
        "sna_results/time", time, "_engagement_graph_by_cluster.png",
        sep = ""
    ),
    width = size, height = size
)
plot(
    clustered_graph,
    layout = layout,
    vertex.label.cex = 0, vertex.label.color = "black",
    vertex.label = NA,
    vertex.frame.color = NA
)
dev.off()


####################
# Descriptive Statistics #
####################


desc_stats <- networkStat(engagements_graph, stat = "all", type = "network")

desc_stats$network$networkSize
desc_stats$network$networkDensity
desc_stats$network$networkDiameter
desc_stats$network$networkTransitivity
desc_stats$network$NetworkAverPathLeng

# T1: 8519, 0.00068852, 10, 0.0091351, 4.95294
# T2: 18353, 0.0004510, 11, 0.0113651, 4.75651
# T3: 24473, 0.0005766, 9, 0.02058, 4.2139

# save the graph for later more sophisticated statistics
save(engagements_graph, file = paste(
    "data/engagement_graph_time", time, ".RData",
    sep = ""
))
