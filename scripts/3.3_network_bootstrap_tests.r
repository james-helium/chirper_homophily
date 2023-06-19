library(igraph)
library(asnipe)
library(report)


#############
# FUNCTIONS #
#############

bootstrap_modularity <- function(graph, membership, n = 1000) {
    modularity_scores <- c()
    for (i in 1:n) {
        random_membership_labels <- sample(membership)
        modularity_scores[i] <- modularity(
            graph, membership = random_membership_labels
        )
    }

    observed_modularity <- modularity(graph, membership = membership)
    p_value <- sum(modularity_scores >= observed_modularity) / n

    print(paste(
        "Observed modularity:", observed_modularity,
        "  p-value:", p_value
    ))
}

bootstrap_assortativity <- function(graph, attribute, n = 1000) {
    assortativity_scores <- c()
    for (i in 1:n) {
        random_attribute <- sample(attribute)
        assortativity_scores[i] <- assortativity(
            graph, random_attribute
        )
    }
    observed_assortativity <- assortativity(graph, attribute)
    p_value <- sum(assortativity_scores >= observed_assortativity) / n

    print(paste(
        "Observed assortativity:", observed_assortativity,
        "  p-value:", p_value
    ))
}


###############
# FULL GRAPHS #
###############


time <- 3
load(paste0("data/networks/engagement_graph_time", time, ".RData"))

# modularity of labels (structural communities)
bootstrap_modularity(
    engagements_graph, membership = V(engagements_graph)$label
)
# t1: Observed modularity: 0   p-value: 1
# t2: Observed modularity: 0.308928723662766   p-value: 0 (<0.001)
# t3: Observed modularity: 0.472327095190494   p-value: 0

# modularity of colors (language communities)
bootstrap_modularity(
    engagements_graph, membership = as.factor(V(engagements_graph)$color)
)
# t1: Observed modularity: -0.00414286596334496   p-value: 0.914
# t2: Observed modularity: 0.30933685398166   p-value: 0
# t3: Observed modularity: 0.431566448672371   p-value: 0

# assortativity of labels (structural communities)
bootstrap_assortativity(
    engagements_graph, attribute = V(engagements_graph)$label
)
# t1: Observed assortativity: NaN   p-value: NA
# t2: Observed assortativity: 0.9355157638116   p-value: 0
# t3: Observed assortativity: 0.922769568798396   p-value: 0

# assortativity of colors (language communities)
bootstrap_assortativity(
    engagements_graph, attribute = as.factor(V(engagements_graph)$color)
)
# t1: Observed assortativity: -0.00586931257315362   p-value: 0.81
# t2: Observed assortativity: 0.665350627885065   p-value: 0
# t3: Observed assortativity: 0.81324931183923    p-value: 0



##################
# SUBGRAPH STATS #
##################

time <- 3
load(paste0("data/networks/en_engagement_graph_time", time, ".RData"))
# or the full graph
load(paste0("data/networks/en_engagement_graph_full.RData"))

# modularity of labels (structural communities)
bootstrap_modularity(
    engagements_graph, membership = V(engagements_graph)$label, 100000
)
# t1: Observed modularity: 0.74379951891577   p-value: 0
# t2: Observed modularity: 0.473712670672308   p-value: 0
# t3: Observed modularity: 0.33243859867745   p-value: 0
# t4: Observed modularity: 0.498420363762219   p-value: 0

# assortativity of labels (structural communities)
bootstrap_assortativity(
    engagements_graph, attribute = V(engagements_graph)$label, 100000
)
# t1: Observed assortativity: 0.791687725447347   p-value: 0
# t2: Observed assortativity: 0.529617518240931   p-value: 0
# t3: Observed assortativity: 0.441991270115674   p-value: 0
# t4: Observed assortativity: 0.74135232722401   p-value: 0

# btw get the size, diamter, density, transitivity, and average path length
vcount(engagements_graph)
#   1149        6814        9131        16003
diameter(engagements_graph)
#   23          12          10          7
graph.density(engagements_graph)
#   0.00246     0.000924    0.001404    0.00228
transitivity(engagements_graph)
#   0.01304     0.0129      0.02457     0.00440
average.path.length(engagements_graph)
#   6.819       4.672       3.906       2.85
