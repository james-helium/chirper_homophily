library(igraph)
library(asnipe)
library(report)

# read RData file
time <- 3
load(paste0("data/networks/engagement_graph_time", time, ".RData"))


# measure the modularity of the assigned membership (label) vs. language (color)
modularity(engagements_graph, membership = V(engagements_graph)$label)
modularity(
    engagements_graph, membership = as.factor(V(engagements_graph)$color)
)
# t1: 0 vs. -0.00414
# t2: 0.3089 vs. 0.3093
# t3: 0.4723 vs. 0.4316

assortativity(engagements_graph, V(engagements_graph)$label)
assortativity(engagements_graph, as.factor(V(engagements_graph)$color))
# t1: na vs. -0.00587
# t2: 0.9355 vs. 0.6654
# t3: 0.9228 vs. 0.8132


# measure the average internal density of groups
avg_internal_density <- function(graph, membership) {
    # calculate the average internal density of groups in a graph
    # graph: igraph object
    # membership: vector of group assignments
    # returns: numeric vector of average internal densities
    internal_densities <- c()
    for (comm in unique(membership)) {
        subgraph <- induced.subgraph(
            graph, which(membership == comm)
        )
        internal_densities[comm] <- graph.density(subgraph)
    }
    # remove NA
    internal_densities <- internal_densities[!is.na(internal_densities)]

    mean(internal_densities)
}
avg_internal_density(engagements_graph, membership = V(engagements_graph)$label)
avg_internal_density(engagements_graph, membership = V(engagements_graph)$color)
# t1: 0.000689 vs. 0.000717
# t2: 0.003801 vs. 0.002789
# t3: 0.003307 vs. 0.003369


# can color predict label? run some stats
color_label_table <- table(
    V(engagements_graph)$label, V(engagements_graph)$color
)
chisq.test(color_label_table)
color_label_table
summary(lm(
    V(engagements_graph)$label ~ V(engagements_graph)$color
))

# t1:
# X-squared = 7652.5, df = 3, p-value < 2.2e-16
""" # nolint
    #000000CC #0000FFCC #BEBEBE66 #FF0000CC
  1        42      3799      4468       210

                                      Estimate Std. Error   t value Pr(>|t|)
V(engagements_graph)$color#0000FFCC -9.556e-25  2.742e-14 0.000e+00    1.000
V(engagements_graph)$color#BEBEBE66  3.650e-15  2.740e-14 1.330e-01    0.894
V(engagements_graph)$color#FF0000CC -9.476e-25  2.987e-14 0.000e+00    1.000

Residual standard error: 1.767e-13 on 8515 degrees of freedom
Multiple R-squared:    0.5,     Adjusted R-squared:  0.4998 
F-statistic:  2838 on 3 and 8515 DF,  p-value: < 2.2e-16
"""

# t2:
# X-squared = 30970, df = 6, p-value < 2.2e-16
"""
    #000000CC #0000FFCC #BEBEBE66 #FF0000CC
  1        73      8430      4684       535
  2         1        93         3      4082
  5       608        26         0         0

                                    Estimate Std. Error t value Pr(>|t|)
V(engagements_graph)$color#0000FFCC -3.54441    0.01317  -269.1   <2e-16 ***
V(engagements_graph)$color#BEBEBE66 -3.56681    0.01356  -263.0   <2e-16 ***
V(engagements_graph)$color#FF0000CC -2.68332    0.01358  -197.6   <2e-16 ***

Residual standard error: 0.331 on 18531 degrees of freedom
Multiple R-squared:  0.8293,    Adjusted R-squared:  0.8293
F-statistic: 3.001e+04 on 3 and 18531 DF,  p-value: < 2.2e-16
"""

# t3:
# X-squared = 39156, df = 6, p-value < 2.2e-16
"""
    #000000CC #0000FFCC #BEBEBE66 #FF0000CC
  1        29     10093      4685       232
  2      1069       287         3         1
  3         1       560        26      7457

                                    Estimate Std. Error t value Pr(>|t|)
V(engagements_graph)$color#0000FFCC -0.84591    0.01177  -71.86   <2e-16 ***
V(engagements_graph)$color#BEBEBE66 -0.96285    0.01246  -77.26   <2e-16 ***
V(engagements_graph)$color#FF0000CC  0.96501    0.01200   80.43   <2e-16 ***

Residual standard error: 0.372 on 24439 degrees of freedom
Multiple R-squared:  0.8395,    Adjusted R-squared:  0.8395
F-statistic: 4.262e+04 on 3 and 24439 DF,  p-value: < 2.2e-16
"""



# distribution of degrees
degrees_list <- degree(engagements_graph)
mean(degrees_list)
sd(degrees_list)
-sort(-table(degrees_list))[1] # mode
min(degrees_list)
quantile(degrees_list, c(0.25, 0.5, 0.75))
max(degrees_list)
# t1: 5.865, 10.497, 4, 2, 3, 5, 7, 921
# t2: 8.360, 13.468, 2, 2, 4, 7, 10, 938
# t3: 14.09, 27.074, 2, 2, 6, 10, 15, 1727


# color-pair assortativity
color_pair_assortativity <- function(graph, color_pair) {
    subgraph <- induced.subgraph(
        graph, which(V(graph)$color %in% color_pair)
    )
    color_list = V(subgraph)$color
    color_list_as_int_levels <- as.integer(factor(color_list))
    assortativity(subgraph, color_list_as_int_levels)
}
# between black and blue
color_pair_assortativity(engagements_graph, c("#000000CC", "#0000FFCC"))
# t1: 0.0167
# t2: 0.8637
# t3: 0.8503
# between black and red
color_pair_assortativity(engagements_graph, c("#000000CC", "#FF0000CC"))
# t1: 0.6
# t2: 0.9833
# t3: 0.9897
# between blue and red
color_pair_assortativity(engagements_graph, c("#0000FFCC", "#FF0000CC"))
# t1: -0.001
# t2: 0.8646
# t3: 0.8825
assortativity()