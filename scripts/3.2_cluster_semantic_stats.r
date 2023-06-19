library(report)

time <- 1
chirper_distances <- read.csv(paste0(
    "data/networks/en_engagement_clusters_time", time, ".csv"
))[, -2]
# or full network
chirper_distances <- read.csv(
    "data/networks/en_engagement_clusters_full.csv"
)[, -2]

# t-test: cluster-distance vs global-distance

test_result <- t.test(
    chirper_distances$semantic_distance_to_cluster,
    chirper_distances$semantic_distance_to_global,
    paired = TRUE
)
report(test_result)

# t1:
# difference = -0.03, 95% CI [-0.04, -0.03], t(1148) = -20.91, p < .001;
# Cohen's d = -0.62, 95% CI [-0.68, -0.55]

# t2:
# difference = -0.00616, 95% CI [-0.00667, -0.00564], t(6813) = -23.37, p <.001 # nolint
# Cohen's d = -0.28, 95% CI [-0.31, -0.26]

# t3:
# difference = -0.02, 95% CI [-0.02, -0.02], t(9130) = -32.81, p < .001;
# Cohen's d = -0.34, 95% CI [-0.36, -0.32]

# full:
# difference = -0.03, 95% CI [-0.03, -0.03], t(16002) = -87.77, p < .001
# Cohen's d = -0.69, 95% CI [-0.71, -0.68]
