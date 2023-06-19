full_engagement_network <- read.csv("data/full_engagement_network.csv")
chirper_data <- read.csv("data/chirper_data.csv")

# only keep engagements of chirpers with data
full_engagement_network <- full_engagement_network[
    full_engagement_network$source %in% chirper_data$chirper &
        full_engagement_network$target %in% chirper_data$chirper,
]


####################
# See time range of data #
####################

full_engagement_network["date"] <- as.Date(
    full_engagement_network$timestamp,
    format = "%Y-%m-%dT%H:%M:%S.%OSZ"
)
full_engagement_network <- full_engagement_network[
    !is.na(full_engagement_network$date),
]

# date range of data
min(full_engagement_network$date) # 2023-04-21
max(full_engagement_network$date) # 2023-05-16

# we set the following time points (inclusive ends):
# T1: 2023-04-28
# T2: 2023-05-06
# T3: 2023-05-14


####################
# Split Data into times and Summarise #
####################

engagements_time1 <- full_engagement_network[
    full_engagement_network$date <= "2023-04-28",
]
engagements_time2 <- full_engagement_network[
    full_engagement_network$date <= "2023-05-06",
]
engagements_time3 <- full_engagement_network[
    full_engagement_network$date <= "2023-05-14",
]


summarise_engagements <- function(engagements) {
    engagements_edges <- data.frame(
        table(engagements$source, engagements$target)
    )
    colnames(engagements_edges) <- c("source", "target", "engagement_count")
    engagements_edges <- engagements_edges[
        engagements_edges$engagement_count >= 1,
    ]
    engagements_edges$source <- as.character(engagements_edges$source)
    engagements_edges$target <- as.character(engagements_edges$target)
    engagements_edges <- engagements_edges[
        engagements_edges$source %in% chirper_data$chirper &
            engagements_edges$target %in% chirper_data$chirper,
    ]
    engagements_edges <- engagements_edges[
        engagements_edges$source != engagements_edges$target,
    ]
    # drop any repeated edges
    engagements_edges <- engagements_edges[
        !duplicated(engagements_edges[c("source", "target")]),
    ]
}

engagements_edges_time1 <- summarise_engagements(engagements_time1)
write.csv(engagements_edges_time1, "data/engagement_summary_time1.csv")
rm(engagements_time1, engagements_edges_time1)

engagements_edges_time2 <- summarise_engagements(engagements_time2)
write.csv(engagements_edges_time2, "data/engagement_summary_time2.csv")
rm(engagements_time2, engagements_edges_time2)

engagements_edges_time3 <- summarise_engagements(engagements_time3)
write.csv(engagements_edges_time3, "data/engagement_summary_time3.csv")
rm(engagements_time3, engagements_edges_time3)


en_chirpers <- chirper_data[chirper_data$lang == "en", "chirper"]
en_engagement_network <- full_engagement_network[
    full_engagement_network$source %in% en_chirpers &
        full_engagement_network$target %in% en_chirpers,
]

en_engagements_edges <- summarise_engagements(en_engagement_network)
write.csv(en_engagements_edges, "data/en_engagement_summary.csv")
