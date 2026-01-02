library(tidyverse)
library(igraph)
library(ggraph)
library(poweRlaw)

nodes <- read_csv("nodes_institute_unipartite.csv")
edges <- read_csv("institute_unipartite_edges.csv")

g <- graph_from_data_frame(d = edges, vertices = nodes, directed = FALSE)

if("Weight" %in% colnames(edges)) {
  E(g)$weight <- edges$Weight
} else {
  E(g)$weight <- rep(1, ecount(g))
}

cat("Graph Summary:\n")
print(g)
cat("Number of nodes:", vcount(g), "\n")
cat("Number of edges:", ecount(g), "\n")
cat("Is directed:", is.directed(g), "\n")

if("Weight" %in% colnames(edges)) {
  cat("Edge weights summary:\n")
  print(summary(E(g)$weight))
}

if("Weight" %in% colnames(edges)) {
  hist(edges$Weight,
       main = "Distribution of Edge Weights",
       xlab = "Edge Weight",
       ylab = "Frequency",
       col = "lightblue",
       border = "black",
       breaks = 20)
}

cat("Network Metrics:\n")
cat("Average degree:", mean(degree(g)), "\n")
cat("Density:", graph.density(g), "\n")
cat("Average path length:", average.path.length(g), "\n")
cat("Diameter:", diameter(g), "\n")

degree_dist <- degree(g)
cat("Degree distribution summary:\n")
print(summary(degree_dist))

cat("Clustering Coefficients:\n")
cat("Global clustering coefficient (transitivity):", transitivity(g, type = "global"), "\n")
cat("Average local clustering coefficient:", transitivity(g, type = "average"), "\n")
cat("Weighted clustering coefficient:", transitivity(g, type = "barrat"), "\n")

hist(degree(g),
     main = "Degree Distribution",
     xlab = "Degree",
     ylab = "Number of Nodes",
     col = "lightcoral",
     border = "darkred",
     breaks = 20)

degree_centrality <- degree(g)
top10_degree <- sort(degree_centrality, decreasing = TRUE)[1:10]
top10_df <- data.frame(
  Institute = names(top10_degree),
  Degree = as.numeric(top10_degree),
  stringsAsFactors = FALSE
)

ggplot(top10_df, aes(x = Degree, y = reorder(Institute, Degree))) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.7) +
  labs(title = "Top 10 Institutes by Degree Centrality", x = "Degree", y = NULL) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 9),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  ) +
  geom_text(aes(label = Degree), hjust = -0.3, size = 3.5) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_y_discrete(labels = function(x) {
    sapply(x, function(label) {
      if(nchar(label) > 40) {
        paste0(substr(label, 1, 40), "\n", substr(label, 41, nchar(label)))
      } else {
        label
      }
    })
  })

weighted_degree <- strength(g, weights = E(g)$weight)
top10_weighted <- sort(weighted_degree, decreasing = TRUE)[1:10]
top10_w_df <- data.frame(
  Institute = names(top10_weighted),
  Weighted_Degree = as.numeric(top10_weighted),
  stringsAsFactors = FALSE
)

ggplot(top10_w_df, aes(x = Weighted_Degree, y = reorder(Institute, Weighted_Degree))) +
  geom_bar(stat = "identity", fill = "darkgreen", width = 0.7, alpha = 0.8) +
  geom_text(aes(label = Weighted_Degree), hjust = -0.3, size = 3.5, fontface = "bold") +
  labs(title = "Top 10 Institutes by Weighted Degree Centrality", x = "Weighted Degree (Strength)", y = NULL) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 9),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  scale_y_discrete(labels = function(x) {
    sapply(x, function(label) {
      if(nchar(label) > 40) {
        paste0(substr(label, 1, 40), "\n", substr(label, 41, nchar(label)))
      } else {
        label
      }
    })
  })

cat("Top 10 Institutes by Weighted Degree:\n")
print(top10_w_df)

betweenness_centrality <- betweenness(g, normalized = TRUE)
top10_betweenness <- sort(betweenness_centrality, decreasing = TRUE)[1:10]
top10_b_df <- data.frame(
  Institute = names(top10_betweenness),
  Betweenness = as.numeric(top10_betweenness),
  stringsAsFactors = FALSE
)

ggplot(top10_b_df, aes(x = Betweenness, y = reorder(Institute, Betweenness))) +
  geom_bar(stat = "identity", fill = "purple", width = 0.7, alpha = 0.8) +
  geom_text(aes(label = round(Betweenness, 3)), hjust = -0.3, size = 3.5, fontface = "bold") +
  labs(title = "Top 10 Institutes by Betweenness Centrality", x = "Betweenness Centrality (Normalized)", y = NULL) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 9),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  scale_y_discrete(labels = function(x) {
    sapply(x, function(label) {
      if(nchar(label) > 40) {
        paste0(substr(label, 1, 40), "\n", substr(label, 41, nchar(label)))
      } else {
        label
      }
    })
  })

cat("Top 10 Institutes by Betweenness Centrality:\n")
print(top10_b_df)

betweenness_unnormalized <- betweenness(g, normalized = FALSE)
top10_betweenness_unnorm <- sort(betweenness_unnormalized, decreasing = TRUE)[1:10]
top10_b_unnorm_df <- data.frame(
  Institute = names(top10_betweenness_unnorm),
  Betweenness = as.numeric(top10_betweenness_unnorm),
  stringsAsFactors = FALSE
)

ggplot(top10_b_unnorm_df, aes(x = Betweenness, y = reorder(Institute, Betweenness))) +
  geom_bar(stat = "identity", fill = "darkorchid", width = 0.7, alpha = 0.8) +
  geom_text(aes(label = round(Betweenness, 1)), hjust = -0.3, size = 3.5, fontface = "bold") +
  labs(title = "Top 10 Institutes by Betweenness Centrality (Unnormalized)", x = "Betweenness Centrality", y = NULL) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 9),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  scale_y_discrete(labels = function(x) {
    sapply(x, function(label) {
      if(nchar(label) > 40) {
        paste0(substr(label, 1, 40), "\n", substr(label, 41, nchar(label)))
      } else {
        label
      }
    })
  })

eigen_centrality <- eigen_centrality(g, scale = TRUE)$vector
top10_eigen <- sort(eigen_centrality, decreasing = TRUE)[1:10]
pagerank_centrality <- page_rank(g, directed = FALSE)$vector
top10_pagerank <- sort(pagerank_centrality, decreasing = TRUE)[1:10]

top10_eigen_df <- data.frame(
  Institute = names(top10_eigen),
  Eigenvector = as.numeric(top10_eigen),
  stringsAsFactors = FALSE
)

ggplot(top10_eigen_df, aes(x = Eigenvector, y = reorder(Institute, Eigenvector))) +
  geom_bar(stat = "identity", fill = "darkorange", width = 0.7, alpha = 0.8) +
  geom_text(aes(label = round(Eigenvector, 3)), hjust = -0.3, size = 3.5, fontface = "bold") +
  labs(title = "Top 10 Institutes by Eigenvector Centrality", x = "Eigenvector Centrality", y = NULL) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.y = element_text(size = 10)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15)))

top10_pr_df <- data.frame(
  Institute = names(top10_pagerank),
  PageRank = as.numeric(top10_pagerank),
  stringsAsFactors = FALSE
)

ggplot(top10_pr_df, aes(x = PageRank, y = reorder(Institute, PageRank))) +
  geom_bar(stat = "identity", fill = "red3", width = 0.7, alpha = 0.8) +
  geom_text(aes(label = round(PageRank, 3)), hjust = -0.3, size = 3.5, fontface = "bold") +
  labs(title = "Top 10 Institutes by PageRank Centrality", x = "PageRank", y = NULL) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.y = element_text(size = 10)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15)))

cat("Top 10 by Eigenvector Centrality:\n")
print(top10_eigen_df)
cat("Top 10 by PageRank Centrality:\n")
print(top10_pr_df)

closeness_centrality <- closeness(g, normalized = TRUE)
top10_closeness <- sort(closeness_centrality, decreasing = TRUE)[1:10]
top10_close_df <- data.frame(
  Institute = names(top10_closeness),
  Closeness = as.numeric(top10_closeness),
  stringsAsFactors = FALSE
)

ggplot(top10_close_df, aes(x = Closeness, y = reorder(Institute, Closeness))) +
  geom_bar(stat = "identity", fill = "darkcyan", width = 0.7, alpha = 0.8) +
  geom_text(aes(label = round(Closeness, 3)), hjust = -0.3, size = 3.5, fontface = "bold") +
  labs(title = "Top 10 Institutes by Closeness Centrality", x = "Closeness Centrality (Normalized)", y = NULL) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.y = element_text(size = 10)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15)))

cat("Top 10 by Closeness Centrality:\n")
print(top10_close_df)

bottom10_closeness <- sort(closeness_centrality, decreasing = FALSE)[1:10]
bottom10_close_df <- data.frame(
  Institute = names(bottom10_closeness),
  Closeness = as.numeric(bottom10_closeness),
  stringsAsFactors = FALSE
)

ggplot(bottom10_close_df, aes(x = Closeness, y = reorder(Institute, -Closeness))) +
  geom_bar(stat = "identity", fill = "firebrick", width = 0.7, alpha = 0.8) +
  geom_text(aes(label = round(Closeness, 3)), hjust = -0.3, size = 3.5, fontface = "bold") +
  labs(title = "10 Most Peripheral Institutes by Closeness Centrality", subtitle = "Institutions with Least Efficient Network Access", x = "Closeness Centrality (Normalized)", y = NULL) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.y = element_text(size = 10)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15)))

cat("10 Most Peripheral Institutes by Closeness Centrality:\n")
print(bottom10_close_df)

comps <- components(g)
giant_comp_id <- which.max(comps$csize)
giant_vids <- which(comps$membership == giant_comp_id)
g_giant <- induced_subgraph(g, vids = giant_vids)

cat("Giant Component Summary:\n")
print(g_giant)
cat("Number of nodes in giant component:", vcount(g_giant), "\n")
cat("Number of edges in giant component:", ecount(g_giant), "\n")
cat("Proportion of original network:", round(vcount(g_giant)/vcount(g) * 100, 2), "%\n")

cat("Giant Component Metrics:\n")
cat("Average degree:", mean(degree(g_giant)), "\n")
cat("Density:", graph.density(g_giant), "\n")
cat("Average path length:", average.path.length(g_giant), "\n")
cat("Diameter:", diameter(g_giant), "\n")

set.seed(123)
deg_giant <- degree(g_giant)
V(g_giant)$degree <- deg_giant
V(g_giant)$size <- deg_giant * 3
top10_giant <- names(sort(deg_giant, decreasing = TRUE))[1:10]
V(g_giant)$label <- ifelse(names(V(g_giant)) %in% top10_giant, names(V(g_giant)), "")

ggraph(g_giant, layout = "kk") +
  geom_edge_link(alpha = 0.3) +
  geom_node_point(aes(size = degree), color = "steelblue") +
  geom_node_text(aes(label = label), repel = TRUE, size = 3) +
  ggtitle("Giant Component (KK Layout)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5))

short_names <- c(
  "Institute Of Actuaries" = "IFoA",
  "Institute Of Paper Chemistry" = "IPC",
  "Mccallum Graduate School Of Business" = "Mccallum GSB",
  "Whu-Otto Beisheim School Of Management" = "WHU",
  "Pakistan Military Academy Kakul" = "PMA",
  "Royal Military Academy Sandhurst" = "RMAS",
  "F.G Sir Syed College Rawalpindi" = "FGSSC",
  "American University Of London" = "AUL",
  "National Defence University Islamabad" = "NDU",
  "London Business School" = "LBS",
  "Regent'S Business School London" = "RBS London",
  "American College Of Switzerland" = "ACS",
  "Business School Lausanne" = "BSL",
  "American Intercontinental University System" = "AIU",
  "Gabelli School Of Business" = "Fordham University"
)

small_vids <- which(comps$membership != giant_comp_id)
g_small <- induced_subgraph(g, vids = small_vids)
if("name" %in% vertex_attr_names(g_small)) {
  V(g_small)$short_name <- recode(V(g_small)$name, !!!short_names)
} else {
  V(g_small)$short_name <- V(g_small)$name
}

ggraph(g_small, layout = "fr") +
  geom_edge_link(alpha = 0.4) +
  geom_node_point(color = "steelblue", size = 5) +
  geom_node_text(aes(label = short_name), repel = TRUE, size = 3.2) +
  ggtitle("Smaller Components") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))

giant_nodes_df <- nodes %>% filter(id %in% V(g_giant)$name)
small_nodes_df <- nodes %>% filter(id %in% V(g_small)$name)

if(all(c("Source","Target") %in% colnames(edges))) {
  giant_edges <- edges %>% filter(Source %in% V(g_giant)$name & Target %in% V(g_giant)$name)
  small_edges <- edges %>% filter(Source %in% V(g_small)$name & Target %in% V(g_small)$name)
} else {
  giant_edges <- edges
  small_edges <- tibble()
}

write_csv(giant_nodes_df, "giant_component_nodes.csv")
write_csv(giant_edges, "giant_component_edges.csv")
write_csv(small_nodes_df, "small_components_nodes.csv")
write_csv(small_edges, "small_components_edges.csv")

cat("Files created successfully:\n- giant_component_nodes.csv\n- giant_component_edges.csv\n- small_components_nodes.csv\n- small_components_edges.csv\n")

fg_community <- cluster_fast_greedy(g_giant)
cat("=== Fast Greedy Community Membership ===\n")
print(fg_community$membership)
cat("Modularity of Fast Greedy:", modularity(fg_community), "\n\n")

wt_community <- cluster_walktrap(g_giant)
cat("=== Walktrap Community Membership ===\n")
print(wt_community$membership)
cat("Modularity of Walktrap:", modularity(wt_community), "\n\n")

le_community <- cluster_leading_eigen(g_giant)
cat("=== Leading Eigenvector Community Membership ===\n")
print(le_community$membership)
cat("Modularity of Leading Eigenvector:", modularity(le_community), "\n\n")

lp_community <- cluster_label_prop(g_giant)
cat("=== Label Propagation Community Membership ===\n")
print(lp_community$membership)
cat("Modularity of Label Propagation:", modularity(lp_community), "\n\n")

V(g_giant)$community <- fg_community$membership
num_comms <- length(unique(fg_community$membership))
comm_colors <- rainbow(num_comms)
V(g_giant)$color <- comm_colors[V(g_giant)$community]

plot(
  g_giant,
  vertex.color = V(g_giant)$color,
  vertex.label = NA,
  main = "Fast Greedy Community Detection (Giant Component)",
  vertex.size = 5,
  edge.arrow.mode = 0
)

plot(
  fg_community,
  g_giant,
  main = "Fast Greedy Communities (igraph community plot)"
)

giant_node_attributes <- data.frame(
  id = V(g_giant)$name,
  community = fg_community$membership,
  degree = degree(g_giant),
  stringsAsFactors = FALSE
)

giant_nodes_df_export <- giant_nodes_df %>%
  left_join(giant_node_attributes, by = "id")

if(all(c("Source","Target") %in% colnames(edges))) {
  giant_edges_export <- giant_edges %>%
    rename(source = Source, target = Target)
} else {
  giant_edges_export <- giant_edges
}

write_csv(giant_nodes_df_export, "gephi_giant_component_nodes.csv")
write_csv(giant_edges_export, "gephi_giant_component_edges.csv")

set.seed(123)
num_nodes <- vcount(g_giant)
num_edges <- ecount(g_giant)
avg_deg <- mean(degree(g_giant))

p_er <- (2 * num_edges) / (num_nodes * (num_nodes - 1))
g_er <- erdos.renyi.game(num_nodes, p_er, type = "gnp", directed = FALSE, loops = FALSE)

m_ba <- max(1, round(avg_deg / 2))
g_ba <- barabasi.game(num_nodes, m = m_ba, directed = FALSE)

k_ws <- max(1, round(avg_deg))
p_ws <- 0.05
g_ws <- watts.strogatz.game(1, num_nodes, k_ws, p_ws)

calc_metrics <- function(graph, graph_name) {
  apl <- average.path.length(graph)
  cc <- transitivity(graph, type = "global")
  degs <- degree(graph)
  pl <- displ$new(degs[degs > 0])
  est <- estimate_xmin(pl)
  pl$setXmin(est)
  if (graph_name == "Actual Graph (Giant Component)") {
    bs <- bootstrap_p(pl, no_of_sims = 200)
    gof <- bs$p
  } else {
    gof <- NA
  }
  return(list(
    APL = apl,
    CC = cc,
    PowerLaw_p = gof
  ))
}

metrics_actual <- calc_metrics(g_giant, "Actual Graph (Giant Component)")
metrics_er     <- calc_metrics(g_er, "ER Graph")
metrics_ba     <- calc_metrics(g_ba, "BA Graph")
metrics_ws     <- calc_metrics(g_ws, "WS Graph")

metrics_table <- data.frame(
  Graph = c(
    "Actual Graph (Giant Component)",
    "Erdos-Renyi Graph",
    "Barabasi-Albert Graph",
    "Watts-Strogatz Graph"
  ),
  Average_Path_Length = c(
    metrics_actual$APL,
    metrics_er$APL,
    metrics_ba$APL,
    metrics_ws$APL
  ),
  Clustering_Coefficient = c(
    metrics_actual$CC,
    metrics_er$CC,
    metrics_ba$CC,
    metrics_ws$CC
  ),
  Power_Law_p_value = c(
    metrics_actual$PowerLaw_p,
    metrics_er$PowerLaw_p,
    metrics_ba$PowerLaw_p,
    metrics_ws$PowerLaw_p
  ),
  stringsAsFactors = FALSE
)

print(metrics_table)

wrap_text <- function(x, width = 20) {
  sapply(x, function(y) paste(strwrap(y, width = width), collapse = "\n"))
}

target_communities <- list(
  "2" = list(name = "The Tech-Business Nexus", color = "lightblue"),
  "4" = list(name = "The Lahore-International Axis", color = "wheat"),
  "1" = list(name = "The National Legacy & Accounting Cluster", color = "lightgreen"),
  "5" = list(name = "The UK Cluster", color = "lavender")
)

par(mfrow=c(1,1), mar=c(1,1,3,1))

for(id in names(target_communities)) {
  comm_nodes <- V(g)[membership(fg_community) == as.numeric(id)]
  g_sub <- induced_subgraph(g, comm_nodes)
  plot_title <- target_communities[[id]]$name
  plot_color <- target_communities[[id]]$color
  V(g_sub)$label <- wrap_text(V(g_sub)$name, width = 15)
  if(vcount(g_sub) > 10) {
    node_importance <- degree(g_sub)
    V(g_sub)$label <- ifelse(node_importance > 1, V(g_sub)$label, NA)
  }
  set.seed(100)
  l <- layout_with_fr(g_sub, niter=10000, repulserad=vcount(g_sub)^2.5)
  plot(g_sub,
       layout = l,
       vertex.color = plot_color,
       vertex.frame.color = "darkgray",
       vertex.size = degree(g_sub) * 2.5,
       vertex.label.cex = 0.6,
       vertex.label.color = "black",
       vertex.label.font = 2,
       vertex.label.dist = 0.5,
       edge.color = "gray80",
       edge.width = if("weight" %in% edge_attr_names(g_sub)) E(g_sub)$weight else 1,
       main = paste(plot_title))
  filename <- paste0("Community_", id, "_Zoom.png")
  dev.copy(png, filename, width=2000, height=2000, res=300)
  dev.off()
  cat("Saved plot:", filename, "\n")
}

