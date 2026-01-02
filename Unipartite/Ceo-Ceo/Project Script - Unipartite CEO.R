library(tidyverse)
library(igraph)
library(ggraph)
library(ggplot2)
library(poweRlaw)

nodes <- tryCatch(read_csv("nodes_ceo_unipartite.csv"), error = function(e) read.csv("nodes_ceo_unipartite.csv", stringsAsFactors = FALSE))
edges <- tryCatch(read_csv("ceo_unipartite_edges.csv"), error = function(e) read.csv("ceo_unipartite_edges.csv", stringsAsFactors = FALSE))

if("id" %in% colnames(nodes) & !"name" %in% colnames(nodes)) {
  nodes <- nodes %>% rename(name = id)
}

if("Source" %in% colnames(edges) & "Target" %in% colnames(edges)) {
  edges <- edges %>% rename(from = Source, to = Target)
} else if("Source" %in% colnames(edges) & "to" %in% colnames(edges)) {
  edges <- edges %>% rename(from = Source)
} else if("source" %in% colnames(edges) & "target" %in% colnames(edges)) {
  edges <- edges %>% rename(from = source, to = target)
}

g <- graph_from_data_frame(d = edges, vertices = nodes, directed = FALSE)

if("Weight" %in% colnames(edges)) {
  E(g)$weight <- edges$Weight
} else if("weight" %in% colnames(edges)) {
  E(g)$weight <- edges$weight
} else {
  E(g)$weight <- rep(1, ecount(g))
}

cat("=== BASIC NETWORK METRICS (ENTIRE NETWORK) ===\n")
cat("Number of nodes:", vcount(g), "\n")
cat("Number of edges:", ecount(g), "\n")
cat("Graph density:", round(graph.density(g), 4), "\n")
cat("Is connected:", is.connected(g), "\n")
cat("Number of connected components:", components(g)$no, "\n")
cat("Average degree:", round(mean(degree(g)), 2), "\n")
cat("Global clustering coefficient:", round(transitivity(g, type = "global"), 4), "\n")
cat("Average local clustering coefficient:", round(transitivity(g, type = "localaverage"), 4), "\n")

if (!is.connected(g)) {
  comp <- components(g)
  largest_comp <- which.max(comp$csize)
  g_lcc <- induced_subgraph(g, which(comp$membership == largest_comp))
  cat("\n=== LARGEST CONNECTED COMPONENT ===\n")
  cat("Nodes in LCC:", vcount(g_lcc), "\n")
  cat("Edges in LCC:", ecount(g_lcc), "\n")
  cat("Average path length:", round(average.path.length(g_lcc), 2), "\n")
  cat("Diameter:", diameter(g_lcc), "\n")
  cat("Clustering coefficient:", round(transitivity(g_lcc), 4), "\n")
}

cat("\n=== DEGREE DISTRIBUTION ANALYSIS ===\n")
ceo_degrees <- degree(g)
cat("Degree statistics:\n")
print(summary(ceo_degrees))
cat("Standard deviation:", round(sd(ceo_degrees), 2), "\n")
hist(ceo_degrees, breaks = 30, main = "CEO Network - Degree Distribution", xlab = "Number of Connections (Degree)", ylab = "Frequency", col = "lightblue", border = "black")

degree_centrality <- ceo_degrees
top10_degree <- sort(degree_centrality, decreasing = TRUE)[1:10]
top10_degree_df <- data.frame(CEO = names(top10_degree), Degree = as.numeric(top10_degree), stringsAsFactors = FALSE)

ggplot(top10_degree_df, aes(x = Degree, y = reorder(CEO, Degree))) +
  geom_bar(stat = "identity", fill = "lightblue", width = 0.7, alpha = 0.8) +
  geom_text(aes(label = Degree), hjust = -0.3, size = 3.5, fontface = "bold") +
  labs(title = "Top 10 CEOs by Degree Centrality", x = "Degree", y = NULL) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), axis.text.y = element_text(size = 10)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15)))

cat("\n## Top 10 CEOs by Degree Centrality\n\n")
for(i in 1:nrow(top10_degree_df)) cat(top10_degree_df$CEO[i], "\n")
cat("\n---\n\n")

eigen_centrality <- eigen_centrality(g, scale = TRUE)$vector
top10_eigen <- sort(eigen_centrality, decreasing = TRUE)[1:10]
pagerank_centrality <- page_rank(g, directed = FALSE)$vector
top10_pagerank <- sort(pagerank_centrality, decreasing = TRUE)[1:10]

top10_eigen_df <- data.frame(CEO = names(top10_eigen), Eigenvector = as.numeric(top10_eigen), stringsAsFactors = FALSE)
ggplot(top10_eigen_df, aes(x = Eigenvector, y = reorder(CEO, Eigenvector))) +
  geom_bar(stat = "identity", fill = "darkorange", width = 0.7, alpha = 0.8) +
  geom_text(aes(label = round(Eigenvector, 3)), hjust = -0.3, size = 3.5, fontface = "bold") +
  labs(title = "Top 10 CEOs by Eigenvector Centrality", x = "Eigenvector Centrality", y = NULL) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), axis.text.y = element_text(size = 10)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15)))

top10_pr_df <- data.frame(CEO = names(top10_pagerank), PageRank = as.numeric(top10_pagerank), stringsAsFactors = FALSE)
ggplot(top10_pr_df, aes(x = PageRank, y = reorder(CEO, PageRank))) +
  geom_bar(stat = "identity", fill = "red3", width = 0.7, alpha = 0.8) +
  geom_text(aes(label = round(PageRank, 3)), hjust = -0.3, size = 3.5, fontface = "bold") +
  labs(title = "Top 10 CEOs by PageRank Centrality", x = "PageRank", y = NULL) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), axis.text.y = element_text(size = 10)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15)))

cat("\n## Top 10 CEOs by Eigenvector Centrality\n\n")
for(i in 1:nrow(top10_eigen_df)) cat(top10_eigen_df$CEO[i], "\n")
cat("\n---\n\n")
for(i in 1:nrow(top10_eigen_df)) cat("|", top10_eigen_df$CEO[i], "|", round(top10_eigen_df$Eigenvector[i], 3), "|\n")

cat("\n## Top 10 CEOs by PageRank Centrality\n\n")
for(i in 1:nrow(top10_pr_df)) cat(top10_pr_df$CEO[i], "\n")
cat("\n---\n\n")
for(i in 1:nrow(top10_pr_df)) cat("|", top10_pr_df$CEO[i], "|", round(top10_pr_df$PageRank[i], 3), "|\n")

betweenness_centrality_unn <- betweenness(g, normalized = FALSE)
top10_betweenness_unn <- sort(betweenness_centrality_unn, decreasing = TRUE)[1:10]
top10_betweenness_df_unn <- data.frame(CEO = names(top10_betweenness_unn), Betweenness = as.numeric(top10_betweenness_unn), stringsAsFactors = FALSE)
ggplot(top10_betweenness_df_unn, aes(x = Betweenness, y = reorder(CEO, Betweenness))) +
  geom_bar(stat = "identity", fill = "purple", width = 0.7, alpha = 0.8) +
  geom_text(aes(label = round(Betweenness, 1)), hjust = -0.3, size = 3.5, fontface = "bold") +
  labs(title = "Top 10 CEOs by Betweenness Centrality (Unnormalized)", x = "Betweenness Centrality", y = NULL) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), axis.text.y = element_text(size = 10)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15)))

cat("\n## Top 10 CEOs by Betweenness Centrality (Unnormalized)\n\n")
for(i in 1:nrow(top10_betweenness_df_unn)) cat(top10_betweenness_df_unn$CEO[i], "\n")
cat("\n---\n\n")
for(i in 1:nrow(top10_betweenness_df_unn)) cat("|", top10_betweenness_df_unn$CEO[i], "|", round(top10_betweenness_df_unn$Betweenness[i], 1), "|\n")

betweenness_centrality <- betweenness(g, normalized = TRUE)
top10_betweenness <- sort(betweenness_centrality, decreasing = TRUE)[1:10]
top10_betweenness_df <- data.frame(CEO = names(top10_betweenness), Betweenness = as.numeric(top10_betweenness), stringsAsFactors = FALSE)
ggplot(top10_betweenness_df, aes(x = Betweenness, y = reorder(CEO, Betweenness))) +
  geom_bar(stat = "identity", fill = "purple", width = 0.7, alpha = 0.8) +
  geom_text(aes(label = round(Betweenness, 3)), hjust = -0.3, size = 3.5, fontface = "bold") +
  labs(title = "Top 10 CEOs by Betweenness Centrality (Normalized)", x = "Betweenness Centrality (Normalized)", y = NULL) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), axis.text.y = element_text(size = 10)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15)))

cat("\n## Top 10 CEOs by Betweenness Centrality (Normalized)\n\n")
for(i in 1:nrow(top10_betweenness_df)) cat(top10_betweenness_df$CEO[i], "\n")
cat("\n---\n\n")
for(i in 1:nrow(top10_betweenness_df)) cat("|", top10_betweenness_df$CEO[i], "|", round(top10_betweenness_df$Betweenness[i], 3), "|\n")

closeness_centrality <- closeness(g, normalized = TRUE)
top10_closeness <- sort(closeness_centrality, decreasing = TRUE)[1:10]
bottom10_closeness <- sort(closeness_centrality, decreasing = FALSE)[1:10]
top10_closeness_df <- data.frame(CEO = names(top10_closeness), Closeness = as.numeric(top10_closeness), stringsAsFactors = FALSE)
ggplot(top10_closeness_df, aes(x = Closeness, y = reorder(CEO, Closeness))) +
  geom_bar(stat = "identity", fill = "darkgreen", width = 0.7, alpha = 0.8) +
  geom_text(aes(label = round(Closeness, 3)), hjust = -0.3, size = 3.5, fontface = "bold") +
  labs(title = "Top 10 CEOs by Closeness Centrality", x = "Closeness Centrality (Normalized)", y = NULL) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), axis.text.y = element_text(size = 10)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15)))

bottom10_closeness_df <- data.frame(CEO = names(bottom10_closeness), Closeness = as.numeric(bottom10_closeness), stringsAsFactors = FALSE)
ggplot(bottom10_closeness_df, aes(x = Closeness, y = reorder(CEO, -Closeness))) +
  geom_bar(stat = "identity", fill = "darkred", width = 0.7, alpha = 0.8) +
  geom_text(aes(label = round(Closeness, 3)), hjust = -0.3, size = 3.5, fontface = "bold") +
  labs(title = "Lowest 10 CEOs by Closeness Centrality", x = "Closeness Centrality (Normalized)", y = NULL) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), axis.text.y = element_text(size = 10)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15)))

cat("\n## Top 10 CEOs by Closeness Centrality\n\n")
for(i in 1:nrow(top10_closeness_df)) cat(top10_closeness_df$CEO[i], "\n")
cat("\n---\n\n")
for(i in 1:nrow(top10_closeness_df)) cat("|", top10_closeness_df$CEO[i], "|", round(top10_closeness_df$Closeness[i], 3), "|\n")
cat("\n## Bottom 10 CEOs by Closeness Centrality\n\n")
for(i in 1:nrow(bottom10_closeness_df)) cat(bottom10_closeness_df$CEO[i], "\n")
cat("\n---\n\n")
for(i in 1:nrow(bottom10_closeness_df)) cat("|", bottom10_closeness_df$CEO[i], "|", round(bottom10_closeness_df$Closeness[i], 3), "|\n")

weighted_degree <- strength(g, weights = E(g)$weight)
top10_weighted <- sort(weighted_degree, decreasing = TRUE)[1:10]
top10_weighted_df <- data.frame(CEO = names(top10_weighted), Weighted_Degree = as.numeric(top10_weighted), stringsAsFactors = FALSE)
ggplot(top10_weighted_df, aes(x = Weighted_Degree, y = reorder(CEO, Weighted_Degree))) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.7, alpha = 0.8) +
  geom_text(aes(label = round(Weighted_Degree, 1)), hjust = -0.3, size = 3.5, fontface = "bold") +
  labs(title = "Top 10 CEOs by Weighted Degree Centrality", x = "Weighted Degree (Strength)", y = NULL) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), axis.text.y = element_text(size = 10)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15)))

cat("\n## Top 10 CEOs by Weighted Degree Centrality\n\n")
for(i in 1:nrow(top10_weighted_df)) cat(top10_weighted_df$CEO[i], "\n")
cat("\n---\n\n")
for(i in 1:nrow(top10_weighted_df)) cat("|", top10_weighted_df$CEO[i], "|", round(top10_weighted_df$Weighted_Degree[i], 1), "|\n")

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

fg_community <- cluster_fast_greedy(g_giant)
cat("Modularity of Fast Greedy:", modularity(fg_community), "\n")
wt_community <- cluster_walktrap(g_giant)
cat("Modularity of Walktrap:", modularity(wt_community), "\n")
le_community <- cluster_leading_eigen(g_giant)
cat("Modularity of Leading Eigenvector:", modularity(le_community), "\n")
lp_community <- cluster_label_prop(g_giant)
cat("Modularity of Label Propagation:", modularity(lp_community), "\n")

ggraph(g_giant, layout = "fr") +
  geom_edge_link(alpha = 0.2, color = "grey") +
  geom_node_point(aes(color = as.factor(fg_community$membership)), size = 4) +
  scale_color_discrete(name = "Community") +
  ggtitle("Giant Component: Fast Greedy Communities") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))

V(g_giant)$community <- fg_community$membership
num_comms <- length(unique(fg_community$membership))
comm_colors <- rainbow(num_comms)
V(g_giant)$color <- comm_colors[V(g_giant)$community]

plot(g_giant, vertex.color = V(g_giant)$color, vertex.label = NA, main = "Fast Greedy Community Detection (Giant Component)", vertex.size = 5, edge.arrow.mode = 0)
plot(fg_community, g_giant, main = "Fast Greedy Communities (igraph community plot)")

giant_nodes_df <- nodes %>% filter(name %in% V(g_giant)$name)
small_nodes_df <- nodes %>% filter(name %in% V(g)$name & !(name %in% V(g_giant)$name))

if(all(c("from","to") %in% colnames(edges))) {
  giant_edges <- edges %>% filter(from %in% V(g_giant)$name & to %in% V(g_giant)$name)
  small_edges <- edges %>% filter(from %in% V(g)$name & !(from %in% V(g_giant)$name) & to %in% V(g)$name & !(to %in% V(g_giant)$name))
} else {
  giant_edges <- edges
  small_edges <- tibble()
}

giant_node_attributes <- data.frame(id = V(g_giant)$name, community = fg_community$membership, degree = degree(g_giant), stringsAsFactors = FALSE)
giant_nodes_df_export <- giant_nodes_df %>% left_join(giant_node_attributes, by = c("name" = "id"))

if(all(c("from","to") %in% colnames(edges))) {
  giant_edges_export <- giant_edges %>% rename(Source = from, Target = to)
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
  return(list(APL = apl, CC = cc, PowerLaw_p = gof))
}

metrics_actual <- calc_metrics(g_giant, "Actual Graph (Giant Component)")
metrics_er     <- calc_metrics(g_er, "ER Graph")
metrics_ba     <- calc_metrics(g_ba, "BA Graph")
metrics_ws     <- calc_metrics(g_ws, "WS Graph")

metrics_table <- data.frame(
  Graph = c("Actual Graph (Giant Component)", "Erdos-Renyi Graph", "Barabasi-Albert Graph", "Watts-Strogatz Graph"),
  Average_Path_Length = c(metrics_actual$APL, metrics_er$APL, metrics_ba$APL, metrics_ws$APL),
  Clustering_Coefficient = c(metrics_actual$CC, metrics_er$CC, metrics_ba$CC, metrics_ws$CC),
  Power_Law_p_value = c(metrics_actual$PowerLaw_p, metrics_er$PowerLaw_p, metrics_ba$PowerLaw_p, metrics_ws$PowerLaw_p),
  stringsAsFactors = FALSE
)

print(metrics_table)

wrap_text <- function(x, width = 20) sapply(x, function(y) paste(strwrap(y, width = width), collapse = "\n"))

target_communities <- list("1" = list(name = "Core Cluster", color = "lightblue"), "2" = list(name = "Secondary Cluster", color = "wheat"), "3" = list(name = "Peripheral Cluster", color = "lightgreen"))
par(mfrow=c(1,1), mar=c(1,1,3,1))

for(id in names(target_communities)) {
  comm_nodes <- V(g)[fg_community$membership == as.numeric(id)]
  if(length(comm_nodes) == 0) next
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
  plot(g_sub, layout = l, vertex.color = plot_color, vertex.frame.color = "darkgray", vertex.size = degree(g_sub) * 2.5, vertex.label.cex = 0.6, vertex.label.color = "black", vertex.label.font = 2, vertex.label.dist = 0.5, edge.color = "gray80", edge.width = if("weight" %in% edge_attr_names(g_sub)) E(g_sub)$weight else 1, main = paste(plot_title))
  filename <- paste0("Community_CEO_", id, "_Zoom.png")
  dev.copy(png, filename, width=2000, height=2000, res=300)
  dev.off()
  cat("Saved plot:", filename, "\n")
}

