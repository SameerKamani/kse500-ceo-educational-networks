library(igraph)
library(dplyr)
library(ggplot2)

edges <- read.csv("edges_clean.csv")
nodes <- read.csv("nodes_clean.csv")

g <- graph_from_data_frame(edges, directed = FALSE, vertices = nodes)

cat("=== GRAPH STRUCTURE ===\n")
cat("Number of vertices:", vcount(g), "\n")
cat("Number of edges:", ecount(g), "\n")
cat("Vertex names sample:", head(V(g)$name), "\n")

cat("\n=== BASIC GRAPH METRICS ===\n")
cat("Number of nodes:", vcount(g), "\n")
cat("Number of CEOs:", sum(V(g)$type == "CEO"), "\n")
cat("Number of Institutes:", sum(V(g)$type == "Institute"), "\n")
cat("Number of edges:", ecount(g), "\n")
cat("Graph density:", graph.density(g), "\n")
cat("Is bipartite:", is_bipartite(g), "\n")

all_degrees <- degree(g)
ceo_degrees <- all_degrees[V(g)$type == "CEO"]
institute_degrees <- all_degrees[V(g)$type == "Institute"]

cat("\n=== DEGREE STATISTICS ===\n")
cat("Average degree:", mean(all_degrees), "\n")
cat("Maximum degree:", max(all_degrees), "\n")
cat("Minimum degree:", min(all_degrees), "\n")
cat("Average CEO degree:", mean(ceo_degrees), "\n")
cat("Average Institute degree:", mean(institute_degrees), "\n")

cat("\n=== TOP 10 CEOs BY DEGREE ===\n")
ceo_degrees_sorted <- sort(ceo_degrees, decreasing = TRUE)
top_ceos <- head(ceo_degrees_sorted, 10)

for(i in 1:length(top_ceos)) {
  cat(i, ". ", names(top_ceos)[i], ": ", top_ceos[i], " connections\n", sep = "")
}

cat("\n=== TOP 10 INSTITUTES BY DEGREE ===\n")
institute_degrees_sorted <- sort(institute_degrees, decreasing = TRUE)
top_institutes <- head(institute_degrees_sorted, 10)

for(i in 1:length(top_institutes)) {
  cat(i, ". ", names(top_institutes)[i], ": ", top_institutes[i], " connections\n", sep = "")
}

cat("\n=== ADDITIONAL NETWORK METRICS ===\n")
cat("Diameter:", diameter(g), "\n")
cat("Average path length:", average.path.length(g), "\n")
cat("Clustering coefficient (transitivity):", transitivity(g), "\n")

ceo_summary <- data.frame(
  CEO = names(ceo_degrees),
  Degree = ceo_degrees,
  stringsAsFactors = FALSE
) %>% arrange(desc(Degree))

institute_summary <- data.frame(
  Institute = names(institute_degrees),
  Degree = institute_degrees,
  stringsAsFactors = FALSE
) %>% arrange(desc(Degree))

cat("\n=== TOP 10 CEOs (TABLE FORMAT) ===\n")
print(head(ceo_summary, 10))

cat("\n=== TOP 10 INSTITUTES (TABLE FORMAT) ===\n")
print(head(institute_summary, 10))

set.seed(123)

if(vcount(g) > 100) {
  cat("\nGraph is large (", vcount(g), " nodes). Creating simplified visualization.\n")
  high_degree_nodes <- V(g)[degree(g) > 1]
  g_simplified <- induced_subgraph(g, high_degree_nodes)
  g_viz <- g_simplified
} else {
  g_viz <- g
}

V(g_viz)$color <- ifelse(V(g_viz)$type == "CEO", "lightblue", "lightgreen")
V(g_viz)$size <- ifelse(V(g_viz)$type == "CEO", 4, 3)
V(g_viz)$label <- ifelse(V(g_viz)$type == "CEO", V(g_viz)$name, "")
V(g_viz)$label.cex <- ifelse(V(g_viz)$type == "CEO", 0.6, 0.5)

layout <- layout_with_fr(g_viz)

par(mar = c(1, 1, 3, 1))
plot(g_viz, 
     layout = layout,
     vertex.frame.color = "white",
     vertex.label.color = "darkblue",
     edge.color = "gray80",
     edge.width = 0.5,
     main = "CEO-Institute Network\n(CEOs in blue, Institutes in green)")

legend("bottomright", 
       legend = c("CEOs", "Institutes"), 
       fill = c("lightblue", "lightgreen"),
       bty = "n", 
       cex = 0.8)

par(mfrow = c(1, 2))

hist(ceo_degrees, 
     breaks = 20, 
     col = "lightblue", 
     border = "darkblue",
     main = "CEO Degree Distribution",
     xlab = "Number of Connections",
     ylab = "Frequency")

hist(institute_degrees, 
     breaks = 20, 
     col = "lightgreen", 
     border = "darkgreen",
     main = "Institute Degree Distribution", 
     xlab = "Number of Connections",
     ylab = "Frequency")

par(mfrow = c(1, 1))

write.csv(ceo_summary, "ceo_degrees.csv", row.names = FALSE)
write.csv(institute_summary, "institute_degrees.csv", row.names = FALSE)

save(g, file = "ceo_institute_network.RData")

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Results saved to:\n")
cat("- ceo_degrees.csv\n") 
cat("- institute_degrees.csv\n")
cat("- ceo_institute_network.RData\n")

cat("\n=== KEY INSIGHTS ===\n")
cat("Most connected CEO:", names(which.max(ceo_degrees)), "with", max(ceo_degrees), "connections\n")
cat("Most connected Institute:", names(which.max(institute_degrees)), "with", max(institute_degrees), "connections\n")
cat("Number of CEOs with multiple degrees:", sum(ceo_degrees > 1), "\n")
cat("Number of Institutes with multiple CEOs:", sum(institute_degrees > 1), "\n")

theme_better <- theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        axis.title = element_text(face = "bold"),
        panel.grid.minor = element_blank())

ceo_degree_df <- data.frame(Degree = ceo_degrees)
p1 <- ggplot(ceo_degree_df, aes(x = Degree)) +
  geom_bar(fill = "lightblue", color = "darkblue", alpha = 0.8) +
  labs(title = "CEO Degree Distribution",
       x = "Number of Educational Qualifications", 
       y = "Number of CEOs") +
  scale_x_continuous(breaks = 1:max(ceo_degrees)) +
  theme_better

print(p1)

institute_degree_df <- data.frame(Degree = institute_degrees)
p2 <- ggplot(institute_degree_df, aes(x = Degree)) +
  geom_bar(fill = "lightgreen", color = "darkgreen", alpha = 0.8) +
  labs(title = "Institute Degree Distribution",
       x = "Number of CEOs Produced", 
       y = "Number of Institutes") +
  theme_better

print(p2)

top_ceos_df <- head(ceo_summary, 17)
top_ceos_df$CEO <- factor(top_ceos_df$CEO, levels = top_ceos_df$CEO[order(top_ceos_df$Degree)])

p3 <- ggplot(top_ceos_df, aes(x = CEO, y = Degree)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.8) +
  labs(title = "Top CEOs by Number of Educational Degrees",
       x = "CEO", 
       y = "Number of Educational Degrees") +
  coord_flip() +
  theme_better

print(p3)

top_institutes_df <- head(institute_summary, 10)
top_institutes_df$Institute <- factor(top_institutes_df$Institute, 
                                      levels = top_institutes_df$Institute[order(top_institutes_df$Degree)])

p4 <- ggplot(top_institutes_df, aes(x = Institute, y = Degree)) +
  geom_bar(stat = "identity", fill = "darkgreen", alpha = 0.8) +
  geom_text(aes(label = Degree), 
            hjust = -0.2,
            size = 3.5,
            color = "black") +
  labs(title = "Top 10 Institutes by Number of CEO Connections",
       x = "Institute", 
       y = "Number of CEOs") +
  coord_flip() +
  theme_better +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

print(p4)

ceos_with_3_degrees <- sum(ceo_degrees == 3)
cat("Number of CEOs with exactly 3 educational qualifications:", ceos_with_3_degrees, "\n")

if(ceos_with_3_degrees > 0) {
  ceos_3_degrees <- ceo_summary %>% filter(Degree == 3)
  cat("\nCEOs with 3 qualifications:\n")
  print(ceos_3_degrees)
}

cat("\n=== GENERATING LOG-LOG PLOT ===\n")

degree_dist_table <- table(institute_degrees)
degree_dist_df <- data.frame(
  Degree = as.numeric(names(degree_dist_table)),
  Frequency = as.numeric(degree_dist_table)
)

p5 <- ggplot(degree_dist_df, aes(x = Degree, y = Frequency)) +
  geom_point(color = "darkgreen", size = 3, alpha = 0.7) +
  scale_x_log10() + 
  scale_y_log10() +
  stat_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  labs(title = "Log-Log Plot of Institute Degree Distribution",
       subtitle = "Straight line indicates Scale-Free (Power Law) Network behavior",
       x = "Log(Degree) - Number of CEOs Produced",
       y = "Log(Frequency) - Number of Institutes") +
  theme_better +
  annotation_logticks()

print(p5)
