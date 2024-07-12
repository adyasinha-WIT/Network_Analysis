#TASK 1
#Part a.)
links <- read.csv("./Data Set/StreetGangLinks.csv")
nodes <- read.csv("./Data Set/StreetGangNodes.csv")

#Part b.)
install.packages("igraph")
library(igraph)

g <- graph_from_data_frame(d=links, vertices=nodes, directed=FALSE)

#Part c.)
# Inspecting node attributes
node_attr <- attributes(V(g))
head(attributes(V(g)))

# Inspecting link attributes
link_attr <- attributes(E(g))
head(attributes(E(g)))

#Part d.)
plot(g)

#Part e.)
plot(g, edge.width=E(g)$weight, vertex.size=V(g)$Age/2)

##############################################
#TASK 2
#Part a.)
# Calculating degree centrality
degree_centrality <- degree(g)
head(degree(g))

# Calculating betweenness centrality
betweenness_centrality <- betweenness(g)
head(betweenness(g))

# Calculating closeness centrality
closeness_centrality <- closeness(g)
head(closeness(g))

#Part b.)
# Order nodes by degree centrality
degree_order <- order(degree_centrality, decreasing = TRUE)
head(degree_order)

top_3_degree <- V(g)$name[degree_order[1:3]]
head(top_3_degree)

# Order nodes by betweenness centrality
betweenness_order <- order(betweenness_centrality, decreasing = TRUE)
head(betweenness_order)

top_3_betweenness <- V(g)$name[betweenness_order[1:3]]
head(top_3_betweenness)

# Order nodes by closeness centrality
closeness_order <- order(closeness_centrality, decreasing = TRUE)
head(closeness_order)

top_3_closeness <- V(g)$name[closeness_order[1:3]]
head(top_3_closeness)

##############################################
#TASK 3
#Part a.)

# Identifying nodes with a degree less than 15
nodes_to_remove <- V(g)[degree_centrality < 15]

# Removing these nodes from the network
g_simplified <- delete_vertices(g, nodes_to_remove)

# Identifying edges with a weight attribute less than 3
edges_to_remove <- E(g_simplified)[E(g_simplified)$weight < 3]

# Removing these edges from the network
g_simplified <- delete.edges(g_simplified, edges_to_remove)

# Plotting the adjusted network using 'layout_nicely'
plot(g_simplified, layout=layout_nicely(g_simplified))


#Part b.)

##############################################
#TASK 4
#Part a.)
# Setting the node colors for rankings
colors_ranking <- c("red", "green", "orange", "purple", "pink")

# Assigning colors to nodes based on their ranking attribute
V(g)$color <- colors_ranking[V(g)$Ranking]

# Plotting the network with node colors based on ranking
plot(g, vertex.color = V(g)$color, vertex.label=NA, edge.width= E(g)$weight, main= "Network Colored by Ranking")
legend("topright", legend=unique(V(g)$Ranking), col=colors_ranking, pch=15, bty="n", title="Ranking")


#Part b.)
# Setting the node color for birthplaces 
colors_birthplace <- c("brown", "cyan", "magenta", "gray")

# Assigning colors to nodes based on their ranking attributes
V(g)$color <- colors_birthplace[V(g)$Birthplace]

# Filtering the network to only include gang members who have served time in prison
g_prison <- induced_subgraph(g, V(g)[V(g)$Prison == 1])

# Plotting the simplified network
plot(g_prison, vertex.color=V(g_prison)$color, vertex.label=NA, edge.width=E(g_prison)$weight, main="Network Colored by Birthplace (Prison Only)")
legend("topright",legend=unique(V(g_prison)$Birthplace), col=colors_birthplace, pch=15, bty="n", title="Birthplace")


#Part c.)
# Filtering the nodes by ranking(less than 3)
g_filtered <- induced_subgraph(g_prison, V(g_prison)[V(g_prison)$Ranking >=3])

# Calculating the hub scores
hub_scores <- hub_score(g_filtered)$vector

# Creating the two-panel plot
par(mfrow=c(1,2))

# Panel 1: Plotting network with node sizes proportional to hub scores
plot(g_filtered, vertex.size= 15*hub_scores, vertex.label=NA, edge.width=E(g_filtered)$weight, main= "Network with hub scores")

# Panel 2: Displaying the communities within the network
communities <- cluster_optimal(g_filtered)
plot(communities, g_filtered, main= "Communities in the network")

# Resetting the plotting layout
par(mfrow=c(1,1))


##############################################
#TASK 5
# Simplifying the network based on Rankings 

# Defining the colors 
colors_ranking <- c("tomato", "cyan", "green", "orange", "grey")
  
# Function to create and plot the subgraph for each ranking
plot_ranking_subgraph <- function(g, ranking, color) {
   
# Filter the nodes by ranking first
subgraph <- induced_subgraph(g, V(g)[V(g)$Ranking == ranking])

# Plotting the subgraph
plot(subgraph, vertex.color=color, vertex.lable=NA, edge.width=E(subgraph)$weight, main = paste("Network for Ranking", ranking))
}

for (i in 1:5){
  plot_ranking_subgraph(g,i, colors_ranking[i])
}

##############################################
#TASK 6
#Part a.)

# Remove links with a weight value equal to 1
g <- delete_edges(g, E(g)[weight == 1])

# Extract nodes for each region
uk_nodes <- V(g)[V(g)$Birthplace == 3]$name
westafrican_nodes<- V(g)[V(g)$Birthplace == 1]$name
caribbean_nodes<- V(g)[V(g)$Birthplace == 2]$name
eastafrican_nodes<- V(g)[V(g)$Birthplace == 4]$name

# Create and plot subgraphs for each ethnicity
par(mfrow=c(2, 2))  # Set up the plotting area to have 2 rows and 2 columns

# UK and West African
westafrican_nodes <- induced_subgraph(g, c(uk_nodes, westafrican_nodes))
westafrican_nodes<- delete_edges(uk_westafrican, E(uk_westafrican)[weight == 1])
V(uk_westafrican)$color <- ifelse(V(uk_westafrican)$Birthplace == 3, "magenta", "blue")
plot(uk_westafrican, main = "UK and West African Interactions", vertex.label=NA, edge.width=E(uk_westafrican)$weight)

# UK and Caribbean
uk_caribbean <- induced_subgraph(g, c(nodes_uk, caribbean_nodes))
uk_caribbean <- delete_edges(uk_caribbean, E(uk_caribbean)[weight == 1])
plot(uk_caribbean, main = "UK and Caribbean Interactions", vertex.label=NA, edge.width=E(uk_caribbean)$weight)

# UK and East African
uk_eastafrican <- induced_subgraph(g, c(nodes_uk, eastafrican_nodes))
uk_eastafrican <- delete_edges(uk_eastafrican, E(uk_eastafrican)[weight == 1])
plot(uk_eastafrican, main = "UK and East African Interactions", vertex.label=NA, edge.width=E(uk_eastafrican)$weight)


#Part b.)
# Create subgraph for UK and Caribbean interactions
uk_caribbean <- induced_subgraph(g, c(uk_nodes, caribbean_nodes))
uk_caribbean <- delete_edges(uk_caribbean, E(uk_caribbean)[weight == 1])

# Calculate authority scores
authority_scores <- authority_score(uk_caribbean)$vector

# Set the size of each node to 10 times the value of authority score
V(uk_caribbean)$size <- 10 * authority_scores

# Create a two-panel plotting window
par(mfrow=c(1, 2))

# Panel 1: Plotting network with authority scores
plot(uk_caribbean, vertex.size=V(uk_caribbean)$size, vertex.label=NA, main="UK and Caribbean Network with Authority Scores")

# Identify communities using cluster_optimal() and plot
communities <- cluster_optimal(uk_caribbean)
plot(communities, uk_caribbean, main="Communities in UK and Caribbean Network")

# Reset plotting layout
par(mfrow=c(1, 1))


