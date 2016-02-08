# https://christophergandrud.github.io/networkD3/

# ------------------------

# Simple network

# Load package
library(networkD3)

# Create fake data
src <- c("A", "A", "A", "A",
        "B", "B", "C", "C", "D")
target <- c("B", "C", "D", "J",
            "E", "F", "G", "H", "I")
networkData <- data.frame(src, target)

# Plot
simpleNetwork(networkData)


# ------------------------------------

# Forced network

# Load data
data(MisLinks)
data(MisNodes)

# Plot
forceNetwork(Links = MisLinks, Nodes = MisNodes,
            Source = "source", Target = "target",
            Value = "value", NodeID = "name",
            Group = "group", opacity = 0.8)

# -----------------------------------------

# Sankey network

# Load energy projection data
# Load energy projection data
URL <- paste0(
        "https://cdn.rawgit.com/christophergandrud/networkD3/",
        "master/JSONdata/energy.json")
Energy <- jsonlite::fromJSON(URL)
# Plot
sankeyNetwork(Links = Energy$links, Nodes = Energy$nodes, Source = "source",
             Target = "target", Value = "value", NodeID = "name",
             units = "TWh", fontSize = 12, nodeWidth = 30)

# ------------------------------------------

# Radial network

URL <- paste0(
        "https://cdn.rawgit.com/christophergandrud/networkD3/",
        "master/JSONdata//flare.json")

## Convert to list format
Flare <- jsonlite::fromJSON(URL, simplifyDataFrame = FALSE)

# Use subset of data for more readable diagram
Flare$children = Flare$children[1:3]

radialNetwork(List = Flare, fontSize = 10, opacity = 0.9)

# -------------------------------------------

# Diagonal network

diagonalNetwork(List = Flare, fontSize = 10, opacity = 0.9)

# -------------------------------------------

# Saving standalone HTML

library(magrittr)

simpleNetwork(networkData) %>%
saveNetwork(file = 'Net1.html')

# ---------------------------------------------