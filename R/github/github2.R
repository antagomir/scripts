# Following http://ropensci.org/blog/2013/03/08/ropensci-collaboration/
# install_github('sandbox', 'ropensci')

library(sandbox)
library(httr)
library(ggplot2)
library(scales)
library(reshape2)
library(bipartite)
library(doMC)
library(plyr)
library(ggthemes)
library(picante)

## Get API keys from https://github.com/settings/applications/
#api <- read.csv("api.txt", header = F, row.names = 1)
#client.id <- as.character(api["id",1])
#client.secret <- as.character(api["secret",1])

# And authenticate - pops open a page in your default browser, then tells
# you authentication was successful
github_auth(client.id, client.secret)

# Login
#library(github)
#ctx <- interactive.login(client.id, client.secret)

# Get all repos for organization
ropensci_repos <- github_allrepos(userorg = "ropensci")


#Get commits broken down in to additions and deletions, though below we
#just collapse them to all commits

registerDoMC(cores = 4)
github_commits_safe <- plyr::failwith(NULL, github_commits)
out <- llply(ropensci_repos, function(x) github_commits_safe("ropensci", x,
    since = "2009-01-01T", limit = 500), .parallel = TRUE)
names(out) <- ropensci_repos
out2 <- compact(out)
outdf <- ldply(out2)


# Plot commits by date and repo

outdf_subset <- outdf[!outdf$.id %in% c("citeulike", "challenge", "docs", "ropensci-book",
    "usecases", "textmine", "usgs", "ropenscitoolkit", "neotoma", "rEWDB", "rgauges",
    "rodash", "ropensci.github.com", "ROAuth"), ]
outdf_subset$.id <- tolower(outdf_subset$.id)
outdf_subset <- ddply(outdf_subset, .(.id, date), summarise, value = sum(value))

mindates <- llply(unique(outdf_subset$.id), function(x) min(outdf_subset[outdf_subset$.id ==
    x, "date"]))
names(mindates) <- unique(outdf_subset$.id)
mindates <- sort(do.call(c, mindates))
outdf_subset$.id <- factor(outdf_subset$.id, levels = names(mindates))


ggplot(outdf_subset, aes(date, value, fill = .id)) +
    geom_bar(stat = "identity", width = 0.5) +
    geom_rangeframe(sides = "b", colour = "grey") +
    theme_bw(base_size = 9) +
    scale_x_date(labels = date_format("%Y"), breaks = date_breaks("year")) +
    scale_y_log10() +
    facet_grid(.id ~ .) +
    labs(x = "", y = "") +
    theme(axis.text.y = element_blank(),
        axis.text.x = element_text(colour = "black"),
        axis.ticks.y = element_blank(),
        strip.text.y = element_text(angle = 0, size = 8, ),
        strip.background = element_rect(size = 0),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 8),
        legend.position = "none",
        panel.border = element_blank())




# In addition, there are quite a few people that have committed code
# now to rOpenSci repos, calling for a network vizualization of course.

outdf_network <- droplevels(outdf[!outdf$.id %in% c("citeulike", "challenge",
    "docs", "ropensci-book", "usecases", "textmine", "usgs", "ropenscitoolkit",
    "retriever", "rodash", "ropensci.github.com", "ROAuth", "rgauges", "sandbox",
    "rfna", "rmetadata", "rhindawi", "rpmc", "rpensoft", "ritis"), ])
casted <- dcast(outdf_network, .id + date + name ~ variable, fun.aggregate = length,
    value.var = "value")
names(casted)[1] <- "repo"
casted2 <- ddply(casted, .(repo, name), summarise, commits = sum(additions))
casted2 <- data.frame(repo = casted2$repo, weight = casted2$commits, name = casted2$name)
mat <- sample2matrix(casted2)
plotweb(sortweb(mat, sort.order = "dec"), method = "normal", text.rot = 90,
    adj.high = c(-0.3, 0), adj.low = c(1, -0.3), y.width.low = 0.05, y.width.high = 0.05,
    ybig = 0.09, labsize = 0.7)
