library(github)

# Get API keys from https://github.com/settings/applications/
api <- read.csv("api.txt", header = F, row.names = 1)
client.id <- as.character(api["id",1])
client.secret <- as.character(api["secret",1])

# Login
ctx <- interactive.login(client.id, client.secret)

# Get my repo info
repos <- get.my.repositories(ctx, type="owned", sort="pushed")

# Name of first repo
repos$content[[1]]$name

# Other stuff
#me <- get.myself(ctx)
#me$public_repos
#star.repository(ctx, "antagomir", "scripts")
#unstar.repository(ctx, "antagomir", "scripts")
