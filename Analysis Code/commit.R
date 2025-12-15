# install.packages("credentials")
# install.packages("gert")

library(credentials)
library(gert)

gert::git_pull() # pull most recent changes from GitHub

gert::git_push() # push your commit to GitHub

gert::git_add(dir(all.files = TRUE)) # select any and all new files created or edited to be 'staged'

gert::git_commit("adding in louise's latest code for prelim results")

