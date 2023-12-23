
purrr::accumulate(rnorm(10),`+`)
install.packages("usethis")
library(usethis)
?use_github
token = "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"

use_github(protocol = "https", auth_token = Sys.getenv("GITHUB_PAT"))
 