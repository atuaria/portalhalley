library(knitr)
## knitr options
opts_chunk$set(
    cache = FALSE,
    tidy = TRUE,
    comment = NA,
    ## collapse = TRUE, ## colapsa chunks em R Markdown
    ## fig.width = 10,
    ## fig.height = 8,
    fig.align = "left",
    ## dpi = 80, ## aumentar o dpi para exibir maior.
    ## dev = "png",
    fig.path = "figures/",
    cache.path = "cache/"
    )

## Set a common seed for all files
set.seed(100)
