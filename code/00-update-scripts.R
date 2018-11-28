# Update R scripts
for (i in 1:6) {
  lecture <- paste0("lecture-0", i)
  knitr::purl(
    input = paste0("docs/slides/", lecture, ".Rmd"), 
    output = paste0("code/", lecture, ".R"),
    documentation = 1L
  )
}
