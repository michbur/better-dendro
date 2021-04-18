library(ggplot2)

source("./dendrogram_data.R")

#strain_dendrogram_data <- dendrogram_data(dendro)
hc <- hclust(dist(USArrests), "ave")
hcdata <- dendrogram_data(hc)

hc[["merge"]]
hc[["order"]]
hc[["labels"]]
hc[["height"]]

lapply(nrow(hc[["merge"]]):1, function(i) {
  data.frame(x = 1, xend = 2, y = hc[["height"]][i], yend = hc[["height"]][i])
})

ggplot(hcdata[[1]][1L:10,], aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_segment()
