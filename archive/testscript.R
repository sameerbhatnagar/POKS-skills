
numskills <- ncol(frac.data$q.matrix)
numitems <- nrow(frac.data$q.matrix)
nodes <- data.frame(name=c(colnames(frac.data$q.matrix),rownames(frac.data$q.matrix)),
                     type=c(rep("skill",numskills),rep("item",numitems)))

rowTotals <- rowSums(frac.data$q.matrix1)
colTotals <- colSums(frac.data$q.matrix1)
from <- apply(frac.data$q.matrix1, 1, rep(rownames(frac.data$q.matrix1),rowTotals))

relations <- data.frame(from=c("Bob", "Cecil", "Cecil", "David",
                               "David", "Esmeralda"),
                        to=c("Alice", "Bob", "Alice", "Alice", "Bob", "Alice"),
g <- graph_from_data_frame(relations, directed=TRUE, vertices=actors)
print(g, e=TRUE, v=TRUE)

