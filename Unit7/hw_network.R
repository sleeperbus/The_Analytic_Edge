edges = read.csv("edges.csv")
users = read.csv("users.csv")
str(edges)
str(users)

table(users$locale)
table(users$gender, users$locale, useNA="no")

library(igraph)
g = graph.data.frame(edges, directed = F, users)
plot(g, vertex.size=5, vertex.label=NA)
table(degree(g))
V(g)$size = degree(g)/2 + 2
V(g)$color = "light blue"
V(g)[degree(g) >= 10]$color = "pink"
plot(g, vertex.size=V(g)$size, vertex.label=NA, vertex.color=V(g)$color)

V(g)$color = "black"
V(g)$color[V(g)$gender == "A"] = "red"
V(g)$color[V(g)$gender == "B"] = "grey"
plot(g, vertex.size=V(g)$size, vertex.label=NA, vertex.color=V(g)$color)

V(g)$color = "grey"
V(g)$color[V(g)$school == "A"] = "red"
V(g)$color[V(g)$school == "AB"] = "blue"
plot(g, vertex.size=V(g)$size, vertex.label=NA, vertex.color=V(g)$color)


V(g)$color = "grey"
V(g)$color[V(g)$locale== "A"] = "red"
V(g)$color[V(g)$locale== "B"] = "blue"
plot(g, vertex.size=V(g)$size, vertex.label=NA, vertex.color=V(g)$color)

