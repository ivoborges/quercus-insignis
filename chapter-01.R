library(DiagrammeR)

graph <- 
  create_graph() %>%
  add_node() %>%
  add_node() %>%
  add_edge(from = 1, to = 2)

render_graph(graph, layout = "nicely")

# Graphviz

nodes_1 <-
  create_node_df(
    nodes = c(1, 2, 3, 4),
    label = FALSE,
    type = "lower",
    style = "filled",
    color = "aqua",
    shape = c("circle", "circle",
              "rectangle", "rectangle"),
    data = c(3.5, 2.6, 9.4, 2.7))

graph2 <- create_graph(nodes_df = nodes)
