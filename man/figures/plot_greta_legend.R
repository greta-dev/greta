# plot legend
library(DiagrammeR)

ns <- 0.3
nodes1 <- create_node_df(n = 4,
                         label = c("data", "variable", "distribution", "operation"),
                         type = "lower",
                         style = "filled",
                         fontcolor = 'black',
                         fontname = 'Helvetica',
                         fontsize = 12,
                         fillcolor = c('PaleTurquoise', 'orange', 'violet', 'lightgray'),
                         shape = c("square", "circle", "diamond", "circle"),
                         width = c(0.5, 0.8, 1, 0.3),
                         height = c(0.5, 0.8, 0.8, 0.3))

gr1 <- create_graph(nodes1)
gr1$global_attrs[1, 'value'] <- 'dot'
render_graph(gr1)


ns <- 0.01
nodes2 <- create_node_df(n = 4,
                         label = '',
                         type = "lower",
                         alpha = 0,
                         style = "filled",
                         fillcolor = rep('#ffffff00', 4),
                         color = rep('#ffffff00', 4),
                         shape = rep("circle", 4),
                         width = rep(ns, 4))

edges2 <- create_edge_df(from = c(1, 3),
                         to = c(2, 4),
                         label = c('deterministic', 'stochastic'),
                         color = rep('Gainsboro', 2),
                         fontname = 'Helvetica',
                         fontcolor = 'black',
                         fontsize = 12,
                         penwidth = 3,
                         style = c('solid', 'dashed'))

gr2 <- create_graph(nodes2, edges2)
gr2$global_attrs[1, 'value'] <- 'dot'
render_graph(gr2)
