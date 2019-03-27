#
# FlowSOM_utils.R
#

plot_marker_tree = function(fsom, marker = "CD4", node.scale = .25) {
  mst = fsom$MST
  size = mst$size
  l = mst$l
  g = mst$graph
  # nodes
  plot(l, cex = mst$size * node.scale, xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
  title(main = marker)

  # edges
  ends = data.frame(ends(g, E(g)))
  for (i in 1:nrow(ends)) {
    p0 = as.numeric(as.character(ends[i, 1]))
    p1 = as.numeric(as.character(ends[i, 2]))
    segments(l[p0, 1], l[p0, 2], l[p1, 1], l[p1, 2], col = 'gray')
  }

  # add colors to the nodes
  vals = fsom$map$codes[, marker]
  cols = pcolor(vals, min_value = min(vals), max_value = max(vals))
  points(l, pch = 20, cex = mst$size * node.scale * 1.1, col = cols)
}














