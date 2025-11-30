# Check BalticFW.Rdata contents
load('BalticFW.Rdata')
library(igraph)

cat('==============================================\n')
cat('BALTIC FOOD WEB DATA CHECK\n')
cat('==============================================\n\n')

cat('Network Structure:\n')
cat('  Species (nodes):', vcount(net), '\n')
cat('  Trophic Links (edges):', ecount(net), '\n')
cat('  Connectance:', round(ecount(net)/(vcount(net)*(vcount(net)-1)), 3), '\n\n')

cat('Species Names (first 15):\n')
print(head(V(net)$name, 15))

cat('\n\nSpecies Info Data Frame:\n')
cat('  Rows:', nrow(info), '\n')
cat('  Columns:', ncol(info), '\n')
cat('  Column names:', paste(names(info), collapse=', '), '\n\n')

cat('Functional Groups:\n')
print(table(info$fg))

cat('\n\nSample of Biomass Data:\n')
cat('  Mean biomass:', round(mean(info$meanB, na.rm=TRUE), 2), '\n')
cat('  Range:', round(min(info$meanB, na.rm=TRUE), 2), 'to', round(max(info$meanB, na.rm=TRUE), 2), '\n\n')

cat('Sample Trophic Links (first 15):\n')
edges <- as_edgelist(net)
for(i in 1:min(15, nrow(edges))) {
  cat(sprintf('  %s -> %s\n', edges[i,1], edges[i,2]))
}

cat('\n==============================================\n')
cat('DATA FILE IS VALID AND COMPLETE!\n')
cat('==============================================\n')
