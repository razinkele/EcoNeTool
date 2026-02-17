source('R/functions/ecobase_connection.R')
library(igraph)

cat('\n=== Model 608: Input vs Output Parameters ===\n\n')

cat('INPUT Parameters:\n')
r1 <- convert_ecobase_to_econetool(608, use_output=FALSE)
cat('  Final: Species =', vcount(r1$net), ', Links =', ecount(r1$net), '\n\n')

cat('OUTPUT Parameters:\n')
r2 <- convert_ecobase_to_econetool(608, use_output=TRUE)
cat('  Final: Species =', vcount(r2$net), ', Links =', ecount(r2$net), '\n')
