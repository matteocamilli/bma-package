require('BDgraph')

# vars: l0, l1, l2, l3, low_connectivity, high_energy, hazard, crash
# l1 -> low_connectivity, high_energy
# l3 -> hazard, crash
structure <- rbind(c(0, 0, 0, 0, 0, 0, 0, 0), 
                   c(0, 0, 0, 0, 1, 1, 1, 0), 
                   c(0, 0, 0, 0, 0, 0, 0, 0), 
                   c(0, 0, 0, 0, 0, 0, 1, 1), 
                   c(0, 1, 0, 0, 0, 0, 0, 0),
                   c(0, 1, 0, 0, 0, 0, 0, 0),
                   c(0, 1, 0, 1, 0, 0, 0, 0),
                   c(0, 0, 0, 1, 0, 0, 0, 0))

data.sim = bdgraph.sim( n = 100, p = 8, graph = structure, vis = T, type = "Gauss")
round( head( data.sim, 4 ), 2 )

bdgraph.obj <- bdgraph( data = data.sim, method = "gcgm", iter = 5000, save = T)

plinks( bdgraph.obj, round = 2, burnin = NULL )

select( bdgraph.obj, cut = NULL, vis = FALSE )
compare( data.sim, bdgraph.obj, main = c( "Target", "BDgraph" ), vis = TRUE )

p_prob <- pgraph( bdgraph.obj, number.g = 10, adj = NULL ) 

bf( p_prob$selected_g[[1]], p_prob$selected_g[[2]], bdgraph.obj, log = TRUE ) 

plotcoda( bdgraph.obj )

summary(bdgraph.obj)

