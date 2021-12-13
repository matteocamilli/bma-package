
generate <- function(X){
  r = 0.125246*X[1] - 0.802732*X[2] + 0.033585*X[3] - 0.859439*X[4] + 0.802732
  if( r < 0.0 )
    return(0.000000001)
  return(r)
}

X.sample = runif(4, min = 0, max = 1)
m <- matrix(c(1, X.sample, generate(X.sample)), ncol = 6)


for(i in 2:50) {
  X.sample = runif(4, min = 0, max = 1)
  m <- rbind(m, c(i, X.sample, generate(X.sample)))
}
df <- data.frame(Run = m[,1], Light = m[,2], Smoke = m[,3], Quality = m[,4], Speed = m[,5] ,Dist = m[,6])

write.csv(df, 'test_gen_data.csv')
