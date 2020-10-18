# Normalized Partition Entropy (NPE)

# NPE is a implementation of the Normalized Partition Entropy index from:

# BEZDEK, J. C. Pattern Recognition with Fuzzy Objective Function Algorithms. Norwell,
# MA, USA: Kluwer Academic Publishers, 1981. 256 p.

NPE = function(U, base_log) {
  
  return(nrow(U) * (-sum(U * log(U, base_log))/nrow(U))/(nrow(U)-ncol(U)))

}