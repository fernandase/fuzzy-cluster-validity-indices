# Partition Entropy (PE)

# PE is a implementation of the Partition Entropy index from:

# BEZDEK, J. C. Mathematical models for systematics and taxonomy. In: 8th Int. Conf.
# Numerical Taxonomy San Francisco. [S.l.: s.n.], 1975. p. 143-166.

PE = function(U, base_log){
  -sum(U * log(U, base_log))/nrow(U)
}
