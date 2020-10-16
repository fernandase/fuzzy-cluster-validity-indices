# Scaled Partition Entropy (PEB)

# PEB is a implementation of the Scaled Partition Entropy index from:

# BEZDEK, J. C. Mathematical models for systematics and taxonomy. In: 8th Int. Conf.
# Numerical Taxonomy San Francisco. [S.l.: s.n.], 1975. p. 143-166.

PEB = function(U, base_log){

  return(PE(U, base_log)/log(ncol(U), base_log))
  
}
