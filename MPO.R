# Robust index of Yang et al. (MPO)

# MPO is a implementation of the Robust index of Yang et al. from:

# HU, Y. et al. A robust cluster validity index for fuzzy c-means clustering. In: International
# Conference on Transportation, Mechanical, and Electrical Engineering. [S.l.: s.n.],
# 2011. p. 448{451.

# This function returns the values of the MPO index (mpo$result), its compactness (mpo$comp) and 
# separation (mpo$sep) of the given fuzzy partition.

MPO = function(U){

  n = nrow(U)
  k = ncol(U)
  
  mpo = list();
  
  oijk = c();
  
  for (c1 in 1:(k - 1)) {
    
    for (c2 in (c1 + 1):k) {
      o = abs(U[, c1] - U[, c2]);

      i = which(round(o, 4) >= round(1/k, 4));
      
      o[i] = 1 - o[i];

      o[-i] = 0;
      
      oijk = c(oijk, o);

    }
  }
  
  am = min(colSums(U^2));

  mpo$comp = ((k+1)/(k-1))^(1/2) * sum(U^2)/am;

  mpo$sep = sum(oijk)/n;

  mpo$result = mpo$comp - mpo$sep;

  return(mpo)
  
}      