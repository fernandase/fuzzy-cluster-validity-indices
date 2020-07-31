# FUZZY SILHOUETTE

# FUZZY.SIL is a implementation of the Fuzzy Silhouette of CAMPELLO, R.; HRUSCHKA, E. A fuzzy extension of the silhouette width criterion
# for cluster analysis. Fuzzy Sets and Systems, v. 157, n. 21, p. 2858

# This function returns the silhouette of each object (obj.sil) and the fuzzy silhouette of the given fuzzy partition

FUZZY.SIL = function (X, U, distance = "euclidean", alpha = 1) 
{

    X = as.matrix(X)
    U = as.matrix(U)
    n = nrow(U)
    k = ncol(U)

    D = as.matrix(dist(X, method = distance))^2 #distance between objects

    clusters.obj = apply(U, 1, which.max) # cth cluster where each object has its maximum membership degree (each object "belongs")

    dif.degrees = rep(0, n)
    a = dif.degrees
    b = a
    obj.sil = a

    for (i in 1:n) {

        dif.degrees[i] = (max(U[i, ]) - max(U[i, ][-which.max(U[i, ])]))^alpha

        B = rep(0, k)
        
        i2 = which(clusters.obj != clusters.obj[i]) # objects that do not have their maximum membership degree in the same cluster of i 
        # i.e, objects that do not belong in the same cluster of i

        c2 = unique(clusters.obj[i2]) # all remaining clusters that object i does not belong

        for (c in c2) {
            i3 = which(clusters.obj == c) # objects in the cth cluster
            B[c] = mean(D[i, i3]) # average distance between object i and all other objects in the remaining clusters (inter-cluster distance).
        }   

        b[i] = min(B[-clusters.obj[i]])
        m = which(clusters.obj == clusters.obj[i]) # # objects in the same cluster of i
        a[i] = mean(D[i, m][-which(m == i)]) # average distance between object i and all other objects in different clusters (intra-cluster distance)
        
        if (length(which(clusters.obj[i] == clusters.obj)) > 1){
            obj.sil[i] = (b[i] - a[i])/max(a[i], b[i])
        }
       
    }

    fuzzy.sil = sum(dif.degrees * obj.sil)/sum(dif.degrees)

    out = list()

    out$obj.sil = obj.sil

    out$fuzzy.sil = fuzzy.sil

    return(out)
}