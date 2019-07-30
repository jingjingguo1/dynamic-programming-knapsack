# function [V_knapsack, obj_count] = knapsackDP(w, v, W_knapsack)
knapsackDP <- function(w,v,W_knapsack){
    N <- length(v)
    Vs <- rep(0,W_knapsack)
    obj_count <- matrix( rep(0,W_knapsack*N), nrow=W_knapsack, ncol=N)

    Vs[1] <- max(v[w<=1])
    r <- which.max(v[w<=1])
    obj_count[1,r] <- obj_count[1,r] + 1
    
    for(i in 2:W_knapsack){
        max_check <- rep(0,N)
        combo_i <- matrix(rep(0,N*N), nrow=N, ncol=N)
        for(j in 1:N){
            if(w[j] < i){
                max_check[j] <- Vs[i -w[j]] + v[j]
                combo_i[j,] <- obj_count[(i-w[j]),]
                combo_i[j,j] <- combo_i[j,j] + 1
            }
        }
        Vs[i] <- max(max_check)
        r <- which.max(max_check)
        max_combo_i <- combo_i[r,]
        obj_count[i,] <- max_combo_i
    }

    list2return <- list("max_value" = Vs[W_knapsack], "items" = obj_count[N,])
    return(list2return)
}


w = c(1,2,3,4,5,6)
v = c(1,8,10,10,19,25) 
W_knapsack = 25
knapsackDP(w,v,W_knapsack)
