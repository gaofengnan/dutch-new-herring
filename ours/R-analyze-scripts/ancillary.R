score_pm_rearrange <- function(score, eps_original = 0.03 , eps_new = 0.25) {
    int.seq <- 1:20/2
    N <- length(score)
    for (i in 1:N) {
        if (min(abs(score[i] - int.seq))  > (eps_original* (1 - 0.00001))  
            & min(abs(score[i] - int.seq)) < (eps_original*(1+ 0.00001)) ) {
            closest <- int.seq[which.min(abs(score[i] - int.seq))]
            # print(closest)
            score[i] <- closest + sign(score[i] - closest) * eps_new
        }
    }
    return(score)
}

