# Log likelihood function
log_Lik <- function(p, n, k, eps = 1e-16) k * log(p) + (n - k) * log(pmax((1 - p), eps))

# Calculation of Chi^2 statistics which will indicate whether particular word occure significantly more often than
# in reference period
# This formulae is also called Dunning statistics
log_Formulae_Dunning <- function(p, p1, n1, k1, p2, n2, k2, eps = 1e-16){
    multiplier <- ifelse(p1 < p2, -1, 1)
    p1 <- pmax(p1, eps)
    p2 <- pmax(p2, eps)
    res <- 2*(log_Lik(p1, n1, k1) + log_Lik(p2, n2, k2)  - log_Lik(p, n1, k1)  - log_Lik(p, n2, k2)) * multiplier
    
    return(res)
    
} 

calc_general_stats <- function(words, group_var){
    
    group_var <- enquo(group_var)
    
    general_stats_ <- words %>%
        group_by(!! group_var) %>%
        summarise(k_general = n()) %>%
        ungroup() %>%
        mutate(perc_general = k_general / sum(k_general))
    return(general_stats_)
}

