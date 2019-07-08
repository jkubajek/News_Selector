my_lexRankFromSimil <- function (s1, s2, simil, threshold = 0.2, n = 3, returnTies = TRUE, 
          usePageRank = TRUE, damping = 0.85, continuous = FALSE) 
{
  #' This is corrected lexRankFromSimil function from LexRank package
  #' in the package damping is not used
  
    if (!is.logical(returnTies)) 
        stop("returnTies must be logical")
    if (length(returnTies) != 1) 
        stop("returnTies must be length 1")
    if (!is.logical(usePageRank)) 
        stop("usePageRank must be logical")
    if (length(usePageRank) != 1) 
        stop("usePageRank must be length 1")
    if (!is.logical(continuous)) 
        stop("continuous must be logical")
    if (length(continuous) != 1) 
        stop("continuous must be length 1")
    if (!is.numeric(simil)) 
        stop("simil must be numeric")
    if (!is.numeric(n)) 
        stop("n must be numeric")
    if (length(n) != 1) 
        stop("n must be length 1")
    if (length(s1) != length(s2) | length(s1) != length(simil)) 
        stop("s1, s2, & simil must all be the same length")
    if (sum(simil) == 0) 
        stop("all simil values are zero")
    if (sum(simil > threshold) == 0 & !continuous) 
        stop("all simil values are below threshold; try lowering threshold or setting continuous to TRUE if you want to retry lexRanking this input data")
    s1 <- as.character(s1)
    s2 <- as.character(s2)
    if (returnTies) 
        tieMethod <- "min"
    else if (!returnTies) 
        tieMethod <- "first"
    edges <- data.frame(s1 = s1, s2 = s2, weight = simil, stringsAsFactors = FALSE)
    if (!continuous | !usePageRank) {
        if (!is.numeric(threshold)) 
            stop("threshold must be numeric")
        if (length(threshold) != 1) 
            stop("threshold must be length 1")
        edges <- edges[edges$weight > threshold, c("s1", "s2")]
    }
    if (usePageRank) {
        if (!is.numeric(damping)) 
            stop("damping must be numeric")
        if (length(damping) != 1) 
            stop("damping must be length 1")
        sentGraph <- igraph::graph_from_data_frame(edges, directed = FALSE)
        sentRank <- igraph::page_rank(sentGraph, directed = FALSE, damping = damping)$vector
        sentRanksRanked <- rank(1/sentRank, ties.method = tieMethod)
        topCentral <- sentRank[which(sentRanksRanked <= n)]
        centralDf <- data.frame(sentenceId = names(topCentral), 
                                value = topCentral, stringsAsFactors = FALSE)
        rownames(centralDf) <- NULL
    }
    else if (!usePageRank) {
        centralDf = data.frame(sentenceId = c(edges$s1, edges$s2), 
                               stringsAsFactors = FALSE)
        centralDfList = split(centralDf, centralDf$sentenceId)
        centralDfList = lapply(centralDfList, function(dfi) {
            dfi[["degree"]] = nrow(dfi)
            unique(dfi)
        })
        centralDf = do.call("rbind", centralDfList)
        centralDf = centralDf[order(-centralDf$degree), ]
        centralDf[["degRank"]] = rank(1/centralDf$degree, ties.method = tieMethod)
        centralDf = centralDf[centralDf$degRank <= n, c("sentenceId", 
                                                        "degree")]
        names(centralDf) = c("sentenceId", "value")
        class(centralDf) <- "data.frame"
        rownames(centralDf) <- NULL
    }
    return(centralDf)
}