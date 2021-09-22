#R

# Christian Panse <cp@fgcz.ethz.ch> 2021-01-28
# http://fgcz-ms-shiny.uzh.ch:8080/queue_generator10/
# see also
# https://doi.org/10.1021/pr8010099
# https://doi.org/10.1021/acs.jproteome.0c00536


.isBlockRandomFeasibible <- function(S, x){
	if (nrow(S) < 1) return (FALSE)
	min(table(S[, names(S) == x])) == max(table(S[, names(S) == x]))
}

blockRandom <- function(S = iris, x = "Species", check = TRUE){
  
	# sanity check - all groups have same number of treatment
	if (check)
	  stopifnot(.isBlockRandomFeasibible(S, x))
  
  if(isFALSE(.isBlockRandomFeasibible(S, x))){
    warning("Unequal cardinality of blocks.")
  }

	n <- max(table(S[, names(S) == x]))

	# split into groups
	rv <- lapply(unique(S[, x]),
		function(l, Y){treatment <- S[S[, x] == l,]; treatment[sample(nrow(treatment)),]}, Y=S)

	# compose block random data.frame
	m <- length(rv)
	base::Reduce(rbind, lapply(1:n,
	    function(i){base::Reduce(rbind, lapply(1:m, 
	        function(j){
	            if (i <= nrow(rv[[j]]))
	                rv[[j]][i, ]
	            else{
	                z <- rv[[j]][1, ]
	                z[1, ] <- NA
	                z[1, ]
	            }
	            }))[sample(m),]}))
}


mapPlatePositionEVOSEP <- function(S,
                                    x = as.character(1:12),
                                    y = c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H'),
                                    plate=1:2, volume=1, ...){
    stopifnot(is.data.frame(S))
    n <-  nrow(S)
    
    platePosition <- length(x) * length(y) 
    
    if (platePosition * length(plate) < nrow(S)){
        stop("More samples than available plate positions!")
    }
    
    S$run <- 1:n
    S$x <- x[((S$run -1) %% length(x)) + 1]
    S$y <- y[(floor((S$run - 1) / length(x)) %% length(y))  + 1]
    S$plate <- plate[floor((S$run - 1 ) / platePosition) + 1]
    
    if ("volume" %in% names(S))
        S$volume[is.na(S$volume)] <- volume
    
    S
}

