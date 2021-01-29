#R

# Christian Panse <cp@fgcz.ethz.ch> 2021-01-28
# http://fgcz-ms-shiny.uzh.ch:8080/queue_generator10/

.isBlockRandomFeasibible <- function(S, x){
	if (nrow(S) < 1) return (FALSE)

	min(table(S[,names(S)  == x])) == max(table(S[,names(S)  == x]))
}

.blockRandom <- function(S = iris, x = "Species"){
	# sanity check
	if (isFALSE(.isBlockRandomFeasibible(S, x))) return(NULL)

	n <- min(table(S[,names(S)  == x]))

	# split into groups
	rv <- lapply(unique(S[, x]),
		function(l, Y){treatment <- S[S[, x] == l,]; treatment[sample(nrow(treatment)),]}, Y=S)

	# compose blockrandom data.frame
	m <- length(rv)
	do.call('rbind', lapply(1:n, function(i){do.call('rbind', lapply(1:m, function(j){rv[[j]][i,]}))[sample(m),]}))
}

testthat::expect_false(.isBlockRandomFeasibible(iris[c(1:3,51:54,101:104), ], x="Species"))
testthat::expect_true(.isBlockRandomFeasibible(iris[c(1:4,51:54,101:104), ], x="Species"))
testthat::expect_null(.blockRandom(iris[c(1:4,51:54,101:103), ], x="Species"))

testthat::expect_false(.isBlockRandomFeasibible(data.frame()))

#set.seed(1)
#.blockRandom(iris[c(1:4,51:54,101:104), ], x = "Species")
#.blockRandom(iris, x = "Species")
#.blockRandom(droplevels(iris[c(1:50), ]), x = "Species")

