#R


library(magrittr)

# Christian Panse <cp@fgcz.ethz.ch> 2021-01-28
# http://fgcz-ms-shiny.uzh.ch:8080/queue_generator10/
# see also
# https://doi.org/10.1021/pr8010099
# https://doi.org/10.1021/acs.jproteome.0c00536

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



# TODO(cp): save slots at the end for QC
.mapPlatePositionMClass <- function(S, x = as.character(1:8), y = c('A', 'B', 'C', 'D', 'E', 'F'), ...){
	n <-  nrow(S)
	
	if (length(x) * length(y) < nrow(S)){
	    stop("more samples than plate positions!")
	}
	S$run <- 1:n
	S$x <- x[((S$run -1) %% length(x)) + 1]
	S$y <- y[floor((S$run -1) / (length(x) )) + 1]
	S
}

.mapPlatePositionNanoElute <- function(S, x = as.character(1:54), y = c('1'), plate=1, volume=1, ...){
    n <-  nrow(S)
    
    if (length(x) * length(y) < nrow(S)){
        stop("more samples than plate positions!")
    }
    S$run <- 1:n
    S$plate <- plate
    S$x <- x[((S$run -1) %% length(x)) + 1]
    S$y <- y[floor((S$run -1) / (length(x) )) + 1]
    S$volume <- volume
    S
}

# we iterate row by row through the data.frame and insert the autoQC vials
.insertStandards <- function(S, howoften = 4, howmany = 2, begin=FALSE, end=FALSE, stdName = "autoQC01", stdPosX='8', stdPosY='F', plate=1, volume=1){
    input <- S
    
    if (! 'type' %in% names(input))
        input$type <- "sample"
    
    output <- data.frame()
    
    # yes - for readability of the code we have a foor loop!
    for (i in 1:nrow(input)){
         
        if (howoften > 0 && i %% howoften == 0){
            
            for (j in seq(1, howmany)){
                tmp <- rep(NA, ncol(input))
                output <- rbind(output, tmp)
            }
        }
        
        output <- rbind(output, input[i, ])
    }
    if (begin){
        tmp <- rep(NA, ncol(input))
        output <- rbind(tmp, output)
    }
    
    if (end){
        tmp <- rep(NA, ncol(input))
        output <- rbind(output, tmp)
    }
    
    output$type[is.na(output$type )] <- stdName
    output$x[is.na(output$x )] <- stdPosX
    output$y[is.na(output$y )] <- stdPosY
    output$volume[output$type == stdName] <- volume
    output$plate[output$type == stdName] <- plate
    

    output
}

.formatHyStar <- function(S, dataPath="D:\\Data2San\\p3657\\Proteomics\\TIMSTOF_1\\cpanse_20210129\\"){
    
    injection.index <- sprintf("%03d", seq(1, nrow(S)))
    S$"Vial" <- paste0( "Slot", S$plate,":",S$x)
    S$"Sample ID" <- paste0(S$sample_name, "_S", S$sample_id )
    
    if ("sample_condition" %in% names(S))
        S$"Sample Comment" <- S$"sample_condition"
    else
        S$"Sample Comment" <- ""
    
    S$"Volume [µl]" <- S$volume
    S$"Data Path" <- dataPath
    S$"Method Set" <- NA
    
    S$"Sample ID"[S$type != 'sample'] <- S$type[S$type != 'sample']
    S$"Sample ID" <- paste(format(Sys.Date(), format = "%Y%m%d"), injection.index, S$"Sample ID", sep='_')
    
    rv <- S[, c("Vial", "Sample ID", "Sample Comment", "Volume [µl]", "Data Path", "Method Set")]
    rownames(rv) <- 1:nrow(rv)
    rv
}

testthat::expect_false(.isBlockRandomFeasibible(iris[c(1:3,51:54,101:104), ], x="Species"))
testthat::expect_true(.isBlockRandomFeasibible(iris[c(1:4,51:54,101:104), ], x="Species"))
testthat::expect_null(.blockRandom(iris[c(1:4,51:54,101:103), ], x="Species"))

testthat::expect_false(.isBlockRandomFeasibible(data.frame()))

#set.seed(1)
#.blockRandom(iris[c(1:4,51:54,101:104), ], x = "Species")
#.blockRandom(iris, x = "Species")
#.blockRandom(droplevels(iris[c(1:50), ]), x = "Species")


# .blockRandom(iris[c(1:4,51:54,101:104), ], x = "Species") %>% .addLayoutXY
