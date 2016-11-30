#pseudo code

```{r}
#R


#test data
  test_data <- function(){
    extract.name <- c("Sample_1", "Sample_2", "Sample_3", "Sample_4", "Sample_5", "Sample_6", "Sample_7", "Sample_8", "Sample_9", "Sample_10", "Sample_11", "Sample_12", "Sample_13", "Sample_14", "Sample_15", "Sample_16", "Sample_17", "Sample_18", "Sample_19", "Sample_20" )
    extract.id <- c(1:20)
    Condition <- c("Control", "Control", "Control", "Control", "Ampicillin", "Ampicillin", "Ampicillin", "Ampicillin", "Kanamycin", "Kanamycin", "Kanamycin", "Kanamycin", "Less", "Less", "Less", "More", "More", "More", "More", "More")
    data.frame(extract.name, extract.id, Condition)
  }


push_back <- function(T, r){rbind(T,r)}

compose_queue <- function(x, 
	nfetuin=0, mfetuin=0, 
	nclean=0, mclean=0, 
	multiple=1, 
	hplc_type='default'){


	S <- x[rep(nrow(x), multiple), ]

	
	# iterate over the sample list
	for(i in nrow(S){
		res <- push_back(res,SS[i,])
	}

}
```

# use cases

## use case 1 - default 

## use case 2 - randomized queue

## use case 3 - block randomized queue

## use case 4 - multiple injection

## use case 5 method testing

multiple injections single sample randomized

