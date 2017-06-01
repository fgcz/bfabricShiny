# bfabricShiny

allows the user to automatically generate a certain set of queues for thermo instruments. Queues can be exported as .csv and directly loaded into Xcalibur for running the sequence.


# INSTALL

```{bash}
apt-get install r-base libcurl4-openssl-dev 
```

use source code from github

```{r}
#  install.packages('devtools')
library(devtools)
install_git('https://github.com/cpanse/bfabricShiny', build_vignettes = TRUE, quiet = FALSE)
```


# Demonstration
## JSON - SOAP proxy using python flask

- run

```{bash}
bfabric_flask_sample.py 
```

- simple json test 

```{bash}
curl http://127.0.0.1:5000/extract/2450
```

## Shiny

### queue generator application

```{r}
qgs <- system.file("shiny", "queue_generator", package = "bfabricShiny")
shiny::runApp(qgs, display.mode = "normal")
```


### bfabric authentification
```{r}
bfabricauth <- system.file("shiny", "simple_auth", package = "bfabricShiny")
shiny::runApp(bfabricauth, display.mode = "normal", port=8080)
```
#### On howto generate keys?

```{sh}
cd bfabricShiny/inst/keys &&  ssh-keygen -f $PWD/bfabricShiny.key -t rsa
```

### PTM-MarkerFinder 

```{r}
ptmmf <- system.file("shiny", "PTM_MarkerFinder", package = "bfabricShiny")
shiny::runApp(ptmmf, display.mode = "normal", port=8080)
```





