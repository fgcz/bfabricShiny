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


# RUN

## flask: JSON - SOAP proxy

- run

```{bash}
bfabric_flask_sample.py 
```

- test 

```{bash}
 curl http://127.0.0.1:5000/extract/2450
```


## shiny queue generator application

```{r}
qgs <- system.file("shiny", "queue_generator", package = "bfabricShiny")
shiny::runApp(qgs, display.mode = "normal")
```