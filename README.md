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



## SOP create your own application

The idea is to fetch a RData file stored in bfabric.

* manage the key thing for housing the bfabric login/webpassword
* RSTUDIO - new shiny app
* in the shiny `ui.R` change
```{r}
   mainPanel(
      tabsetPanel(
        tabPanel("bfabric", bfabricInput("bfabric8")),
        tabPanel("plot", plotOutput("distPlot"))
      )
```
* on the shiny `server.R`
  * ```library(bfabricShiny)```
  * add to shiny server function ```bf <- callModule(bfabric, "bfabric8",  applicationid = c(155))```
  * define the way you are going to ``stage'' the data

```{r}
.ssh_load_RData <- function(host = 'fgcz-r-021.uzh.ch', user = 'cpanse', file = NULL){
  e <- new.env()

  cmd <- paste('cat ',  file)

  ssh_cmd <- paste("ssh ", user, "@", host, " '", cmd, "'", sep="")
  message(ssh_cmd)

  S <- load(pipe(ssh_cmd))

  for (x in S){
    assign(x, get(x), e)
  }
  e
}

.load_RData <- function(file = NULL){
  e <- new.env()

  S <- load(file)

  for (x in S){
    assign(x, get(x), e)
  }
  e
}
```


```{r}
  # returns an env
  getRDataEnv <- eventReactive(input$load, {
    # this is the ``output'' of the bfabric shiny module
    message(input$relativepath)

    filename <- file.path('/srv/www/htdocs/', input$relativepath)

    if (file.exists(filename)){
      .load_RData(file=filename)
    }else{
      .ssh_load_RData(file = filename, host = 'fgcz-r-021.uzh.ch')
    }
  })
```


