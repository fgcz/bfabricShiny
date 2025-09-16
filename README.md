

[![JIB](https://img.shields.io/badge/JIB-10.1515%2Fjib.2022.0031-brightgreen)](https://doi.org/10.1515/jib-2022-0031)
[![codecov](https://codecov.io/gh/fgcz/bfabricShiny/branch/bfabric11/graph/badge.svg?token=Q9G0WFEH6K)](https://codecov.io/gh/fgcz/bfabricShiny)
![Downloads](https://img.shields.io/github/downloads/fgcz/bfabricShiny/total)


# bfabricShiny R package

## Demonstration

### Enables connecting R and bfabric using REST

![bfabricPy-read-R](https://user-images.githubusercontent.com/4901987/65041207-7c757c80-d956-11e9-90ca-9c3e2e0ca724.gif)

### Command line triggered analytics

The animated gif below illustrates the interaction between an application (performing quality control on mass spectrometric measurements using [rawDiag](https://pubs.acs.org/doi/10.1021/acs.jproteome.8b00173)) and the [B-Fabric](https://fgcz-bfabric.uzh.ch) platform on the command line level using the  R console.

![code_snippet_demo](https://user-images.githubusercontent.com/4901987/181242377-a9bf3988-b193-494c-91c9-a010500ee3f2.gif)

The R code as used in the animated gif.
```{r}
## R --no-save < code_snippet.R
## devtools::install_github("fgcz/bfabricShiny")
## devtools::install_github("fgcz/rawDiag")
stopifnot(R.Version()['major'] >= '4',
    require('rawDiag'),
    require('bfabricShiny'))

## Define B-Fabric input workunit
workunitid <- 165473

## Query metadata from B-Fabric
Q <- bfabricShiny::read(login, webservicepassword,
  endpoint = 'resource',
  query = list('workunitid' = workunitid), as_data_frame=FALSE)

## setting root directory
rawfilenames <- Q$res |>
    sapply(function(x)file.path('/srv/www/htdocs/', x$relativepath))

## Extract MS data from BLOBs using the rawDiag R package
## That requires storage access via SSH, NFS, or SAMBA
RAW <- rawfilenames |>
    parallel::mclapply(rawDiag::read.raw, mc.cores = 12) |>
    base::Reduce(f = rbind)

## Print a summary
RAW |> rawDiag::summary.rawDiag()

## Have fun with visualization https://doi.org/10.1021/acs.jproteome.8b00173
## (a)
RAW |> rawDiag::PlotPrecursorHeatmap(bins = 25)
## (b)
RAW |> rawDiag::PlotPrecursorHeatmap(bins = 25) +
  ggplot2::facet_wrap(~ filename)
## (c)
RAW |> rawDiag::PlotTicBasepeak(method = 'overlay')
## (d)
RAW |> rawDiag::PlotInjectionTime(method = 'overlay')
```


## INSTALL


```{bash}
## Debian 12
apt-get install r-base libcurl4-openssl-dev lcmaps-openssl-interface -y
```

use source code from GitHub

```{r}
if (!requireNamespace("devtools", quietly = TRUE))
    install.packages("devtools")

if (!requireNamespace("PKI", quietly = TRUE))
    install.packages('PKI')

devtools::install_github("cpanse/shinyStore")
devtools::install_github('fgcz/bfabricShiny', build_vignettes = FALSE,
  quiet = FALSE)
```


# Use cases

## Run shiny based queue generator application


```{r}
pkgs <- c('devtools', 'tidyverse', 'shiny', 'affy', 'limma')
pkgs[!pkgs %in% installed.packages()] |> 
  BiocManager::install()

devtools::install_github('protViz/SRMService', build_vignettes = FALSE,
  quiet = FALSE)
```

```
system.file("shiny", "queue_generator10", package = "bfabricShiny") |>
  shiny::runApp(display.mode = "normal")
```

## JSON - SOAP proxy using python 


- run

```{bash}
# Development Server
uv run --with bfabric-scripts -m bfabric_scripts.bfabric_flask --port 5000

# Production Server
uv run --with gunicorn,bfabric-scripts gunicorn -w 4 bfabric_scripts.bfabric_flask:app --bind 0.0.0.0:5000 --ssl-keyfile=/etc/ssl/private/fgcz-host_key.pem --ssl-certfile=/etc/ssl/fgcz-host.pem
```

See [bfabric\_flask.py on GitHub](https://github.com/fgcz/bfabricPy/blob/main/bfabric_scripts/src/bfabric_scripts/bfabric_flask.py) for more details.

- simple tests 

```{bash}
curl http://localhost:5000/config/remote_base_url
```
```{r}
# R
rv <- httr::POST("https://host:5001/read", body = jsonlite::toJSON(list(login = "login", webservicepassword = "webservicepassword", endpoint = "user", query = list("login" = "cpanse"))), encode = "json")
httr::content(rv)
```

## Sample Query

```{r, eval=FALSE
R> (Q <- query(login, webservicepassword, 
   endpoint = 'resource',
   query = list('filechecksum' = "65518d3ccc6b4f3c83c132dae147fc0e")))
$res
$res[[1]]
$res[[1]]$id
[1] 1301179

$res[[1]]$created
[1] "Thu, 11 Jul 2019 07:00:11 GMT"

$res[[1]]$createdby
[1] "pfeeder"

$res[[1]]$description
[1] "```{r}\r\nR> library(bfabricShiny)\r\nR> (Q <- query(login, webservicepassword, endpoint = 'resource', query = list('filechecksum' = \"65518d3ccc6b4f3c83c132dae147fc0e\")))\r\n$res\r\n$res[[1]]\r\n$res[[1]]$id\r\n[1] 1301179\r\n\r\n$res[[1]]$created\r\n[1] \"Thu, 11 Jul 2019 07:00:11 GMT\"\r\n\r\n$res[[1]]$createdby\r\n[1] \"pfeeder\"\r\n\r\n$res[[1]]$filechecksum\r\n[1] \"65518d3ccc6b4f3c83c132dae147fc0e\"\r\n\r\n$res[[1]]$junk\r\n[1] FALSE\r\n\r\n$res[[1]]$modified\r\n[1] \"Thu, 11 Jul 2019 07:00:11 GMT\"\r\n\r\n$res[[1]]$modifiedby\r\n[1] \"pfeeder\"\r\n\r\n$res[[1]]$name\r\n[1] \"20190710_005_autoQC4L.raw\"\r\n\r\n$res[[1]]$project\r\n$res[[1]]$project$id\r\n[1] 3181\r\n\r\n$res[[1]]$relativepath\r\n[1] \"p3181/Proteomics/QEXACTIVEHFX_1/tobiasko_20190710/20190710_005_autoQC4L.raw\"\r\n\r\n$res[[1]]$sample\r\n$res[[1]]$sample$id\r\n[1] 200295\r\n\r\n$res[[1]]$size\r\n[1] 235625610\r\n\r\n$res[[1]]$status\r\n[1] \"available\"\r\n\r\n$res[[1]]$storage\r\n$res[[1]]$storage$id\r\n[1] 2\r\n\r\n$res[[1]]$uris\r\n$res[[1]]$uris[[1]]\r\n[1] \"http://fgcz-proteomics.uzh.ch/dm/p3181/Proteomics/QEXACTIVEHFX_1/tobiasko_20190710/20190710_005_autoQC4L.raw\"\r\n\r\n$res[[1]]$uris[[2]]\r\n[1] \"http://fgcz-proteomics.uzh.ch/p3181/Proteomics/QEXACTIVEHFX_1/tobiasko_20190710/20190710_005_autoQC4L.raw\"\r\n\r\n$res[[1]]$uris[[3]]\r\n[1] \"scp://fgcz-ms.uzh.ch/srv/www/htdocs/p3181/Proteomics/QEXACTIVEHFX_1/tobiasko_20190710/20190710_005_autoQC4L.raw\"\r\n\r\n$res[[1]]$uris[[4]]\r\n[1] \"scp://fgcz-r-021.uzh.ch/export/lv_iduzh01/projects/,/export/lv_iduzh02/projects/,/export/lv_iduzh03/projects/,/export/lv_iduzh04/projects/,/export/lv_iduzh05/PAS/p65/RawData_Archive/,/export/lv_iduzh06/projects/,/export/lv_iduzh07/projects/p3181/Proteomics/QEXACTIVEHFX_1/tobiasko_20190710/20190710_005_autoQC4L.raw\"\r\n\r\n$res[[1]]$url\r\n[1] \"scp://fgcz-ms.uzh.ch/srv/www/htdocs/p3181/Proteomics/QEXACTIVEHFX_1/tobiasko_20190710/20190710_005_autoQC4L.raw\"\r\n\r\n$res[[1]]$workunit\r\n$res[[1]]$workunit$id\r\n[1] 200896\r\n\r\nR> \r\n\r\nR> table(sapply((Q <- query(login, webservicepassword, endpoint = 'resource', query = list('name' = \"%autoQC4L%\")))[[1]], function(x){x$project$id}))\r\n\r\n  65 1000 1147 1352 1654 1687 1875 1876 1951 2059 2069 2135 2175 2192 \r\n  13   74   12    6    3    1    2   16    2    1    2    3    1    1 \r\n2193 2211 2213 2272 2310 2342 2433 2447 2479 2501 2558 2621 2631 2687 \r\n   5    1    1    2    1    2    1    1    1    1    1    4    2    2 \r\n2692 2695 2702 2748 2749 2760 2799 2830 2858 2882 2885 2889 2898 2901 \r\n   1    2    2    2    5    2    2    2    1    1    8    2    3    1 \r\n2915 2916 2928 2933 2946 2954 2960 2961 2993 2995 2997 3000 3024 3025 \r\n   4    5    7    3    1    4    2    1    8    2    1   87    1    5 \r\n3026 3036 3047 3053 3061 3067 3075 3086 3101 3106 3127 3134 3144 3146 \r\n  12    1    3    1    3    1    1    4    2    5    1    6    4    3 \r\n3147 3165 3175 3180 3181 \r\n   1    1    2    2    1 \r\nR> \r\n\r\n```"

$res[[1]]$filechecksum
[1] "65518d3ccc6b4f3c83c132dae147fc0e"

$res[[1]]$junk
[1] FALSE

$res[[1]]$modified
[1] "Tue, 23 Jul 2019 14:27:57 GMT"

$res[[1]]$modifiedby
[1] "cpanse"

$res[[1]]$name
[1] "20190710_005_autoQC4L.raw"

$res[[1]]$project
$res[[1]]$project$id
[1] 3181


$res[[1]]$relativepath
[1] "p3181/Proteomics/QEXACTIVEHFX_1/tobiasko_20190710/20190710_005_autoQC4L.raw"

$res[[1]]$sample
$res[[1]]$sample$id
[1] 200295


$res[[1]]$size
[1] 235625610

$res[[1]]$status
[1] "available"

$res[[1]]$storage
$res[[1]]$storage$id
[1] 2


$res[[1]]$uris
$res[[1]]$uris[[1]]
[1] "http://fgcz-proteomics.uzh.ch/dm/p3181/Proteomics/QEXACTIVEHFX_1/tobiasko_20190710/20190710_005_autoQC4L.raw"

$res[[1]]$uris[[2]]
[1] "http://fgcz-proteomics.uzh.ch/p3181/Proteomics/QEXACTIVEHFX_1/tobiasko_20190710/20190710_005_autoQC4L.raw"

$res[[1]]$uris[[3]]
[1] "scp://fgcz-ms.uzh.ch/srv/www/htdocs/p3181/Proteomics/QEXACTIVEHFX_1/tobiasko_20190710/20190710_005_autoQC4L.raw"

$res[[1]]$uris[[4]]
[1] "scp://fgcz-r-021.uzh.ch/export/lv_iduzh01/projects/,/export/lv_iduzh02/projects/,/export/lv_iduzh03/projects/,/export/lv_iduzh04/projects/,/export/lv_iduzh05/PAS/p65/RawData_Archive/,/export/lv_iduzh06/projects/,/export/lv_iduzh07/projects/p3181/Proteomics/QEXACTIVEHFX_1/tobiasko_20190710/20190710_005_autoQC4L.raw"


$res[[1]]$url
[1] "scp://fgcz-ms.uzh.ch/srv/www/htdocs/p3181/Proteomics/QEXACTIVEHFX_1/tobiasko_20190710/20190710_005_autoQC4L.raw"

$res[[1]]$workunit
$res[[1]]$workunit$id
[1] 200896




R> 

```

```{r, eval=FALSE}
R> table(sapply((Q <- query(login, webservicepassword,
   endpoint = 'resource',
   query = list('name' = "%autoQC4L%")))[[1]], function(x){x$project$id}))

  65 1000 1147 1352 1654 1687 1875 1876 1951 2059 2069 2135 2175 2192 
  13   74   12    6    3    1    2   16    2    1    2    3    1    1 
2193 2211 2213 2272 2310 2342 2433 2447 2479 2501 2558 2621 2631 2687 
   5    1    1    2    1    2    1    1    1    1    1    4    2    2 
2692 2695 2702 2748 2749 2760 2799 2830 2858 2882 2885 2889 2898 2901 
   1    2    2    2    5    2    2    2    1    1    8    2    3    1 
2915 2916 2928 2933 2946 2954 2960 2961 2993 2995 2997 3000 3024 3025 
   4    5    7    3    1    4    2    1    8    2    1   87    1    5 
3026 3036 3047 3053 3061 3067 3075 3086 3101 3106 3127 3134 3144 3146 
  12    1    3    1    3    1    1    4    2    5    1    6    4    3 
3147 3165 3175 3180 3181 
   1    1    2    2    1 
R> 

```

### restart the shiny server

```{r}
service shiny-server stop && service shiny-server start
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
#### How to generate the keys

```
cd bfabricShiny/inst/keys && bash generateKeys.bash

## test
R -q -e "PKI::PKI.load.key(file = 'bfabricShiny.key.pub.pem')"
```

### PTM-MarkerFinder 

```{r}
ptmmf <- system.file("shiny", "PTM_MarkerFinder", package = "bfabricShiny")
shiny::runApp(ptmmf, display.mode = "normal", port=8080)
```



## SOP create your own application

The idea is to fetch a RData file stored in bfabric.

### Setup


* install the python package https://github.com/cpanse/bfabricPy
* install `install_github("https://github.com/cpanse/bfabricShiny")`
* run the SOAP-REST proxy `python3 bfabric_flask.py` 
* manage the key thing for housing the bfabric login/webpassword
* run RStudio - create a new shiny app
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

* run the shiny application; check `input$relativepath`


### Data staging
* on the shiny `server.R` - define the way you are going to ``stage'' the data

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



