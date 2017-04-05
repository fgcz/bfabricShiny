# bfabricShiny

allows the user to automatically generate a certain set of queues for thermo instruments. Queues can be exported as .csv and directly loaded into Xcalibur for running the sequence.


# INSTALL

```{bash}
apt-get install r-base libcurl4-openssl-dev 
git clone git@github.com:cpanse/bfabric_shiny.git
R CMD build bfabric_shiny
sudo R CMD INSTALL bfabric_shiny*.tgz
```

```{bash}
R -e "install.packages(c('shiny', 'jsonlite', 'httr','testthat'))"
```


# RUN

- run soap rest proxy

```{bash}
bfabric_flask_sample.py 
```

- run shiny application

```{r}
qgs <- system.file("shiny", "queue_generator", package = "bfabricShiny")
shiny::runApp(qgs, display.mode = "normal")

# 
# shiny::runApp(qgs, display.mode = "normal", host='myhost', port='7934')
```
