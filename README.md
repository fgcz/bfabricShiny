# bfabric_shiny
 allows the user to automatically generate a certain set of queues for thermo instruments. Queues can be exported as .csv and directly loaded into Xcalibur for running the sequence.


```{bash}
apt-get install r-base 
git clone git@github.com:cpanse/bfabric_shiny.git
R CMD build bfabric_shiny
R CMD INSTALL bfabric_shiny*.tgz
```
