#R


.TIC.BasePeak <- function(x){
  df <- x %>% 
    filter(grepl("ms ", ScanType)) %>% 
    select(StartTime, TIC, BasePeakIntensity) %>% 
    rename(Base_Peak = BasePeakIntensity)
  
  df <- df %>% gather(key = "Type", value = "Intensity", TIC, Base_Peak)
  
  plot <- ggplot(df,aes(x= StartTime, y = Intensity))+
    geom_line()+
    facet_grid(Type~.)
  
  return(plot)
}