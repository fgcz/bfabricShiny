#R


.TIC.BasePeak <- function(x){
  df <- x %>% 
    filter(grepl("ms ", ScanType)) %>% 
    select(StartTime, TIC, BasePeakIntensity) %>% 
    rename(Base_Peak = BasePeakIntensity) %>% 
    gather(key = "Type", value = "Intensity", TIC, Base_Peak)
  plot <- ggplot(df,aes(x= StartTime, y = Intensity))+
    geom_line()+
    facet_grid(Type~.)
  return(plot)
}

.cycle.time <- function(x){
  df <- x %>% 
    filter(MSOrder == "Ms") %>% 
    select(StartTime) %>% 
    mutate(CycleTime = (StartTime - lag(StartTime))*60) %>% 
    select(CycleTime, StartTime) %>% 
    na.omit()
  plot <- ggplot(df, aes(x = StartTime, y = CycleTime))+ 
    geom_point(size = 0.5)+
    geom_smooth()
  return(plot)
}


.mz.dist <- function(x){
  df <- x %>% 
    filter(MSOrder == "Ms2")
  plot <- ggplot(df, aes(x = StartTime, y = PrecursorMass))+ 
    geom_point(size = 0.8)+
    geom_smooth(colour = "red")
  return(plot)
}

.charge.states <- function(x){
  res <- x %>% 
    filter(MSOrder == "Ms2") %>% 
    count(ChargeState) %>% 
    rename(Counts = n)
  xbreaks <- unique(res$ChargeState)
  figure <- ggplot(res, aes(x = ChargeState, y = Counts)) +
    geom_bar(stat = "identity")+
    geom_text(aes(label = Counts), vjust=-0.3, size=3.5)+
    scale_x_continuous(breaks = xbreaks)
  return(figure)
}

.scan.times <- function(x){
  if("ElapsedScanTimesec" %in% names(x)){
    figure <- ggplot(x, aes(x = scanNumber, y = ElapsedScanTimesec))+
      geom_point(size = 0.8)+
      facet_grid(MSOrder~.)+
      geom_smooth(colour = "red")
    return(figure)
  } else {
    res <- x %>% 
      mutate(ElapsedScanTimesec = (lead(x$StartTime)-x$StartTime)*60) %>% 
      select(scanNumber, MSOrder, ElapsedScanTimesec) %>% 
      filter(!is.na(.$ElapsedScanTimesec))
    figure <- ggplot(res, aes(x = scanNumber, y = ElapsedScanTimesec))+
      geom_point(size = 0.8)+
      facet_grid(MSOrder~.)+
      geom_smooth(colour = "red")
    return(figure)
  }
}

.injection.times <- function(x){
  if("Max.IonTimems" %in% names(x)){
    maxtimes <- x %>% 
      group_by(MSOrder) %>% 
      summarise(max(Max.IonTimems)) %>% 
      rename(max = 'max(Max.IonTimems)')
  } else {
    maxtimes <- x %>% 
      group_by(MSOrder) %>% 
      summarise(max(IonInjectionTimems)) %>% 
      rename(max = 'max(IonInjectionTimems)')
  }
  figure <- ggplot(x, aes(x = scanNumber, y = IonInjectionTimems))+
    geom_point(size = 0.8, alpha = 0.5)+
    geom_hline(data = maxtimes, aes(yintercept = max), colour = "blue")+
    facet_grid(MSOrder~.)+
    geom_smooth(colour = "red")
  return(figure)
}

