#R


#data shaping

.calc.master.scan <- function(x){
  if("MasterScanNumber" %in% names(x)){
    x <- dplyr::mutate(df1, MasterScanNumber = replace(MasterScanNumber, MasterScanNumber == 0, NA))
    return(x)
  } else {
    set1 <- x %>% 
      dplyr::filter(MSOrder == "Ms") %>% 
      dplyr::select(MasterScanNumber = scanNumber, CycleNumber)
    set2 <- dplyr::select(x, scanNumber, CycleNumber)
    res <- dplyr::left_join(set2, set1, by = "CycleNumber") %>% 
      dplyr::mutate(type = x$ScanType) %>% 
      dplyr::mutate(MasterScanNumber = replace(MasterScanNumber, scanNumber == MasterScanNumber, NA)) %>% 
      dplyr::select(MasterScanNumber) %>% 
      dplyr::bind_cols(x, .)
    return(res)
  }
}

#plot functions
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
    geom_point(shape = ".")+
    geom_smooth()
  return(plot)
}


.mz.dist <- function(x){
  df <- x %>% 
    filter(MSOrder == "Ms2")
  plot <- ggplot(df, aes(x = StartTime, y = PrecursorMass))+ 
    geom_point(shape = ".")+
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
      geom_point(shape = ".")+
      facet_grid(MSOrder~.)+
      geom_smooth(colour = "red")
    return(figure)
  } else {
    res <- x %>% 
      mutate(ElapsedScanTimesec = (lead(x$StartTime)-x$StartTime)*60) %>% 
      select(scanNumber, MSOrder, ElapsedScanTimesec) %>% 
      filter(!is.na(.$ElapsedScanTimesec))
    figure <- ggplot(res, aes(x = scanNumber, y = ElapsedScanTimesec))+
      geom_point(shape = ".")+
      facet_grid(MSOrder~.)+
      geom_smooth(colour = "red")
    return(figure)
  }
}

.injection.times <- function(x){
  if("Max.IonTimems" %in% names(x)){
    maxtimes <- x %>% 
      group_by(MSOrder) %>% 
      summarise(maxima = max(Max.IonTimems))
  } else {
    maxtimes <- x %>% 
      group_by(MSOrder) %>% 
      summarise(maxima = max(IonInjectionTimems))
  }
  figure <- ggplot(x, aes(x = scanNumber, y = IonInjectionTimems))+
    geom_point(shape = ".")+
    geom_hline(data = maxtimes, aes(yintercept = maxima), colour = "blue")+
    facet_grid(MSOrder~.)+
    geom_smooth(colour ="red")
  return(figure)
}

.lm.correction <- function(x){
  if("LMCorrectionppm" %in% names(x)){
    res <- x %>% 
      filter(MSOrder == "Ms")
    figure <- ggplot(res, aes(x = scanNumber , y = LMCorrectionppm))+
      ylim(-10,10)+
      geom_hline(yintercept = c(-5,5), colour = "red")+
      geom_line()
    return(figure)
  } else {
    res <- x %>% 
      filter(MSOrder == "Ms")
    figure <- ggplot(res, aes(x = scanNumber, y = `LMmZ-Correctionppm`)) +
      ylim(-10,10)+
      geom_hline(yintercept = c(-5,5), colour = "red")+
      geom_line()
    return(figure)
  }
}

.ms2.frequency <- function(x){
  res <- x %>% 
    dplyr::filter(!is.na(MasterScanNumber)) %>%
    dplyr::count(MasterScanNumber) %>% 
    dplyr::count(n) %>% 
    dplyr::rename(NumberOfMS2Scans = n, Counts = nn)
  xbreaks <- res$NumberOfMS2Scans
  figure <- ggplot(res, aes(x = NumberOfMS2Scans, y = Counts)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = Counts), vjust=-0.3, size=3.5)+
    scale_x_continuous(breaks = xbreaks)
  return(figure)
}