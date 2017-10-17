#R


#data shaping

.calc.master.scan <- function(x){
  if("MasterScanNumber" %in% names(x)){
    x <- dplyr::mutate(x, MasterScanNumber = replace(MasterScanNumber, MasterScanNumber == 0, NA))
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
  df$Type <- factor(df$Type, levels = c("TIC", "Base_Peak"))
  plot <- ggplot(df,aes(x= StartTime, y = Intensity))+
    geom_line(size = 0.3)+
    facet_wrap(~Type, scales = "free", nrow = 2, ncol = 1)+
    labs(title = "TIC and Base-Peak Plot")+
    labs(x = " Retention Time [min]", y = "Intensity Counts [arb. unit]")+
    scale_x_continuous(breaks = scales::pretty_breaks(8))+
    scale_y_continuous(breaks = scales::pretty_breaks(8))
  return(plot)
}

.cycle.time <- function(x){
  df <- x %>% 
    filter(MSOrder == "Ms") %>% 
    select(StartTime) %>% 
    mutate(CycleTime = (StartTime - lag(StartTime))*60) %>% 
    select(CycleTime, StartTime) %>% 
    na.omit()
  figure<- ggplot(df, aes(x = StartTime, y = CycleTime)) + 
    geom_point(shape = ".") +
    geom_line(stat = "smooth", method = "gam", formula = y ~ s(x, bs= "cs"), colour = "red3", alpha =0.8, se = FALSE)+
    labs(title = "Cycle Time Plot", subtitle = "Caclulated cycle time vs retention time")+
    labs(x = "Retention Time [min]", y = "Cycle Time [sec]")+
    geom_hline(yintercept = quantile(df$CycleTime, 0.95), colour = "blue")+
    scale_x_continuous(breaks = scales::pretty_breaks(8))+
    scale_y_continuous(breaks = scales::pretty_breaks(8))
  return(figure)
}


.mz.dist <- function(x){
  df <- x %>% 
    filter(MSOrder == "Ms2")
  figure <- ggplot(df, aes(x = StartTime, y = PrecursorMass)) + 
    geom_point(shape = ".") +
    geom_line(stat = "smooth", method = "gam", formula = y ~ s(x, bs= "cs"), colour = "red3", alpha = 0.8, se = FALSE)+
    scale_x_continuous(breaks = scales::pretty_breaks(8))+
    scale_y_continuous(breaks = scales::pretty_breaks(8))+
    labs(title = "Time vs m/Z distribution of all selected precursors")+
    labs(x = "Retention Time", y = "Presursor m/Z value") 
  return(figure)
}

.charge.states <- function(x){
  res <- x %>% 
    filter(MSOrder == "Ms2") %>% 
    count(ChargeState) %>% 
    rename(Counts = n)
  xbreaks <- unique(res$ChargeState)
  figure <- ggplot(res, aes(x = ChargeState, y = Counts)) +
    geom_bar(stat = "identity", fill = "forestgreen")+
    geom_text(aes(label = Counts), vjust=-0.3, size=3.5)+
    scale_x_continuous(breaks = xbreaks)+
    labs(title = "Charge State Plot", subtitle = " ploting the number of occurances of all selected precursor charge states")+
    labs(x = "Charge State", y = "Number of Occurances")
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
      mutate(ElapsedScanTimesec = (lead(x$StartTime)-x$StartTime)*60000) %>% 
      select(StartTime, scanNumber, MSOrder, ElapsedScanTimesec) %>% 
      filter(!is.na(.$ElapsedScanTimesec))
    figure <- ggplot(res, aes(x = StartTime, y = ElapsedScanTimesec))+
      geom_point(shape = ".")+
      facet_grid(MSOrder~.)+
      geom_line(stat = "smooth", method = "gam", formula = y~s(x), colour ="red3", se = FALSE, alpha = 0.8)+
      labs(title = "Scan Time Plot", subtitle = " ploting the elapsed time for each individual scan ")+
      labs(x = "Retentione Time [min]", y = "Scan Time [ms]")+
      scale_x_continuous(breaks = scales::pretty_breaks((n = 8)))+
      scale_y_continuous(breaks = scales::pretty_breaks((n = 8)))
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

  figure <- ggplot(x, aes(x = StartTime, y = IonInjectionTimems))+
    geom_hline(data = maxtimes, aes(yintercept = maxima), colour = "red3", alpha = 0.8, linetype = "longdash")+
    geom_hline(data = maxtimes, aes(yintercept = maxima), colour = "blue")+
    facet_grid(MSOrder~.)+
    geom_point(shape = ".")+
    geom_line(stat = "smooth", method = "gam", formula = y~s(x), colour ="red3", se = FALSE, alpha = 0.8)+
    scale_y_continuous(breaks = scales::pretty_breaks((n = 8)))+
    scale_x_continuous(breaks = scales::pretty_breaks((n = 8)))+
    labs(title = "Injection Time Plot", subtitle = "ploting injection time agains retention time for MS and MSn level")+
    labs(x = "Retentione Time [min]", y = "Injection Time [ms]")
  return(figure)
}

.lm.correction <- function(x){
  if("LMCorrectionppm" %in% names(x)){
    res <- x %>% 
      filter(MSOrder == "Ms")
    figure <- ggplot(res, aes(x = StartTime , y = LMCorrectionppm))+
      geom_hline(yintercept = c(-5,5), colour = "red3", linetype = "longdash")+
      geom_line(size = 0.3)+
      geom_line(stat = "smooth", method= "gam", formula = y ~ s(x, bs ="cs"), colour = "red3", se =FALSE, alpha = 0.8)+
      labs(title = "Lock Mass Correction Plot", subtitle = "ploting lock mass correction value over time")+
      labs(x = "Retention Time [min]", y ="Lock Mass Correction [ppm]")+
      scale_x_continuous(breaks = scales::pretty_breaks(8))+
      scale_y_continuous(breaks = scales::pretty_breaks(8), limits = c(-10,10))
    return(figure)
  } else {
    res <- x %>% 
      filter(MSOrder == "Ms")
    figure <- ggplot(res, aes(x = StartTime, y = `LMmZ-Correctionppm`)) +
      geom_hline(yintercept = c(-5,5), colour = "red3", linetype = "longdash")+
      geom_line(size = 0.3)+
      geom_line(stat = "smooth", method= "gam", formula = y ~ s(x, bs ="cs"), colour = "red3", se =FALSE, alpha = 0.8)+
      labs(title = "Lock Mass Correction Plot", subtitle = "ploting lock mass correction value over time")+
      labs(x = "Retention Time [min]", y ="Lock Mass Correction [ppm]")+
      scale_x_continuous(breaks = scales::pretty_breaks(8))+
      scale_y_continuous(breaks = scales::pretty_breaks(8), limits = c(-10,10))
    return(figure)
  }
}

.ms2.frequency <- function(x){
  res <- x %>% 
    dplyr::filter(!is.na(MasterScanNumber)) %>%
    dplyr::count(MasterScanNumber) %>% 
    dplyr::count(n) %>% 
    dplyr::rename(NumberOfMS2Scans = n, Counts = nn) %>% 
    dplyr::mutate(percentage = signif((100/sum(Counts) * Counts), 2))
  xbreaks <- res$NumberOfMS2Scans
  if(max(res$NumberOfMS2Scans >=25)){
    res <- res %>% 
      bind_cols(parts = unlist(cut(.$NumberOfMS2Scans, breaks = c(0,25,50,75,100))))
    levels(res$parts) <- list("1-25" = levels(res$parts)[1], 
                              "26-50" = levels(res$parts)[2], 
                              "51-75" = levels(res$parts)[3], 
                              "76-100" = levels(res$parts)[4]) 
    res <- res %>% 
      select(NumberOfMS2Scans, Counts, percentage) %>% 
      mutate(parts = as.factor("ALL")) %>% 
      bind_rows(res) %>% 
      mutate(x_min = case_when(parts == "ALL" ~ 1,
                               parts == "1-25" ~ 1,
                               parts == "26-50" ~ 26,
                               parts == "51-75" ~ 51,
                               parts == "76-100" ~ 76)) %>% 
      mutate(x_max = case_when(parts == "ALL" ~ max(res$NumberOfMS2Scans)+5,
                               parts == "1-25" ~ 25,
                               parts == "26-50" ~ 50,
                               parts == "51-75" ~ 75,
                               parts == "76-100" ~ 100))
    res$parts <- factor(res$parts, levels = c("ALL", "1-25", "26-50", "51-75", "76-100"))  
    figure <- ggplot(res, aes(x=NumberOfMS2Scans, y = percentage))+ 
      geom_bar(stat = "identity", fill = "forestgreen") + 
      geom_text(aes(label = Counts), vjust=-0.3, size=3.5)+ 
      facet_wrap(~parts, scales = "free", nrow = 5, ncol = 1)+
      ylim(0, max(res$percentage+5))+ 
      geom_blank(aes(x = x_min)) +
      geom_blank(aes(x = x_max))+
      scale_x_continuous(breaks = xbreaks)+
      labs(title = "Cycle Load Plot", subtitle = "ploting the number of MSn scans associated with each MS1 scan")+
      labs(x = "Number of MSn associated with an MS1 scan", y = "Percentage [%]")
    return(figure)
  } else {
    figure <- ggplot(res, aes(x = NumberOfMS2Scans, y = percentage)) +
      geom_bar(stat = "identity", fill = "forestgreen") +
      geom_text(aes(label = Counts), vjust=-0.3, size=3.5)+
      scale_x_continuous(breaks = xbreaks)+
      labs(title = "Cycle Load Plot", subtitle = "ploting the number of MSn scans associated with each MS1 scan")+
      labs(x = "Number of MSn associated with an MS1 scan", y = "Percentage [%]")
    return(figure)
  }
}

.ms.data.points <- function(x){
  binSize <- 15
  binNumber <- ceiling(nrow(x)/binSize)
  binVector <- rep(1:binNumber, each = binSize)
  res <- x %>% 
    filter(MSOrder == "Ms") %>% 
    select(StartTime) %>% 
    mutate(CycleTime = (lead(StartTime) - StartTime)*60) %>% 
    filter(!is.na(CycleTime)) %>% 
    mutate("10sec" = floor(10/CycleTime)) %>% 
    mutate("20sec" = floor(20/CycleTime)) %>% 
    mutate("30sec" = floor(30/CycleTime)) %>% 
    select(StartTime, "10sec", "20sec", "30sec") %>% 
    mutate(Bins = binVector[1:nrow(.)]) %>% 
    group_by(Bins) %>% 
    summarise_all(funs(mean)) %>% 
    mutate_at(c("10sec","20sec","30sec"), funs(floor)) %>% 
    gather("PeakWidthAtBaseline", "Points", 3:5)
  figure <- ggplot(res, aes(x = StartTime, y = Points, colour = PeakWidthAtBaseline))+
    geom_point(size = 0.3)+
    scale_colour_manual(values = c("red3", "forestgreen", "steelblue3"))+
    scale_y_continuous(breaks = scales::pretty_breaks((n = 20)))+ 
    scale_x_continuous(breaks = scales::pretty_breaks((n = 8)))+
    labs(title ="Point Over Chromatographic Peak", 
         subtitle = "ploting the number of Ms data points over different preselected chromatographic peak widths")+
    labs(x = "Retention Time", y = "Points over Peak")
  return(figure)
}
