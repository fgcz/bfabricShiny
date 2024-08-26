# Proteomics ========================================
## 2024-07-04 Clauda Fortes / Christian Panse


qconfigProteomicsEVOSEP6x12x8PlateHystar <- function(x, howOftenQC = 48,  ...){
  
  ## as a function of howOftenQC
  howOftenClean <- as.integer(round(0.5 * howOftenQC))
  
  stopifnot(is.integer(howOftenClean))
  message(paste0("howOftenClean:\t", howOftenClean))
  message(paste0("howOftenQC:\t", howOftenQC))
  
  df <- x
  Y <- c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H')
  
  currentdate <- format(Sys.time(), "%Y%m%d")
  output <- data.frame(matrix(ncol = 8, nrow = 0))
  colnames(output) <- c("File Name", "Path", "Position", "Inj Vol", "L3 Laboratory", "Sample ID", "Sample Name", "Instrument Method")
  
  clean <- data.frame(matrix(ncol = 8, nrow = 0))
  cleanAutoQC03 <- data.frame(matrix(ncol = 8, nrow = 0))
  
  colnames(df) <- c("File Name", "Path", "Position", "Inj Vol", "L3 Laboratory", "Sample ID", "Sample Name", "Instrument Method")
  
  colnames(clean) <- c("File Name", "Path", "Position", "Inj Vol", "L3 Laboratory", "Sample ID", "Sample Name", "Instrument Method")
  clean <- c("Clean", df$Path[1], "5:X:X", 1, "FGCZ", "clean", "clean", "clean")
  cleancount <- 1
  cleancountx <- 1
  cleancounty <- 1
  
  colnames(cleanAutoQC03) <- c("File Name", "Path", "Position", "Inj Vol", "L3 Laboratory", "Sample ID", "Sample Name", "Instrument Method")
  autoQC03count <- 1
  autoQC03countx <- 1
  autoQC03county <- 1
  
  ## ADD QC/CLEAN INBETWEEN =================================
  for (i in 1:nrow(df)){
    output <- rbind(output, df[i, ])
    
    
    if (i %% howOftenClean == 0) {
      clean <- c(sprintf("%s_@@@_clean_%02d", currentdate, cleancount), df$Path[1], sprintf("5:%s,%d", Y[cleancounty], cleancountx), 1, "FGCZ", "clean", "clean", "clean")
      cleancountx <- cleancountx + 1
      cleancount <- cleancount + 1
      output <- rbind(output, clean)
    }
  
    if(i %% howOftenQC == 0) {
      #clean <- c(sprintf("%s_@@@_clean_%02d", currentdate, cleancount), df$Path[1], sprintf("5:%s,%d", Y[cleancounty], cleancountx), 1, "FGCZ", "clean", "clean", "clean")
      #cleancountx <- cleancountx + 1
      #cleancount <- cleancount + 1
      #output <- rbind(output, clean)
      
      autoQC03 <- c(sprintf("%s_@@@_autoQC03dia_%02d", currentdate, autoQC03countx), df$Path[1], sprintf("6:%s,%d", Y[autoQC03county], autoQC03countx), 1, "FGCZ", "autoQC03", "autoQC03", "autoQC03")
      autoQC03countx <- autoQC03countx + 1
      autoQC03count <- autoQC03count + 1
      output <- rbind(output, autoQC03)
    } 
    

    if (cleancountx > 12){
      cleancountx <- 1
      cleancounty <- cleancounty + 1
    }
    
    if (autoQC03countx > 12){
      autoQC03countx <- 1
      autoQC03county <- autoQC03county + 1
    }
    
  } # for loop
  
  ## START
  clean <- c(sprintf("%s_@@@_clean_00", currentdate, cleancount), df$Path[1], sprintf("5:%s,%d", Y[cleancounty], cleancountx), 1, "FGCZ", "clean", "clean", "clean")
  cleancountx <- cleancountx + 1
  cleancount <- cleancount + 1
  output <- rbind(clean, output)
  
  ## TODO add autoQC03
  
  output 
}
