#### Presentacion ####
# Universidad Nacional Autonoma de Mexico 
# Autor: Felix Ernesto Charry Pastrana
# Profesor tutor: Joan Albert Sanchez Cabeza 
# Maestria en Ciencias Fisicas 
# Segundo semestre 2018
#
# Codigo realizado para el Laboratorio de Geoquimia Isotopica y Geocronologia
# UNAM, Unidad Academica Mazatlan, Sinaloa, México.
#
# Finalidad del codigo: 
#   Mediante los datos de CN y XRF, definir la composicion de cada seccion de un nucleo sedimentario. 
#   Ejecutar ANGLE y realizar la correccion a la actividad debido a la composicion para algunos readionuclidos.
#
# Este codigo lee los archivos originales de: 
#     1) El analizador elemental carbono - nitrogeno (.csv)
#     2) Los archivos RPT generados de la espectrometria Gamma
#     3) Los elementos medidos mediante los rayos X de flourescencia, XRF (.txt)
# Todos los anteriores archivos se encuentran en la misma carpeta. 
# 
# Esto genera un archivo <limpio> de los archivos originales llamados:
#     1) 0_CN_Clean.csv     2) 0_Gamma_Clean.csv      3) 0_XRF_Clean.csv
# Los anteriores archivos (archivos originales limpios) se resumen en 0_AllData_O%_H%.csv
#
# Los resultados despues de ejecutar ANGLE se guardan en el archivo 1_AllData_O%_H%.csv
#
# Archivos de entrada:    C-N (.csv), XRF (.txt) y Gamma (.RPT)
# Archivos de salida:     0_( CN, XRF, Gamma)_Clean.csv
#                         0_AllData_o%_H%.csv
#                         1_AllData_o%_H%.csv
#### Datos introducidos por el usuario ####

rm(list = ls()); library(stringr); library(readr)

# Seleccionar o introducir el directorio de trabajo en donde se encuentra la carpeta  
# con los archivos originales de CN, Gamma y XRF. 
# iComputador=1: Windows UNAM FECHP, 
# iComputador=2: MAC personal FECHP, 
# iComputador=3: Windows JASC, 
# iComputador=0: otro. 
iComputador = 1
if(iComputador==1){
  directoryBase = "/Users/Ernesto Charry/Google Drive/4_Thesis_Master/Data/datos_tesis_unam_2018"
  directoryBase = paste0(directoryBase,"/0_Modelo_JASC_FEChP/0_Datos_Iniciales/")  
}else if (iComputador==2){
  directoryBase = "/Users/F.E.CharryPastrana/Google Drive/4_Thesis_Master/Data/datos_tesis_unam_2018"
  directoryBase = paste0(directoryBase,"/0_Modelo_JASC_FEChP/0_Datos_Iniciales/")  
}else if(iComputador==3){
  directoryBase <- "C:/Users/jasc2/Dropbox/Tesis Ernesto/0_Datos_Iniciales/"
} else{ directoryBase <- "Ubicacion" }

# Introducir Nombre de Carpeta
directoryBase1 = "GOMRI_500"

directoryBase  = paste0(directoryBase,directoryBase1,"/");
setwd(directoryBase)

# Seleccionar que datos iniciales se tienen. 
# 1 denora que SI se encuentran en carpeta, 0 que NO se tienen.
iCN       = 1
iGamma    = 1
iXRF      = 1

# Archivos iniciales de CN y XRF con extension (.csv, .txt)
FileNameInicialCN     = "20180727_Total GOMRI 500_Organico CRM PC1.csv"
FileNameInicialXRF    = "GOMRI-500.txt"

# Introducir algunos datos de Carbono - Nitrogeno 
# Ver los datos:
if(iCN == 1){
  fileNameInicial = paste0(directoryBase,FileNameInicialCN)
  CNOriginal <- read.table(fileNameInicial, header = TRUE, sep = ",",skip = 1)
  #View(CNOriginal)
}
# Revisar los siguientes valores: 
if(iCN == 1){
  SecuenciaLetras = "T G" # Secuencia de letras iniciales para identificar los secciones a analizar de CN
  mEliminar = 1           # Introducir mEliminar = 0 para eliminar alguna seccion manualmente despues.
}

# Introducir algunos datos de XRF 
# Ver los datos:
if(iXRF==1){
  fileNameTXT = paste0(directoryBase,FileNameInicialXRF)
  XRFOriginal = read.table(fileNameTXT, header = FALSE, sep = "\t", quote = "", dec = ".",
                        blank.lines.skip = TRUE,  fill = TRUE, skip = 4, comment.char = "", na.strings = "")
  #View(XRFOriginal)
}
# Revisar los siguientes valores: 
if(iXRF==1){
  nDel = 1                      # No. de columnas que se quiere eliminar de XRFOriginal antes del elemento Na
  LocColNames = 1               # No. de columna en donde se encuentra los nombres
  SecuenciasLetrasXRF = "GOM"   # Introducir la secuencia de las letras iniciales para seleccionar los nucleos
  SecuenciasLetrasXRF2 = " EU"  # Posibilidad dos de secuencia iniciales de MISMA longitud. El espacio cuenta.
}

# Definir 100 por 100 la composicion de cada seccion
# Introducir el porcentaje de oxigeno asignado a la composicion faltante. 
porcentajeOxy = 75; 

# Presicion ANGLE: desde 10 hasta 50 
AnglePresicion = 20

#### CN: Inicio Seccion - Parte I ####
if(iCN==1){ 
  fileNameFinal   = paste0(directoryBase,"0_CN_Clean.csv")
  numberRow = nrow(CNOriginal) 
  numberCols = ncol(CNOriginal)  
  NumberSec = nchar(SecuenciaLetras) 
  NoSect = 0
  
  # Contando el numero de secciones que coinciden con el criterio de busqueda: SecuenciaLetras
  for (i in 1:numberRow){
    nameString = as.character(CNOriginal[i,3])
    PrimerasLetras = substring(nameString,1,NumberSec)
    if(is.na(PrimerasLetras)!=TRUE && PrimerasLetras==SecuenciaLetras){NoSect = NoSect+1}
  }
  
  cat(sprintf("\n\tSe encontraron %1.0f posibles secciones del archivo original.\n",NoSect))
  
  # Creando matriz: Depth, Names, N, N.u, C, C.u
  # AllData[i,1]: Depth    AllData[i,2]: Names   AllData[i,3]: N...
  AllData = data.frame(matrix(NA, NoSect,1), c(rep(NA, NoSect)) , matrix(NA, NoSect, 4))
  NamesAllData   = c('Depth', 'Name', 'N.abs', 'N.u', 'C.abs', 'C.u')
  colnames(AllData) <- NamesAllData
  
  # Copiando valores seleccionados a la matriz AllData
  NoSect  <- 0
  for (i in 1:numberRow){
    nameString = as.character(CNOriginal[i,3])
    PrimerasLetras = substring(nameString,1,NumberSec)
    if(is.na(PrimerasLetras)!=TRUE && PrimerasLetras==SecuenciaLetras)
    {
      NoSect = NoSect+1;
      
      #Estraer la profundidad 
      PosicionNumeros   = str_locate(nameString,"\\(?[0-9,.]+\\)?-\\(?[0-9,.]+\\)?")
      NumerosDepth      = substr(nameString, PosicionNumeros[1], PosicionNumeros[2])
      PosicionGuion     = str_locate(NumerosDepth,"-")
      Depth1           = as.numeric(substr(NumerosDepth, 1, PosicionGuion[1]-1))
      Depth2           = as.numeric(substr(NumerosDepth, PosicionGuion[1]+1, str_count(NumerosDepth)))

      AllData[NoSect,1] = (Depth1+Depth2)/2
      AllData[NoSect,2] = nameString
      AllData[NoSect,3] = as.numeric(CNOriginal[i,7])
      AllData[NoSect,5] = as.numeric(CNOriginal[i,8])    
    }
  }
  rm(Depth1, Depth2, PosicionNumeros, NumerosDepth, PosicionGuion)
  rm(nameString, PrimerasLetras, SecuenciaLetras, NumberSec)
  
  #Eliminando secciones sin datos. Pone la profundidad a cero
  for(i in 1:NoSect){if(AllData[i,3]==0 && AllData[i,5]==0){ AllData[i,1]=0 }}
  
  # Creando la posibilidad de eliminar una seccion manualmente
  while (mEliminar==0) 
  {
  for(i in 1:NoSect){if(AllData[i,1]!=0){
    cat(sprintf("\nNo. Seccion: %2.0f,\t Depth: %1.2f\t",i,AllData[i,1]))
    cat(sprintf("N: %1.3f,\t C:%1.3f",AllData[i,3], AllData[i,5]))}}
    cat(sprintf("\n\n Para eliminar alguna seccion, introducir el No. de la seccion."))
    cat(sprintf(" En caso contrario, 0."))
    cat(sprintf("\n Solo debe quedar UNA seccion repetidas para el calculo de las incertidumbres\n"))
    i <- readline(prompt="Digitar valor: ")
    i <- as.integer(i)
    
    if(i==0){mEliminar=1
    }else{AllData[i,1] = 0}
  }
}

#### CN: Inicio Seccion - Parte II ####
if(iCN==1){ 
  
  # Calculando el valor de secciones repetidas
  mDepth = AllData[,1]
  vecDuplicated = mDepth[duplicated(mDepth)]
  vecDuplicated = vecDuplicated[!duplicated(vecDuplicated)] 
  NoSecDuplic = length(vecDuplicated)
  rm(mDepth)
  
  Repetitions = 0; 
  for(i in 1:NoSect){if(AllData[i,1]==vecDuplicated[1]){Repetitions = Repetitions+1}}
  
  if(NoSecDuplic==0){
    cat(sprintf("\t No hay secciones repetidas"))
    CMean = 1;  CSigma = 0; 
    NMean = 1;  NSigma = 0; 
  }else if(NoSecDuplic!=1){
    cat(sprintf("\t No hay secciones repetidas unicas"))
    CMean = 1;  CSigma = 0; 
    NMean = 1;  NSigma = 0; 
  } else{ 
      CNSigma = matrix(NA, Repetitions, 2)
      k = 1; 
      for(i in 1:NoSect){if(AllData[i,1]==vecDuplicated[1]){
        CNSigma[k,1] = AllData[i,3] # N
        CNSigma[k,2] = AllData[i,5] # C
        k=k+1}}
          
      NMean = mean(CNSigma[,1]);  NSigma = sd(CNSigma[,1])
      CMean = mean(CNSigma[,2]);  CSigma = sd(CNSigma[,2])
            
      for(i in 1:NoSect){if(AllData[i,1]==vecDuplicated[1]){
        AllData[i,1] = 0; AllData[i,3] = 0; AllData[i,5] = 0; 
        k=i}}
        
      AllData[k,1] = vecDuplicated[1]
      AllData[k,3] = NMean;     AllData[k,4] = NSigma; 
      AllData[k,5] = CMean;     AllData[k,6] = CSigma;  
  }
  # Buscando secciones con valores de N nulo, C no nuclos y que no hayan estado repetidas
  for(i in 1:NoSect){if(AllData[i,1]!=0 && AllData[i,3]==0){ AllData[i,3] = 0.09 }}
  
  # Creando matriz con valores definitivos
  k=1
  CNFinal = data.frame(matrix(NA, k,1), c(rep(NA, k)) , matrix(NA, k, 4))
  NamesCNFinal   = c('Depth', 'Name', 'N.abs', 'N.u', 'C.abs', 'C.u')
  colnames(CNFinal) <- NamesCNFinal
  
  # Calculando la incertidumbre absoluta para las demas secciones
  for(i in 1:NoSect){if(AllData[i,1]!=0 ){
    AllData[i,4] = AllData[i,3]*NSigma/NMean;
    AllData[i,6] = AllData[i,5]*CSigma/CMean;
      
    CNFinal[k,1] = AllData[i,1]
    CNFinal[k,2] = AllData[i,2]
    CNFinal[k,3] = AllData[i,3]/100
    CNFinal[k,4] = AllData[i,4]/100
    CNFinal[k,5] = AllData[i,5]/100
    CNFinal[k,6] = AllData[i,6]/100
    k=k+1
    }}
  
  write.csv(CNFinal,file = fileNameFinal, row.names = FALSE, na = "")
  rm(vecDuplicated, Repetitions, numberRow, numberCols, NSigma, NoSect)
  rm(NoSecDuplic, NMean, NamesAllData, NamesCNFinal, fileNameFinal, fileNameInicial)
  rm(CMean, CSigma, AllData, CNSigma)
}



#### GAMMA: Inicio Seccion ####
if(iGamma==1){
  # Nombre del nucleo
  fileNameCSV     = paste0(directoryBase,"0_Gamma_from_RPT.csv")
  
  # Leer todos los archivos .rpt en el directorio
  ListaArchivos = list.files(directoryBase, pattern = ".rpt")   
  NoDePuntos = length(ListaArchivos)
  
  # Se agrega una columna adicional para identifica la profundidad (Depth)
  Act         = matrix(NA, nrow = NoDePuntos, ncol = 10)
  ActU        = matrix(NA, nrow = NoDePuntos, ncol = 10) # Incertidumbre
  MDA         = matrix(NA, nrow = NoDePuntos, ncol = 10)
  ActAndMDA   = matrix(NA, nrow = NoDePuntos, ncol = 30)
  
  VecDepth <- vector();    VecDate <- vector();    VecDetector <- vector()
  VecCalibration <- vector();     VecDetLimit <- vector(); VecNameFile <- vector()
  VecAnalysis <- vector();      VecBackground <- vector();  VecPoints <- vector()
  VecMass <- vector();
  
  NombresNucleos  = c("Be-7","Cs-134","Tl-208","Ac-228","K-40","Am-241",
                      "Pb-210","Pb-214","Cs-137","Th-234")
  colnames(Act)   <- NombresNucleos
  colnames(MDA)   <- NombresNucleos
  
  rm(NombresNucleos)
  
  for(ii in 1:NoDePuntos){
    file          <- ListaArchivos[ii] 
    singleString  <- readLines(file, skipNul=TRUE) 
    maxLines      <- length(singleString)
    
    #Estraer la profundidad 
    PosicionNumeros   = str_locate(file,"\\(?[0-9,.]+\\)?-\\(?[0-9,.]+\\)?")
    NumerosDepth      = substr(file, PosicionNumeros[1], PosicionNumeros[2])
    PosicionGuion     = str_locate(NumerosDepth,"-")
    Depth1           = as.numeric(substr(NumerosDepth, 1, PosicionGuion[1]-1))
    Depth2           = as.numeric(substr(NumerosDepth, PosicionGuion[1]+1, str_count(NumerosDepth)))
    
    VecDepth[ii] = (Depth1+Depth2)/2
    print(VecDepth[ii])
    nam   <- paste("Sec", VecDepth[ii], sep = "")                         # Para guardar los datos
    
    # Inicializando variables globales
    sDate = "0";      sDetector = "0";    sCalibration = "0";   sBackground = "0"
    sPoints = "0";    sAnalysis = "0";    sDetecLimit = "0";    sMass = "0"
    
    ActivityTable <- table(NA)
    
    # Guardando ciertos valores asociadas a determinadas palabras
    for(i in 1:maxLines)
    {
      
      if(singleString[i]==" *****   S U M M A R Y   O F   N U C L I D E S   I N   S A M P L E   *****")
      {
        ActivityTable <- read.table(file, header = FALSE, sep = "", quote = "", dec = ".",
                                    blank.lines.skip = TRUE, nrows=10,  fill = TRUE, skip = i+4, 
                                    comment.char = "", col.names=c("1","2","3","4","5","6"))
        assign(nam, ActivityTable)
      }
      if(str_detect(singleString[i],"Acquisition information"))
      {
        sDate = singleString[i+1] # s means String along the document
        sDate = gsub("Start time:","",sDate)
        sDate = trimws(sDate)     # Eliminar espacios al inicio y al final
      } 
      if(str_detect(singleString[i],"Detector system"))
      {
        sDetector = singleString[i+1]
        sDetector = trimws(sDetector)   
      }
      if(str_detect(singleString[i],".Clb"))
      {
        sCalibration = singleString[i]
        sCalibration = paste0(sCalibration,singleString[i+2])
        sCalibration = gsub("Filename:","",sCalibration)
        sCalibration = trimws(sCalibration)
      }
      if(str_detect(singleString[i],"Peaked background correction:"))
      {
        sBackground = singleString[i]
        sBackground = gsub("Peaked background correction:","",sBackground)
        sBackground = gsub("YES","",sBackground)
        sBackground = trimws(sBackground)
      }
      if(str_detect(singleString[i],"Background width:"))
      {
        sPoints = singleString[i]
        sPoints = gsub("Background width:","",sPoints)
        sPoints = gsub("average of","",sPoints)
        sPoints = gsub("points.","",sPoints)
        sPoints = trimws(sPoints)
      }
      if(str_detect(singleString[i],"Analysis engine:"))
      {
        sAnalysis = singleString[i]
        sAnalysis = gsub("Analysis engine:","",sAnalysis)
        sAnalysis = trimws(sAnalysis)
      }
      if(str_detect(singleString[i],"Detection limit method:"))
      {
        sDetecLimit = singleString[i]
        sDetecLimit = gsub("Detection limit method:","",sDetecLimit)
        sDetecLimit = trimws(sDetecLimit)
      }
      if(str_detect(singleString[i],"Sample Size"))
      {
        sMass = singleString[i]
        sMass = gsub("Sample Size:","",sMass)
        sMass = trimws(sMass)
      }
    }
    
    VecNameFile[ii] <- file;              VecDate[ii] = sDate
    VecDetector[ii] = sDetector;          VecCalibration[ii] = sCalibration
    VecDetLimit[ii] = sDetecLimit;        VecAnalysis[ii] = sAnalysis
    VecBackground[ii] = sBackground;      VecPoints[ii] = sPoints
    VecMass[ii] = sMass
    
    ActivityTable   <- as.matrix(ActivityTable)
    numberCols      <- nrow(ActivityTable)

    for(j in 1:numberCols)
    {  

      k=1

      if(identical(as.character(ActivityTable[j,2]),"*")){ k=0 }
      if(identical(as.character(ActivityTable[j,2]),"&")){ k=0 }
      if(identical(as.character(ActivityTable[j,2]),"A")){ k=0 }
      if(identical(as.character(ActivityTable[j,2]),"B")){ k=0 }
      if(identical(as.character(ActivityTable[j,2]),"C")){ k=0 }
      if(identical(as.character(ActivityTable[j,2]),"F")){ k=0 }
      if(identical(as.character(ActivityTable[j,2]),"H")){ k=0 }
      
      if(identical(as.character(ActivityTable[j,2]),"#") ||
         identical(as.character(ActivityTable[j,2]),"A") ||
         identical(as.character(ActivityTable[j,2]),"#A") )     # Bad shape
      { 
        k=0
        Act[ii,j] <- as.numeric(ActivityTable[j,3]) 
        ActU[ii,j] <- as.numeric(ActivityTable[j,5])
        MDA[ii,j] <- as.numeric(ActivityTable[j,6])  
      }
      if( (identical(as.character(ActivityTable[j,2]),"<")) && (is.na(ActivityTable[j,4])==TRUE) ) 
      { 
        k=0
        MDA[ii,j] <- as.numeric(ActivityTable[j,3])  
      }
      if( (is.na(ActivityTable[j,6])==TRUE) && (k==1))
      {
        Act[ii,j] <- as.numeric(ActivityTable[j,2])
        ActU[ii,j] <- as.numeric(ActivityTable[j,4])
        MDA[ii,j] <- as.numeric(ActivityTable[j,5])  
      }
    }
  }
  
  rm(ListaArchivos,ActivityTable, Depth1, Depth2, nam, LocNumberFileGamma, singleString)
  rm(file, sDate, sDetector, sCalibration, sDetecLimit, sAnalysis, sBackground, sPoints, sMass)
  rm(PosicionGuion, PosicionNumeros, NumerosDepth)
  
  # Ubicando la actividad y el MDA juntos
  for(ii in 1:NoDePuntos)
  { 
    for(j in 1:10)
      {
      ActAndMDA[ii,3*j-2] = Act[ii,j]; 
      ActAndMDA[ii,3*j-1] = ActU[ii,j];
      ActAndMDA[ii,3*j] = MDA[ii,j] 
      
    }
  }
  
  GammaFinal = data.frame(VecDepth, VecNameFile, ActAndMDA, 
                       VecDate, VecDetector, VecCalibration, VecAnalysis, 
                       VecDetLimit, VecBackground, VecPoints,
                       VecMass)
  Names   = c("Depth", "Name",
              
              "Be-7.Bq.kg-1","Be-7.u","Be-7-MDA",
              "Cs-134.Bq.kg-1","Cs-134.u","Cs-134-MDA",
              "Tl-208.Bq.kg-1","Tl-208.u","Tl-208-MDA",
              "Ac-228.Bq.kg-1","Ac-228.u","Ac-228-MDA",
              "K-40.Bq.kg-1","K-40.u","K-40-MDA",
              "Am-241.Bq.kg-1","Am-241.u","Am-241-MDA",
              "Pb-210.Bq.kg-1","Pb-210.u","Pb-210-MDA",
              "Pb-214.Bq.kg-1","Pb-214.u","Pb-214-MDA",
              "Cs-137.Bq.kg-1","Cs-137.u","Cs-137-MDA",
              "Th-234.Bq.kg-1","Th-234.u","Th-234-MDA",
              
              "Date", "DetectorFromRPT", "CalibrationFile", "AnalysisMethod", "DetectionLimit", 
              "BackgroundFile", "NumberOfPoints-Background",
              "Mass")
  
  rm(VecDepth, VecNameFile,VecDate, VecDetector, VecCalibration, VecAnalysis, 
     VecDetLimit, VecBackground, VecPoints)
  
  colnames(GammaFinal) <- Names
  
  # Calculando la densidad. Hay que tener cuidado para extraer la masa
  
  MassVolDensity <- matrix(NA, NoDePuntos,9)
  colnames(MassVolDensity) <- c("mass.g", "mass.u", "vol.cm3", "vol.u", "density", "density.u",
                                "geometry", "detector", "Depth")
  
  v = 0; dv = 0; sMass = "0";
  
  for(i in 1:NoDePuntos){
    sMass = as.character(GammaFinal$Mass[i])             # String
    m  = as.numeric(substring(sMass,1,10))*1000      # Valor en g 
    dm = 0.0001*1000/2/sqrt(3)
    
    sGeometry = as.character(GammaFinal$CalibrationFile[i])
    
    if(is.na(str_extract(sGeometry,"2cm"))!=TRUE)         {v  = 1.7207;   dv  = 0.007; MassVolDensity[i,7] = "2cm"}
    else if(is.na(str_extract(sGeometry,"4cm"))!=TRUE)    {v  = 3.6805;   dv  = 0.005; MassVolDensity[i,7] = "4cm" }
    else if (is.na(str_extract(sGeometry,"2ml"))!=TRUE)   {v  = 1.939;    dv  = 0.007; MassVolDensity[i,7] = "2ml"}
    else if (is.na(str_extract(sGeometry,"4ml"))!=TRUE)   {v  = 4.071;    dv  = 0.004; MassVolDensity[i,7] = "4ml" }
    
    if(is.na(str_extract(sGeometry,"G1"))!=TRUE)         {MassVolDensity[i,8] = "G1"}
    else if(is.na(str_extract(sGeometry,"G2"))!=TRUE)    {MassVolDensity[i,8] = "G2"}
    else if (is.na(str_extract(sGeometry,"G3"))!=TRUE)   {MassVolDensity[i,8] = "G3"}
    else if (is.na(str_extract(sGeometry,"G4"))!=TRUE)   {MassVolDensity[i,8] = "G4"}
    
    Density = m/v   # g cm-3
    DensityU = sqrt( (dm/v)^2 + ( m*dv/(v*v) )^2 )
    
    MassVolDensity[i,1] = m
    MassVolDensity[i,2] = dm
    MassVolDensity[i,3] = v
    MassVolDensity[i,4] = dv
    MassVolDensity[i,5] = Density
    MassVolDensity[i,6] = DensityU
    MassVolDensity[i,9] = GammaFinal[i,1]
  }
  
  GammaFinal   = merge(GammaFinal,MassVolDensity,by="Depth")
  
  write.csv(GammaFinal,file = fileNameCSV, row.names = FALSE, na = "")
  rm(Act, ActU, ActAndMDA, MDA, VecMass, numberCols, NoDePuntos, Names, maxLines, mEliminar)
  #rm(m,dm,v,dv,Density,DensityU,MassVolDensity,sGeometry,sMass)
}

#### XRF: Inicion Seccion ####
if(iXRF==1){
  fileNameCSV     = paste0(directoryBase,"0_XRF_Clean.csv")

  numberRow = nrow(XRFOriginal)
  numberCols = ncol(XRFOriginal)    
  NumberSec = nchar(SecuenciasLetrasXRF); 
  vecDepth = vector()
  vecName = vector()
  
  # Contando el numero de secciones que coinciden con el criterio de busqueda: SecuenciasLetrasXRF
  NoSect <- 0; 
  for (i in 1:numberRow){
    nameString = as.character(XRFOriginal[i,LocColNames])
    PrimerasLetras = substring(nameString,1,NumberSec)
    
    if(is.na(PrimerasLetras)!=TRUE && 
       (PrimerasLetras==SecuenciasLetrasXRF || PrimerasLetras==SecuenciasLetrasXRF2) ){
      NoSect = NoSect+1

      #Estraer la profundidad 
      PosicionNumeros   = str_locate(nameString,"\\(?[0-9,.]+\\)?-\\(?[0-9,.]+\\)?")
      NumerosDepth      = substr(nameString, PosicionNumeros[1], PosicionNumeros[2])
      PosicionGuion     = str_locate(NumerosDepth,"-")
      Depth1           = as.numeric(substr(NumerosDepth, 1, PosicionGuion[1]-1))
      Depth2           = as.numeric(substr(NumerosDepth, PosicionGuion[1]+1, str_count(NumerosDepth)))
    
      vecDepth[NoSect] = (Depth1+Depth2)/2
      vecName[NoSect] = nameString
    }
  }
  
  XRFNumeric  <- matrix(data = NA, nrow = NoSect, ncol = numberCols-nDel-1)
  
  NoSect <- 0
  for (i in 1:numberRow){
    nameString = as.character(XRFOriginal[i,LocColNames])
    PrimerasLetras = substring(nameString,1,NumberSec)
    if(is.na(PrimerasLetras)!=TRUE && 
       (PrimerasLetras==SecuenciasLetrasXRF || PrimerasLetras==SecuenciasLetrasXRF2) ){
      NoSect = NoSect+1
      for(j in (nDel+1):numberCols-1)
      { 
        if (suppressWarnings(all(!is.na(as.numeric(as.character(XRFOriginal[i,j])))))) 
        {
          XRFNumeric[NoSect,j-nDel]     = as.numeric(as.character(unlist(XRFOriginal[[i,j]])))
          if(XRFNumeric[NoSect,j-nDel] < 0)
          {
            XRFNumeric[NoSect,j-nDel] = 0; print("Valor negativo encontrado")
          }
        } 
      }
    }
  }
  rm(Depth1, Depth2,nameString, PrimerasLetras, SecuenciasLetrasXRF)
  rm(NumberSec, SecuenciasLetrasXRF2, LocColNames)
  rm(PosicionNumeros, NumerosDepth, PosicionGuion)
  
  numberRow       <- nrow(XRFNumeric)
  numberCols      <- ncol(XRFNumeric)   
  
  # Convertir XRFNumeric a valores absoluto 
  # Desde Na hasta Ti es en PORCENTAJE (.p). Fe tambien es en PORCENTAJE 
  # Los demas, desde V hasta U (excepto Fe) son E-6, MU (.mu)
  
  for( i in 1:numberRow){
    for(j in 1:20){ XRFNumeric[i,j] = XRFNumeric[i,j]/100}
    for(j in 21:numberCols)
    { 
      XRFNumeric[i,j] = XRFNumeric[i,j]/1000000
      if(j==(21+6) || j==(21+7)) { XRFNumeric[i,j] = XRFNumeric[i,j]*10000 }# Fe      SIEMPRE REVISAR
    }
  }
  
  XRFFinal <- data.frame(vecDepth, vecName, XRFNumeric)
  
  NamesXRF   = c('Depth', 'Name', 
                 'Na.abs', 'Na.u', 'Mg.abs', 'Mg.u', 'Al.abs', 'Al.u', 'Si.abs', 'Si.u', 'P.abs', 'P.u', 
                 'S.abs', 'S-u', 'Cl.abs', 'Cl.u', 'K.abs', 'K.u', 'Ca.abs', 'Ca.u', 'Ti.abs', 'Ti.u', 'V..abs', 'V.u', 
                 'Cr.abs', 'Cr.u', 'Mn.abs', 'Mn.u', 'Fe.abs', 'Fe.u', 'Co.abs', 'Co.u', 'Ni.abs', 'Ni.u',
                 'Cu.abs', 'Cu.u', 'Zn.abs', 'Zn.u', 'Ga.abs', 'Ga.u', 'Ge.abs', 'Ge.u', 'As.abs', 'As.u', 
                 'Se.abs', 'Se.u', 'Br.abs', 'Br.u', 'Rb.abs', 'Rb.u', 'Sr.abs', 'Sr.u', 'Y.abs', 'Y.u', 
                 'Zr.abs', 'Zr.u', 'Nb.abs', 'Nb.u', 'Mo.abs', 'Mo.u', 'Ag.abs', 'Ag.u', 'Cd.abs', 'Cd.u', 
                 'Sn.abs', 'Sn.u', 'Sb.abs', 'Sb.u', 'Te.abs', 'Te.u', 'I.abs', 'I.u', 'Cs.abs', 'Cs.u', 
                 'Ba.abs', 'Ba.u', 'La.abs', 'La.u', 'Ce.abs', 'Ce.u', 'Pr.abs', 'Pr.u', 'Hf.abs', 'Hf.u', 
                 'Ta.abs', 'Ta.u', 'W.abs', 'W.u', 'Hg.abs', 'Hg.u', 'Tl.abs', 'Tl.u', 'Pb.abs', 'Pb.u', 
                 'Bi.abs', 'Bi.u', 'Th.abs', 'Th.u', 'U.abs', 'U.u')
  
  colnames(XRFFinal) <- NamesXRF
  
  write.csv(XRFFinal,file = fileNameCSV, row.names = FALSE, na = "")
  rm(XRFNumeric, vecName, vecDepth, NoSect, nDel, NamesXRF, numberCols, numberRow, fileNameTXT)
}

#### 100 por 100 de la composicion y ANGLE ####
# Se debe ejecutar en donde se encuentre instalado ANGLE (Windows UNAM FECHP)
directoryBase = "/Users/Ernesto Charry/Google Drive/4_Thesis_Master/Data/"

DirANGLEFiles = directoryBase
directoryBase = paste0(directoryBase,"datos_tesis_unam_2018/0_Modelo_JASC_FEChP/0_Datos_Iniciales/")
DirANGLEFiles = paste0(DirANGLEFiles,"datos_tesis_unam_2018/0_Modelo_JASC_FEChP/2_ANGLE_Files/")

# Creando directorio analisis oxigeno
directoryBase2 = paste0(directoryBase1,"/2_Resultados")
directoryBase3  = paste0(directoryBase,directoryBase2,"/");

# Creando carpeta para archivos AllData.csv
dir.create(directoryBase3)
setwd(directoryBase3)

#porcentajeOxy = 50
#for(iOxigeno in seq(10,100,by=10)){
for(iOxigeno in porcentajeOxy){
  
  # Porcentaje de los elementos faltantes. Debe sumar el 100 por 100
  nH = 100-iOxigeno ;    # H
  nO = iOxigeno;    # Oxy 
  nHe = 0;  nLi = 0; nBe = 0;  nB = 0;   nF = 0;  nNe = 0; 
  
  vecPorcentaje = c(nH, nHe, nLi, nBe, nB, nO, nF, nNe)
  OName = as.character(nO); HName = as.character(nH);
  
  # Creando carpeta para .SAVX y BASH file
  DirSAVX     = paste0(directoryBase3,"1_SAVX_ANGLE_Files_O",OName,"_H",HName,"/")
  dir.create(DirSAVX)
  BashFile      = paste0(DirSAVX,"BashANGLE",".sh")
  TextBashFile  = '/c/ProgramData/ANGLE\\ 4/Angle.exe '
  
  # Nombre de la carpeta con caracteres especiales para generar orden en archivos .sh
  DirSAVXANGLE = '/c/Users/Ernesto\\ Charry/Google\\ Drive/4_Thesis_Master/Data'
  DirSAVXANGLE = paste0(DirSAVXANGLE,'/datos_tesis_unam_2018/0_Modelo_JASC_FEChP/0_Datos_Iniciales/')
  DirSAVXANGLE = paste0(DirSAVXANGLE,directoryBase2,"/1_SAVX_ANGLE_Files_O",OName,"_H",HName,"/")
  
  BashDirectoryName = paste0("1_SAVX_ANGLE_Files_O",OName,"_H",HName,"/")
  
  FileAllData0 = paste0("0_AllData_O",OName,"_H",HName,".csv")
  FileAllData1 = paste0("1_AllData_O",OName,"_H",HName,".csv")  
  if(iCN==1 & iXRF==1){
    
    CompositionMatrix   = merge(CNFinal,XRFFinal,by="Depth")
    CompositionMatrix   = CompositionMatrix[,-which(names(CompositionMatrix) == "Name.x")]
    CompositionMatrix   = CompositionMatrix[,-which(names(CompositionMatrix) == "Name.y")]
    NoSecciones = nrow(CompositionMatrix) 
    
    MissElementsMatrix = matrix(NA,NoSecciones,17)
    colnames(MissElementsMatrix) <- c("Depth","H.abs", "H.u", "He.abs", "He.u", "Li.abs", "Li.u", "Be.abs", "Be.u",
                                      "B.abs", "B.u", "O.abs", "O.u", "F.abs", "F.u", "Ne.abs", "Ne.u")
    for(j in 1:NoSecciones){
      ElementsSum   = 0;  # Suma de todos los elementos 
      ElementsSumU  = 0;  # Calculo de las incertidumbres
      for(i in 2:ncol(CompositionMatrix)){
        if(is.na(CompositionMatrix[j,i])==TRUE){CompositionMatrix[j,i]=0}
        if(i%%2==0){ElementsSum = ElementsSum+CompositionMatrix[j,i]}
        if(i%%2!=0){ElementsSumU = ElementsSumU+CompositionMatrix[j,i]^2}
      }
      #print(ElementsSum);    print(ElementsSumU)
      ElementsSumU = sqrt(ElementsSumU)
      
      MissElementsMatrix[j,1] = CompositionMatrix[j,1]  # Depth
      for(k in 1:8)
      {
        MissElementsMatrix[j,2*k] = (1-ElementsSum)*vecPorcentaje[k]/100; 
        MissElementsMatrix[j,2*k+1] = ElementsSumU*vecPorcentaje[k]/100; 
      }
    }
    
    CompositionMatrix   = merge(MissElementsMatrix,CompositionMatrix,by="Depth")
    rm(MissElementsMatrix, vecPorcentaje, ElementsSum, ElementsSumU)
    
    # Merge CN, Gamma and CRF
    if(iGamma==1){
      AllData = merge(CompositionMatrix, GammaFinal,by="Depth")
    #  FileAllData = "0_AllData.csv"
      FileAllData2 = paste0(directoryBase3,"/",FileAllData0)
      write.csv(AllData,file = FileAllData2, row.names = FALSE, na = "")
    }
    
    rm(CompositionMatrix)
  }

  NoSecciones = nrow(AllData)
  
  HColumnNumber = as.numeric(which(names(AllData) == "H.abs"))
  UColumnNumber = as.numeric(which(names(AllData) == "U.abs"))
  Elements      = matrix(NA, nrow = 1, ncol = (UColumnNumber/2 -1))
  ElementsNames = matrix(NA, nrow = 1, ncol = (UColumnNumber/2 -1))
  
  # Extrayendo nombres de elementos
  for(j in seq(HColumnNumber,UColumnNumber,by=2)){ ElementsNames[j/2] = toupper(names(AllData[j]))}
  ElementsNames = gsub("\\.", "", ElementsNames)
  ElementsNames = gsub("ABS", "", ElementsNames)
  ElementsNames = gsub("ASB", "", ElementsNames)
  
  # Creando matrix para la curva de referencia AGUA
  EffReff = matrix(NA,NoSecciones,20*2)
  NamesEffReff = c("Energy.keV","Eff.Agua.Ref","Energy.keV","Eff.Agua.Ref","Energy.keV","Eff.Agua.Ref",
                   "Energy.keV","Eff.Agua.Ref","Energy.keV","Eff.Agua.Ref","Energy.keV","Eff.Agua.Ref",
                   "Energy.keV","Eff.Agua.Ref","Energy.keV","Eff.Agua.Ref","Energy.keV","Eff.Agua.Ref",
                   "Energy.keV","Eff.Agua.Ref","Energy.keV","Eff.Agua.Ref","Energy.keV","Eff.Agua.Ref",
                   "Energy.keV","Eff.Agua.Ref","Energy.keV","Eff.Agua.Ref","Energy.keV","Eff.Agua.Ref",
                   "Energy.keV","Eff.Agua.Ref","Energy.keV","Eff.Agua.Ref","Energy.keV","Eff.Agua.Ref",
                   "Energy.keV","Eff.Agua.Ref","Energy.keV","Eff.Agua.Ref")
  
  colnames(EffReff) <- NamesEffReff
  rm(NamesEffReff)
  
  i=2

  #### Extrayendo valores y generando .SAVX file ####
  for(i in 1:NoSecciones)
  {
    ValorDensity  = AllData$density[i]
    ValorGeometry = as.character(AllData$geometry[i])
    ValorDetector = as.character(AllData$detector[i])
    
    for(j in seq(HColumnNumber,UColumnNumber,by=2)){
      Elements[j/2] = as.numeric(AllData[i,j]) 
    }
    # Escribiendo el tipo de detector
    {
      TextToFile = '<?xml version="1.0" encoding="utf-8"?>
      <angle generator="ANGLE" version="4.0" units="cm">'   
      if(ValorDetector=="G1"){
        AngleFileName = paste0(DirANGLEFiles,'detector/detectorG1.txt')
      }else if(ValorDetector=="G2"){
        AngleFileName = paste0(DirANGLEFiles,'detector/detectorG2.txt')
      }else if(ValorDetector=="G3"){
        AngleFileName = paste0(DirANGLEFiles,'detector/detectorG3.txt')
      }else if(ValorDetector=="G4"){
        AngleFileName = paste0(DirANGLEFiles,'detector/detectorG4.txt')
      }else if(ValorDetector=="G5"){
        AngleFileName = paste0(DirANGLEFiles,'detector/detectorG5.txt')
      }else{ cat(sprintf("\n\n Valor detector no encontrado.")) }
      TextFromANGLEFile = read_file(AngleFileName)
      TextToFile = paste0(TextToFile,TextFromANGLEFile)
    }
    
    # Escribiendo el contenedor
    {
      if(ValorGeometry=="2cm"){
        AngleFileName = paste0(DirANGLEFiles,'container/container_2cm.txt')
      }else if(ValorGeometry=="4cm"){
        AngleFileName = paste0(DirANGLEFiles,'container/container_2cm.txt')
      }else if(ValorGeometry=="2ml"){
        AngleFileName = paste0(DirANGLEFiles,'container/container_2ml.txt')
      }else if(ValorGeometry=="4ml"){
        AngleFileName = paste0(DirANGLEFiles,'container/container_4ml.txt')
      }else{ cat(sprintf("\n\n VALOR GEOMETRIA NO ENCONTRADO.")) }
      TextFromANGLEFile = read_file(AngleFileName)
      TextContenedor    = read_file(AngleFileName)
      TextToFile = paste0(TextToFile,TextFromANGLEFile)
    }
    
    # Escribiendo el 100 por 100 de la composicion 
    {
      TextToFile = paste0(TextToFile,'<material name="')
      
      nameSection = as.character(AllData$Depth[i])
      nameSection = gsub("\\.", "-", nameSection)
      nameSection = gsub('*"', "", nameSection)
      
      TextToFile = paste0(TextToFile,nameSection,'"  density="', ValorDensity,'">\n')
      TextToFile = paste0(TextToFile,"\t<elements>\n")
      
      for(j in 1:length(Elements))
      {
        if(Elements[j]!=0)
        {
          TextToFile = paste0(TextToFile,'\t\t<element symbol="')
          TextToFile = paste0(TextToFile,ElementsNames[j])
          TextToFile = paste0(TextToFile,'" massFraction="')
          TextToFile = paste0(TextToFile,100*Elements[j])
          TextToFile = paste0(TextToFile,'" /> \n')
        }
      }
    }
    
    # Escribiendo energias y presicion Angle
    {
      AngleFileNameEnergies = paste0(DirANGLEFiles,'energies/18_Energies_GammaLabMztln.txt') 
      TextFromANGLEFile = read_file(AngleFileNameEnergies)
      TextToFile = paste0(TextToFile,TextFromANGLEFile,AnglePresicion,'</precision>')
    }
    
    # Escribiendo la curva de referencia (DEPENDE DE LA FECHA)
    {
      TextCRef = '<referenceEfficiencyCurve name="Eff Agua" description="Eff agua">'
      TextCRef = paste0(TextCRef,'<experimentalPoints>')
      
      if(ValorDetector=="G1" && ValorGeometry=="2ml" ){
        TextDetectorName = '<detector name="G1 49-P22600A" />'
        AngleFileName2 = paste0(DirANGLEFiles,'referenceEfficiencyCurve/G1_2ml_agua.csv')
      }else if(ValorDetector=="G1" && ValorGeometry=="2cm" ){
        TextDetectorName = '<detector name="G1 49-P22600A" />'
        AngleFileName2 = paste0(DirANGLEFiles,'referenceEfficiencyCurve/G1_2cm_agua.csv')
      }else if(ValorDetector=="G1" && ValorGeometry=="4cm" ){
        TextDetectorName = '<detector name="G1 49-P22600A" />'
        AngleFileName2 = paste0(DirANGLEFiles,'referenceEfficiencyCurve/G1_4cm_agua.csv')
      }else if(ValorDetector=="G1" && ValorGeometry=="4ml" ){
        TextDetectorName = '<detector name="G1 49-P22600A" />'
        AngleFileName2 = paste0(DirANGLEFiles,'referenceEfficiencyCurve/G1_4ml_agua.csv')
      }else if(ValorDetector=="G2" && ValorGeometry=="2ml"){
        TextDetectorName = '<detector name="G2 54-P51326a" />'
        AngleFileName2 = paste0(DirANGLEFiles,'referenceEfficiencyCurve/G2_2ml_agua.csv')
      }else if(ValorDetector=="G2" && ValorGeometry=="4ml"){
        TextDetectorName = '<detector name="G2 54-P51326a" />'
        AngleFileName2 = paste0(DirANGLEFiles,'referenceEfficiencyCurve/G2_4ml_agua.csv')
      }else if(ValorDetector=="G3" && ValorGeometry=="2ml"){
        TextDetectorName = '<detector name="G3 57-P 13768A v2" />'
        AngleFileName2 = paste0(DirANGLEFiles,'referenceEfficiencyCurve/G3_2ml_agua.csv')
      }else if(ValorDetector=="G3" && ValorGeometry=="4ml"){
        TextDetectorName = '<detector name="G3 57-P 13768A v2" />'
        AngleFileName2 = paste0(DirANGLEFiles,'referenceEfficiencyCurve/G3_4ml_agua.csv')
      }else if(ValorDetector=="G4" && ValorGeometry=="2ml"){
        TextDetectorName = '<detector name="G4 54-P51393A" />'
        AngleFileName2 = paste0(DirANGLEFiles,'referenceEfficiencyCurve/G4_2ml_agua.csv')
      }else if(ValorDetector=="G4" && ValorGeometry=="4ml"){
        TextDetectorName = '<detector name="G4 54-P51393A" />'
        AngleFileName2 = paste0(DirANGLEFiles,'referenceEfficiencyCurve/G4_4ml_agua.csv')
      }else( cat(sprintf("\n\n CURVA DE REFERENCIA NO ENCONTRADO.")) )
      
      DataCurvaRef = read.csv(AngleFileName2, header = FALSE)
      
      for(iCurva in 1:nrow(DataCurvaRef))
      {
        ValorEnergia      = as.character(DataCurvaRef[iCurva,1])
        ValorEficiencia   = as.character(DataCurvaRef[iCurva,2])
        
        TextCRef = paste0(TextCRef,'<point energy="')
        TextCRef = paste0(TextCRef,ValorEnergia)
        TextCRef = paste0(TextCRef,'" efficiency="')
        TextCRef = paste0(TextCRef,ValorEficiencia)
        TextCRef = paste0(TextCRef,'" />')
        
        EffReff[i,2*iCurva-1] = ValorEnergia
        EffReff[i,2*iCurva] = ValorEficiencia
      }
      
      TextCRef = paste0(TextCRef,'</experimentalPoints><regions>')
      TextCRef = paste0(TextCRef,'<region start="30" end="1800" polynomOrder="0" />')
      TextCRef = paste0(TextCRef,'</regions>')
      TextCRef = paste0(TextCRef,TextDetectorName)
      TextCRef = paste0(TextCRef,TextContenedor)
      TextCRef = paste0(TextCRef,'<material name="Water" />')
      TextCRef = paste0(TextCRef,'</source></referenceEfficiencyCurve></angle>')
      
      TextFromANGLEFile = read_file(AngleFileName)
      TextToFile = paste0(TextToFile,TextCRef)
    }
    
    # Generando archivo .SAVX
    FileAngleName = paste0(DirSAVX,nameSection,".savx")
    TextToFile = gsub('*\"', '"', TextToFile)  
    write(TextToFile, FileAngleName)
    
    # Complementando archivo bash
    TextBashFile = paste0(TextBashFile,DirSAVXANGLE,nameSection,".savx ")
    
  }
  #### Generando archivo .bash, ejecuando ANGLE ####
  
  # Generando archivo .bash
  
  TextBashFile = paste0(TextBashFile,'//samefolder //close')
  TextBashFile = gsub('*\"', '"', TextBashFile)
  
  write(TextBashFile, BashFile)
  BashDirectoryName = paste0("sh 1_SAVX_ANGLE_Files_O",OName,"_H",HName,"/BashANGLE.sh")
  OrdenAngle = 'sh 1_SAVX_ANGLE_Files/BashANGLE.sh'
  shell(BashDirectoryName)
  
  #### Leyendo archivos generados por ANGLE ####
  
  # Contando el numero de energies del archivo de ANGLE
  singleString  <- readLines(AngleFileNameEnergies,  encoding = "unknown", skipNul=TRUE, warn = FALSE)
  NoDeEnergies <- length(singleString)
  k = 0
  
  for(j in 1:NoDeEnergies){ if(str_detect(singleString[j],"energy")){ k = k+1 } }
  NoDeEnergies = k
  
  DataFromAngle = matrix(NA, nrow = NoSecciones, ncol = 2*NoDeEnergies)
  singleString = '0'
  vecNames <- vector(length =  2*NoDeEnergies)
  
  for(i in 1:NoSecciones)
  {
    nameSection = as.character(AllData$Depth[i])
    nameSection = gsub("\\.", "-", nameSection)
    nameSection = gsub('*"', "", nameSection)
    nameSection = paste0(DirSAVX,nameSection,".outx")
    
    singleString  <- readLines(nameSection, skipNul=TRUE) 
    maxLines      <- length(singleString)
    
    k = 1; 
    for(j in 1:maxLines)
    {
      if(str_detect(singleString[j],"result energy"))
      { 
        #Buscamos la ubicacion de las comillas, es el patron de los resultados
        pos = unlist(gregexpr('"', singleString[j]), use.names=FALSE)
        
        Energy      = substr(singleString[j], pos[1]+1,pos[2]-1)
        SolidAngle  = as.numeric(substr(singleString[j], pos[3]+1,pos[4]-1) )
        Eff         = as.numeric(substr(singleString[j], pos[5]+1,pos[6]-1) )
        
        DataFromAngle[i,2*k-1] = SolidAngle
        vecNames[2*k-1] = paste0("SolidAngle-at-",Energy,".keV")
        DataFromAngle[i,2*k] = Eff
        vecNames[2*k] = paste0("Efficiency-at-",Energy,".keV")
        
        k = k+1
      }
    }
  }
  
  colnames(DataFromAngle) <- vecNames
  
  AllData1 = data.frame(AllData,DataFromAngle,EffReff)
  fileAllData2 = paste0(directoryBase3,FileAllData1)
  write.csv(AllData1,file = fileAllData2, row.names = FALSE, na = "")
}
