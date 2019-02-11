# F. E. Charry-Pastrana; 20180904
# Este codigo lee el archivo inicial: 1_AllData_ANGLE.csv
# y genera las graficas

rm(list = ls())
library(stringr)
library(readr)
library("RColorBrewer")
library(latex2exp)

# i=1: Windows FECHP, i=2: MAC FECHP, i=3: Windows JASC, i=0: otro. 
iComputador = 2

# Definiendo directorio de trabajo
if(iComputador==1){
  directoryBase = "/Users/Ernesto Charry/Google Drive/4_Thesis_Master/Data/datos_tesis_unam_2018"
  directoryBase = paste0(directoryBase,"/0_Modelo_JASC_FEChP/")  
}else if(iComputador==2){
  directoryBase = "/Users/F.E.CharryPastrana/Google Drive/4_Thesis_Master/Data/datos_tesis_unam_2018"
  directoryBase = paste0(directoryBase,"/0_Modelo_JASC_FEChP/")  
}

directoryBase  = paste0(directoryBase,"2_ANGLE_Files/referenceEfficiencyCurve/"); 
setwd(directoryBase)

G1_2ml_agua = read.csv("G1_2ml_agua.csv", h = F)
G1_4ml_agua = read.csv("G1_4ml_agua.csv", h = F)
G2_2ml_agua = read.csv("G2_2ml_agua.csv", h = F)
G2_4ml_agua = read.csv("G2_4ml_agua.csv", h = F)
G3_2ml_agua = read.csv("G3_2ml_agua.csv", h = F)
G3_4ml_agua = read.csv("G3_4ml_agua.csv", h = F)
G4_2ml_agua = read.csv("G4_2ml_agua.csv", h = F)
G4_4ml_agua = read.csv("G4_4ml_agua.csv", h = F)


#### Grafica ####
Graph1Name = paste0("Eficiencia_agua.pdf")
widthPDFcm = 16           # cm
heightPDFcm = 12        # cm
pdf(Graph1Name,width=widthPDFcm/(cm(1)),height=heightPDFcm/(cm(1)))


# Parametros para TODAS las sub-graficas
NoDatos = 5               # Cuentas series de datos en misma grafica
cexT = 0.5                # Point size 
ldwT = 1.7                # Line width
yLimit = c(0.04,1)        
xLimit = c(0,1500)      
names = rep(NA,NoDatos)
colors = rep(NA,NoDatos)
pchs = rep(NA,NoDatos)
ltys = rep(NA,NoDatos)
xLabel = TeX('Energía (keV)')
yLabel = TeX('Eficiencia en agua, $\\epsilon$')
PosicionLabels = 2.5
sizeP = 1

MarginLeft = 2        # cm
MarginRight = 0.4     # cm
MarginTop = 0.2         # cm 
MarginBottom = 2    # cm 

par(mai=c(MarginBottom,MarginLeft,MarginTop,MarginRight)/cm(1)) # in in 
plot(NA, NA, 
     type = 'n',
     xaxt='n', yaxt='n', ann=FALSE, 
     xlim = xLimit, ylim = (yLimit),
     xaxs = "i", yaxs = 'i', 
     lwd = 5, log="y")
axis(2, las=2,cex.axis=sizeP, line = 0)
axis(1, las=1,cex.axis=sizeP, line = 0)
mtext(xLabel, side=1, line=PosicionLabels, cex=sizeP)
mtext(yLabel, side=2, line=PosicionLabels, cex=sizeP)


# G1, 2ml
i = 1
names[i] = TeX('G1, 2 mL')
pchs[i] = 17
ltys[i] = 1
colors[i] = "black"

x = unlist(G1_2ml_agua[,1]);
y = unlist(G1_2ml_agua[,2]);

lines(x,y,lty = ltys[i], lwd = ldwT, col=colors[i])
points(x,y,pch=pchs[i], cex = cexT, col=colors[i])




# G1, 2ml
i = 2
names[i] = TeX('G1, 4 mL')
pchs[i] = 16
ltys[i] = 3
colors[i] = "red"

x = unlist(G1_4ml_agua[,1]);
y = unlist(G1_4ml_agua[,2]);

lines(x,y,lty = ltys[i], lwd = ldwT, col=colors[i])
points(x,y,pch=pchs[i], cex = cexT, col=colors[i])


# G2, 4ml
i = 3
names[i] = TeX('G2, 4 mL')
pchs[i] = 15
ltys[i] = 2
colors[i] = "blue"

x = unlist(G2_4ml_agua[,1]);
y = unlist(G2_4ml_agua[,2]);

lines(x,y,lty = ltys[i], lwd = ldwT, col=colors[i])
points(x,y,pch=pchs[i], cex = cexT, col=colors[i])

# G2, 4ml
i = 4
names[i] = TeX('G3, 2 mL')
pchs[i] = 19
ltys[i] = 2
colors[i] = "orange"

x = unlist(G3_2ml_agua[,1]);
y = unlist(G3_2ml_agua[,2]);

lines(x,y,lty = ltys[i], lwd = ldwT, col=colors[i])
points(x,y,pch=pchs[i], cex = cexT, col=colors[i])


# G4, 4ml
i = 5
names[i] = TeX('G4, 4 mL')
pchs[i] = 4
ltys[i] = 6
colors[i] = "black"

x = unlist(G4_2ml_agua[,1]);
y = unlist(G4_2ml_agua[,2]);

lines(x,y,lty = ltys[i], lwd = ldwT, col=colors[i])
points(x,y,pch=pchs[i], cex = cexT, col=colors[i])

legend("topright",bg="white", legend = names, col = colors, pch = pchs, lty = ltys, cex=sizeP)

dev.off()
graphics.off()
