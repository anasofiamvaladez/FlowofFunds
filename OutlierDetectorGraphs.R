library(gridExtra)

# Keep only necessary
# Run Generador de Archivos up to line 120  THEN
rm(list=ls()[-which(ls()=="ExpandedArray" |  ls()=="TotalAssets" | ls()=="TotalInstrumentos" |  ls()=="TotalLiabs")])
  Sectores <- dimnames(ExpandedArray)[[2]]
  Fechas <- dimnames(ExpandedArray)[[4]]
  Instrumentos <- dimnames(ExpandedArray)[[3]]
# Difference <- aaply(ExpandedArray, c(1,2,3), function(x) diff(x,lag = 1))
  TolCook <- 0.13  # Cook distance tolerance
  TolRes <- 2.6    # residual deviation tolerance
  
  
results <- as.data.frame (matrix(NA, nrow= 80, ncol=length(Fechas)))  
i <- 1 

##########################################################    
#                      Plots
##########################################################

newcol <- c("#182b47","#52661C","#51163F","#6C4F1D","#7B9DD1","#BED976","#D473B7","#DAB677","#00727c","#9933ff","#7ABD00","#CC6000","#CCAB00","#A60058","#8c734a","#646567")
newcolINS <- newcol
newcolSEC <- newcol


for (ins in Instrumentos){
  for (sec in Sectores){
    X <- TotalAssets[sec,ins,]
    if (any(X!=0)) {
      X <- diff(X,lag = 1)
      m1 <- lm(X ~ +1 )
      cooksd <- cooks.distance(m1)
      resid <- abs(rstandard(m1))
      if (length(cooksd[which(cooksd>TolCook)])>0 & length(resid[which(resid>TolRes)])>0) {
        t<-names(cooksd[which(cooksd>TolCook)])
        OutputSTR <- paste0("Se encontró un outlier en los activos en ", ins, " de ",sec, " para ", paste(t, collapse = ', '))
        X <- as.matrix(t(TotalAssets[sec,ins,]))
        results[i,]<- X
        colnames(results) <- Fechas
        rownames(results)[i] <- paste0("Activos en ", ins, " de ",sec, " para ", paste(t, collapse = ', '))
        i=i+1
        print(OutputSTR)
 

      } # else no outliere found! 
    } #else all are zeroes
  } # for sectores
} # for instrumentos

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}

setwd("H:\\Proyectos Especiales\\Proyectos\\FyURFE\\1 Fuentes y usos\\Proyectos\\OECD\\proyecto FF 2017\\Whom to Whom Saldos y FE Marzo 2019");
direccion <- "H:\\Proyectos Especiales\\Proyectos\\FyURFE\\1 Fuentes y usos\\Proyectos\\OECD\\proyecto FF 2017\\Whom to Whom Saldos y FE Marzo 2019"
write.csv(results, file="results2.csv")
datevalues <- as.numeric(paste0(rep(2005:2018, each = 4),c("03","06","09","12")))  # Se crean las Fechas  
datevalues <- datevalues[4:56]
results <- as.data.frame(t(results))
numcol <- ncol(results)

i=1 
GG_list<-list()

for(i in 1:numcol){ 
  value <- results[,i]
  ploGG<-ggplot() +  
    geom_bar(data = results, aes(x=Fechas, y=value/1000),stat = "identity", width = 0.7 ) +      
    labs(x="Fecha", y="Miles de Millones de Pesos") +      
    ggtitle(colnames(results)[i]) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    theme(legend.position="none", legend.direction="horizontal",
          legend.title = element_blank(),legend.justification=c(-1,0))+
    theme(axis.line.x = element_line(color="black", size = .5),
          axis.line.y = element_line(color="black", size = .5))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank()) +
    theme(plot.title = element_text(size = 6, face = "bold"),            
          axis.text.x=element_text(colour="black", size = 6),
          axis.text.y=element_text(colour="black", size = 6),
          axis.title=element_text(size=6))+
    scale_y_continuous()
  
  GG_list[[i]]<-ggplotGrob(ploGG)
  
  i=i+1
}     


ml <- marrangeGrob(GG_list, nrow=2, ncol=2)

ggsave("multipage.pdf", ml)


