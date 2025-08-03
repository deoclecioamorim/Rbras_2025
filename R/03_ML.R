#'######################
#'########################################################################
#'Código para extração de dados climáticos de arquivos raster
#'
#'Autores:
#'
#'Deoclecio Amorim e Gabriella Araújo
#'
#'############################################################################################
#'                                                                                           #
#'Obtenção de dados geoespaciais e integração com modelos de machine learning                #
#'Estudo de caso:  https://doi.org/10.1002/ecs2.3223                                         #
#'                                                                                           #
#'###########################################################################################
#'
#'
#'
#'
# Limpar do ambiente ---------------------------------------------------------------------
rm(list = ls())      # Remove todos os objetos do ambiente de trabalho
gc(reset = TRUE)     # Libera memória
graphics.off()       # Fecha todas as janelas gráficas abertas


# Pacotes -------------------------------------------------------------------------------------
if(!require(readxl))install.packages("readxl", dep = TRUE)
if(!require(tidyverse))install.packages("tidyverse", dep = TRUE)
if(!require(caret))install.packages("caret", dep = TRUE)
if(!require(randomForest))install.packages("randomForest", dep = TRUE)
if(!require(FSA))install.packages("FSA", dep = TRUE)
if(!require(forcats))install.packages("forcats", dep = TRUE)
if(!require(ggpmisc ))install.packages("ggpmisc", dep = TRUE)


set_reproducibility <- function(seed = 1124) {
  set.seed(seed)
  RNGkind(kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rounding")
}

# Set reproducibility
set_reproducibility()



# Dados-----------------------------------------------------------------------------------
dn15_var_clim <- read_excel("dados/dn15_var_clim.xlsx", sheet = 1)

head(dn15_var_clim) #leitura das primeiras 6 linhas
str(dn15_var_clim) #Estrutura dos dados

#Excluindo as coordenadas
dn15_sc <- dn15_var_clim[,3:7]

# Pre-processamento ------------------------------------------------------------------------------

###Explora e filtrar variáveis com variância zero 
nzv <- caret::nearZeroVar(dn15_sc [,2:ncol(dn15_sc)], saveMetrics= TRUE)
nzv



# Divisão de dados ----------------------------------------------------------------------------

set.seed(0917)

partition <- caret::createDataPartition(dn15_sc$d15n.obs, p = 0.8, list = FALSE)
training <- dn15_sc[partition, ]
testing <- dn15_sc[-partition, ]

# Controle de Treinamento -----------------------------------------------------------------------
fitControl <- caret::trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 7,
  verboseIter = FALSE,
  savePredictions = "all"
)

# Modelos Random Forest e outros ----------------------------------------------------------------
set.seed(1455)

bestmtry <- randomForest::tuneRF(
  training[, 2:5], training$d15n.obs, 
  stepFactor = 1.5, improve = 0.01, ntree = 1000, trace = FALSE
)
mtry <- bestmtry[which.min(bestmtry[, 2]), 1]
mtry

tunegrid <- expand.grid(.mtry = mtry)


mrf<-caret::train(d15n.obs~prec_mean+tmax_mean+tmin_mean,
                  data=training, method="rf",metric='RMSE',tuneGrid=tunegrid,
                  trControl=fitControl)


mrf


# Modelos adicionais (Cubist e GBM) -------------------------------------------------------------
mcubist <- caret::train(
  d15n.obs ~ prec_mean + tmax_mean + tmin_mean, data = training,
  method = 'cubist', trControl = fitControl
)

tunegrid_gbm <- base::expand.grid(
  n.trees = 500, interaction.depth = 30, shrinkage = 0.1, n.minobsinnode = 5
)

mgbm <- caret::train(
  d15n.obs ~ prec_mean + tmax_mean + tmin_mean, data = training,
  method = "gbm", trControl = fitControl, tuneGrid = tunegrid_gbm,
  metric = 'RMSE', maximize = FALSE
)


# Seleção do modelo ----------------------------------------------------------------------------
results <- caret::resamples(list(rf=mrf,cubist=mcubist, gbm=mgbm))
summary(results)

comp.result<-data.frame(results$values)

# Criação de gráficos de comparação-----------------------------------------------------------

rf.performance<-comp.result[,2:4]
cubist.performance<-comp.result[,5:7]
gbm.performance<-comp.result[,8:10]
rf.performance["model"]<-c("RF")
cubist.performance["model"]<-c("CUBIST")
gbm.performance["model"]<-c("GBM")
colnames(rf.performance)<-c("MAE", "RMSE", "R2", "model")
colnames(cubist.performance)<-c("MAE", "RMSE", "R2", "model")
colnames(gbm.performance)<-c("MAE", "RMSE", "R2", "model")

comp.models<-rbind(rf.performance, cubist.performance, gbm.performance)
comp.models

#Test Kruskal-Walis
Datamodels <- mutate(comp.models,
                     Group = factor(model, levels=unique(model)))


#library(FSA)

Summarize(R2 ~ Group,
          data = Datamodels)

kruskal.test(R2 ~ Group, 
             data = Datamodels)


#Define ther of models manually
model_order<-c("RF", "CUBIST", "GBM")
comp.models$model <-factor(comp.models$model, levels = model_order)


# Resultados - boxplot ------------------------------------------------------------------------

#library(forcats) ## package to reorder boxplots
fun_mean <- function(x){return(round(data.frame(y=mean(x),label=mean(x,na.rm=T)),digit=4))}
bplot.r2<- ggplot(comp.models, aes(reorder(model, R2,fun =median,decreasing=T), y = R2)) +
  geom_boxplot(fill = "grey") +
  stat_summary(fun.y=mean, colour="darkred", geom="point", shape=19, size=2,show.legend = FALSE) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-.1) +
  scale_y_continuous(limits=c(0, 1)) +
  xlab("") + # adiciona descrição do eixo x
  ylab(expression (R^{2})) + 
  ggtitle("A")+
  theme_classic()+ # adciona tema "Black and White"
  theme(panel.grid = element_blank(),axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        plot.title=element_text( hjust=.01, vjust=-7),
        plot.margin=unit(c(-0.5,1,0,0.6), "cm"))

bplot.r2

bplot.rmse<-ggplot(comp.models, aes(reorder(model, RMSE, fun = median), y = RMSE)) +
  geom_boxplot(fill = "grey") +
  stat_summary(fun.y=mean, colour="darkred", geom="point", shape=19, size=2,show.legend = FALSE) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-.5)+
  scale_y_continuous(limits=c(0.8, 1.75)) +
  xlab("") + # adiciona descrição do eixo x
  ylab("RMSE") + 
  ggtitle("B")+
  theme_classic()+ # adciona tema "Black and White"
  theme(panel.grid = element_blank(),axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        plot.title=element_text( hjust=.01, vjust=-7))

bplot.mae<-ggplot(comp.models, aes(reorder(model, MAE, fun = mean), y = MAE)) +
  geom_boxplot(fill = "grey") +
  stat_summary(fun.y=mean, colour="darkred", geom="point", shape=19, size=2,show.legend = FALSE) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-.5)+
  xlab("Model") + # adiciona descrição do eixo x
  ylab("MAE") + 
  ggtitle("C")+
  theme_classic()+ # adciona tema "Black and White"
  theme(panel.grid = element_blank(),axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        plot.title=element_text( hjust=.01, vjust=-7))

gridExtra::grid.arrange(bplot.r2, bplot.rmse, bplot.mae)


##Exportar figura
if (!base::dir.exists("figuras")) {  # Verifica se o diretório existe
  base::dir.create("figuras")        # Cria o diretório, se necessário
}

png(filename="figuras/boxplot_comp.jpg", # Nome do arquivo e extensão
    width = 3.5,    # largura
    height = 7,   # Altura
    res= 400,# Resolução em dpi
    family = "serif", #fonte
    units = "in")  # Unidades.
gridExtra::grid.arrange(bplot.r2, bplot.rmse, bplot.mae)
dev.off() # Fecha a janela gráfica



# Explorando o melhor modelo --------------------------------------------------------------------------

varImpPlot(mrf$finalModel,main='Variable Importance Plot: Base Model')

imp<-varImp(mrf$finalModel)
imp$varnames <- rownames(imp) # row names to column
rownames(imp) <- NULL  

imp


varimport<-ggplot(imp, aes(x=reorder(varnames, Overall), y=Overall)) + 
  geom_point() +
  geom_segment(aes(x=varnames,xend=varnames,y=0,yend=Overall)) +
  ylab("%IncMSE") +
  xlab("Variable Name") +
  coord_flip()+
  theme_bw()+
  theme(panel.grid = element_blank(),axis.title = element_text(size = 14),axis.text = element_text(size = 12))

varimport

##Exportar figura

png(filename="figuras/varimport.png", # Nome do arquivo e extensão
    width = 3,    # largura
    height = 4,   # Altura
    res= 400,# Resolução em dpi
    family = "serif", #fonte
    units = "in")  # Unidades.
varimport
dev.off() # Fecha a janela gráfica



# Validação do modelo -------------------------------------------------------------------------

#Valores preditos levando em conta os dados de teste
pred<-as.data.frame(predict(mrf,testing))
pred
comp.obspre.rf<-data.frame(pred$`predict(mrf, testing)`, testing$d15n.obs)


#R2
(cor(comp.obspre.rf$pred..predict.mrf..testing.., comp.obspre.rf$testing.d15n.obs))^2


plot.valid <-ggplot(comp.obspre.rf, aes(x = pred..predict.mrf..testing.., y = testing.d15n.obs)) + 
  geom_point(alpha = 1/2) + 
  geom_abline(intercept = 0, slope = 1, colour = "magenta") +
  stat_smooth(method = lm, colour = "black", se = T) + 
  ggpmisc::stat_poly_eq(mapping = aes(label = paste(after_stat(rr.label), sep = "\", \""))) +
  scale_x_continuous(limits=c(-5, 18), breaks = seq(-5,18,5)) +
  scale_y_continuous(limits=c(-5, 18),breaks = seq(-5,18,5)) +
  xlab(expression (Predicted~delta^{15}~N~"(\u2030)")) + 
  ylab(expression (Observed~delta^{15}~N~"(\u2030)")) + 
  theme_classic()+ # adciona tema "Black and White"
  theme(panel.grid = element_blank(),axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        plot.title=element_text( hjust=.01, vjust=-7))


plot.valid


#Exportar figura
png(filename="figuras/parplot.png.png", # Nome do arquivo e extensão
    width = 6,    # largura
    height = 7,   # Altura
    res= 400,# Resolução em dpi
    family = "serif", #fonte
    units = "in")  # Unidades.
plot.valid
dev.off() # Fecha a janela gráfica



# Isoscape ------------------------------------------------------------------------------------

# Listar todos os arquivos raster na pasta 'raster/' -------------------------------------------
raster_files <- list.files(path = 'raster/', pattern = '\\.tif$', full.names = TRUE)  

# Carregar todos os rasters em uma lista -------------------------------------------------------
raster_list <- lapply(raster_files, terra::rast)  # terra: para carregar cada arquivo raster
raster_list

# Empilhar os rasters em um único objeto SpatRaster --------------------------------------------
r_stack <- terra::rast(raster_list)  # terra: para empilhar todos os rasters em um único objeto

# Verificar os nomes dos rasters ---------------------------------------------------------------
names(r_stack) <- basename(raster_files) %>% tools::file_path_sans_ext()  
print(names(r_stack))  
class(r_stack)

isoscape <- terra::predict(r_stack, mrf, na.rm = TRUE)

class (isoscape)

# Verificar e criar diretório para salvar os rasters, se não existir
if (!dir.exists("isoscape")) {
  dir.create("isoscape")
}

terra::writeRaster(isoscape, filename = 'isoscape/isoscape_dn15_ML.tif', overwrite = TRUE)


# Step 6: Visualize the isoscape
# Plot the raster
terra::plot(isoscape, main = "Isoscape")

