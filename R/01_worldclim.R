#'##############################################################################################
#'Código para extração de dados climaticos do Worldclim: https://www.worldclim.org/
#'
#'Variaveis climaticas: "tmin", "tmax", "tavg", "prec", "wind", "vapr" e "bio"
#'
#'Prof.Deoclecio Amorim e Gabriella Araújo
#'
#'O código R a seguir tem como objetivo principal manipular dados climáticos para a região 
#'da Amazônia. Primeiramente, ele utiliza diversos pacotes para carregar, limpar e 
#'processar informações geoespaciais e rasterizadas. Os passos principais incluem a 
#'leitura dos biomas de 2019, filtragem para selecionar apenas a Amazônia, e a obtenção 
#'e recorte de dados de temperatura média global para a América do Sul. Após o recorte
#'para a região desejada, o código cria um stack de camadas de temperatura média e realiza
#'um recorte específico para a Amazônia. Por fim, calcula a média da temperatura para todo 
#'o período e salva o resultado como um arquivo raster em formato TIFF.
#'
#'
# Limpar área de trabalho ---------------------------------------------------------------------
rm(list = ls())      # Remove todos os objetos do ambiente de trabalho
gc(reset = TRUE)     # Libera memória
graphics.off()       # Fecha todas as janelas gráficas abertas


# Pacotes -------------------------------------------------------------------------------------
if(!require(readxl))install.packages("readxl", dep = TRUE)
if(!require(tidyverse))install.packages("tidyverse", dep = TRUE)
if(!require(terra))install.packages("terra", dep = TRUE)
if(!require(geodata))install.packages("geodata", dep = TRUE)
if(!require(geobr))install.packages("geobr", dep = TRUE)
if(!require(sf))install.packages("sf", dep = TRUE)
if(!require(sp))install.packages("sp", dep = TRUE)



# Exemplo: temperatura média  -----------------------------------------------------------------
# Baixar dados de temperatura média do mundo
tavg_mundo <- geodata::worldclim_global(var = "tavg", res = 10, path = "worldclim")  # Função 'worldclim_global' do pacote 'geodata'

# Verificar a classe do objeto tavg_mundo
print(class(tavg_mundo))  # Funções 'print' e 'class' do pacote base

# Checar o Sistema de Referência de Coordenadas (CRS) do raster
print(terra::crs(tavg_mundo))  # Função 'crs' do pacote 'terra'

# Definir a extensão para a América
ext_america <- terra::ext(-82, -34, -60, 15)  # Função 'ext' do pacote 'terra'

# Recortar o raster para a extensão da América
tavg_america <- terra::crop(tavg_mundo, ext_america)  # Função 'crop' do pacote 'terra'

# Plotar a camada de temperatura média recortada para a América (por exemplo, mês 10)
terra::plot(tavg_america[[1]], main = "Temperatura Média - Mês 10")  # Função 'plot' do pacote 'terra'

# Reprojetar o raster para SIRGAS2000 (EPSG:4674)
tavg_america_sirgas <- terra::project(tavg_america, "EPSG:4674")  # Função 'project' do pacote 'terra'

# Calcular a temperatura média de todo o período
tavg_mean <- terra::app(tavg_america_sirgas, mean)  # Função 'app' do pacote 'terra' e 'mean' do pacote base

# Plotar a temperatura média
terra::plot(tavg_mean, main = "Temperatura Média Anual")  # Função 'plot' do pacote 'terra'

# Verificar a classe do objeto tavg_mean
print(class(tavg_mean))  # Funções 'print' e 'class' do pacote base

# Criar diretório para salvar o raster, se não existir
if (!dir.exists("raster")) {      # Funções 'dir.exists' e 'dir.create' do pacote base
  dir.create("raster")            # Função 'dir.create' do pacote base
}

# Salvar o raster de temperatura média
terra::writeRaster(tavg_mean, filename = 'raster/tavg_mean.tif', overwrite = TRUE)

# Exemplo: temperatura máxima  -----------------------------------------------------------------

# Baixar dados de temperatura média do mundo
tmax_mundo <- geodata::worldclim_global(var = "tmax", res = 10, path = "worldclim")  # Função 'worldclim_global' do pacote 'geodata'

# Verificar a classe do objeto tmax_mundo
print(class(tmax_mundo))  # Funções 'print' e 'class' do pacote base

# Checar o Sistema de Referência de Coordenadas (CRS) do raster
print(terra::crs(tmax_mundo))  # Função 'crs' do pacote 'terra'

# Definir a extensão para a América
ext_america <- terra::ext(-82, -34, -60, 15)  # Função 'ext' do pacote 'terra'

# Recortar o raster para a extensão da América
tmax_america <- terra::crop(tmax_mundo, ext_america)  # Função 'crop' do pacote 'terra'

# Plotar a camada de temperatura média recortada para a América (por exemplo, mês 10)
terra::plot(tmax_america[[10]], main = "Temperatura Média - Mês 10")  # Função 'plot' do pacote 'terra'

# Reprojetar o raster para SIRGAS2000 (EPSG:4674)
tmax_america_sirgas <- terra::project(tmax_america, "EPSG:4674")  # Função 'project' do pacote 'terra'

# Calcular a temperatura média de todo o período
tmax_mean <- terra::app(tmax_america_sirgas, mean)  # Função 'app' do pacote 'terra' e 'mean' do pacote base

# Plotar a temperatura média
terra::plot(tmax_mean, main = "Temperatura Média Anual")  # Função 'plot' do pacote 'terra'

# Verificar a classe do objeto tmax_mean
print(class(tmax_mean))  # Funções 'print' e 'class' do pacote base

# Criar diretório para salvar o raster, se não existir
if (!dir.exists("raster")) {      # Funções 'dir.exists' e 'dir.create' do pacote base
  dir.create("raster")            # Função 'dir.create' do pacote base
}

# Salvar o raster de temperatura média
terra::writeRaster(tmax_mean, filename = 'raster/tmax_mean.tif', overwrite = TRUE)



# Código automatizado -------------------------------------------------------------------------

# Definir o caminho para salvar os dados do WorldClim
data_path <- "worldclim"

# Lista de variáveis climáticas que deseja processar
vars <- c("tmax", "tmin", "prec")

# Extensão para a América do Sul
ext_america <- terra::ext(-82, -34, -60, 15)  # Longitude e latitude da extensão

# Verificar e criar diretório para salvar os rasters, se não existir
if (!dir.exists("raster")) {
  dir.create("raster")
}

# Loop para processar cada variável climática
for (var in vars) {
  # Baixar dados climáticos para a variável
  climate_data <- geodata::worldclim_global(var = var, res = 10, path = data_path)
  
  # Verificar a classe do objeto baixado (opcional)
  print(paste("Processando:", var))
  print(class(climate_data))
  
  # Recortar o raster para a extensão da América do Sul
  climate_america <- terra::crop(climate_data, ext_america)
  
  # Reprojetar o raster para SIRGAS2000 (EPSG:4674)
  climate_america_sirgas <- terra::project(climate_america, "EPSG:4674")
  
  # Calcular a média da variável climática
  climate_mean <- terra::app(climate_america_sirgas, mean)
  
  # Plotar o raster da média
  terra::plot(climate_mean, main = paste("Média Anual -", var))
  
  # Salvar o raster de média no diretório 'raster'
  output_filename <- paste0("raster/", var, "_mean.tif")
  terra::writeRaster(climate_mean, filename = output_filename, overwrite = TRUE)
  
  print(paste("Arquivo salvo em:", output_filename))
}

print("Processamento concluído!")
