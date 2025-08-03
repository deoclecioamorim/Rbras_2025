#'##############################################################################################
#'Código para extração de dados climáticos de arquivos raster
#'
#'Autores:
#'
#'Deoclecio Amorim e Gabriella Araújo
#'
#'O código começa importando dados de amostras de madeira da Amazônia de um arquivo, 
#'filtrando para remover entradas sem coordenadas geográficas ou dados de isótopo. 
#'Em seguida, transforma esses dados em um objeto `sf` para representação espacial, utilizando 
#'o sistema de coordenadas SIRGAS 2000. Ele também carrega um raster, por exemplo, de temperatura
#'média global anual, e extrai esses valores para as localizações das amostras.
#'


# Limpar área de trabalho ---------------------------------------------------------------------
rm(list = ls())      # Base R: Remove todos os objetos do ambiente de trabalho
gc(reset = TRUE)     # Base R: Libera memória
graphics.off()       # Base R: Fecha todas as janelas gráficas abertas

# Pacotes -------------------------------------------------------------------------------------
if(!require(readxl)) install.packages("readxl", dependencies = TRUE)
if(!require(tidyverse)) install.packages("tidyverse", dependencies = TRUE)
if(!require(terra)) install.packages("terra", dependencies = TRUE)
if(!require(writexl)) install.packages("writexl", dependencies = TRUE)
if(!require(sf)) install.packages("sf", dependencies = TRUE)

# Importando o banco de dados amostral ---------------------------------------------------------
#'
#'A planilha deve conter as informações de latitude (Y) e longitude (X). Essa planilha pode
#'ser em .xlsx, .csv ou .txt.
#'
#'O arquivo abaixo refere-se ao isótopo de delta-N15.
#'
dn15 <- read.table("dados/Sena-Souza_soil15n.txt", header = TRUE)  

#Remover linhas com coordenadas XY ou d15n.obs faltando
dn15 <- dn15 %>%
  dplyr::filter(!is.na(x) & !is.na(y) & !is.na(d15n.obs))  # dplyr (tidyverse): para filtrar linhas com valores não nulos

# Converter dados de amostra em objeto sf ------------------------------------------------------
dn15_proj <- dn15 %>%
  sf::st_as_sf(coords = c("x", "y"), crs = 4674)  # sf: para converter o data frame em um objeto espacial


## Plot dos pontos de coleta
plot(dn15_proj[1], pch = 20, col = "black", 
     main = NA, axes = TRUE, graticule = TRUE)




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

# Certificar que o CRS do raster e dos pontos é o mesmo ----------------------------------------
if (!sf::st_crs(dn15_proj) == sf::st_crs(r_stack)) {  # sf: para comparar sistemas de coordenadas
  r_stack <- terra::project(r_stack, sf::st_crs(dn15_proj))  # terra: para reprojetar o raster
}

# Extrair valores dos rasters para os locais das amostras --------------------------------------
extracted_values <- terra::extract(r_stack, dn15_proj,  method="simple")  # terra: para extrair valores de raster

# Combinar os valores extraídos com os dados originais -----------------------------------------
dn15_proj <- cbind(dn15_proj, extracted_values[,-1]) 

# Visualizar os dados combinados ---------------------------------------------------------------
head(dn15_proj) 

# Extrair coordenadas (x, y) do objeto sf
coordinates <- sf::st_coordinates(dn15_proj)

# Converter o objeto sf para data frame e excluir a coluna geometry
dn15_proj_df <- as.data.frame(dn15_proj)
dn15_proj_df <- dn15_proj_df[, !names(dn15_proj_df) %in% "geometry"]  # Exclui a coluna geometry

# Adicionar as coordenadas como as primeiras duas colunas
dn15_proj_df <- cbind(x = coordinates[,1], y = coordinates[,2], dn15_proj_df)
head(dn15_proj_df)

# Exportar os dados no formato .xlsx
if (!base::dir.exists("dados")) {  # Verifica se o diretório existe
  base::dir.create("dados")        # Cria o diretório, se necessário
}

writexl::write_xlsx(dn15_proj_df, "dados/dn15_var_clim.xlsx")  # Exporta os dados


