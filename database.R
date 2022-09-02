###### Carregar o banco de dados #####

# Importante: selecionar o diretorio de trabalho (working directory)
# Isso pode ser feito manualmente: Session > Set Working Directory > Choose Directory

##dados <- read.csv2('Banco de Dados 11.csv') # Carregamento do arquivo csv
##glimpse(dados)                              # Visualizacao de um resumo dos dados


# set projection for states shapefile
#projection <- CRS("+proj=longlat +datum=WGS84 +no_defs")

# import taquari provinces
#taquari <- read_sf(dsn = "imports/estacoes_sed_gee_taquari_2.shp", layer = "estacoes_sed_gee_taquari")
#taquari_geom <- st_geometry(taquari)
# attributes(taquari_geom)

# convert shapefile to Spatial class
#taquari <- as(taquari, 'Spatial')
#taquari <- spTransform(taquari, projection)

# fortify state shapefile.
#taquari <- fortify(taquari)

#####Se quiser visualizar os pontos, descomenta a linha abaixo
#plot(taquari)

#### FUNCTION IMPORT IN SITU DATA ###
importInSituData <- function(){
  insitu_data <- na.omit(fread('taquari_insitu_coxim.csv'))
  insitu_data$ConcentracaoMatSuspensao = as.double(insitu_data$ConcentracaoMatSuspensao)
  insitu_data <- insitu_data[, ':=' (
    site_no = as.character(Codigo),
    sample_date = insitu_data$Data <- dmy(insitu_data$Data),
    #sample_date = Date(Data, '%d/%m/%Y'),
    ConcentracaoMatSuspensao = as.double(ConcentracaoMatSuspensao),
    station_nm = NomeEstacao
  )]
  return(insitu_data)
}

#insitu_data$ConcentracaoMatSuspensao = as.double(insitu_data$ConcentracaoMatSuspensao)

ls_sr_raw <- fread("ssc_data_landsat_coxim.csv")
#View(ls_sr_raw)

#library(conflicted)
#library(dplyr)
#library(plyr)

### FUNCTION IMPORT GEE DATA ###
# Display log axes labels nicely
importLandsatSurfaceReflectanceData <- function() { 
 
  ls_sr_raw <- fread("ssc_data_landsat_coxim.csv")
  
  ######
  ls_sr_data <- 
    na.omit(ls_sr_raw[,
                      ':='(
                        site_no = as.character(Codigo), # Rename columns for simplicity
                        station_nm = Estacao,
                        B1 = B1_median,
                        B2 = B2_median,
                        B3 = B3_median,
                        B4 = B4_median,
                        B5 = B5_median,
                        B6 = B6_median,
                        B7 = B7_median,
                        nd52 = nd_median,
                        num_pix = B2_count,
                        landsat_dt = dmy(date)
                        )], cols = c('B1','B2','B3','B4','B5','B7'))[
                        B1 > 0 & B2 > 0 & B3 > 0 & B4 > 0 & B5 > 0 & B7 > 0][
                          ,':='( 
                            # add new columns with band ratios
                            B2.B1 = B2/B1,
                            B3.B1 = B3/B1,
                            B3.B4 = B3/B4,
                            B4.B1 = B4/B1,
                            B5.B1 = B5/B1,
                            B7.B1 = B7/B1,
                            B3.B2 = B3/B2,
                            B4.B2 = B4/B2,
                            B5.B2 = B5/B2,
                            B7.B2 = B7/B2,
                            B4.B3 = B4/B3,
                            B5.B3 = B5/B3,
                            B7.B3 = B7/B3,
                            B5.B4 = B5/B4,
                            B7.B4 = B7/B4,
                            B7.B5 = B7/B5,
                            B1.2 = B1^2,
                            B2.2 = B2^2,
                            B3.2 = B3^2,
                            B4.2 = B4^2,
                            B5.2 = B5^2,
                            B7.2 = B7^2,
                            Latitude = lat,
                            Longitude = lon,
                            sensor = ifelse(grepl('LT',`system:index`),'Landsat 5','Landsat 7'),
                            ndssi = (B4-B1)/(B4+B1),
                            nsmi = (B3+B2-B1)/(B3+B2+B1),
                            index_montaigner_2014 = (B4+B3)/(B2+B1),
                            bi = sqrt((B3^2+B2^2)/2),
                            B3.B2.B1 = B3/(B2+B1))
                        ][ 
                          # select only columns of interest
                          ,.(station_nm, sensor, site_no, Latitude,Longitude, num_pix, landsat_dt,
                             B1,B2,B3,B4,B5,B6,B7,B2.B1,B3.B1,B4.B1,B5.B1,B7.B1,B3.B2,B4.B2,B3.B4,B5.B2,
                             B7.B2,B4.B3,B5.B3,B7.B3,B5.B4,B7.B4,B7.B5,B1.2,B2.2,B3.2,B4.2,B5.2,B7.2,
                             ndssi, nsmi, index_montaigner_2014, bi, B3.B2.B1
                        )]
  return(ls_sr_data)
} 

#### JOIN SR AND IN SITU DATA ####
joinSRInSituData <- function(ls_sr_data, insitu_data, lagdays){
  # taquari_insitu_raw <- setDT(ls_clean)[all_acy_insitu_daily_mean, roll = lag_days][ # uni-directional join
  ls_sr_insitu_data <- setDT(ls_sr_data)[, ':='( 
    match_dt_start = landsat_dt - lag_days,
    match_dt_end = landsat_dt + lag_days)
  ][
    insitu_data[,':='( match_dt = sample_date)
    ], 
    on = .(site_no == site_no, match_dt_start <= match_dt, match_dt_end >= match_dt)
  ][ 
    # bi-directional join
    !is.na(B1)
  ][ # removes days with in situ SSC but no Landsat image
    , lag_days := as.numeric(difftime(sample_date,landsat_dt),'days')
  ][
    # Add Log10 SSC, squared cols, and remove no values, too cold (B6 < 269) values
    as.double(ConcentracaoMatSuspensao) > 0,
    ':='(
      log10_SSC_mgL = log10(ConcentracaoMatSuspensao),
      ln_SSC_mgL = ln(ConcentracaoMatSuspensao),
      sqr_SSC_mgl = sqrt(ConcentracaoMatSuspensao),
      B1.2 = B1^2,
      B2.2 = B2^2,
      B3.2 = B3^2,
      B4.2 = B4^2,
      B5.2 = B5^2,
      B7.2 = B7^2
    )
  ][
    #retornar esse valor
    !is.na(log10_SSC_mgL) & B6 > 0
  ]
  return(ls_sr_insitu_data)
}

#### SUMMARIZE RIZE DATASET ####
summarizeDataSet <- function(ls_sr_data, insitu_data, ls_sr_insitu_data){
  # Plot number of satellite samples per site as a histogram
  # Renan - Plotar nÃºmero de amostras por estaÃ§Ã£o
  # image <-  ls_sr_data[landsat_dt>="2005-01-01" & landsat_dt<="2021-12-31",.(image = .N), by = .(site_no)]
  
  image_landsat5 <-  ls_sr_data[landsat_dt>=min(insitu_data$sample_date) & landsat_dt<=max(insitu_data$sample_date)
                                & sensor == 'Landsat 5', .(image_landsat5 = .N), by = .(site_no)]
  image_landsat7 <-  ls_sr_data[landsat_dt>=min(insitu_data$sample_date) & landsat_dt<=max(insitu_data$sample_date)
                                & sensor == 'Landsat 7', .(image_landsat7 = .N), by = .(site_no)]
  insitu <- insitu_data[,.(insitu = .N, station_nm), by = .(site_no)]
  match <- ls_sr_insitu_data[,.(match = .N), by = .(site_no)]
  
  dataset_aux1 <- data.table(left_join(image_landsat5, match))
  dataset_aux2 <- data.table(left_join(image_landsat7, insitu))
  dataset <- left_join(dataset_aux1, dataset_aux2)
  
  return(unique(dataset))
}


getLandsatHistoricalSerieByStation <- function(ls_sr_data, insitu_data_site_nos){
  landsat_stations <- ls_sr_data[site_no  %chin% insitu_data_site_nos]
  
  return(getLandsatHistoricalSerie(landsat_stations))
}


getLandsatHistoricalSerie <- function(ls_sr_data){
  #Removendo imagens sobrepostas do landsat 5 e landsat 7
  landsat_stations5 <- ls_sr_data[ls_sr_data$sensor=='Landsat 5' & ls_sr_data$landsat_dt<"2009-12-31", ]
  landsat_stations7 <- ls_sr_data[ls_sr_data$sensor=='Landsat 7' & ls_sr_data$landsat_dt>"2009-12-31", ]
  
  landsat_serie <- rbind(landsat_stations5, landsat_stations7)
  
  return(landsat_serie)
}

