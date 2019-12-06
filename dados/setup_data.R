##-- Pacotes ----
library(dplyr)
library(stringr)
library(spdep)

source("R/utils.R")

ano <- "2019"
data_exec <- TRUE

##-- Dados limpos, agregados e organizados ----
if(ano <= 2019 & data_exec){
  dados_ds_19 <- etl_data(folder_sih = "outputs/2019/SIH", folder_sia = "outputs/2019/SIA", 
                          file_forma_org = "outputs/DS_INFO/forma_organizacao.csv", 
                          file_ibge = sprintf("outputs/%s/POP/populacao.csv", ano), 
                          file_ans = sprintf("outputs/%s/ANS/ans.csv", ano), 
                          file_cnes = sprintf("outputs/%s/CNES/%s_12.csv", ano, ano),
                          data_exec = data_exec, ano_base = ano)
  
  rstudioapi::restartSession(command = "library(dplyr); library(stringr); library(spdep)")
}

if(ano <= 2018 & data_exec){
  dados_ds_18 <- etl_data(folder_sih = "outputs/2018/SIH", folder_sia = "outputs/2018/SIA", 
                          file_forma_org = "outputs/DS_INFO/forma_organizacao.csv", 
                          file_ibge = sprintf("outputs/%s/POP/populacao.csv", ano), 
                          file_ans = sprintf("outputs/%s/ANS/ans.csv", ano), 
                          file_cnes = sprintf("outputs/%s/CNES/%s_12.csv", ano, ano),
                          data_exec = data_exec, ano_base = ano)
  
  rstudioapi::restartSession(command = "library(dplyr); library(stringr); library(spdep)")
}

if(ano <= 2017 & data_exec){
  dados_ds_17 <- etl_data(folder_sih = "outputs/2017/SIH", folder_sia = "outputs/2017/SIA", 
                          file_forma_org = "outputs/DS_INFO/forma_organizacao.csv", 
                          file_ibge = sprintf("outputs/%s/POP/populacao.csv", ano), 
                          file_ans = sprintf("outputs/%s/ANS/ans.csv", ano), 
                          file_cnes = sprintf("outputs/%s/CNES/%s_12.csv", ano, ano),
                          data_exec = data_exec, ano_base = ano)
  
  rstudioapi::restartSession(command = "library(dplyr); library(stringr); library(spdep)")
}

dados_ds <- mget(x = ls(pattern = "dados_ds")) %>% bind_rows()
rm(list = ls(pattern = "dados_ds_")); gc()
rstudioapi::restartSession(command = "library(dplyr); library(stringr); library(spdep)")

if(data_exec){
  dados_ds <- dados_ds %>%
    group_by(ANO, MES, MUN_ESTAB, CNES_ESTAB, MUN_RES, ALVO) %>%
    summarise(SIGLA_UF_ESTAB = first(SIGLA_UF_ESTAB), 
              NOME_UF_ESTAB = first(NOME_UF_ESTAB), 
              COD_MESO_ESTAB = first(COD_MESO_ESTAB), 
              COD_MICRO_ESTAB = first(COD_MICRO_ESTAB), 
              NOME_MUN_ESTAB = first(NOME_MUN_ESTAB), 
              COD_MICRO_ESTAB = first(COD_MICRO_ESTAB), 
              POP_MUN_ESTAB = first(POP_MUN_ESTAB), 
              NOME_ESTAB = first(NOME_ESTAB),
              SIGLA_UF_RES = first(SIGLA_UF_RES), 
              NOME_UF_RES = first(NOME_UF_RES), 
              COD_MESO_RES = first(COD_MESO_RES), 
              COD_MICRO_RES = first(COD_MICRO_RES),
              NOME_MUN_RES = first(NOME_MUN_RES), 
              COD_MICRO_RES = first(COD_MICRO_RES), 
              POP_MUN_RES = first(POP_MUN_RES), 
              POP_MUN_RES_ANS = first(POP_MUN_RES_ANS),
              ALVO_NAME = first(ALVO_NAME), 
              TIPO = first(TIPO), 
              QTD = sum(QTD), 
              VLR = sum(VLR)) %>%
    ungroup() %>%
    mutate(VLR_MEDIO = VLR/QTD) %>%
    select(ANO, MES, 
           SIGLA_UF_ESTAB, NOME_UF_ESTAB, COD_MESO_ESTAB, COD_MICRO_ESTAB, MUN_ESTAB, NOME_MUN_ESTAB, COD_MICRO_ESTAB, POP_MUN_ESTAB, 
           CNES_ESTAB, NOME_ESTAB,
           SIGLA_UF_RES, NOME_UF_RES, COD_MESO_RES, COD_MICRO_RES, MUN_RES, NOME_MUN_RES, COD_MICRO_RES, POP_MUN_RES, POP_MUN_RES_ANS,
           ALVO, ALVO_NAME, TIPO, 
           QTD, VLR, VLR_MEDIO)
  
  gc()
}

##-- Gera base com os excessos ----
dados_ds <- calcula_excessos(prob = 0.99, 
                             dados_ds = dados_ds, 
                             file_out = sprintf("outputs/excessos_%s.RData", ano), 
                             return_env = TRUE)

gc()

write.table(file = sprintf("outputs/excessos_%s.txt", ano), x = dados_ds, sep = ";", dec = ".", row.names = FALSE)

rm(list = ls())
gc()
