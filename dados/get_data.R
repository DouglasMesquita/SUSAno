##-- Pacotes ----
library(RCurl)
library(rvest)
library(dplyr)
library(stringr)
library(read.dbc)
library(readxl)

source("R/utils.R")

ano <- "2019"
dir_out <- "outputs"
log_file <- sprintf("documentos/log_%s.txt", ano)
unlink(log_file)

##-- SIH ----
download_sih(ano = ano, dir_out = dir_out, log_file = log_file)

##-- SIA ----
download_sia(ano = ano, dir_out = dir_out, log_file = log_file)

##-- Informações dos procedimentos ----
download_dsinfo(level = "forma_organizacao", dir_out = dir_out, log_file = log_file)
# download_dsinfo(level = "procedimento", dir_out = dir_out, log_file = log_file)
  
##-- Informações do CNES ----
download_cnes(ano = ano, dir_out = dir_out, log_file = log_file)

##-- IBGE ----
download_ibge(ano = ano, dir_out = dir_out, log_file = log_file)

##-- ANS ----
download_ans(ano = ano, mes = "01", dir_out = dir_out, log_file = log_file)
