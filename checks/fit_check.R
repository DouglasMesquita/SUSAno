##-- Pacotes ----
library(dplyr)
library(stringr)
library(ggplot2)
library(cowplot)

source("../relatorios/R/utils.R")

ano <- "2018"

##-- Bases utilizadas ----
load(sprintf('../dados/outputs/excessos_%s.RData', ano))

lista_alvos <- unique(dados_ds$ALVO)
lista_meses <- unique(dados_ds$MES)

dir.create(path = sprintf("outputs/%s", ano), recursive = TRUE, showWarnings = FALSE)

for(i in seq_along(lista_alvos)){
  alvo <- lista_alvos[i]
  list_plots <- list()
  
  for(j in seq_along(lista_meses)){
    mes <- lista_meses[j]
    
    dados_alvo_mes <- dados_ds %>%
      filter(ALVO == alvo & MES == mes & COB_MUN_EBEST != 0)
    
    if(nrow(dados_alvo_mes) > 0){
      
      mes_abreviado <- month.abb[as.numeric(mes)]
      excesso <- real(sum(dados_alvo_mes$VLR_EXCESSO_ESTAB))
      nome_alvo <- str_wrap(str_replace(string = dados_alvo_mes$ALVO_NAME[1], pattern = "\\s{2,}[0-9]+", replacement = ""), width = 50)
      nome_alvo <- paste0(mes_abreviado, ": ", excesso, " \n", nome_alvo)
      
      nbins = ceiling(sqrt(nrow(dados_alvo_mes)))
      fake_data <- data.frame(x = seq(min(dados_alvo_mes$COB_MUN_EBEST), max(dados_alvo_mes$COB_MUN_EBEST), length.out = 1000)) 
      
      meanlog <- dados_alvo_mes$LN_MEAN_COB[1]
      sdlog <- dados_alvo_mes$LN_SD_COB[1]
      limiar <- dados_alvo_mes$LIMIAR[1]
      
      list_plots[[j]] <- ggplot() +
        geom_histogram(data = dados_alvo_mes, aes(x = COB_MUN_EBEST, y = ..density..), bins = nbins, fill = "#4682b3", color = "white", size = 0.05) +
        stat_function(data = fake_data, aes(x), fun = dlnorm, args = list(meanlog = meanlog, sdlog = sdlog), color = "#D62728") +
        geom_vline(xintercept = limiar) +
        scale_x_continuous(labels = function(x) x*100000) +
        xlab("Taxa de cobertura por 100.000 habitantes") + ylab("Densidade") +
        ggtitle(nome_alvo) +
        theme_bw()
    } else{
      list_plots[[j]] <- NULL
    }
  }
  
  if(length(list_plots) > 0){
    png(filename = sprintf("outputs/%s/%s.png", ano, alvo), width = 1200, height = 900)
    print(plot_grid(plotlist = list_plots, ncol = 3))
    dev.off()  
  }
  
}
