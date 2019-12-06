##-- Calcula os excessos com base nas taxas bayesianas empíricas ----
calcula_excessos <- function(prob = 0.99,
                             dados_ds,
                             file_out,
                             return_env = FALSE) {
  ##-- Estatísticas por município ----
  dados_ds <- dados_ds %>%
    group_by(MES, ALVO, MUN_RES) %>%
    mutate(QTD_MUN = sum(QTD, na.rm = TRUE),
           VLR_MUN = sum(VLR, na.rm = TRUE),
           VLR_MEDIO_MUN = VLR_MUN/QTD_MUN) %>%
    ungroup() %>%
    mutate(QTD_MUN_OUTROS = QTD_MUN - QTD)
  
  gc()
  
  ##-- Cobertura (EBest por mesorregião) ----
  dados_ds_pop0 <- filter(dados_ds, POP_MUN_RES_ANS == 0)
  dados_ds <- filter(dados_ds, POP_MUN_RES_ANS != 0)
  
  dados_ds_pop0 <- dados_ds_pop0 %>%
    mutate(COB_MUN_RAW = 0,
           COB_MUN_EBEST = 0)
  
  dados_ds <- dados_ds %>%
    group_by(ALVO, COD_MESO_RES) %>%
    mutate(COB_MUN_RAW = QTD_MUN/POP_MUN_RES_ANS,
           COB_MUN_EBEST = EBest(n = QTD_MUN,
                                 x = POP_MUN_RES_ANS, # POP_MUN_RES,
                                 family = "poisson")[, 2]) %>%
    ungroup()
  
  ##--  Calculando a média e o desvio padrão do logarítmo das taxas (para ajustar pela lognormal) e os excessos ----
  dados_ds <- dados_ds %>%
    group_by(MES, ALVO) %>%
    mutate(LN_MEAN_COB = rob_mean(log(COB_MUN_EBEST), trim = 0.01, na.rm = T),
           LN_SD_COB = rob_sd(log(COB_MUN_EBEST), trim = 0.01, na.rm = T)) %>%
    ungroup() %>%
    mutate(LIMIAR = qlnorm(p = prob, meanlog = LN_MEAN_COB, sdlog = LN_SD_COB),
           LIMIAR_QTD = LIMIAR*POP_MUN_RES,
           QTD_EXCESSO = if_else(QTD_MUN - LIMIAR_QTD < 0, 0, QTD_MUN - LIMIAR_QTD),
           QTD_EXCESSO_MIN = if_else(QTD_EXCESSO - QTD_MUN_OUTROS < 0, 0, QTD_EXCESSO - QTD_MUN_OUTROS),
           VLR_EXCESSO = QTD_EXCESSO*VLR_MEDIO,
           PROP_ESTAB = QTD/QTD_MUN,
           VLR_EXCESSO_ESTAB = PROP_ESTAB*VLR_EXCESSO,
           VLR_EXCESSO_ESTAB_MIN = QTD_EXCESSO_MIN*VLR_MEDIO)
  
  dados_ds_pop0 <- dados_ds_pop0 %>%
    mutate(LN_MEAN_COB = NA,
           LN_SD_COB = NA,
           LIMIAR = Inf,
           LIMIAR_QTD = Inf,
           QTD_EXCESSO = 0,
           VLR_EXCESSO = 0,
           PROP_ESTAB = QTD/QTD_MUN,
           VLR_EXCESSO_ESTAB = 0)
  
  dados_ds <- bind_rows(dados_ds, dados_ds_pop0)
  rm(dados_ds_pop0); gc()
  
  save(file = file_out, dados_ds)
  
  if(return_env){
    return(dados_ds)
  } else{
    return(sprintf("Dados salvos no arquivo %s", file_out))  
  }
  
}

##-- Organiza, limpa e junta os dados ----
etl_data <- function(folder_sih,
                     folder_sia,
                     file_procedimentos = NULL,
                     file_forma_org,
                     file_ibge,
                     file_ans,
                     file_cnes,
                     data_exec = FALSE,
                     ano_base = NULL) {
  
  classes_sih <- c(rep("character", 10), rep("numeric", 2))
  classes_sia <- c(rep("character", 8), rep("numeric", 2))
  
  sih_files <- list.files(path = folder_sih, full.names = TRUE, pattern = ".txt")
  dados_sih <- lapply(sih_files, read.table, header = TRUE, sep = ";", dec = ".", colClasses = classes_sih)
  
  sia_files <- list.files(path = folder_sia, full.names = TRUE, pattern = ".txt")
  dados_sia <- lapply(sia_files, read.table, sep = ";", header = TRUE, dec = ".", colClasses = classes_sia)
  
  if(!is.null(file_procedimentos)){
    ##-- SIH ----
    if(data_exec){
      dados_sih <- bind_rows(dados_sih) %>%
        # select(-ANO, -MES) %>% rename(ANO = ANO_SAIDA, MES = MES_SAIDA) %>% filter(ANO == ano_base) %>%
        select(-ANO, -MES) %>% rename(ANO = ANO_INTER, MES = MES_INTER) %>% filter(ANO == ano_base) %>%
        group_by(ANO, MES, MUN_ESTAB, CNES_ESTAB, MUN_RES, ALVO) %>%
        summarise(QTD = sum(QTD), VLR = sum(VLR)) %>%
        ungroup() %>%
        mutate(VLR_MEDIO = VLR/QTD)
    } else{
      dados_sih <- bind_rows(dados_sih) %>%
        group_by(ANO, MES, MUN_ESTAB, CNES_ESTAB, MUN_RES, ALVO) %>%
        summarise(QTD = sum(QTD), VLR = sum(VLR)) %>%
        ungroup() %>%
        mutate(VLR_MEDIO = VLR/QTD)
    }
    
    ##-- SIA ----
    if(data_exec){
      dados_sia <- bind_rows(dados_sia)  %>%
        select(-ANO, -MES) %>% rename(ANO = ANO_REALIZ, MES = MES_REALIZ) %>% filter(ANO == ano_base) %>%
        group_by(ANO, MES, MUN_ESTAB, CNES_ESTAB, MUN_RES, ALVO) %>%
        summarise(QTD = sum(QTD), VLR = sum(VLR)) %>%
        ungroup() %>%
        mutate(VLR_MEDIO = VLR/QTD)
    } else{
      dados_sia <- bind_rows(dados_sia)  %>%
        group_by(ANO, MES, MUN_ESTAB, CNES_ESTAB, MUN_RES, ALVO) %>%
        summarise(QTD = sum(QTD), VLR = sum(VLR)) %>%
        ungroup() %>%
        mutate(VLR_MEDIO = VLR/QTD)
    }
    
  } else{
    ##-- SIH ----
    if(data_exec){
      dados_sih <- bind_rows(dados_sih) %>%
        # select(-ANO, -MES) %>% rename(ANO = ANO_SAIDA, MES = MES_SAIDA) %>% filter(ANO == ano_base) %>%
        select(-ANO, -MES) %>% rename(ANO = ANO_INTER, MES = MES_INTER) %>% filter(ANO == ano_base) %>%
        mutate(ALVO = str_sub(string = ALVO, start = 1, end = 6)) %>% # Forma de organização
        group_by(ANO, MES, MUN_ESTAB, CNES_ESTAB, MUN_RES, ALVO) %>%
        summarise(TIPO = "SIH", QTD = sum(QTD), VLR = sum(VLR)) %>%
        ungroup() %>%
        mutate(VLR_MEDIO = VLR/QTD)
    } else{
      dados_sih <- bind_rows(dados_sih) %>%
        mutate(ALVO = str_sub(string = ALVO, start = 1, end = 6)) %>% # Forma de organização
        group_by(ANO, MES, MUN_ESTAB, CNES_ESTAB, MUN_RES, ALVO) %>%
        summarise(TIPO = "SIH", QTD = sum(QTD), VLR = sum(VLR)) %>%
        ungroup() %>%
        mutate(VLR_MEDIO = VLR/QTD)
    }
    
    ##-- SIA ----
    if(data_exec){
      dados_sia <- bind_rows(dados_sia) %>%
        select(-ANO, -MES) %>% rename(ANO = ANO_REALIZ, MES = MES_REALIZ) %>% filter(ANO == ano_base) %>%
        mutate(ALVO = str_sub(string = ALVO, start = 1, end = 6))  %>% # Forma de organização
        group_by(ANO, MES, MUN_ESTAB, CNES_ESTAB, MUN_RES, ALVO) %>%
        summarise(TIPO = "SIA", QTD = sum(QTD), VLR = sum(VLR)) %>%
        ungroup() %>%
        mutate(VLR_MEDIO = VLR/QTD)
    } else{
      dados_sia <- bind_rows(dados_sia) %>%
        mutate(ALVO = str_sub(string = ALVO, start = 1, end = 6))  %>% # Forma de organização
        group_by(ANO, MES, MUN_ESTAB, CNES_ESTAB, ALVO, MUN_RES) %>%
        summarise(TIPO = "SIA", QTD = sum(QTD), VLR = sum(VLR)) %>%
        ungroup() %>%
        mutate(VLR_MEDIO = VLR/QTD)
    }
  }
  
  dados_ds <- bind_rows(dados_sih, dados_sia)
  
  rm(dados_sih, dados_sia); gc()
  
  ##-- Removendo entradas com quantidade ou valor iguais a zero
  dados_ds <- filter(.data = dados_ds, VLR != 0)
  
  ##-- Forma de organização ou procedimento ----
  if(!is.null(file_procedimentos)){
    dados_proc <- read.table(file_procedimentos, header = TRUE, sep = ";", colClasses = "character")
  } else{
    dados_proc <- read.table(file_forma_org, header = TRUE, sep = ";", colClasses = "character")
  }
  
  dados_ds <- dados_ds %>%
    left_join(dados_proc, by = "ALVO")
  
  rm(dados_proc); gc()
  
  ##-- IBGE ----
  dados_ibge <- read.table(file = file_ibge, header = TRUE, sep = ";", dec = ".", colClasses = "character") %>%
    mutate(POP = as.numeric(POP),
           COD_MUN = str_sub(string = COD_MUN, start = 1, end = 6)) %>%
    select(SIGLA_UF, NOME_UF, COD_MESO, COD_MICRO, COD_MUN, NOME_MUN, POP)
  
  dados_ds <- dados_ds %>%
    mutate(MUN_RES = str_replace_all(MUN_RES, pattern = "^53[0-9]{4}", replacement = "530010"),
           MUN_ESTAB = str_replace_all(MUN_ESTAB, pattern = "^53[0-9]{4}", replacement = "530010")) %>%
    left_join(dados_ibge, by = c("MUN_ESTAB" = "COD_MUN")) %>%
    rename(NOME_MUN_ESTAB = NOME_MUN,
           COD_MICRO_ESTAB = COD_MICRO,
           COD_MESO_ESTAB = COD_MESO,
           SIGLA_UF_ESTAB= SIGLA_UF,
           NOME_UF_ESTAB = NOME_UF,
           POP_MUN_ESTAB = POP) %>%
    left_join(dados_ibge, by = c("MUN_RES" = "COD_MUN")) %>%
    rename(NOME_MUN_RES = NOME_MUN,
           COD_MICRO_RES = COD_MICRO,
           COD_MESO_RES = COD_MESO,
           SIGLA_UF_RES= SIGLA_UF,
           NOME_UF_RES = NOME_UF,
           POP_MUN_RES = POP) %>%
    mutate(POP_MUN_ESTAB = if_else(is.na(POP_MUN_ESTAB), 0, POP_MUN_ESTAB),
           POP_MUN_RES = if_else(is.na(POP_MUN_RES), 0, POP_MUN_RES))
  
  rm(dados_ibge); gc()
  
  ##-- ANS ----
  dados_ans <- read.table(file = file_ans, header = TRUE, sep = ";", dec = ".", colClasses = "character") %>%
    filter(TIPO_PLANO == "Médico-hospitalar") %>%
    select(COD_MUN, ATIVOS)
  
  dados_ds <- dados_ds %>%
    left_join(dados_ans, by = c("MUN_RES" = "COD_MUN")) %>%
    mutate(ATIVOS = if_else(is.na(ATIVOS), POP_MUN_RES, as.numeric(ATIVOS))) %>%
    mutate(POP_MUN_RES_ANS = POP_MUN_RES - ATIVOS) %>%
    mutate(POP_MUN_RES_ANS = if_else(is.na(POP_MUN_RES_ANS), 0, POP_MUN_RES_ANS)) %>%
    mutate(POP_MUN_RES_ANS = if_else(POP_MUN_RES_ANS <= 0, POP_MUN_RES, POP_MUN_RES_ANS)) %>%
    select(ANO, MES, 
           SIGLA_UF_ESTAB, NOME_UF_ESTAB, COD_MESO_ESTAB, COD_MICRO_ESTAB, MUN_ESTAB, NOME_MUN_ESTAB, COD_MICRO_ESTAB, POP_MUN_ESTAB, 
           CNES_ESTAB, 
           SIGLA_UF_RES, NOME_UF_RES, COD_MESO_RES, COD_MICRO_RES, MUN_RES, NOME_MUN_RES, COD_MICRO_RES, POP_MUN_RES, POP_MUN_RES_ANS,
           ALVO, ALVO_NAME, TIPO, 
           QTD, VLR, VLR_MEDIO)
  
  rm(dados_ans); gc()
  
  ##-- CNES ----
  dados_cnes <- read.table(file = file_cnes, header = TRUE, sep = ";", dec = ".", colClasses = "character") %>%
    select(CNES_ESTAB, NOME_ESTAB)
  
  dados_ds <- dados_ds %>%
    left_join(dados_cnes, by = "CNES_ESTAB") %>%
    select(ANO, MES, 
           SIGLA_UF_ESTAB, NOME_UF_ESTAB, COD_MESO_ESTAB, COD_MICRO_ESTAB, MUN_ESTAB, NOME_MUN_ESTAB, COD_MICRO_ESTAB, POP_MUN_ESTAB, 
           CNES_ESTAB, NOME_ESTAB,
           SIGLA_UF_RES, NOME_UF_RES, COD_MESO_RES, COD_MICRO_RES, MUN_RES, NOME_MUN_RES, COD_MICRO_RES, POP_MUN_RES, POP_MUN_RES_ANS,
           ALVO, ALVO_NAME, TIPO, 
           QTD, VLR, VLR_MEDIO)
  
  dados_ds <- dados_ds %>%
    mutate(ALVO = paste0(ALVO, "-", str_sub(string = TIPO, start = 3, end = 3)))
  
  rm(dados_cnes); gc()

  return(dados_ds)
}

##-- Baixa os dados de SIH ----
# "ANO_CMPT",      ## Ano de competência
# "MES_CMPT",      ## Mês de competência
# "DT_INTER",      ## Data da internação
# "DT_SAIDA"       ## Data da saída
# "MUNIC_MOV",     ## Município do estabelecimento
# "CNES",          ## CNES do estabelecimento
# "N_AIH",         ## Número da AIH
# "MUNIC_RES",     ## Município de residência do paciente 
# "PROC_REA",      ## Procedimento realizado
# "VAL_TOT"        ## Valor total da AIH

download_sih <- function(ano = "2018", dir_out = "", log_file = "log.txt") {
  ##-- ftp
  ftp <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/200801_/Dados/"
  
  ##-- Variáveis de interesse
  vars_out <- c("ANO", "MES", "ANO_INTER", "MES_INTER", "ANO_SAIDA", "MES_SAIDA", 
                "MUN_ESTAB", "CNES_ESTAB", "ID", "MUN_RES", "ALVO", "VLR", "QTD")
  ano2d <- str_sub(string = ano, start = 3, end = 4)
  
  sih_pattern <- sprintf("^RD[A-Z]{2}%s[0-9]{2}", ano2d)
  
  links_sih <- getURL(ftp, ftp.use.epsv = TRUE, dirlistonly = TRUE) %>%
    str_split(pattern = "\n", simplify = TRUE) %>%
    str_subset(pattern = sih_pattern)
  
  if(length(links_sih) == 0) stop("Não há dados para o ano escolhido")
  
  dir_out <- sprintf("%s/%s/SIH", dir_out, ano)
  dir_dbc <- sprintf("%s/dbc", dir_out, ano)
  dir.create(dir_dbc, recursive = TRUE, showWarnings = FALSE)
  
  for(i in seq_along(links_sih)){
    dbc_file <- sprintf("%s/dbc/%s", dir_out, links_sih[i])
    
    download.file(url = paste0(ftp, links_sih[i]), destfile = dbc_file)
    file_df <- read.dbc(file = dbc_file)
    
    file_df <- file_df %>%
      mutate(QTD = 1,
             ANO_INTER = str_sub(string = DT_INTER, start = 1, end = 4),
             MES_INTER = str_sub(string = DT_INTER, start = 5, end = 6),
             ANO_SAIDA = str_sub(string = DT_SAIDA, start = 1, end = 4),
             MES_SAIDA = str_sub(string = DT_SAIDA, start = 5, end = 6)) %>%
      rename(ANO = ANO_CMPT,
             MES = MES_CMPT,
             MUN_ESTAB = MUNIC_MOV,
             CNES_ESTAB = CNES,
             ID = N_AIH,
             MUN_RES = MUNIC_RES,
             ALVO = PROC_REA,
             VLR = VAL_TOT) %>%
      select(vars_out)
    
    file_df <- file_df %>%
      group_by(ANO, MES, ANO_INTER, MES_INTER, ANO_SAIDA, MES_SAIDA, MUN_ESTAB, CNES_ESTAB, MUN_RES, ALVO) %>%
      summarise(QTD = sum(QTD),
                VLR = sum(VLR))
    
    file_out <- str_replace(string = links_sih[i], pattern = ".dbc", replacement = ".txt")
    write.table(file = sprintf("%s/%s", dir_out, file_out), x = file_df, sep = ";", dec = ".", row.names = FALSE)
    
    ##-- Log
    log_tab <- data.frame(YEAR = ano, TABLE = "SIH", LINK = paste0(ftp, links_sih[i]), PATH = dbc_file, DATE = Sys.time())
    cn <- !file.exists(log_file)
    suppressWarnings(write.table(x = log_tab, file = log_file, append = TRUE, row.names = FALSE, sep = ";", col.names = cn))
  }
  
  res <- sprintf("Dados de SIH salvos na pasta %s", dir_out)
  return(res)
}

##-- Baixa os dados de SIA ----
# "PA_MVM",        ## Data do processamento
# "PA_CMP"         ## Data de realização do procedimento
# "PA_UFMUN",      ## Município do estabelecimento
# "PA_CODUNI",     ## CNES do estabelecimento
# "PA_AUTORIZ",    ## Número da autorização
# "PA_MUNPCN",     ## Município de residência do paciente 
# "PA_PROC_ID",    ## Procedimento realizado
# "PA_VALAPR"      ## Valor total do procedimento
# "PA_QTDAPR"      ## Quantidade realizada

download_sia <- function(ano = "2018", dir_out = "", log_file = "log.txt"){
  ##-- ftp
  ftp <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/"
  
  ##-- Variáveis de interesse
  vars_out <- c("ANO", "MES", "ANO_REALIZ", "MES_REALIZ", "MUN_ESTAB", "CNES_ESTAB", "ID", "MUN_RES", "ALVO", "VLR", "QTD")
  ano2d <- str_sub(string = ano, start = 3, end = 4)
  
  sih_pattern <- sprintf("^PA[A-Z]{2}%s[0-9]{2}", ano2d)
  
  links_sia <- getURL(ftp, ftp.use.epsv = TRUE, dirlistonly = TRUE) %>%
    str_split(pattern = "\n", simplify = TRUE) %>%
    str_subset(pattern = sih_pattern)
  
  if(length(links_sia) == 0) stop("Não há dados para o ano escolhido")
  
  dir_out <- sprintf("%s/%s/SIA", dir_out, ano)
  dir_dbc <- sprintf("%s/dbc", dir_out, ano)
  dir.create(dir_dbc, recursive = TRUE, showWarnings = FALSE)
  
  for(i in seq_along(links_sia)){
    dbc_file <- sprintf("%s/dbc/%s", dir_out, links_sia[i])
    
    download.file(url = paste0(ftp, links_sia[i]), destfile = dbc_file)
    file_df <- read.dbc(file = dbc_file)
    
    file_df <- file_df %>%
      mutate(ANO = str_sub(string = PA_MVM, start = 1, end = 4),
             MES = str_sub(string = PA_MVM, start = 5, end = 6),
             ANO_REALIZ = str_sub(string = PA_CMP, start = 1, end = 4),
             MES_REALIZ = str_sub(string = PA_CMP, start = 5, end = 6)) %>%
      rename(MUN_ESTAB = PA_UFMUN,
             CNES_ESTAB = PA_CODUNI,
             ID = PA_AUTORIZ,
             MUN_RES = PA_MUNPCN,
             ALVO = PA_PROC_ID,
             VLR = PA_VALAPR,
             QTD = PA_QTDAPR) %>%
      select(vars_out)
    
    file_df <- file_df %>%
      group_by(ANO, MES, ANO_REALIZ, MES_REALIZ, MUN_ESTAB, CNES_ESTAB, MUN_RES, ALVO) %>%
      summarise(QTD = sum(QTD),
                VLR = sum(VLR))
    
    file_out <- str_replace(string = links_sia[i], pattern = ".dbc", replacement = ".txt")
    write.table(file = sprintf("%s/%s", dir_out, file_out), x = file_df, sep = ";", dec = ".", row.names = FALSE)
    
    ##-- Log
    log_tab <- data.frame(YEAR = ano, TABLE = "SIA", LINK = paste0(ftp, links_sia[i]), PATH = dbc_file, DATE = Sys.time())
    cn <- !file.exists(log_file)
    suppressWarnings(write.table(x = log_tab, file = log_file, append = TRUE, row.names = FALSE, sep = ";", col.names = cn))
  }
  
  res <- sprintf("Dados de SIA salvos na pasta %s", dir_out)
  return(res)
}

##-- Baixa informações sobre os grupos, subgrupos, forma de organização e procedimentos ----
download_dsinfo <- function(level = "forma_organizacao", dir_out = "", log_file = "log.txt"){
  
  if(!(level %in% c("forma_organizacao", "procedimento"))) 
    "No momento só aceitamos 'forma_organizacao' ou 'procedimento' para esse argumento "
  
  ##-- ftp
  ftp <- "ftp://ftp2.datasus.gov.br/public/sistemas/dsweb/SIHD/Arquivos/Tabela_Unificada_201607_2016_07_12.zip"
  
  dir_out <- sprintf("%s/DS_INFO", dir_out)
  dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)
  
  temp_file <- tempfile(fileext = ".zip")
  download.file(url = ftp, destfile = temp_file)
  unzip(temp_file, exdir = paste0(dir_out, "/docs"))
  
  if(level == "procedimento"){
    layout_procedimento <- read.table(paste0(dir_out, "/docs/tb_procedimento_layout.txt"), sep = ",", header = TRUE) %>%
      filter(Coluna %in% c("CO_PROCEDIMENTO", "NO_PROCEDIMENTO"))
    tab_procedimento <- read.fwf(file = paste0(dir_out, "/docs/tb_procedimento.txt"), 
                                 widths = layout_procedimento$Tamanho, 
                                 row.names = NULL, 
                                 col.names = c("ALVO", "ALVO_NAME"),
                                 fileEncoding = "Windows-1252",
                                 colClasses = "character") %>%
      mutate(ALVO_NAME = str_to_title(ALVO_NAME))
    
    write.table(file = sprintf("%s/procedimentos.csv", dir_out), x = tab_procedimento, 
                sep = ";", dec = ".", row.names = FALSE)
    
    log_tab <- data.frame(YEAR = "ALL", 
                          TABLE = "DS_INFO", 
                          LINK = ftp, 
                          PATH = sprintf("%s/procedimentos.csv", dir_out), 
                          DATE = Sys.time())
    
    cn <- !file.exists(log_file)
    suppressWarnings(write.table(x = log_tab, file = log_file, append = TRUE, row.names = FALSE, sep = ";", col.names = cn))
  }
  
  if(level == "forma_organizacao"){
    layout_forma_org <- read.table(paste0(dir_out, "/docs/tb_forma_organizacao_layout.txt"), sep = ",", header = TRUE) %>%
      filter(Coluna %in% c("CO_FORMA_ORGANIZACAO", "NO_FORMA_ORGANIZACAO"))
    tab_forma_org <- read.fwf(file = paste0(dir_out, "/docs/tb_forma_organizacao.txt"), 
                              widths = layout_forma_org$Fim, 
                              row.names = NULL, 
                              col.names = c("ALVO", "ALVO_NAME"),
                              fileEncoding = "Windows-1252",
                              colClasses = "character") %>%
      mutate(ALVO_NAME = str_to_title(ALVO_NAME))
    
    file_out <- sprintf("%s/forma_organizacao.csv", dir_out)
    write.table(file = file_out, x = tab_forma_org, 
                sep = ";", dec = ".", row.names = FALSE)
    
    ##-- Log
    log_tab <- data.frame(YEAR = "ALL", TABLE = "DS_INFO", LINK = ftp, PATH = file_out, DATE = Sys.time())
    cn <- !file.exists(log_file)
    suppressWarnings(write.table(x = log_tab, file = log_file, append = TRUE, row.names = FALSE, sep = ";", col.names = cn))
  }
  
  res <- sprintf("Dados de informações dos procedimentos salvos na pasta %s", dir_out)
  return(res)
}

##-- Baixa informações dos estabelecimentos ----
download_cnes <- function(ano = "2018", dir_out = "", log_file = "log.txt"){
  ##-- ftp
  ftp <- "ftp://ftp.datasus.gov.br/cnes/"
  
  cnes_pattern <- sprintf("BASE_DE_DADOS_CNES_%s[0-9]{2}.ZIP", ano)
  
  links_cnes <- getURL(ftp, ftp.use.epsv = TRUE, dirlistonly = TRUE) %>%
    str_split(pattern = "\n", simplify = TRUE) %>%
    str_subset(pattern = cnes_pattern)
  
  vars_out <- c("ANO", "MES", "CNES_ESTAB", "NOME_ESTAB", "LOGRADOURO_ESTAB", "NUMERO_ESATB", "CEP_ESTAB")
  
  dir_out <- sprintf("%s/%s/CNES", dir_out, ano)
  dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)
  
  for(i in seq_along(links_cnes)){
    temp_file <- tempfile(fileext = ".zip")
    temp_dir <- tempdir()
    
    download.file(url = paste0(ftp, links_cnes[i]), destfile = temp_file)
    unzip(temp_file, exdir = temp_dir)
    file_csv <- list.files(path = temp_dir, pattern = "tbEstabelecimento", full.names = TRUE)
    
    file_df <- read.table(file = file_csv, header = TRUE, sep = ";", colClasses = "character")
    unlink(file_csv)
    
    data_base <- str_sub(string = str_extract(string = basename(file_csv), pattern = "[0-9]+"), start = c(1, 5), end = c(4, 6))
    
    file_df <- file_df %>%
      mutate(ANO = data_base[1],
             MES = data_base[2],
             NO_FANTASIA = str_to_title(NO_FANTASIA),
             NO_LOGRADOURO = str_to_title(NO_LOGRADOURO)) %>%
      rename(CNES_ESTAB = CO_CNES,
             NOME_ESTAB = NO_FANTASIA,
             LOGRADOURO_ESTAB = NO_LOGRADOURO,
             NUMERO_ESATB = NU_ENDERECO,
             CEP_ESTAB = CO_CEP) %>%
      select(vars_out)
    
    file_out <- sprintf("%s/%s.csv", dir_out, paste0(data_base, collapse = "_"))
    write.table(file = file_out, x = file_df, 
                sep = ";", dec = ".", row.names = FALSE)
    
    log_tab <- data.frame(YEAR = ano, TABLE = "CNES", LINK = paste0(ftp, links_cnes[i]), PATH = file_out, DATE = Sys.time())
    cn <- !file.exists(log_file)
    suppressWarnings(write.table(x = log_tab, file = log_file, append = TRUE, row.names = FALSE, sep = ";", col.names = cn))
  }
  
  res <- sprintf("Dados do CNES salvos na pasta %s", dir_out)
  return(res)
}

##-- Baixa dados populacionais ----
download_ibge <- function(ano = "2018", dir_out = "", log_file = "log.txt"){
  ##-- ftp
  ftp_mun <- sprintf("ftp://geoftp.ibge.gov.br/organizacao_do_territorio/estrutura_territorial/divisao_territorial/%s/DTB_%s.zip", ano, ano)
  ftp_pop <- sprintf("ftp://ftp.ibge.gov.br/Estimativas_de_Populacao/Estimativas_%s/", ano)
                     "ftp://ftp.ibge.gov.br/Estimativas_de_Populacao/Estimativas_2018/"
  ftp_pop <- ftp_pop %>%
    getURL(ftp.use.epsv = TRUE, dirlistonly = TRUE) %>%
    str_split(pattern = "\n", simplify = TRUE) %>%
    str_subset(pattern = sprintf("estimativa_TCU_%s_[0-9]{8}.xls", ano, ano)) %>%
    paste0(ftp_pop, .)  
  
  ##-- + Dados de regiões
  dir_out <- sprintf("%s/%s/POP/original", dir_out, ano)
  dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)
  
  temp_file <- tempfile(fileext = ".zip")
  download.file(url = ftp_mun, destfile = temp_file)
  unzip(temp_file, exdir = dir_out)
  
  files_out <- list.files(dir_out, pattern = "MUNICIPIO.{1,}xls", full.names = TRUE)
  dados_mun <- read_xls(path = files_out)
  names(dados_mun) <- c("COD_UF", "NOME_UF", "COD_MESO", "NOME_MESO", "COD_MICRO", "NOME_MICRO", "MUN", "COD_MUN", "NOME_MUN")
  
  ##-- + Dados populacionais
  temp_file <- tempfile(fileext = ".xls")
  download.file(url = ftp_pop, destfile = temp_file)
  
  dados_ibge <- read_xls(path = temp_file, sheet = 2, 
                         skip = 2, n_max = 5570,                     # Pulando o cabeçalho e o rodapé
                         col_names = c("SIGLA_UF", "COD_UF", "MUN", "NOME_MUN", "POP")) %>%
    mutate(COD_MUN = paste0(COD_UF, MUN),
           POP = str_remove(string = POP, pattern = "\\s?\\([0-9]+\\)"),
           POP = str_remove(string = POP, pattern = "\\.")) %>%
    select(SIGLA_UF, COD_MUN, POP)
  
  ##-- + Juntando as bases do ibge e salvando-as
  dados_pop <- dados_mun %>% 
    left_join(dados_ibge, by = "COD_MUN")
  
  file_out <- sprintf("%s/populacao.csv", dirname(dir_out))
  write.table(x = dados_pop, file = file_out, sep = ";", row.names = FALSE)
  
  ##-- Log
  log_tab <- data.frame(YEAR = ano, TABLE = "IBGE", LINK = ftp_mun, PATH = file_out, DATE = Sys.time())
  cn <- !file.exists(log_file)
  suppressWarnings(write.table(x = log_tab, file = log_file, append = TRUE, row.names = FALSE, sep = ";", col.names = cn))
  log_tab <- data.frame(YEAR = ano, TABLE = "IBGE", LINK = ftp_pop, PATH = file_out, DATE = Sys.time())
  suppressWarnings(write.table(x = log_tab, file = log_file, append = TRUE, row.names = FALSE, sep = ";", col.names = FALSE))
  
  res <- sprintf("Dados do IBGE salvos na pasta %s", dirname(dir_out))
  return(res)
}

##-- Baixa dados populacionais ----
download_ans <- function(ano = "2018", mes = "01", dir_out = "", log_file = "log.txt"){
  ##-- ftp
  ftp <- sprintf("http://ftp.dadosabertos.ans.gov.br/FTP/PDA/informacoes_consolidadas_de_beneficiarios/%s%s/", ano, mes)
  
  links_ans <- read_html(ftp) %>% html_table()  %>% .[[1]] %>% .$Name %>% str_subset(pattern = ".zip")
  
  vars_ans <- c("ANO", "MES", "COD_MUN", "TIPO_PLANO", "ATIVOS", "ADERIDOS", "CANCELADOS")
  
  dir_out <- sprintf("%s/%s/ANS/original", dir_out, ano)
  dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)
  
  for(i in seq_along(links_ans)){
    temp_file <- tempfile(fileext = ".zip")
    temp_dir <- tempdir()
    
    download.file(url = paste0(ftp, links_ans[i]), destfile = temp_file)
    unzip(temp_file, exdir = temp_dir)
    file_csv <- list.files(path = temp_dir, pattern = "ben[0-9]{6}_[A-Z]{2}", full.names = TRUE)
    
    file_df <- read.table(file = file_csv, header = TRUE, sep = ";", fileEncoding = "Windows-1252", comment.char = "", quote = "\"")
    unlink(file_csv)
    
    data_base <- str_sub(string = str_extract(string = basename(file_csv), pattern = "[0-9]+"), start = c(1, 5), end = c(4, 6))
    
    file_df <- file_df %>%
      mutate(ANO = data_base[1],
             MES = data_base[2]) %>%
      rename(COD_MUN = CD_MUNICIPIO,
             TIPO_PLANO = COBERTURA_ASSIST_PLAN,
             ATIVOS = QT_BENEFICIARIO_ATIVO,
             ADERIDOS = QT_BENEFICIARIO_ADERIDO,
             CANCELADOS = QT_BENEFICIARIO_CANCELADO) %>%
      select(vars_ans) %>%
      group_by(ANO, MES, COD_MUN, TIPO_PLANO) %>%
      summarise(ATIVOS = sum(ATIVOS),
                ADERIDOS = sum(ADERIDOS),
                CANCELADOS = sum(CANCELADOS))
    
    file_out <- sprintf("%s/%s", dir_out, basename(file_csv))
    write.table(file = file_out, x = file_df, 
                sep = ";", dec = ".", row.names = FALSE)
    
    log_tab <- data.frame(YEAR = ano, TABLE = "ANS", LINK = paste0(ftp, links_ans[i]), PATH = file_out, DATE = Sys.time())
    cn <- !file.exists(log_file)
    suppressWarnings(write.table(x = log_tab, file = log_file, append = TRUE, row.names = FALSE, sep = ";", col.names = cn))
  }
  
  files_ans <- list.files(dir_out, full.names = TRUE)
  data_ans <- lapply(files_ans, read.table, header = TRUE, sep = ";", dec = ".") %>%
    bind_rows()
  
  write.table(file = sprintf("%s/ans.csv", dirname(dir_out)), x = data_ans, sep = ";", dec = ".")
  
  res <- sprintf("Dados da ANS salvos na pasta %s", dirname(dir_out))
  return(res)
}

##-- Calcula o desvio padrão excluíndo os valores extremos ----
rob_sd <- function(x, trim, ...){
  quants <- quantile(x = x, probs = c(trim, 1-trim), ...)
  x <- x[x >= quants[1] & x <= quants[2]]
  sd_trim <- sd(x, ...)
  
  return(sd_trim)
}

##-- Calcula a média excluíndo os valores extremos ----
rob_mean <- function(x, trim, ...){
  quants <- quantile(x = x, probs = c(trim, 1-trim), ...)
  x <- x[x >= quants[1] & x <= quants[2]]
  mean_trim <- mean(x, ...)
  
  return(mean_trim)
}
