source("codigos/00_pacotes.R")
source("codigos/00_variaveis.R")


# funcao ------------------------------------------------------------------

##
## Para obter as coordenadas via google maps 
##
get_coordinates_from_google <- function(endereco, cidade, estado, cep, 
                                        pais = "Brazil") {
  
  endereco_parsed <- postal_parse(endereco) %>% 
    t %>% 
    as.data.frame()
  endereco <- paste(endereco_parsed$road, endereco_parsed$house_number)
  
  base_url <- "https://maps.googleapis.com/maps/api/geocode/json"
  query_params <- list(
    endereco = endereco,
    components = paste0(
      "locality:", cidade, "|",
      "administrative_area:", estado, "|",
      "postal_code:", cep, "|",
      "country:", pais
    ),
    key = api_gmap
  )
  
  response <- GET(url = base_url, query = query_params)
  result <- content(response, "parsed")
  
  if(result$status == "ZERO_RESULTS"){
    query_params <- list(
      adress = endereco,
      components = paste0(
        "locality:", cidade, "|",
        "administrative_area:", estado, "|",
        "country:", pais
      ),
      key = api_gmap
    )
    response <- GET(url = base_url, query = query_params)
    result <- content(response, "parsed")
  }
  if (result$status == "OK") {
    lat <- result$results[[1]]$geometry$location$lat
    lng <- result$results[[1]]$geometry$location$lng
    return(c(lat, lng))
  } else {
    warning(paste("Falhou. Status:", result$status, "Mensagem:", 
                  result$error_message))
    return(NULL)
  }
}

##
## Para obter as coordenadas de um local de votação utilizando seus dados
## batendo com a tabela do Cadastro Nacional de Endereços para Fins Estatísticos
## e se não conseguir utiliza a função acima
get_ll_lv <- function(lv, tab_cnf) {
  
  # CEP
  cep <- lv$NR_CEP
  
  # Os 5 primeiros dígitos do CEP
  cep5 <- substring(cep, 1, 5)
  
  # Endereço sem acentos
  endereco <- lv$DS_ENDERECO %>% iconv(to = "ASCII//TRANSLIT")
  
  # Nome do local de votação sem acentos
  nome <- lv$NM_LOCAL_VOTACAO_ORIGINAL %>%  iconv(to = "ASCII//TRANSLIT")
  
  # Nome da cidade e da UF
  cidade <- lv$NM_MUNICIPIO
  uf <- lv$SG_UF
  
  # Se temos o CEP
  if(!is.na(cep)){
    
    # Esses serão os filtros utilizados
    # 1. Verifica se tem algum endereço com os 5 primeiros dígitos do CEP
    # 2. Dentre os endereços do passo 1. verifica se tem algum endereço com o 
    #    CEP completo
    # 3. Dentre os endereços do passo 2. verifica se tem algum endereço que 
    #    contém o logradouro do local de votação
    # 4. Dentre os endereços do passo 3. verifica se tem algum endereço com o 
    #    número do local de votação
    # 5. Dentre os endereços do passo 3. verifica se tem algum endereço com um 
    #    nome igual ao do local de votação
    filters <- list(
      function(df) df %>% filter(CEP5 == cep5),
      function(df) df %>% filter(CEP == cep),
      function(df) df %>% filter(sapply(NOM_SEGLOGR, grepl, endereco)),
      function(df) df %>% filter(str_detect(endereco, paste0("\\b", NUM_ENDERECO, "\\b"))),
      function(df) df %>% filter(stringdist(DSC_ESTABELECIMENTO, nome) <= 5)
    )
    
    
    # Vai filtrando a tabela de acordo com os filtros    
    result <- tab_cnf
    for (i in seq_along(filters)) {
      filtered <- filters[[i]](result)
      
      # Se não tiver nenhum endereço já no primeiro filtro, se recorre ao Google
      if (i == 1 && nrow(filtered) == 0) {
        ll <- get_coordinates_from_google(endereco = endereco, cidade = cidade, 
                                          estado = uf, cep = cep)
        return(ll)
      }
      # Se não tiver nenhum resultado depois do filtro ficamos o resultado 
      # anterior
      # Se tiver algum resultado com somente uma linha paramos e ficamos om 
      # esse resutado
      if (nrow(filtered) == 1) {
        ll <- c(filtered$LATITUDE, filtered$LONGITUDE)
        return(ll)
      } else if (nrow(filtered) > 0) {
        result <- filtered
      }
    }
    
    # Se pega o endereço com valor mais próximo da mediana dos endereços obtidos
    # após o filtro
    if (nrow(result) > 0) {
      
      # Mediana 
      median_lat <- median(result$LATITUDE)
      median_lon <- median(result$LONGITUDE)
      
      # Find the point in the dataset closest to the median
      closest_point <- result %>%
        mutate(dist = sqrt((LATITUDE - median_lat)^2 + (LONGITUDE - median_lon)^2)) %>%
        arrange(dist) %>%
        slice(1)
      ll <- c(closest_point$LATITUDE, closest_point$LONGITUDE)
      return(ll)
    } 
  } else {
    # Se não tem CEP pega via google maps
    ll <- get_coordinates_from_google(endereco = endereco, 
                                      cidade = cidade, 
                                      estado = uf, 
                                      cep = cep)
    return(ll)
  }
}




# tabelas -----------------------------------------------------------------

# Tabela com endereços e suas geolocalizações segundo o censo
cnfes_files <- dir_cnfe %>% 
  list.files(full.names = TRUE, pattern = "csv$") 


# Tabela para fazer a conversão de IDs ibge e TSE
tse_ibge <- read.csv(path_tse_ibge)

n_ufs <- length(cnfes_files)

# Regex para pergamos os arquivos csvs das eleições
rgx_eleicoes <- seq(2012, 2024, 2) %>% 
  paste(collapse = "|") %>% 
  paste0("(", ., ").*.csv")


# Tabela com locais de votação
locais_de_votacao <- dir_eleitorado %>% 
  list.files(full.names = TRUE, pattern = rgx_eleicoes) %>%
  map(fread,
      select = c("DS_ENDERECO", "NM_LOCAL_VOTACAO_ORIGINAL",
                 "SG_UF", "CD_MUNICIPIO", "NM_MUNICIPIO", 
                 "NR_ZONA", "NR_SECAO", "NR_LOCAL_VOTACAO", 
                 "NR_TURNO", "AA_ELEICAO", "NR_CEP",
                 "NR_LATITUDE", "NR_LONGITUDE"),  
      colClasses = "character", 
      encoding = "Latin-1") %>% 
  bind_rows() %>% 
  mutate(CD_MUNICIPIO = as.numeric(CD_MUNICIPIO)) %>% 
  mutate(id_lv = stringr::str_c(NR_ZONA, NR_SECAO, AA_ELEICAO, 
                                NR_TURNO, CD_MUNICIPIO, sep = "_")) %>% 
  distinct(id_lv, .keep_all = TRUE)


# Alguns locais de votação estão nos resultados, mas não aparecem na tabela
# de locais de votação. Aqui incuímos esses locais na tabela
locais_de_votacao_por_resultado <- dir_resultado %>% 
  list.files(full.names = TRUE, pattern = rgx_eleicoes) %>% 
  map(fread,
      select = c("DS_LOCAL_VOTACAO_ENDERECO", "NM_LOCAL_VOTACAO", 
                 "SG_UF", "CD_MUNICIPIO", "NM_MUNICIPIO", 
                 "NR_ZONA", "NR_SECAO", 
                 "NR_TURNO", "ANO_ELEICAO"),
      colClasses = "character", encoding = "Latin-1", 
      .progress = TRUE,) %>% 
  bind_rows() %>% 
  mutate(CD_MUNICIPIO = as.numeric(CD_MUNICIPIO)) %>% 
  rename(DS_ENDERECO = DS_LOCAL_VOTACAO_ENDERECO, 
         NM_LOCAL_VOTACAO_ORIGINAL = NM_LOCAL_VOTACAO, 
         AA_ELEICAO = ANO_ELEICAO) %>% 
  distinct() %>% 
  mutate(id_lv = stringr::str_c(NR_ZONA, NR_SECAO, AA_ELEICAO, 
                                NR_TURNO, CD_MUNICIPIO, sep = "_")) %>% 
  distinct(id_lv, .keep_all = TRUE) %>%
  filter(!id_lv %in% locais_de_votacao$id_lv)

locais_de_votacao <- locais_de_votacao %>% 
  bind_rows(locais_de_votacao_por_resultado) %>% 
  mutate_at(vars(NR_LATITUDE, NR_LONGITUDE), ~ ifelse(.x == "-1", NA, .x)) %>% 
  mutate_at(vars(NR_LATITUDE, NR_LONGITUDE), as.numeric)       




# faz a geolocalização ----------------------------------------------------

# Para cada UF...
for(i_uf in seq_len(n_ufs)){
  
  # Se pega o path dos endereçoes da UF 
  cf <- cnfes_files[i_uf]
  
  # Se obtem a sigla da UF
  uf <- cf %>% 
    str_extract("/\\d+_([[:upper:]]{2,2})\\.csv", group = 1)
  
  
  glue("{uf} ({i_uf}/{n_ufs})") %>% cat("\n")
  
  # Se lê os endereços
  enderecos_uf <- fread(cf, colClasses = "character") %>% 
    mutate_at(vars(LATITUDE, LONGITUDE), as.numeric) %>% 
    mutate(CEP5 = substring(CEP, 1, 5))
  
  # Para cada município
  id_municipios <- unique(enderecos_uf$COD_MUNICIPIO)
  n_mun <- length(id_municipios)
  for(i_mun in seq_len(n_mun)){
    
    
    # Código IBGE do município
    cod_ibge <- id_municipios[i_mun]
    
    glue("|----- {cod_ibge} ({i_mun}/{n_mun})") %>% cat("\n")
    
    # Arquivo que vamos salvar com os endereçoes geolocalizados
    path_out <- glue("{dir_eleitorado_ll}/{cod_ibge}.csv")
    
    # Se já processamos os dados desse municípios, passamos para o próximo 
    if(file.exists(path_out)) next
    
    # Filtra para endereços do município
    enderecos_mun <- enderecos_uf %>%
      filter(COD_MUNICIPIO == cod_ibge)
    
    # Obtêm código do TSE do município
    cod_tse <- tse_ibge %>% 
      filter(codigo_ibge == cod_ibge) %>% 
      pull(codigo_tse)
    
    
    # Locais de votação do município
    lv_mun <- locais_de_votacao %>% 
      filter(CD_MUNICIPIO == cod_tse) %>% 
      mutate(NR_LATITUDE_2 = NA,
             NR_LONGITUDE_2 = NA) 
    
    # Para cada linha (seção em uma cidade em um ano em um turno)...
    n_linhas <- nrow(lv_mun)
    
    for(i_end in seq_len(n_linhas)){
      glue("|--------- {i_end}/{n_linhas}") %>% cat("\n")
      # Obtêm os dados daquela seção
      lv <- lv_mun[i_end,]
      
      # Se já tiver dados de geolocalização passa para a próxima linha
      if(!is.na(lv$NR_LATITUDE)) next
      
      # Obtêm os dados
      ll <- get_ll_lv(lv = lv, tab_cnf = enderecos_mun)
      
      
      # Casos que foram necessário pegar manualmente
      if(is.null(ll)) {
        
        nm_lv_org <- lv$NM_LOCAL_VOTACAO_ORIGINAL 
        
        if(cod_ibge == "3163201" & 
           nm_lv_org == "ESCOLA ESTADUAL MARIA LINA DE JESUS"){
          ll <- c(-22.33035642749051, -45.52194736971382)  
        }
        
        if(cod_ibge == "3151008" & 
           nm_lv_org == "ESCOLA MUNICIPAL DR. ATALIBA DE MORAES"){
            ll <- c(-22.32367801479518, -45.580371135739476)
        }
        
        if(cod_ibge == "1702158" & 
           nm_lv_org == "ESCOLA MUNICIPAL JOSE PEREIRA DE MIRANDA"){
            ll <- c(-6.7886696540927804, -48.61031433375837)
        }
        
        if(cod_ibge == "3165800" & 
           nm_lv_org == "ESCOLA ESTADUAL PROFESSOR MENDONCA"){
          ll <- c(-22.16414488412876, -46.177737750392126)
        }
        
        
        if(cod_ibge == "2111300" & 
           nm_lv_org == "UNIDADE INTEGRADA ALBERTO PINHEIRO"){
          ll <- c(-2.5291116288679056, -44.29309636695105)
        }
        
        if(cod_ibge == "1500800" & nm_lv_org == "COLÉGIO ASPECTO"){
          ll <- c(-1.3608415533067642, -48.402922549863945)
        }
        
        if(cod_ibge == "2806909" & 
           nm_lv_org == "COLÉGIO ESTADUAL JOAO DIAS GUIMARÃES"){
          ll <- c(-10.349720933813964, -36.88200445821281)
        }
        
        if(cod_ibge == "2806909" & 
           nm_lv_org == "PRÉ ESCOLAR ADELINA SILVA NASCIMENTO (CRECHE)"){
          ll <- c(-10.346843608993114, -36.88516461815911)
        }
        
        if(cod_ibge == "2806909" & 
           nm_lv_org == "ESCOLA MUNICIPAL LEANDRO MACIEL"){
          ll <- c(-10.386682220732322, -36.897133211554305)
        }
        
        if(cod_ibge == "2806909" & 
           nm_lv_org == "ESCOLA MUNICIPAL GETÚLIO VARGAS"){
          ll <- c(-10.350091795371075, -36.884734386319565)
        }
        
        if(cod_tse == "73318" & 
           nm_lv_org == "ESCOLA MUNICIPAL JOSE PEREIRA DE MIRANDA"){
          ll <- c(-48.61070103289607, -6.788574576231661)
        }
        
        if(cod_tse == "9210" & 
           nm_lv_org == "UNIDADE INTEGRADA ALBERTO PINHEIRO"){
          ll <- c(-2.52919320143476, -44.29325022940545)
        }
        
        if(cod_tse == "95958" & 
           nm_lv_org == "ESCOLA MUNICIPAL PINGO DE SABEDORIA"){
          ll <- c(-15.455456957127005, -47.605490468479765)
        }
      }
      
      # Adciona os dados na tabela
      lv_mun$NR_LATITUDE_2[i_end] <- ll[1]
      lv_mun$NR_LONGITUDE_2[i_end] <- ll[2]
    }
    
    # Salva o resultado
    fwrite(lv_mun, path_out, row.names = FALSE)
  }
}
