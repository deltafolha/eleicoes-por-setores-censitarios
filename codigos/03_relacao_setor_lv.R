source("codigos/00_pacotes.R")
source("codigos/00_variaveis.R")

# Para paralelizar o processo de atrelar locais de votação aos setores 
# censitários
plan(multisession)

# função ------------------------------------------------------------------

# Função que atrela locais de votação à setores censitários
#     chunk: Linhas com os locais de votação
#     malha_mun: Malha dos setores censitários do município
#     malha_mun_centroids: Centróides dos setores censitários
process_chunk <- function(chunk, malha_mun, malha_mun_centroids) {
  
  dist_polygon <- chunk %>% 
    # Distância dos locais de votação para cada setor
    # --> O resultado é uma matriz com os setores nas colunas
    # e os locais de votação nas linhas 
    st_distance(malha_mun) %>% 
    as_tibble() %>%
    # Coloca os códigos dos setores nas colunas e identifica os locais de 
    # votação nas linhas
    set_colnames(malha_mun$CD_SETOR) %>%
    mutate(id_lv = chunk$id_lv) %>%
    # Transforma a matrix em uma tabela no formato longo
    pivot_longer(cols = -id_lv, 
                 names_to = "CD_SETOR", 
                 values_to = "distancia_poligono") %>% 
    # Divide os locais de votação em grupos de acordo com a distância para os
    # setores. Cada grupo está a uma distância de 100 metros:
    #     Grupo 1: Locais de votação dentro ou até 100 metros do setor
    #     Grupo 2: Locais de votação entre 100 e 200 metros do setor
    #     Grupo 3: Locais de votação entre 200 e 300 metros do setor
    #     E por aí vai
    mutate(grupo_distancia_poly = floor(distancia_poligono / 100) + 1) %>%  
    # Filtra para cada setor só ter os locais de votação do menor grupo. 
    group_by(CD_SETOR) %>% 
    filter(grupo_distancia_poly == min(grupo_distancia_poly, na.rm = TRUE)) %>% 
    ungroup()
  
  # adiciona distância para o centroide
  resultado_distancia <- st_distance(chunk, malha_mun_centroids) %>% 
    set_colnames(malha_mun$CD_SETOR) %>%
    as.data.frame() %>% 
    mutate(id_lv = chunk$id_lv) %>% 
    pivot_longer(cols = -id_lv, 
                 names_to = "CD_SETOR", 
                 values_to = "distancia_centroide") %>% 
    right_join(dist_polygon, by = join_by(id_lv, CD_SETOR))
  
  return(resultado_distancia)
}




# lê os dados -------------------------------------------------------------


# Tabela com a relação dos IDs do IBGE com os do TSE
tse_ibge <- path_tse_ibge %>% 
  fread()

# Regex para pergamos os arquivos csvs das eleições
rgx_eleicoes <- seq(2012, 2024, 2) %>% 
  paste(collapse = "|") %>% 
  paste0("(", ., ").*.csv")

# Locais de votação que foram utilizados nas eleições
lv_com_resultados <- dir_resultado %>% 
  list.files(full.names = TRUE, pattern = rgx_eleicoes) %>% 
  map(~ fread(.x,
              colClasses = "character", 
              select = c("NR_ZONA", "NR_SECAO",
                         "SG_UF",
                         "ANO_ELEICAO", "NR_TURNO" , "CD_MUNICIPIO")) %>% 
        distinct(), .progress = TRUE
  ) %>% 
  bind_rows() %>% 
  mutate(id_lv = stringr::str_c(NR_ZONA, NR_SECAO, 
                                ANO_ELEICAO,  NR_TURNO, CD_MUNICIPIO, 
                                sep = "_")) 

# Locais de votação georeferenciados
lvs <- list.files(dir_eleitorado_ll, full.names = TRUE) %>% 
  map(fread, colClasses = "character", encoding = "Latin-1") %>%
  bind_rows() %>%
  mutate_at(c("NR_ZONA", "NR_SECAO",
              "AA_ELEICAO", "NR_TURNO" , "CD_MUNICIPIO", 
              "NR_LATITUDE", "NR_LONGITUDE",
              "NR_LATITUDE_2", "NR_LONGITUDE_2"), as.numeric) %>%
  
  # Onde não tinha o geoferenciamento no TSE usamos o ID que obtivemos 
  # no código anterior
  mutate(NR_LATITUDE = ifelse(is.na(NR_LATITUDE), 
                              NR_LATITUDE_2, NR_LATITUDE)) %>% 
  mutate(NR_LONGITUDE = ifelse(is.na(NR_LONGITUDE), 
                               NR_LONGITUDE_2, NR_LONGITUDE)) %>% 
  select(-c(NR_LATITUDE_2, NR_LONGITUDE_2)) %>% 
  mutate(id_lv = stringr::str_c(NR_ZONA, NR_SECAO, 
                                AA_ELEICAO,  NR_TURNO, CD_MUNICIPIO, 
                                sep = "_")) %>% 
  distinct(id_lv, .keep_all = TRUE) %>% 
  # Filtra ara só usarmos os que foram utilizados
  filter(id_lv %in% lv_com_resultados$id_lv)


# relação setor <-> lv ----------------------------------------------------

# Anos das malhas dos setores
anos_malha <- c(2010, 2022)

for(ano_malha in anos_malha){
  
  # Cria uma pasta que irá armazenar a relação setor <--> Local de votação
  # Teremos uma pasta para cada malha
  dir_relacao_lv_setor_ano_malha <- glue("{dir_relacao_lv_setor}/malha_{ano_malha}/")
  if(!dir.exists(dir_relacao_lv_setor_ano_malha)) {
    dir.create(dir_relacao_lv_setor_ano_malha)
  }
  
  # projeta as malhas no CRS 4326
  if(ano_malha == 2022){
    malha <- path_malha_censo_2022 %>% 
      read_sf() %>%
      st_transform(crs = 4326)
    
  }
  
  # no caso de 2010 precisamos renomear as colunas para terem os mesmos nomes 
  # das de 2022
  if(ano_malha == 2010){
    malha <- path_malha_censo_2010 %>% 
      read_sf() %>%
      st_transform(crs = 4326) %>%
      rename(CD_MUN  = code_mn, 
             CD_SETOR = cd_trct) %>% 
      st_make_valid()
  }
  
  # Anos das eleições
  anos <- lvs$AA_ELEICAO %>% unique %>% sort %>% rev
  n_anos <- length(anos)
  
  for(i_ano in seq_len(n_anos)){
    
    
    ano <- anos[[i_ano]]
    
    
    glue("ANO: {ano} ({i_ano}/{n_anos})") %>% cat("\n")
    
    # Locais de votação usados naquele ano
    lvs_ano <- lvs %>%
      filter(AA_ELEICAO == ano)
    
    # Cidades que tiveram eleição naquela ano
    cidades <- lvs_ano$CD_MUNICIPIO %>% 
      unique
    
    n_cidades <- length(cidades)
    
    for(i_cidade in seq_len(n_cidades)){
      
      # Locais de votação utilizados naquele ano e naquela cidade 
      id_mun_tse <- cidades[[i_cidade]]
      lvs_ano_mun <- lvs_ano %>% 
        filter(CD_MUNICIPIO == id_mun_tse)
      
      
      id_mun_ibge <- tse_ibge %>% 
        filter(codigo_tse == id_mun_tse) %>% 
        pull(codigo_ibge)
      
      glue("|---- CIDADE: {id_mun_ibge} ({i_cidade}/{n_cidades})") %>% cat("\n")
      
      # Pega os setores do município
      malha_mun <- malha %>% 
        filter(CD_MUN == id_mun_ibge) %>% 
        st_make_valid()
      
      # No caso da malha 2010 alguns municípios não exisitam
      if(nrow(malha_mun) == 0) next
      
      # Quantidade de turnos naquela cidade naquele ano
      turnos <- lvs_ano_mun$NR_TURNO %>% unique %>% sort
      n_turnos <- length(turnos)
      
      
      for(i_turno in seq_len(n_turnos)){
        
        turno <- turnos[[i_turno]]
        
        # Caminho do arquivo que vamos salvar com as informações 
        relacao_lv_setor_path <- glue("{dir_relacao_lv_setor_ano_malha}/", 
                                      "{id_mun_ibge}_{ano}_{turno}.csv")
        if(file.exists(relacao_lv_setor_path)) next
        
        glue("|-------- TURNO: {turno} ({i_turno}/{n_turnos})") %>% cat("\n")
        
        # Locais de votaão no ano, na cidade e no turno
        lvs_ano_mun_turno <- lvs_ano_mun %>% 
          filter(NR_TURNO == turno)
        
        # Transforma a tabela de local de votação em pontos espaciais, para
        # podemos calcular a distância dos poligonos dos setores
        lv_ano_mun_turno_sf <- st_as_sf(lvs_ano_mun_turno,
                                        coords = c("NR_LONGITUDE",
                                                   "NR_LATITUDE"), 
                                        crs = 4326)
        
        
        # Divide os locais de votação em conjuntos de 1.000 para podemos
        # processa-los de forma paralela
        chunk_size <- 1000
        chunks <- split(lv_ano_mun_turno_sf, 
                        ceiling(seq_along(lv_ano_mun_turno_sf$id_lv) / chunk_size))
        
        # Centroide dos setores. Vamos calular a distância para eles e armazenar
        # essa informação também na tabela. Será usado para ponderar o peso
        # dos locais de votação para cada setor
        malha_mun_centroids <- malha_mun %>%
          sf::st_centroid() 
        
        # Verifica se era temos mais e um conjunto de 1000 linhas de locais de 
        # votação. Se sim, fazemos em paralelo.  
        if(length(chunks) > 1){
          distancias <- future_map(chunks, 
                                   process_chunk, 
                                   malha_mun, 
                                   malha_mun_centroids, 
                                   .progress = TRUE) %>% 
            bind_rows() %>% 
            # Pegamos os setores do menor grupo da distância 
            # (ver os comentários da função `process_chunk` para entender 
            # como isso funciona)
            group_by(CD_SETOR) %>% 
            filter(grupo_distancia_poly == min(grupo_distancia_poly, 
                                               na.rm = TRUE)) %>% 
            ungroup() %>%
            select(-c(grupo_distancia_poly))  
        } else {
          distancias <- process_chunk(chunks[[1]],
                                      malha_mun, 
                                      malha_mun_centroids) %>% 
            select(-c(grupo_distancia_poly_num))
        }
        
        # Salva as informações
        fwrite(distancias, relacao_lv_setor_path, row.names = FALSE)
      }
    }
  }
}