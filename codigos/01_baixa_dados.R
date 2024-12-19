source("codigos/00_pacotes.R")
source("codigos/00_variaveis.R")

# Anos para os quais vamos baixar os dados do TSE
anos <- seq(2010, 2024, 2)

# Para não ter problema com demora para baixar os arquivos
options(timeout = 1000)

# Anos de eleições municipais (para criar exceções referentes ao DF e eleição 
# para presidente
anos_eleicao_municipal <- seq(2008, 2024, 4)

# ufs ---------------------------------------------------------------------

# Tabela com informações dos estados
if(!file.exists(path_ufs)){
  ufs <- paste0("https://raw.githubusercontent.com/kelvins/", 
                "municipios-brasileiros/refs/heads/main/csv/estados.csv") %>% 
    read.csv()
  write.csv(ufs, path_ufs, row.names = FALSE)
}



# dados TSE ---------------------------------------------------------------


# Adiciona "BR" (dados da eleição presidencial às ufs)
ufs <- path_ufs %>% 
  read.csv() %>% 
  pull(uf) %>% 
  c("BR")
options(warn = 2)
for(ano in anos){
  
  ##
  ## Eleitorado
  ##
  url_eleitorado_ano <- glue("https://cdn.tse.jus.br/estatistica/sead/odsele/", 
                             "eleitorado_locais_votacao/", 
                             "eleitorado_local_votacao_{ano}.zip")
  path_zip_eleitorado <- str_replace(url_eleitorado_ano, ".*/", dir_eleitorado)
  if(ano > 2008){
    if(!file.exists(path_zip_eleitorado)) {
      download.file(url_eleitorado_ano, path_zip_eleitorado)
    }
    check_csv <- list.files(dir_eleitorado) %>%
      keep(str_ends, glue("{ano}.csv")) %>% 
      length() %>%
      as.logical()
    if(!check_csv){
      unzip(path_zip_eleitorado, exdir = dir_eleitorado) 
    }
  }
  
  ##
  ## Resultado por município
  ##
  url_resultado_mun <- glue("https://cdn.tse.jus.br/estatistica/sead/odsele/", 
                            "votacao_candidato_munzona/", 
                            "votacao_candidato_munzona_{ano}.zip")
  path_zip_resultado_mun <- str_replace(url_resultado_mun, ".*/", 
                                        dir_resultado_mun)
  if(!file.exists(path_zip_resultado_mun)) {
    download.file(url_resultado_mun, path_zip_resultado_mun)
  }
  
  check_csv <- list.files(dir_resultado_mun) %>%
    keep(str_ends, glue("{ano}_BRASIL.csv")) %>% 
    length() %>%
    as.logical()
  if(!check_csv){
    unzip(path_zip_resultado_mun, exdir = dir_resultado_mun) 
  }
  
  
  ##
  ## Votação por seção
  ##
  for(uf in ufs){
    if(uf %in% c("DF", "BR") & ano %in% anos_eleicao_municipal ) next
    url_resultado_ano <- glue("https://cdn.tse.jus.br/estatistica/sead/",
                              "odsele/votacao_secao/", 
                              "votacao_secao_{ano}_{uf}.zip")
    path_zip_resultado <- str_replace(url_resultado_ano, ".*/", dir_resultado)
    
    if(!file.exists(path_zip_resultado)) {
      download.file(url_resultado_ano, path_zip_resultado)
    }
    check_csv <- path_zip_resultado %>% 
      str_replace("zip$", "csv") %>% 
      file.exists()
    
    if(!check_csv){
      unzip(path_zip_resultado, exdir = dir_resultado) 
    }
  }
  
  ##
  ## candidatos
  ##
  url_resultado_cand <- glue("https://cdn.tse.jus.br/estatistica/sead/odsele/", 
                             "consulta_cand/", 
                             "consulta_cand_{ano}.zip")
  path_zip_resultado_cand <- str_replace(url_resultado_cand, ".*/", 
                                         dir_candidatos)
  if(!file.exists(path_zip_resultado_cand)) {
    download.file(url_resultado_cand, path_zip_resultado_cand)
  }
  
  check_csv <- list.files(dir_candidatos) %>%
    keep(str_ends, glue("{ano}_BRASIL.csv")) %>% 
    length() %>%
    as.logical()
  
  if(!check_csv){
    unzip(path_zip_resultado_cand, exdir = dir_candidatos) 
  }
}



# cnfe --------------------------------------------------------------------

# Baixa o Cadastro_Nacional_de_Enderecos_para_Fins_Estatisticos de 2022, onde
# tem os endereços e seus CEPs com latitude e longitude

url_root_cnfe <- paste0("https://ftp.ibge.gov.br/", 
                        "Cadastro_Nacional_de_Enderecos_para_Fins_Estatisticos/", 
                        "Censo_Demografico_2022/Arquivos_CNEFE/CSV/UF/")

zips_cnfe <- url_root_cnfe %>% 
  read_html() %>% 
  html_nodes("a") %>% 
  html_attr("href") %>% 
  keep(str_ends, "zip")


for(z in zips_cnfe){
  url_cnfe_uf <- glue("{url_root_cnfe}/{z}")
  path_zip_cnfe <- str_replace(url_cnfe_uf, ".*/", dir_cnfe)
  if(!file.exists(path_zip_cnfe)) {
    download.file(url_cnfe_uf, path_zip_cnfe)
  }
  check_csv <- path_zip_cnfe %>% 
    str_replace("zip$", "csv") %>% 
    file.exists()
  if(!check_csv){
    unzip(path_zip_cnfe, exdir = dir_cnfe) 
  }
}

# Baixa o shape do Mato Grosso com Boa Esperança do Norte
if(!dir.exists(dir_ms_com_boa_esperanca_do_norte)){
  path_zip <- glue("{dir_ms_com_boa_esperanca_do_norte}/shp_mt.zip")
  paste0("https://www.intermat.mt.gov.br/documents/3124425/64430243/", 
         "LIM_LIMITE_POLITICO_ADMINISTRATIVO_A.zip") %>% 
    download.file(destfile = path_zip)
  
  unzip(path_zip, exdir = dir_ms_com_boa_esperanca_do_norte)
}

# Verifica se já adicionamos Boa Esperança do Norte nos endereços do Mato Grosso
check_added_ben_cnfe <- "dados/brutos/check_mt_cnfe.log"

if(!file.exists(check_added_ben_cnfe)){
  
  # Pega o shape só de Boa Esperança do Norte
  shp_ben <- glue("{dir_ms_com_boa_esperanca_do_norte}/", 
                  "LIM_LIMITE_POLITICO_ADMINISTRATIVO_A.shp") %>% 
    read_sf() %>% 
    filter(mn_no == "BOA ESPERANÇA DO NORTE")
  
  
  # Lista os endereçoes do Mato Grosso
  path_cnfe_mt <- dir_cnfe %>%
    list.files("MT.csv$", full.names = TRUE)
  
  # As latitudes e longitudes dos endereços que estão dentro do shape de 
  # Boa Esperança alteramos o município deles
  mt_com_ben <- path_cnfe_mt %>%  
    fread(colClasses = "character", encoding = "Latin-1") %>%
    st_as_sf(coords = c("LONGITUDE", "LATITUDE"),  crs = st_crs(shp_ben), 
             remove = FALSE) %>% 
    mutate(ben = st_intersects(., shp_ben, sparse = FALSE)[,1])  %>%
    mutate_at(vars(COD_MUNICIPIO, COD_DISTRITO,  COD_SUBDISTRITO, COD_SETOR), 
              ~ ifelse(ben, gsub("^.{,7}", "5106241", .x), .x)) %>%
    st_drop_geometry() %>%
    select(-ben)  
  
  # Reescreve o arquivo de endereços do Mato Grosso
  fwrite(mt_com_ben, path_cnfe_mt, row.names = FALSE)
  
  # Adiciona um arquivo para checarmos se a adição já foi feita
  file.create(check_added_ben_cnfe)
}



# tse <--> ibge -----------------------------------------------------------

# Tabela com a correspondência dos IDs do IBGE e TSE 
if(!file.exists(path_tse_ibge)){
  paste0("https://raw.githubusercontent.com/betafcc/", 
         "Municipios-Brasileiros-TSE/refs/heads/master/", 
         "municipios_brasileiros_tse.csv") %>% 
    read.csv(colClasses = "character") %>% 
    # Adiciona nova esperança do norte
    bind_rows(c(codigo_tse = "73709", uf = "MT", 
                nome_municipio = "BOA ESPERANÇA DO NORTE", 
                capital = "0", 
                codigo_ibge = "5106241")) %>% 
    fwrite(path_tse_ibge, row.names = FALSE)
}



# malhas e informações (de 2010) dos setores censitários ------------------

##
## 2022
##

url_malha_censo_2022 <- paste0("https://geoftp.ibge.gov.br/organizacao_do_territorio/",
                               "malhas_territoriais/", 
                               "malhas_de_setores_censitarios__divisoes_intramunicipais/", 
                               "censo_2022_preliminar/setores/shp/BR/", 
                               "BR_Malha_Preliminar_2022.zip")

zip_malha_censo_2022 <- str_replace(url_malha_censo_2022, ".*/", dir_malha_censo)
if(!file.exists(url_malha_censo_2022)){
  download.file(url_malha_censo_2022, zip_malha_censo_2022)
}

dir_malha_censo_2022 <- zip_malha_censo_2022 %>% 
  str_remove("\\.zip$") 

check_dir_malha_2022 <- dir_malha_censo_2022 %>% 
  dir.exists()

if(!check_dir_malha_2022){
  unzip(zip_malha_censo_2022, exdir = dir_malha_censo_2022)  
}



# Verifica se já adicionamos Boa Esperan
check_added_ben_malha <- "dados/brutos/check_mt_malha.log"
if(!file.exists(check_added_ben_malha)){
  
  # Shape de Boa Esperança do Norte
  shp_ben <- glue("{dir_ms_com_boa_esperanca_do_norte}/", 
                  "LIM_LIMITE_POLITICO_ADMINISTRATIVO_A.shp") %>% 
    read_sf() %>% 
    filter(mn_no == "BOA ESPERANÇA DO NORTE")
  
  ##
  ## Adiciona setores que tem mais de  50%  de suas áreas em Nova Esperança do 
  ## Norte para a cidade
  ##
  
  # Malha do censo
  malha_2022 <- path_malha_censo_2022 %>% read_sf() %>% st_make_valid()
  
  # Poligono de Boa Esperança do Norte
  shp_ben <- shp_ben %>% st_transform(sf::st_crs(malha_2022))
  
  # Área dos setores
  original_area <- as.numeric(st_area(malha_2022))
  
  # Poligonos das interseções com Boa Esperança do Norte
  intersections <- st_intersection(malha_2022, shp_ben)
  
  # Área dessas interseções
  intersection_area <- as.numeric(st_area(intersections))
  
  # Qual % dos poligonos intersectam com Boa Esperança do Norte
  pct_intersecacao <- intersection_area / original_area[malha_2022$CD_SETOR %in% intersections$CD_SETOR]
  
  # Adiciona coluna se vamos considerar o setor como de Boa Esperança do Norte
  malha_2022$ben <- FALSE
  malha_2022$ben[malha_2022$CD_SETOR %in% intersections$CD_SETOR] <- pct_intersecacao >= 0.9
  
  # Altara o nome e o código do município para esses setores
  malha_2022 <- malha_2022 %>% 
    mutate_at(vars(CD_SETOR, CD_MUN,CD_DIST, CD_SUBDIST), 
              ~ ifelse(ben, gsub("^.{,7}", "5106241", .x), .x)) %>% 
    mutate(NM_MUN = ifelse(ben, "BOA ESPERANCA DO NORTE",  NM_MUN))  
  
  # Salva a malha com essa alteração
  write_sf(malha_2022, path_malha_censo_2022)
  
  # Cria um arquivo que será usado para checar se já adicionamos 
  # Boa Esperança do Norte na malha
  file.create(check_added_ben_malha)
}

##
## Malha de 2010
##
if(!file.exists(path_malha_censo_2010)){
  malha_2010 <- read_census_tract(code_tract = "all", simplified = FALSE)
  write_sf(malha_2010, path_malha_censo_2010)
}

##
## Dados do censo de 2010 (como renda por setor)
##
ufs_detalhes_info <- ufs %>% 
  toupper() %>% 
  purrr::discard(is_in, "SP") %>% 
  c("SP_Capital","SP_Exceto_Capital")

for(uf in ufs_detalhes_info){
  
  url <- glue("https://ftp.ibge.gov.br/Censos/Censo_Demografico_2010/", 
              "Resultados_do_Universo/Agregados_por_Setores_Censitarios/", 
              "{uf}_20231030.zip")
  
  
  zip_path <- str_replace(url, ".*/", dir_detalhe_censo)
  if(!file.exists(zip_path)){
    download.file(url, zip_path)  
  }
  unzip(zip_path, exdir = dir_detalhe_censo)
}

if(!file.exists(path_info_censo_2010)){
  dir_detalhe_censo %>% 
    list.files(full.names = TRUE, recursive = TRUE) %>% 
    keep(str_detect, "/EXCEL/Basico.*(xls|XLS)$")  %>%
    map(read_xls, col_types = "text") %>%   
    bind_rows() %>% 
    fwrite(path_info_censo_2010, row.names = FALSE)  
}

# numero dos partidos ------------------------------------------------------


# número de cada partido. Isso é importante porque nos resultados detalhados
# por seção não há a sigla do partido, mas há o número do candidato.
if(!file.exists(path_numero_partidos)){
  "https://www.tse.jus.br/partidos/partidos-registrados-no-tse" %>% 
    read_html() %>% 
    html_table() %>% 
    extract2(1) %>%
    rename_all(tolower) %>%
    rename_all(iconv, to = 'ASCII//TRANSLIT') %>% 
    rename_all(str_replace_all,  "( |[[:punct:]])+", "_") %>% 
    select(partido = sigla,  nr_partido = no_da_legenda) %>% 
    head(-1) %>% 
    mutate(partido = ifelse(partido == "PCdoB", "PC do B", partido)) %>% 
    write.csv(path_numero_partidos, row.names = FALSE)
}


# ideologia ---------------------------------------------------------------

# Ideologia dos partidos segundo o GPS partidário da Folha de 2024

if(!file.exists(path_ideologia_partidos)){
  paste0("https://raw.githubusercontent.com/deltafolha/", 
         "proximidade-partidaria-2024/refs/heads/main/", 
         "tabela_final.csv") %>% 
    fread() %>% 
    bind_rows(c(partido = "PCO", 
                label = "esquerda_1")) %>% 
    write.csv(path_ideologia_partidos, row.names = FALSE) 
}


# distritos de SP ---------------------------------------------------------

url_distrito_sp <- paste0("http://dados.prefeitura.sp.gov.br/dataset/", 
                          "af41e7c4-ae27-4bfc-9938-170151af7aee/resource/", 
                          "9e75c2f7-5729-4398-8a83-b4640f072b5d/download/", 
                          "layerdistrito.zip")

zip_distrito_sp <- glue("{dir_distritos_sp}/distritos_sp.zip")

if(!file.exists(zip_distrito_sp)) {
  download.file(url_distrito_sp, zip_distrito_sp)
}


dir_unzip <- glue("{dir_distritos_sp}/LAYER_DISTRITO")
if(!dir.exists(dir_unzip)){
  unzip(zip_distrito_sp, exdir = dir_distritos_sp)
}


