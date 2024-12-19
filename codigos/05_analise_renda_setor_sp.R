source("codigos/00_pacotes.R")
source("codigos/00_variaveis.R")

# lê as tabelas -----------------------------------------------------------

# Padrão para pegar somente os arquivos dos setores com votação para vereadores 
# com a a malha de 2010 da cidade são paulo
rgx_vereadores_partidos_sp <- "2010_3550308_\\d{4,4}_VEREADOR_1_partido.csv"


# Tabela com o resultado da votação para vereadores por setor censitário
# na cidade de São Paulo
resultados_sp_vereador_partidos <- dir_resultados_por_setor %>% 
  list.files(full.names = TRUE, 
             pattern = rgx_vereadores_partidos_sp) %>%
  map(fread, encoding = "Latin-1") %>%
  bind_rows() %>% 
  mutate(CD_SETOR = as.character(CD_SETOR)) %>% 
  # Partidos que mudaram de nome. No caso do União estamos juntado 
  # os dois partidos que se juntaram nele (PSL e DEM)
  mutate(partido = recode(partido, 
                          "PMDB" = "MDB", 
                          "PMN" = "MOBILIZA", 
                          "PPS" = "CIDADANIA",
                          "PRB" = "REPUBLICANOS", 
                          "SD" = "SOLIDARIEDADE",
                          "PSDC" = "DC",
                          "PR" = "PL",
                          "PT do B" =  "AVANTE",
                          "PTC" = "AGIR",
                          "PTN" = "PODE", 
                          "PFL" = "UNIÃO",
                          "DEM" = "UNIÃO", 
                          "PSL" = "UNIÃO")) %>%
  select(CD_SETOR, partido, votos, pct_votos_ponderados_validos, ano) %>%
  # Para incluir dado de setores para partidos que nào tiveram nenhum votos 
  # neles
  complete(CD_SETOR, partido, ano, fill = list(pct_votos_ponderados_validos = 0,
                                               votos = 0))
# Renda dos setores censitários
renda_por_setor <- path_info_censo_2010 %>%
  fread(colClasses = "character") %>%
  filter(Cod_setor %in% unique(resultados_sp_vereador_partidos$CD_SETOR)) %>%
  mutate(V009 = str_replace(V009, ",", ".")) %>% 
  mutate(V009 = as.numeric(V009)) %>%
  mutate(percentile = ntile(V009, 100))   %>% 
  mutate(tercil = ntile(V009, 3)) %>% 
  select(Cod_setor, tercil, percentile, Nome_do_distrito)

# Junta as tabelas de votação e renda
partidos_com_percentile <- resultados_sp_vereador_partidos %>% 
  left_join(renda_por_setor, by = c("CD_SETOR" = "Cod_setor" ))  

# mapa com setores censitários de São Paulo
malha_2010 <- read_sf(path_malha_censo_2010) %>% 
  filter(cd_trct %in% unique(partidos_com_percentile$CD_SETOR))


distritos_sp <- glue("{dir_distritos_sp}/LAYER_DISTRITO/DEINFO_DISTRITO.shp") %>% 
  read_sf() %>% 
  st_transform(crs = st_crs(malha_2010))

# tabulação por partido ---------------------------------------------------


# Votação por partido de acorodo com a renda para cada ano. 
tbl_percentile_partido <- partidos_com_percentile %>% 
  group_by(ano, partido, percentile) %>% 
  summarise(pct_votos = mean(pct_votos_ponderados_validos)) %>% 
  ungroup()  %>% 
  # Remove locais que não tinham renda
  drop_na() %>% 
  # Remove votos que não foram em partidos
  filter(partido != "")


tbl_tercil_partido <- partidos_com_percentile %>% 
  group_by(ano, partido, tercil) %>% 
  summarise(pct_votos = mean(pct_votos_ponderados_validos)) %>% 
  ungroup()  %>% 
  # Remove locais que não tinham renda
  drop_na() %>% 
  # Remove votos que não foram em partidos
  filter(partido != "") 


# Quanto cada partido cresceu em cada tercil desde 2012
tercil_1_qto_cresceu_desde_2012 <- tbl_tercil_partido %>% 
  filter(tercil == 1) %>% 
  select(-tercil) %>% 
  filter(ano %in% c(2012, 2024)) %>% 
  pivot_wider(names_from = ano, values_from = pct_votos, values_fill = 0, 
              names_prefix = "ano_") %>% 
  mutate(dif = ano_2024 - ano_2012) %>% 
  arrange(desc(dif))


# por ideologia -----------------------------------------------------------

ideologia_por_tercil <- path_ideologia_partidos %>% 
  fread %>%
  mutate(ideologia = str_remove(label, "_\\d")) %>% 
  select(partido, ideologia) %>%
  right_join(partidos_com_percentile, by = "partido") %>%
  # Partidos que não existem em 2024. A ideologia está de acordo com os partidos
  # que atualmente absorveram esses partidos
  mutate(ideologia = case_when(partido %in% c("DEM", "PSL", "PTB", 
                                              "PATRIOTA") ~ "direita",
                               partido %in% c("PHS", "PSC", "PRP",
                                              "PROS") ~  "centro",
                               partido %in% c("PPL") ~  "esquerda", 
                               TRUE ~ ideologia)) %>% 
  
  filter(partido != "") %>%
  group_by(ideologia, ano, tercil) %>% 
  summarise(votos = sum(votos)) %>% 
  ungroup() %>% 
  drop_na() %>% 
  group_by(ano, tercil) %>% 
  mutate(pct_votos = votos/sum(votos) * 100) %>% 
  ungroup() %>% 
  select(-votos) %>% 
  pivot_wider(names_from = ano, values_from = pct_votos)


# por partido e bairro ----------------------------------------------------
partidos_com_percentile %>% 
  filter(partido != "") %>% 
  group_by(partido, ano, Nome_do_distrito) %>% 
  summarise(pct_tercil_1 = mean(tercil == 1) * 100, 
            votos = sum(votos)) %>% 
  ungroup() %>% 
  group_by(ano, Nome_do_distrito) %>% 
  mutate(pct_votos = votos/sum(votos) * 100)



# gráficos ----------------------------------------------------------------

##
## Como variou a votação no PT de acordo com a renda ao longo dos anos
##
tbl_percentile_partido %>% 
  filter(partido == "PT") %>%
  ggplot(aes(x = percentile, y = pct_votos, colour = as.factor(ano))) + 
  geom_line() + 
  theme_minimal()

ggsave("~/Desktop/votos_pt_renda.pdf", scale = 2)


##
## Como variou a votação em diferentes partidos entre os mais pobres
##

partidos_com_percentile %>%
  # Remove locais que não tinham renda
  drop_na() %>%
  # Remove votos que não foram em partidos
  filter(partido != "") %>%
  # Junta os partidos com menos votos em 'Outros'
  mutate(partido = ifelse(!partido %in% c("PSDB","UNIÃO", "PT", "PSD", "PL", 
                                          "MDB", "REPUBLICANOS", "PSOL", "PSD", 
                                          "PODE"), 
                          "Outros", partido)) %>% 
  group_by(ano, partido, tercil) %>% 
  summarise(votos_total = sum(pct_votos_ponderados_validos)) %>% 
  ungroup()   %>%
  group_by(ano, tercil) %>% 
  mutate(pct_votos = votos_total/sum(votos_total)) %>% 
  ungroup() %>% 
  filter(tercil == 1) %>% 
  ggplot(aes(x = ano, y = pct_votos, colour = partido)) + 
  geom_line() + 
  theme_minimal()


ggsave("~/Desktop/votos_entre_os_mais_pobres.pdf", scale = 2)

##
## Mapa da votação em diferentes partidos
##
cores_partidos <- c(PT = "#6B0C35",
                    UNIÃO = "#A87F35",
                    MDB = "#4085C5",
                    PL = "#A87F35",
                    PSDB = "#A87F35",
                    PSOL = "#6B0C35")

limiar_quantile <- 0.05

trunca_por_quantile <- function(x, limiar = limiar_quantile){
  superior <- quantile(x, 1 - limiar_quantile, na.rm = TRUE)
  inferior <- quantile(x, limiar_quantile, na.rm = TRUE)
  case_when(x >   superior ~ superior,
            x <   inferior ~ inferior,
            TRUE ~ x)
}

obtem_cor_setor <- function(cor, valor) {
  color_gradient <- colorRampPalette(c("white", cor))
  color <- color_gradient(100)[round(valor * 99) + 1]
  return(color)
}



df_para_mapa <- partidos_com_percentile %>% 
  filter(partido %in% names(cores_partidos)) %>% 
  # Trunca as porcentagens
  mutate(pct_votos_ponderados = trunca_por_quantile(pct_votos_ponderados_validos)) %>%
  # Valor entre 0 e 1
  mutate(pct_votos_ponderados_01 = rescale(pct_votos_ponderados_validos, 
                                           from = c(0, max(pct_votos_ponderados_validos)), 
                                           to = c(0, 1))) %>% 
  # Cor dos setores
  mutate(cor_setor = map2_chr(cores_partidos[partido],
                              pct_votos_ponderados_01,
                              obtem_cor_setor)) %>% 
  mutate(partido = factor(partido,  levels = names(cores_partidos), 
                          ordered = TRUE)) %>% 
  left_join(malha_2010, by = c("CD_SETOR" = "cd_trct")) %>% 
  st_as_sf()

df_para_mapa %>% 
  ggplot(aes(fill = cor_setor)) + 
  geom_sf(color = NA) + 
  scale_fill_identity(guide = "none") +
  theme_void() + 
  facet_wrap(~partido + ano, ncol = 4)

ggsave("~/Desktop/votos_entre_os_mais_pobres.pdf", scale = 2)
