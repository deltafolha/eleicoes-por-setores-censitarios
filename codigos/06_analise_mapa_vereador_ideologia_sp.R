source("codigos/00_pacotes.R")
source("codigos/00_variaveis.R")


# variáveis ---------------------------------------------------------------
cores_ideologia <- c(esquerda = "#6B0C35", 
                     centro = "#4085C5", 
                     direita = "#A87F35")


limiar_quantile <- 0.01
id_municipio <- "3550308"


# funções -----------------------------------------------------------------
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


# lê as tabelas -----------------------------------------------------------
malha_2022 <- path_malha_censo_2022 %>% 
  read_sf() %>% 
  filter(CD_MUN == id_municipio)

resultado_ideologia_vereador_sp <- glue("{dir_resultados_por_setor}/",
                                        "2022_{id_municipio}_2024_VEREADOR_1_", 
                                        "ideologia.csv") %>%  
  fread(select = c("CD_SETOR", "ideologia", "pct_votos_ponderados_validos")) %>%
  # Remove votos inválidos
  filter(ideologia != "") %>% 
  # Só fica com lado vitorioso
  group_by(CD_SETOR) %>%
  filter(pct_votos_ponderados_validos == max(pct_votos_ponderados_validos)) %>% 
  ungroup() %>%
  # Trunca as porcentagens
  mutate(pct_votos_ponderados_validos = trunca_por_quantile(pct_votos_ponderados_validos)) %>%
  # Valor entre 0 e 1
  mutate(pct_votos_ponderados_validos_01 = rescale(pct_votos_ponderados_validos, 
                                           from = c(0, max(pct_votos_ponderados_validos)), 
                                           to = c(0, 1))) %>% 
  # Cor dos setores
  mutate(cor_setor = map2_chr(cores_ideologia[ideologia],
                              pct_votos_ponderados_validos_01,
                              obtem_cor_setor))


# plota o gráfico ---------------------------------------------------------
malha_2022_com_resultado <- malha_2022 %>% 
  left_join(resultado_ideologia_vereador_sp, by = "CD_SETOR")

  

malha_2022_com_resultado %>% 
  ggplot(aes(fill = cor_setor)) + 
  geom_sf(color = NA) + 
  scale_fill_identity(guide = "none") +
  theme_void()

ggsave("~/Desktop/mapa.pdf")
