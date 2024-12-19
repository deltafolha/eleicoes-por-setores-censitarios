# Chaves
api_gmap <- "XXXXXXXXXXX"
gs4_auth("XXXXXXXXXXXX@gmail.com")

# Dados brutos
dir_eleitorado <- "dados/brutos/tse/eleitorado/"
dir_cnfe <- "dados/brutos/censo/cnfe/"
dir_resultado <- "dados/brutos/tse/resultado/"
dir_malha_censo <- "dados/brutos/censo/malha/"
dir_resultado_mun <- "dados/brutos/tse/resultado_mun/"
dir_candidatos <- "dados/brutos/tse/candidatos/"
dir_detalhe_censo <- "dados/brutos/detalhes_censo/"
dir_ms_com_boa_esperanca_do_norte <- "dados/brutos/MT-LIMITE_POLITICO_ADMINISTRATIVO/"
dir_distritos_sp <- "dados/brutos/distritos_sp/"

path_tse_ibge <- "dados/brutos/tse_ibge.csv"
path_ufs <- "dados/brutos/ufs.csv"
path_malha_censo_2022 <- paste0("dados/brutos/censo/malha/BR_Malha_Preliminar_2022/", 
                                "BR_Malha_Preliminar_2022.shp")
path_malha_censo_2010 <- "dados/brutos/censo/malha/BR_Malha_2010.shp"
path_numero_partidos <- "dados/brutos/numero_partidos.csv"
path_ideologia_partidos <- "dados/brutos/ideologia_partidos.csv"
path_info_censo_2010 <- "dados/brutos/detalhe_censo_2010.csv"

# Dados interim
dir_eleitorado_ll <- "dados/interim/eleitorado_ll_2/"
dir_relacao_lv_setor <- "dados/interim/relacao_lv_setor/"

# Dados processados
dir_resultados_por_setor <- "dados/processados/resultados_por_setor/"

# Cria os diretórios
dirs <- c(dir_eleitorado, dir_cnfe, dir_resultado, dir_resultado_mun, 
          dir_malha_censo, dir_candidatos, dir_detalhe_censo, 
          dir_eleitorado_ll, dir_relacao_lv_setor, 
          dir_ms_com_boa_esperanca_do_norte,
          dir_resultados_por_setor, dir_distritos_sp)
for(dir in dirs){
  if(!dir.exists(dir)) dir.create(dir, recursive = TRUE)
}