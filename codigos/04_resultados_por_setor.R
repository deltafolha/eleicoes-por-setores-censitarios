source("codigos/00_pacotes.R")
source("codigos/00_variaveis.R")


# tabelas -----------------------------------------------------------------
tse_ibge <- fread(path_tse_ibge)

# Regex para pergamos os arquivos csvs das eleições
rgx_eleicoes <- seq(2012, 2024, 2) %>% 
  paste(collapse = "|") %>% 
  paste0("(", ., ").*.csv")


resultados <- dir_resultado %>% 
  list.files(full.names = TRUE, pattern = rgx_eleicoes) %>% 
  map(fread,
      select = c("CD_MUNICIPIO" = "character", 
                 "NR_ZONA" = "character", 
                 "NR_SECAO" = "character",
                 "NR_TURNO" = "character", 
                 "ANO_ELEICAO" = "character", 
                 "NR_VOTAVEL" = "numeric",
                 "QT_VOTOS" = "numeric",
                 "DS_CARGO" = "character"),
      encoding = "Latin-1", 
      .progress = TRUE,) %>% 
  bind_rows() %>% 
  mutate(CD_MUNICIPIO = as.numeric(CD_MUNICIPIO)) %>% 
  rename(AA_ELEICAO = ANO_ELEICAO) %>% 
  mutate(id_lv = stringr::str_c(NR_ZONA, NR_SECAO, AA_ELEICAO, 
                                NR_TURNO, CD_MUNICIPIO, sep = "_")) %>% 
  mutate(DS_CARGO = toupper(DS_CARGO)) %>% 
  select(CD_MUNICIPIO, id_lv, AA_ELEICAO, DS_CARGO, NR_VOTAVEL, NR_TURNO, 
         QT_VOTOS)


numeros_partidos_anos <- dir_candidatos %>% 
  list.files(full.names = TRUE, pattern = "BRASIL.csv") %>% 
  keep(str_detect, rgx_eleicoes) %>% 
  map(fread, 
      encoding = "Latin-1",
      select = c(NR_CANDIDATO = "character", 
                 SG_PARTIDO = "character", 
                 ANO_ELEICAO = "numeric")) %>% 
  bind_rows()  %>% 
  mutate(NR_CANDIDATO = substring(NR_CANDIDATO, 1, 2)) %>% 
  group_by(SG_PARTIDO, NR_CANDIDATO, ANO_ELEICAO) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(NR_CANDIDATO, ANO_ELEICAO) %>% 
  filter(n == max(n)) %>%
  select(SG_PARTIDO, ANO_ELEICAO, NR_CANDIDATO) %>% 
  rename(partido = SG_PARTIDO, 
         nr_partido = NR_CANDIDATO, 
         ano = ANO_ELEICAO)

ideologia_2024 <- read.csv(path_ideologia_partidos) %>% 
  mutate(ideologia = str_remove(label, "_\\d$")) %>% 
  select(partido, ideologia) 


# tabula os resultados ----------------------------------------------------

malhas <- dir_relacao_lv_setor %>% 
  list.files(full.names = TRUE)

n_malhas <- length(malhas)

anos <- resultados$AA_ELEICAO %>% 
  unique %>% 
  rev()

n_anos <- length(anos)

for(i_malha in seq_len(n_malhas)){
  
  
  cat("Malha:", i_malha, "/", n_malhas, "\n")
  
  dir_relacao_lv_setor_ano_malha <- malhas[[i_malha]]
  ano_malha <- str_remove(dir_relacao_lv_setor_ano_malha, ".*_")
  
  for(i_ano in seq_len(n_anos)){
    
    cat("    |- Ano:", i_ano, "/", n_anos, "\n")
    
    
    ano <- anos[[i_ano]]
    
    resultad_ano <- resultados %>% 
      filter(AA_ELEICAO == ano)
    
    id_tses <-  resultad_ano %>% 
      pull(CD_MUNICIPIO) %>% 
      unique()
    
    n_muns <- length(id_tses)
    
    numeros_partidos_ano <- numeros_partidos_anos %>% 
      filter(ano == anos[[i_ano]])
    
    for(i_mun in seq_len(n_muns)){
      
      
      cat("         |- Município:", i_mun, "/", n_muns, "\n")
      
      id_tse <- id_tses[[i_mun]]
      
      id_ibge <- tse_ibge %>% 
        filter(codigo_tse == id_tse) %>% 
        pull(codigo_ibge)
      
      # Caso de cidades no exterior
      if(length(id_ibge) == 0) next
      
      
      resultad_mun <- resultad_ano %>% 
        filter(CD_MUNICIPIO == id_tse)  
        
      
      cargos <- resultad_mun %>% 
        pull(DS_CARGO) %>% 
        unique()
      
      n_cargos <- length(cargos) 
      
      for(i_cargo in seq_len(n_cargos)){
        
        
        cat("             |- Cargo:", i_cargo, "/", n_cargos, "\n")
        
        cargo <- cargos[[i_cargo]]
        
        if(cargo == "CONSELHEIRO DISTRITAL") next
        
        
        resultad_cargo <- resultad_mun %>% 
          filter(DS_CARGO == cargo)  
          
        
        turnos <- resultad_cargo %>% 
          pull(NR_TURNO) %>% 
          unique()
        
        n_turnos <- length(turnos)
        
        for(i_turno in seq_len(n_turnos)){
          
          
          cat("                 |- Turno:", i_turno, "/", n_turnos, "\n")
          
          
          turno <- turnos[[i_turno]]
          
          path_resultado_candidato <- glue("{dir_resultados_por_setor}/", 
                                           "{ano_malha}_{id_ibge}_{ano}_", 
                                           "{cargo}_{turno}_candidato.csv")
          
          if(file.exists(path_resultado_candidato)) next
          
          resultad_turno <- resultad_cargo %>% 
            filter(NR_TURNO == turno) 
            
          
          path_relacao <- glue("{dir_relacao_lv_setor_ano_malha}/", 
                               "{id_ibge}_{ano}_{turno}.csv")
          
          # Caso de municpios que não existiam em 2010
          if(!file.exists(path_relacao)) next
          
          relacao_lv_setor <-  path_relacao %>% 
            fread() %>%  
            select(id_lv, CD_SETOR, distancia_centroide)
          
          
          resultado_candidato <-  resultad_turno %>% 
            mutate(nr_partido = substring(NR_VOTAVEL, 1, 2)) %>% 
            left_join(numeros_partidos_ano, by = "nr_partido") %>% 
            inner_join(relacao_lv_setor, by = "id_lv", 
                       relationship = "many-to-many")  %>%
            group_by(CD_SETOR, NR_VOTAVEL, partido) %>% 
            summarise(votos = sum(QT_VOTOS), 
                      votos_ponderados = sum(QT_VOTOS * 1/distancia_centroide), 
                      .groups = "keep") %>% 
            ungroup() %>% 
            group_by(CD_SETOR) %>% 
            mutate(pct_voto = votos/sum(votos) * 100, 
                   pct_votos_ponderados = votos_ponderados/sum(votos_ponderados) * 100,
                   vt_val = NR_VOTAVEL < 95,
                   pct_votos_validos = votos/sum(votos[vt_val]) * 100,
                   pct_votos_ponderados_validos = votos_ponderados/sum(votos_ponderados[vt_val]) * 100) %>%
            ungroup() %>% 
            select(- vt_val)
          
          fwrite(resultado_candidato, path_resultado_candidato, 
                 row.names = FALSE)
          
          if(cargo %in% c("VEREADOR", "DEPUTADO FEDERAL", "DEPUTADO ESTADUAL", 
                          "DEPUTADO DISTRITAL")){
            
            
            path_resultado_partido <- glue("{dir_resultados_por_setor}/", 
                                           "{ano_malha}_{id_ibge}_{ano}_", 
                                           "{cargo}_{turno}_partido.csv")
            
            resultado_partido <- resultado_candidato %>% 
              group_by(CD_SETOR, partido) %>% 
              summarise(votos = sum(votos), 
                        votos_ponderados = sum(votos_ponderados), 
                        .groups = "keep") %>% 
              ungroup() %>% 
              group_by(CD_SETOR) %>% 
              mutate(pct_voto = votos/sum(votos) * 100, 
                     pct_votos_ponderados = votos_ponderados/sum(votos_ponderados) * 100,
                     vt_val = !is.na(partido),
                     pct_votos_validos = votos/sum(votos[vt_val]) * 100,
                     pct_votos_ponderados_validos = votos_ponderados/sum(votos_ponderados[vt_val]) * 100) %>%
              ungroup() %>% 
              select(-vt_val) %>% 
              mutate(ano = ano, 
                     cargo = cargo, 
                     turno = turno, 
                     id_ibge = id_ibge, 
                     id_tse = id_tse)
            
            fwrite(resultado_partido, path_resultado_partido, 
                   row.names = FALSE)
            
            if(ano == 2024){
              
              path_resultado_ideologia <- glue("{dir_resultados_por_setor}/", 
                                               "{ano_malha}_{id_ibge}_{ano}_", 
                                               "{cargo}_{turno}_ideologia.csv")
              
              resultado_ideologia <- resultado_partido %>% 
                left_join(ideologia_2024, by = "partido") %>%
                group_by(CD_SETOR, ideologia) %>% 
                summarise(votos = sum(votos), 
                          votos_ponderados = sum(votos_ponderados), 
                          .groups = "keep") %>% 
                ungroup() %>% 
                group_by(CD_SETOR) %>% 
                mutate(pct_voto = votos/sum(votos) * 100, 
                       pct_votos_ponderados = votos_ponderados/sum(votos_ponderados) * 100,
                       vt_val = !is.na(ideologia),
                       pct_votos_validos = votos/sum(votos[vt_val]) * 100,
                       pct_votos_ponderados_validos = votos_ponderados/sum(votos_ponderados[vt_val]) * 100) %>%
                ungroup() %>% 
                select(-vt_val) %>% 
                mutate(ano = ano, 
                       cargo = cargo, 
                       turno = turno, 
                       id_ibge = id_ibge, 
                       id_tse = id_tse)
              
              fwrite(resultado_ideologia, path_resultado_ideologia, 
                     row.names = FALSE)
            }
          }      
        }
      }
    } 
  }
}


