# Resultados eleitorais brasileiros por setores censitários

Estes códigos agregam os resultados das eleições por candidato, partido e ideologia pelos setores censitários.

## Códigos:

Abaixo o que cada código faz:

### 1. Coleta de Dados (`01_baixa_dados.R`)
Realiza o download dos seguintes dados:
- Resultados eleitorais, candidatos e eleitorado (TSE, 2010-2024)
- Dados socioeconômicos e malhas dos setores censitários (IBGE)
- Cadastro Nacional de Endereços para Fins Estatísticos (CNFE/IBGE)
- Correspondência entre códigos de municípios TSE-IBGE
- Números e ideologia dos partidos em 2024
- Malha específica do Mato Grosso com Boa Esperança do Norte
- Adiciona Boa Esperança do Norte na malha atual
- Distritos da cidade de São Paulo

### 2. Geolocalização dos Locais de Votação (`02_geolocalizacao_lv.R`)
Determina as coordenadas geográficas dos locais de votação por município nos casos que elas não são fornecidas pelo TSE:
- Tenta correspondência com endereços do CNFE usando CEP e logradouro
- Usa API do Google Maps quando não encontra no CNFE
- Inclui tratamento manual para casos não resolvidos

### 3. Vinculação com Setores Censitários com os locais de votação (`03_relacao_setor_lv.R`)
Estabelece a relação entre locais de votação e setores censitários:
- Calcula distâncias entre locais de votação e setores usando geometria
- Agrupa locais de votação por faixas de distância (100 em 100 metros) aos setores
- Seleciona os setores do agrupamento mais próximo
- Calcula distância para o centroide do setor para ponderação

### 4. Agregação dos Resultados por setores censitários (`04_resultados_por_setor.R`)
Processa os resultados eleitorais por setor censitário:
- Agrega votos por candidato, partido e ideologia para cada setor
- Calcula porcentagens de votos normais e ponderadas por distância do centróide
- Processa separadamente cada cargo, turno e ano

### 5. Análise de Renda (`05_analise_renda_setor_sp.R`):
- Análise da votação do PT para vereador ao longo dos anos na cidade de São Paulo segundo a renda dos setores censitários

### 6. Análise Ideológica (`06_analise_mapa_vereador_ideologia_sp.R`):
- Análise da votação para vereador dos setores paulistas em 2024 de acordo com a ideologia


## Observação:

Antes de rodar os códigos é necessário adicionar alterar essas linhas do arquivo `00_variaveis.R`:

```
# Chaves
api_gmap <- "XXXXXXXXXXX"
gs4_auth("XXXXXXXXXXXX@gmail.com")
``` 

com a chave da API do google maps para acessar o Google Maps e um email do google para ler e escrever os arquivos no drive.

Também é necessário instalar os pacotes presentes no arquivo `00_pacotes.R`.


## Resultados:

Os códigos acima criam três pastas principais que contêm dados brutos (que são baixados pelo código `01_baixa_dados`), intermediários e processados. Os dados intermediários se encontram aqui:

* [Locais de votação georeferenciados](https://drive.google.com/file/d/16VdkbFPEpHRwbKLvQqzpeEuAK13D49g-/view?usp=sharing)(criado pelo código `02_geolocalizacao_lv`): Cada arquivo contém os locais de votação de uma cidade. O nome dos arquivos são os códigos IBGE das cidade. No caso de Boa Esperança do Norte que ainda não existia em 2024 usei 5106241.
* [Relação dos locais de votação com setores censitários](https://drive.google.com/file/d/19U7m-P-yT_zmqt88-c1GZahEpSrwxAJu/view?usp=sharing) (criadas pelo código `03_relacao_setor_lv`): Há duas pastas, uma para cada malha de setores censtitários. Os arquivos dentro dentro das pastas das malhas estão estão nomeados como `<id_ibge_municipio>_<ano_eleicao>_<turno>.csv`
* [Resultado agregado por setores censitários](https://drive.google.com/file/d/1q6B1DHBbR0cbWQLunnMshRQJnitV79gM/view?usp=sharing) (criados pelo código `04_resultados_por_setor`): Os arquivos estão nomedos como `<ano_malha>_<id_ibge_cidade>_<ano_eleicao>_<cargo>_<turno>_<classe_de_agregacao>.csv` (São 715 mil arquivos que descompatados totalizam 189 gb).
