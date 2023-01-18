
###### ANÁLISE - ESCOLAS ATIVAS EM 2020 E NÃO ATIVAS EM 2021

#### PASSO 1 - LIMPAR DADOS BRUTOS

# Origem das bases: Censo Escolar/Inep (edições 2019, 2020 e 2021)
# https://www.gov.br/inep/pt-br/acesso-a-informacao/dados-abertos/microdados/censo-escolar

# Se os pacotes ainda nao estiverem instalados no computador:
# install.packages(c("tidyverse", tidylog", "data.table", "janitor"))

# Carregar pacotes

library(tidyverse)
library(tidylog)
library(data.table)
library(janitor)


# Carregar base 2019, com colunas selecionadas
# e alterando o nome de algumas colunas para facilitar o cruzamento

mat_2019 <- fread("raw/microdados_ed_basica_2019.csv", sep = ";", encoding = "Latin-1") %>%
  select(CO_ENTIDADE,
         NO_ENTIDADE,
         TP_SITUACAO_FUNCIONAMENTO,
         IN_INF_CRE,
         IN_INF_PRE,
         QT_MAT_INF_CRE,
         QT_MAT_INF_PRE) %>%
  rename(nome_escola_2019 = NO_ENTIDADE,
         situacao_2019 = TP_SITUACAO_FUNCIONAMENTO,
         creche_mat_2019 = QT_MAT_INF_CRE,
         pre_mat_2019 = QT_MAT_INF_PRE,
         tem_creche_2019 = IN_INF_CRE,
         tem_pre_2019 = IN_INF_PRE)

# Carregar base 2020, com colunas selecionadas
# e alterando o nome de algumas colunas para facilitar o cruzamento
# Filtrar apenas pelas escolas que em 2020 ofereciam ensino infantil (creche ou pré-escola)

mat_2020_p <- fread("raw/microdados_ed_basica_2020.csv", sep = ";", encoding = "Latin-1") %>%
  select(CO_ENTIDADE,
         NO_ENTIDADE,
         TP_DEPENDENCIA,
         TP_CATEGORIA_ESCOLA_PRIVADA,
         IN_CONVENIADA_PP,
         TP_CONVENIO_PODER_PUBLICO,
         TP_LOCALIZACAO,
         TP_SITUACAO_FUNCIONAMENTO,
         NO_MUNICIPIO,
         CO_MUNICIPIO,
         SG_UF,
         IN_INF_CRE,
         IN_INF_PRE,
         QT_MAT_INF_CRE,
         QT_MAT_INF_PRE,
         DS_ENDERECO,
         NU_ENDERECO,
         DS_COMPLEMENTO,
         NO_BAIRRO,
         CO_CEP,
         NU_DDD,
         NU_TELEFONE) %>%
  filter((IN_INF_CRE == 1 | IN_INF_PRE == 1)
         & (QT_MAT_INF_CRE >= 1 | QT_MAT_INF_PRE >=1)) %>%
  rename(nome_escola_2020 = NO_ENTIDADE,
         situacao_2020 = TP_SITUACAO_FUNCIONAMENTO,
         creche_mat_2020 = QT_MAT_INF_CRE,
         pre_mat_2020 = QT_MAT_INF_PRE,
         tem_creche_2020 = IN_INF_CRE,
         tem_pre_2020 = IN_INF_PRE) %>%
  mutate(NU_DDD = as.integer(NU_DDD),
         NU_TELEFONE = as.integer(NU_TELEFONE))

# Carregar base 2021, com colunas selecionadas
# e alterando o nome de algumas colunas para facilitar o cruzamento

mat_2021 <- fread("raw/microdados_ed_basica_2021.csv", sep = ";", encoding = "Latin-1") %>%
  select(CO_ENTIDADE,
         NO_ENTIDADE,
         TP_SITUACAO_FUNCIONAMENTO,
         IN_INF_CRE,
         IN_INF_PRE,
         QT_MAT_INF_CRE,
         QT_MAT_INF_PRE) %>%
  rename(nome_escola_2021 = NO_ENTIDADE,
         situacao_2021 = TP_SITUACAO_FUNCIONAMENTO,
         creche_mat_2021 = QT_MAT_INF_CRE,
         pre_mat_2021 = QT_MAT_INF_PRE,
         tem_creche_2021 = IN_INF_CRE,
         tem_pre_2021 = IN_INF_PRE)

# Juntar as tabelas de 2019, 2020 e 2021, considerando a de 2020 como "principal"

matriculas_por_escola_2020 <-
  list(mat_2020_p, mat_2019, mat_2021) %>%
  reduce(left_join, by = c("CO_ENTIDADE")) %>%
  mutate(dependencia = case_when(
    TP_DEPENDENCIA == 1 ~ "1_publica",
    TP_DEPENDENCIA == 2 ~ "1_publica",
    TP_DEPENDENCIA == 3 ~ "1_publica",
    TP_DEPENDENCIA == 4 & IN_CONVENIADA_PP == 1 ~ "2_privada_conveniada",
    TP_DEPENDENCIA == 4 & IN_CONVENIADA_PP == 0 ~ "3_privada_nao_conveniada"
  ),
  situacao_2019 = case_when(
    situacao_2019 == 1 ~ "ativa_2019",
    situacao_2019 == 2 ~ "paralisada_2019",
    situacao_2019 == 3 ~ "extinta_2019"
  ),
  situacao_2020 = case_when(
    situacao_2020 == 1 ~ "ativa_2020",
    situacao_2020 == 2 ~ "paralisada_2020",
    situacao_2020 == 3 ~ "extinta_2020"
  ),
  situacao_2021 = case_when(
    situacao_2021 == 1 ~ "ativa_2021",
    situacao_2021 == 2 ~ "paralisada_2021",
    situacao_2021 == 3 ~ "extinta_2021"
  ),
  etapa = case_when(
    tem_creche_2020 == 1 & tem_pre_2020 == 1 ~ "creche_e_pre",
    tem_creche_2020 == 1 & tem_pre_2020 == 0 ~ "creche",
    tem_creche_2020 == 0 & tem_pre_2020 == 1 ~ "pre",
    T ~ "erro"
  ))


# Salvar base de trabalho
write.csv2(matriculas_por_escola_2020, "data/matriculas_por_escola_2020.csv", fileEncoding = "Windows-1252", row.names = F)




