
###### ANÁLISE - ESCOLAS ATIVAS EM 2020 E NÃO ATIVAS EM 2021

#### PASSO 2 - FILTROS E AGRUPAMENTOS


# Se precisar, carregar base tratada de creches ativas em 2020 e não ativas em 2021

matriculas_por_escola_2020 <- read.csv2("data/matriculas_por_escola_2020.csv",
                        fileEncoding = "Windows-1252")

### 1- Total de creches e pré-escolas ativas em 2020

# BRASIL

escolas_br_creches_2020 <- matriculas_por_escola_2020 %>%
  filter(tem_creche_2020 == 1)

# Gabarito: 70.894

escolas_br_pre_2020 <- matriculas_por_escola_2020 %>%
  filter(tem_pre_2020 == 1)

# Gabarito: 101.012

# SÃO PAULO

escolas_sp_creches_2020 <- matriculas_por_escola_2020 %>%
  filter(SG_UF == "SP" & tem_creche_2020 == 1)

write.csv2(escolas_sp_creches_2020, "data/escolas_sp_creches_2020.csv",
           fileEncoding = "Windows-1252", row.names = F)

# Gabarito: 14.305

escolas_sp_pre_2020 <- matriculas_por_escola_2020 %>%
  filter(SG_UF == "SP" & tem_pre_2020 == 1)

# Gabarito: 12.626

# CAPITAL

escolas_capital_creches_2020 <- matriculas_por_escola_2020 %>%
  filter(SG_UF == "SP" & tem_creche_2020 == 1 & NO_MUNICIPIO == "São Paulo")

# Gabarito: 4.125

escolas_capital_pre_2020 <- matriculas_por_escola_2020 %>%
  filter(SG_UF == "SP" & tem_pre_2020 == 1 & NO_MUNICIPIO == "São Paulo")

# Gabarito: 2.605

### 2- Creches ativas em 2020 e não ativas em 2021

# BRASIL

creches_fechadas_br_2021 <- matriculas_por_escola_2020 %>%
  filter(tem_creche_2020 == 1) %>%
  group_by(situacao_2021, dependencia) %>%
  summarise(total = n()) %>%
  pivot_wider(names_from = situacao_2021, values_from = total) %>%
  mutate(total_escolas_2021 = ativa_2021 + paralisada_2021 + extinta_2021 + `NA`,
         pct_extintas_2021 = round((( extinta_2021 * 100 ) / total_escolas_2021),1),
         pct_paralisadas_2021 = round((( paralisada_2021 * 100 ) / total_escolas_2021),1),
         pct_nao_ativas_2021 = round((( ( extinta_2021 + paralisada_2021 ) * 100) / total_escolas_2021),1)) %>%
  rename(situacao_nao_informada_2021 = `NA`,
         creches_brasil = dependencia)

# SÃO PAULO

creches_fechadas_sp_2021 <- matriculas_por_escola_2020 %>%
  filter(tem_creche_2020 == 1 & SG_UF == "SP") %>%
  group_by(situacao_2021, dependencia) %>%
  summarise(total = n()) %>%
  pivot_wider(names_from = situacao_2021, values_from = total) %>%
  mutate(total_escolas_2021 = ativa_2021 + paralisada_2021 + extinta_2021,
         pct_extintas_2021 = round((( extinta_2021 * 100 ) / total_escolas_2021),1),
         pct_paralisadas_2021 = round((( paralisada_2021 * 100 ) / total_escolas_2021),1),
         pct_nao_ativas_2021 = round((( ( extinta_2021 + paralisada_2021 ) * 100) / total_escolas_2021),1)) %>%
  rename(creches_sp = dependencia)

write.csv2(creches_fechadas_sp_2021, "data/creches_fechadas_sp_2021.csv", fileEncoding = "Windows-1252", row.names = F)


# CAPITAL

creches_fechadas_capital_2021 <- matriculas_por_escola_2020 %>%
  filter(tem_creche_2020 == 1 & SG_UF == "SP" & NO_MUNICIPIO == "São Paulo") %>%
  group_by(situacao_2021, dependencia) %>%
  summarise(total = n()) %>%
  pivot_wider(names_from = situacao_2021, values_from = total) %>%
  mutate(total_escolas_2021 = ativa_2021 + paralisada_2021 + extinta_2021,
         pct_extintas_2021 = round((( extinta_2021 * 100 ) / total_escolas_2021),1),
         pct_paralisadas_2021 = round((( paralisada_2021 * 100 ) / total_escolas_2021),1),
         pct_nao_ativas_2021 = round((( ( extinta_2021 + paralisada_2021 ) * 100) / total_escolas_2021),1)) %>%
  rename(creches_capital = dependencia)

### 3- Pré-escolas ativas em 2020 e não ativas em 2021

# BRASIL

preescolas_fechadas_br_2021 <- matriculas_por_escola_2020 %>%
  filter(tem_pre_2020 == 1) %>%
  group_by(situacao_2021, dependencia) %>%
  summarise(total = n()) %>%
  pivot_wider(names_from = situacao_2021, values_from = total) %>%
  mutate(total_escolas_2021 = ativa_2021 + paralisada_2021 + extinta_2021 + `NA`,
         pct_extintas_2021 = round((( extinta_2021 * 100 ) / total_escolas_2021),1),
         pct_paralisadas_2021 = round((( paralisada_2021 * 100 ) / total_escolas_2021),1),
         pct_nao_ativas_2021 = round((( ( extinta_2021 + paralisada_2021 ) * 100) / total_escolas_2021),1)) %>%
  rename(situacao_nao_informada_2021 = `NA`,
         preescolas_brasil = dependencia)

# SÃO PAULO

preescolas_fechadas_sp_2021 <- matriculas_por_escola_2020 %>%
  filter(tem_pre_2020 == 1 & SG_UF == "SP") %>%
  group_by(situacao_2021, dependencia) %>%
  summarise(total = n()) %>%
  pivot_wider(names_from = situacao_2021, values_from = total) %>%
  mutate(total_escolas_2021 = ativa_2021 + paralisada_2021 + extinta_2021,
         pct_extintas_2021 = round((( extinta_2021 * 100 ) / total_escolas_2021),1),
         pct_paralisadas_2021 = round((( paralisada_2021 * 100 ) / total_escolas_2021),1),
         pct_nao_ativas_2021 = round((( ( extinta_2021 + paralisada_2021 ) * 100) / total_escolas_2021),1)) %>%
  rename(preescolas_sp = dependencia)

# CAPITAL

preescolas_fechadas_capital_2021 <- matriculas_por_escola_2020 %>%
  filter(tem_pre_2020 == 1 & SG_UF == "SP" & NO_MUNICIPIO == "São Paulo") %>%
  group_by(situacao_2021, dependencia) %>%
  summarise(total = n()) %>%
  pivot_wider(names_from = situacao_2021, values_from = total) %>%
  mutate(total_escolas_2021 = ativa_2021 + paralisada_2021 + extinta_2021,
         pct_extintas_2021 = round((( extinta_2021 * 100 ) / total_escolas_2021),1),
         pct_paralisadas_2021 = round((( paralisada_2021 * 100 ) / total_escolas_2021),1),
         pct_nao_ativas_2021 = round((( ( extinta_2021 + paralisada_2021 ) * 100) / total_escolas_2021),1)) %>%
  rename(preescolas_capital = dependencia)
