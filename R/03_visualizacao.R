
###### ANÁLISE - ESCOLAS ATIVAS EM 2020 E NÃO ATIVAS EM 2021

#### PASSO 4 - MAPA DE ESCOLAS NÃO ATIVAS EM 2021

# Carregar pacotes

library(tidyverse)
library(tidylog)
library(data.table)
library(janitor)
library(geobr)

# Função pra melhorar a visualização no Windows

trace(grDevices::png, quote({
  if (missing(type) && missing(antialias)) {
    type <- "cairo-png"
    antialias <- "subpixel"
  }
}), print = FALSE)

# Carregar base tratada de creches ativas em 2020 e não ativas em 2021

creches_br <- read.csv2("data/matriculas_por_escola_2020.csv",
                        fileEncoding = "Windows-1252")

# Carregar dados geolocalizados das escolas (pacote geobr)

coord_escolas_br <- read_schools()

# Arrumar nome da coluna-chave para o cruzamento

coord_escolas_br_limpa <- coord_escolas_br %>%
  rename(CO_ENTIDADE = code_school) %>%
  select(CO_ENTIDADE,
         education_level,
         address,
         phone_number,
         geom)

# Juntar bases

creches_br_coord <- left_join(creches_br, coord_escolas_br_limpa)

### Selecionar apenas escolas de SP que não funcionaram em 2021

creches_inativas_sp <- creches_br_coord %>%
  filter(SG_UF == "SP" & situacao_2021 != "ativa_2021") %>%
  mutate(matriculas_2020 = creche_mat_2020 + pre_mat_2020,
         rede = ifelse(dependencia == "3_privada_nao_conveniada", "privada", "pública ou conveniada"))

# Exportar para usar no Flourish -- Estado de SP

escolas_flourish <- creches_inativas_sp %>%
  select(CO_ENTIDADE,
         nome_escola_2020,
         situacao_2021,
         dependencia,
         rede,
         matriculas_2020,
         address,
         geom) %>%
  mutate(geom = gsub('[()°]', '', geom)) %>%
  separate(col = geom, into = c('Latitude', 'Longitude '), sep = '\\,') %>%
  mutate(Latitude = str_sub(Latitude, start = 2))

write.csv(escolas_flourish, "data/mapa_para_flourish.csv",
           row.names = F, fileEncoding = "Windows-1252")

# Criar camada com o mapa de SP

sp <- read_municipality(code_muni = "SP", year = 2020)

# Tela 1 - Todas as escolas sem distinção

mapa_creches_sp_1 <- creches_inativas_sp %>%
  ggplot() +
  geom_sf(data = sp, fill = "#CCCCCC", color = "#FFFFFF") +
  geom_sf(aes(geometry=geom),
          color = "#A80000",
          size = 1.2,
          show.legend = F) +
  theme_void() +
  labs(title="CRECHES ATIVAS EM 2020 E INATIVAS EM 2021",
       subtitle="Todas as redes de ensino",
       caption="Fonte: Levantamento TV Globo com dados do Censo Escolar/Inep")

# Tela 2 - Todas as escolas colorindo segundo a rede de ensino

mapa_creches_sp_2 <- creches_inativas_sp %>%
  ggplot() +
  geom_sf(data = sp, fill = "#CCCCCC", color = "#FFFFFF") +
  geom_sf(aes(geometry=geom,
          color = dependencia),
          size = 1.2) +
  scale_color_manual(
    values=c("#dc9999", "#a80000", "#3D748F"),
    labels = c("Pública", "Privada conveniada", "Privada não conveniada")
  )+
  theme_void() +
  theme(legend.position="top",
        legend.title = element_blank()) +
  labs(title="CRECHES ATIVAS EM 2020 E INATIVAS EM 2021",
       subtitle="Segundo a rede de ensino",
       caption="Fonte: Levantamento TV Globo com dados do Censo Escolar/Inep")

# Tela 3 - Todas as escolas colorindo segundo a rede de ensino e pelo total de matrículas em 2020

mapa_creches_sp_3 <- creches_inativas_sp %>%
  ggplot() +
  geom_sf(data = sp, fill = "#CCCCCC", color = "#DDDDDD", stroke = 0.3) +
  geom_sf(aes(geometry=geom,
              fill = rede,
          size = matriculas_2020),
          stroke = 1,
          shape = 21,
          color = "#EDEDED"
          ) +
  scale_fill_manual(
    values=c("#3D748F", "#a80000"),
    labels = c("Pública ou conveniada", "Privada não conveniada")
  )+
  theme_void() +
  theme(legend.position="top",
        legend.title = element_blank()) +
  labs(title="CRECHES E PRÉ-ESCOLAS ATIVAS EM 2020 E INATIVAS EM 2021",
       subtitle="Segundo a rede de ensino e o total de matrículas",
       caption="Fonte: Levantamento TV Globo com dados do Censo Escolar/Inep")

# Tela 4 - Zoom na Região Metropolitana de São Paulo

# Criar camada com o mapa da Grande SP

grande_sp <- subset(sp,
                    code_muni == 3503901 |
                      code_muni == 3505708 |
                      code_muni == 3506607 |
                      code_muni == 3509007 |
                      code_muni == 3509205 |
                      code_muni == 3510609 |
                      code_muni == 3513009 |
                      code_muni == 3513801 |
                      code_muni == 3515004 |
                      code_muni == 3515103 |
                      code_muni == 3515707 |
                      code_muni == 3516309 |
                      code_muni == 3516408 |
                      code_muni == 3518305 |
                      code_muni == 3518800 |
                      code_muni == 3522208 |
                      code_muni == 3522505 |
                      code_muni == 3523107 |
                      code_muni == 3525003 |
                      code_muni == 3526209 |
                      code_muni == 3528502 |
                      code_muni == 3529401 |
                      code_muni == 3530607 |
                      code_muni == 3534401 |
                      code_muni == 3539103 |
                      code_muni == 3539806 |
                      code_muni == 3543303 |
                      code_muni == 3544103 |
                      code_muni == 3545001 |
                      code_muni == 3546801 |
                      code_muni == 3547304 |
                      code_muni == 3547809 |
                      code_muni == 3548708 |
                      code_muni == 3548807 |
                      code_muni == 3549953 |
                      code_muni == 3550308 |
                      code_muni == 3552502 |
                      code_muni == 3552809 |
                      code_muni == 3556453)

# Filtras base de escolas para reduzir para as da Grande SP

creches_inativas_grande_sp <- creches_inativas_sp %>%
  filter(CO_MUNICIPIO == 3503901 |
           CO_MUNICIPIO == 3505708 |
           CO_MUNICIPIO == 3506607 |
           CO_MUNICIPIO == 3509007 |
           CO_MUNICIPIO == 3509205 |
           CO_MUNICIPIO == 3510609 |
           CO_MUNICIPIO == 3513009 |
           CO_MUNICIPIO == 3513801 |
           CO_MUNICIPIO == 3515004 |
           CO_MUNICIPIO == 3515103 |
           CO_MUNICIPIO == 3515707 |
           CO_MUNICIPIO == 3516309 |
           CO_MUNICIPIO == 3516408 |
           CO_MUNICIPIO == 3518305 |
           CO_MUNICIPIO == 3518800 |
           CO_MUNICIPIO == 3522208 |
           CO_MUNICIPIO == 3522505 |
           CO_MUNICIPIO == 3523107 |
           CO_MUNICIPIO == 3525003 |
           CO_MUNICIPIO == 3526209 |
           CO_MUNICIPIO == 3528502 |
           CO_MUNICIPIO == 3529401 |
           CO_MUNICIPIO == 3530607 |
           CO_MUNICIPIO == 3534401 |
           CO_MUNICIPIO == 3539103 |
           CO_MUNICIPIO == 3539806 |
           CO_MUNICIPIO == 3543303 |
           CO_MUNICIPIO == 3544103 |
           CO_MUNICIPIO == 3545001 |
           CO_MUNICIPIO == 3546801 |
           CO_MUNICIPIO == 3547304 |
           CO_MUNICIPIO == 3547809 |
           CO_MUNICIPIO == 3548708 |
           CO_MUNICIPIO == 3548807 |
           CO_MUNICIPIO == 3549953 |
           CO_MUNICIPIO == 3550308 |
           CO_MUNICIPIO == 3552502 |
           CO_MUNICIPIO == 3552809 |
           CO_MUNICIPIO == 3556453)

# Repetir o mapa da Tela 3, mas com a camada da Grande SP

mapa_creches_sp_4 <- creches_inativas_grande_sp %>%
  ggplot() +
  geom_sf(data = grande_sp, fill = "#CCCCCC", color = "#FFFFFF") +
  geom_sf(aes(geometry=geom,
              shape = 21,
              color = "white",
              fill = rede,
              stroke = 2,
              size = matriculas_2020)) +
  scale_color_manual(
    values=c("#dc9999", "#a80000", "#3D748F"),
    labels = c("Pública", "Privada conveniada", "Privada não conveniada")
  )+
  theme_void() +
  theme(legend.position="top",
        legend.title = element_blank()) +
  labs(title="CRECHES ATIVAS EM 2020 E INATIVAS EM 2021",
       subtitle="Por rede de ensino e matrículas - Grande SP",
       caption="Fonte: Levantamento TV Globo com dados do Censo Escolar/Inep")


# Exportar para usar no Flourish -- Grande SP

escolas_flourish_rmsp <- creches_inativas_grande_sp %>%
  select(CO_ENTIDADE,
         nome_escola_2020,
         situacao_2021,
         dependencia,
         rede,
         matriculas_2020,
         address,
         geom) %>%
  mutate(geom = gsub('[()°]', '', geom)) %>%
  separate(col = geom, into = c('Latitude', 'Longitude '), sep = '\\,') %>%
  mutate(Latitude = str_sub(Latitude, start = 2))

write.csv(escolas_flourish_rmsp, "data/mapa_para_flourish_rmsp.csv",
          row.names = F, fileEncoding = "Windows-1252")


# Salvar todos os mapas em formato PNG

library(cowplot)

save_plot("img/mapa1.png", mapa_creches_sp_1, base_height = 10, base_width = 12)
save_plot("img/mapa2.png", mapa_creches_sp_2, base_height = 10, base_width = 12)
save_plot("img/mapa3.png", mapa_creches_sp_3, base_height = 10, base_width = 12)
save_plot("img/mapa4.png", mapa_creches_sp_4, base_height = 10, base_width = 12)


# Gerar um gif dos mapas no R usando o pacote magick

install.packages("magick")

library(magick)

# Lista o nome dos arquivos
imagens <- list.files("img", full.names = TRUE)

lista_imagens <- lapply(imgagens, image_read)

# Juntar as imagens
juntar_imagens <- image_join(lista_imagens)

# Animar as imagens com FPS 5 (5 frames por segundo)
animar_imagens <- image_animate(juntar_imagens, fps = .5)

# Salvar o arquivo
image_write(image = animar_imagens,
            path = "img/mapas.gif")
