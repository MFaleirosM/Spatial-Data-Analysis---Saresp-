#### IDH no mapa de São Paulo 
pacotes <- c("plotly","tidyverse","rgdal","spdep","knitr","kableExtra","tmap",
             "gtools","grid","knitr","kableExtra","viridis",  "ggplot2", "spdep"
)

lapply(pacotes, require, character.only = TRUE)

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

# Carregando um shapefile:
shp_sp <- readOGR(dsn = "shapefile_sp",layer = "estado_sp")

# Visualizando o shapefile carregado:
tmap_mode("view")
tm_shape(shp = shp_sp) +
  tm_borders()

shp_sp %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 21)
# Quantidade de muncipios no shapefile
nrow(as.data.frame(unique(shp_sp@data$NM_MUNICIP)))

# Carregando IDH:
load("dados_sp.RData")

# Observando a base de dados carregada
dados_sp %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 16)

# Dataframe com informações adicionais dos municípios
df <- as.data.frame(dados_sp)
nrow(df)

# Dataframe com informações com IDH e PIB dos municípios
df_idh <- df[ , c(1,4,5)]

colnames(df_idh) <- c("CD_GEOCMU", "IDH", "PIB")
View(df_idh)

## MERGE com o shape_file
shp_dados_sp <- merge(x = shp_sp,
                      y = df_idh,
                      by.x = "CD_GEOCMU",
                      by.y = "CD_GEOCMU")

shp_dados_sp@data %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)
nrow(shp_dados_sp@data)
############### mapas de quantis ############################
# Paletas de cores
display.brewer.all()

quantis_idh <- tm_shape(shp = shp_dados_sp) +
  tm_fill(col="IDH", style = "quantile", n = 5, 
          title="Faixa por quantil",
          palette = "Accent",
          # c("#A0522D", "#FFD700", "#FA8072", "#8A2BE2", "#008000"),
          # sienna, gold, salmon, blue violet, green
          legend.hist=TRUE,
          legend.hist.title = "Histograma de IDH's"
          # labels=c("< 20%", "20% - 40%", "40% - 60%", "60% - 80%","> 80%")
  ) +
  tm_borders(col="black",lwd=0.8) +
  tm_layout(legend.outside = TRUE, legend.outside.position = "right",
            title.size = 0.8, title.position = c("left","bottom"))
quantis_idh
tmap_save(tm = quantis_idh, filename = "idh.jpeg", dpi = 600)

quantis_pib <- tm_shape(shp = shp_dados_sp) +
  tm_fill(col="PIB", style = "quantile", n = 5, 
          title="Faixa por quantil",
          palette = "Accent",
          # c("#A0522D", "#FFD700", "#FA8072", "#8A2BE2", "#008000"),
          # sienna, gold, salmon, blue violet, green
          legend.hist=TRUE,
          legend.hist.title = "Histograma de PIB's"
          # labels=c("< 20%", "20% - 40%", "40% - 60%", "60% - 80%","> 80%")
  ) +
  tm_borders(col="black",lwd=0.8) +
  tm_layout(legend.outside = TRUE, legend.outside.position = "right",
            title.size = 0.8, title.position = c("left","bottom"))
quantis_pib
tmap_save(tm = quantis_pib, filename = "pib.jpeg", dpi = 600)


############## MORAN ##############################
vizinhos <- poly2nb(pl = shp_sp,
                    queen = TRUE,
                    row.names = shp_sp@data$NM_MUNICIP)
W <- nb2mat(neighbours = vizinhos, style = "W", zero.policy = TRUE)
colnames(W) <- shp_sp@data$NM_MUNICIP
View(W)
W_listw <- mat2listw(W)
shp_dados_sp@data["Z_idh"] <- scale(shp_dados_sp@data$IDH)
shp_dados_sp@data["Z_pib"] <- scale(shp_dados_sp@data$PIB)
#Checando a nova coluna criada com as médias padronizadas
summary(shp_dados_sp@data$Z_idh)
summary(shp_dados_sp@data$Z_pib)
sd(shp_dados_sp@data$Z_idh, na.rm = T)
sd(shp_dados_sp@data$Z_pib, na.rm = T)
mean(shp_dados_sp@data$Z_idh, na.rm = T)
mean(shp_dados_sp@data$Z_pib, na.rm = T)
##
medias_idh <- as.numeric(na.omit(shp_dados_sp@data$Z_idh))
medias_pib <- as.numeric(na.omit(shp_dados_sp@data$Z_pib))

moran.test(x = medias_idh, 
           listw = W_listw, 
           zero.policy = TRUE)
moran.test(x = medias_pib, 
           listw = W_listw, 
           zero.policy = TRUE)
####### GRÁFICO DE DISPERSÃO DE MORAN ###############################
#### IDH
defasagem_idh <- lag.listw(x=W_listw,
                           var=na.omit(shp_dados_sp@data$IDH),
                           zero.policy = T)
df_idh <- data.frame(cidade = shp_dados_sp@data$NM_MUNICIP,
                 idh = as.numeric(shp_dados_sp@data$IDH),
                 defasagem_idh = as.numeric(defasagem_idh))
str(df_idh)
p <- df_idh %>% ggplot(aes(label = cidade))+
  geom_point(aes(x = idh, y = defasagem_idh), alpha = 0.5, size = 2) +
  geom_smooth(aes(x = idh, y = defasagem_idh), color = "#FF8C00", method ='lm', se = F) +
  geom_hline(yintercept = mean(df_idh$defasagem_idh), lty = 2) +
  geom_vline(xintercept = mean(df_idh$idh), lty = 2) +
  scale_x_continuous(limits = c(0.65, 0.85)) +
  scale_y_continuous(limits = c(0.65, 0.85)) +
  xlab("IDH") +
  ylab("Média espacialmente defasada") +
  annotate("text", x = 0.85, y = 0.85, label = "Alto-Alto", size=4) +
  annotate("text", x = 0.85, y = 0.65, label = "Alto-Baixo", size=4) +
  annotate("text", x = 0.65, y = 0.85, label = "Baixo-Alto", size=4) +
  annotate("text", x = 0.651, y = 0.65, label = "Baixo-Baixo", size=4) +
  theme_bw() +
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15))
plotly::ggplotly(p)
rm(df_idh)
### PIB
defasagem_pib <- lag.listw(x=W_listw,
                           var=na.omit(shp_dados_sp@data$PIB),
                           zero.policy = T)
df_pib <- data.frame(cidade = shp_dados_sp@data$NM_MUNICIP,
                     pib = as.numeric(shp_dados_sp@data$PIB),
                     defasagem_pib = as.numeric(defasagem_pib))
str(df_pib)
p <- df_pib %>% ggplot(aes(label = cidade))+
  geom_point(aes(x = pib, y = defasagem_pib), alpha = 0.5, size = 2) +
  geom_smooth(aes(x = pib, y = defasagem_pib), color = "#FF8C00", method ='lm', se = F) +
  geom_hline(yintercept = mean(df_pib$defasagem_pib), lty = 2) +
  geom_vline(xintercept = mean(df_pib$pib), lty = 2) +
  # scale_x_continuous(limits = c(0.65, 0.85)) +
  # scale_y_continuous(limits = c(0.65, 0.85)) +
  xlab("PIB") +
  ylab("Média espacialmente defasada") +
  # annotate("text", x = 0.85, y = 0.85, label = "Alto-Alto", size=4) +
  # annotate("text", x = 0.85, y = 0.65, label = "Alto-Baixo", size=4) +
  # annotate("text", x = 0.65, y = 0.85, label = "Baixo-Alto", size=4) +
  # annotate("text", x = 0.651, y = 0.65, label = "Baixo-Baixo", size=4) +
  theme_bw() +
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15))
plotly::ggplotly(p)

####### MAPA I de MORAN LOCAL IDH ############################################################################
moran_local_idh <- localmoran(x = as.numeric(shp_dados_sp@data$IDH),
                          listw = W_listw, 
                          zero.policy = TRUE)

#Matriz com as estatísticas locais de Moran
View(moran_local_idh) 
class(moran_local_idh)

shp_dados_sp <- cbind(shp_dados_sp, as.data.frame(moran_local_idh))

View(shp_dados_sp@data)
## extração do quadrante de dispersão de moran 
AT <- attr(x = moran_local_idh, which = "quadr")
View(AT)
moran_idh <- as.data.frame(x = AT$mean)
colnames(moran_idh) <- c("M")
View(moran_idh)

shp_dados_sp <- cbind(shp_dados_sp, moran_idh)

# Observando a nova base de dados do nosso shapefile:
shp_dados_sp@data %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)

A <- as.data.frame(shp_dados_sp@data$M) %>% 
  mutate("Mm" = case_when(
    shp_dados_sp@data$M == "Low-Low" ~ "Baixo-Baixo",
    shp_dados_sp@data$M == "Low-High" ~ "Baixo-Alto",
    shp_dados_sp@data$M == "High-Low" ~ "Alto-Baixo",
    shp_dados_sp@data$M == "High-High" ~ "Alto-Alto"
  )
  )
View(A)
nrow(A)
shp_dados_sp <- cbind(shp_dados_sp, A[2])

# Observando a nova base de dados do nosso shapefile:
shp_dados_sp@data %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)

# Criando um vetor para armazenar os quadrantes AA, AB,...
quad <- vector(mode = "numeric", length = nrow(shp_dados_sp))


# Criando um vetor para armazenar as médias centralizadas
centro_idh <- shp_dados_sp@data$IDH - mean(shp_dados_sp@data$IDH)

# Criando um vetor para armazenar as defasagens centralizadas
centro_lag <- df_idh$defasagem_idh - mean(df_idh$defasagem_idh)
length(centro_lag)
View(as.data.frame(centro_lag))

# significância a ser adotada:
alpha <- 0.05

# Enquadrando nossas observações em seus respectivos quadrantes:
quad[centro_idh > 0 & centro_lag > 0] <- "Alto-Alto"
quad[centro_idh > 0 & centro_lag < 0] <- "Alto-Baixo"
quad[centro_idh < 0 & centro_lag > 0] <- "Baixo-Alto"
quad[centro_idh < 0 & centro_lag < 0] <- "Baixo-Baixo"
quad
pvalor <- moran_local_idh[,5]

# Ajustando a presença da observação em razão de sua significância estatística:
quad[pvalor > alpha] <- "p > 0,05"
quad

# Juntando o objeto quadrantes ao objeto shp_sp
shp_dados_sp@data["Quadrantes_novos"] <- factor(quad)
View(shp_dados_sp@data)

# Clusters com significância
cluster_idh <- tm_shape(shp = shp_dados_sp) +
  tm_fill(col="Quadrantes_novos", 
          title="Quadrantes do gráfico de \ndispersão de Moran",
          palette = c(`Alto-Alto` = "#d53e4f",
                      `Alto-Baixo` = "#fee08b", 
                      `Baixo-Alto` = "#53c4a0", 
                      `Baixo-Baixo` = "#3288bd",
                      `p > 0,05` = "#e2f4ff"
          )
  ) +
  tm_borders(col="black",lwd=0.8) +
  tm_layout(legend.outside = F, legend.outside.position = "right",
            title.size = 0.8, title.position = c("left","bottom"))
cluster_idh
tmap_save(tm = cluster_idh, filename = "cluster_idh.jpeg", dpi = 600)

################ MAPA I de MORAN LOCAL PIB #########################################
moran_local_pib <- localmoran(x = as.numeric(shp_dados_sp@data$PIB),
                              listw = W_listw, 
                              zero.policy = TRUE)

#Matriz com as estatísticas locais de Moran
View(moran_local_pib) 
class(moran_local_pib)

shp_dados_sp <- cbind(shp_dados_sp, as.data.frame(moran_local_pib))
View(shp_dados_sp@data)

## extração do quadrante de dispersão de moran 
AT2 <- attr(x = moran_local_pib, which = "quadr")
View(AT2)
moran_pib <- as.data.frame(x = AT2$mean)
colnames(moran_pib) <- c("P")
View(moran_pib)

shp_dados_sp <- cbind(shp_dados_sp, moran_pib)

# Observando a nova base de dados do nosso shapefile:
shp_dados_sp@data %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)

B <- as.data.frame(shp_dados_sp@data$P) %>% 
  mutate("Pp" = case_when(
    shp_dados_sp@data$P == "Low-Low" ~ "Baixo-Baixo",
    shp_dados_sp@data$P == "Low-High" ~ "Baixo-Alto",
    shp_dados_sp@data$P == "High-Low" ~ "Alto-Baixo",
    shp_dados_sp@data$P == "High-High" ~ "Alto-Alto"
  )
  )
View(B)
nrow(B)
shp_dados_sp <- cbind(shp_dados_sp, B[2])

# Observando a nova base de dados do nosso shapefile:
shp_dados_sp@data %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)

# Criando um vetor para armazenar as médias centralizadas
centro_pib <- shp_dados_sp@data$PIB - mean(shp_dados_sp@data$PIB)

# Criando um vetor para armazenar as defasagens centralizadas
centro_lag2 <- df_pib$defasagem_pib - mean(df_pib$defasagem_pib)
length(centro_lag2)
View(as.data.frame(centro_lag2))


# Enquadrando nossas observações em seus respectivos quadrantes:
rm(quad2)
quad2 <- vector(mode = "numeric", length = nrow(shp_dados_sp))
quad2[centro_pib > 0 & centro_lag2 > 0] <- "Alto-Alto"
quad2[centro_pib > 0 & centro_lag2 < 0] <- "Alto-Baixo"
quad2[centro_pib < 0 & centro_lag2 > 0] <- "Baixo-Alto"
quad2[centro_pib < 0 & centro_lag2 < 0] <- "Baixo-Baixo"
  #quad2
  #View(moran_local_pib)
pvalor2 <- moran_local_pib[,5]

# significância a ser adotada:
alpha <- 0.05

# Ajustando a presença da observação em razão de sua significância estatística:
quad2[pvalor2 > alpha] <- "p > 0,05"
  #quad2
View(as.data.frame(quad2))
# # Juntando o objeto quadrantes ao objeto shp_sp
shp_dados_sp@data["Quadrantes_novos_5"] <- factor(quad2)
#   #View(shp_dados_sp@data)
# shp_dados_sp@data["Quadrantes_novos_6"] <- factor(quad2)
# shp_dados_sp@data["Quadrantes_novos_10"] <- factor(quad2)
# Clusters com significância
cluster_pib <- tm_shape(shp = shp_dados_sp) +
  tm_fill(col="Quadrantes_novos_5", 
          title="Quadrantes do gráfico de \ndispersão de Moran",
          palette = c(`Alto-Alto` = "#d53e4f",
                      `Alto-Baixo` = "#fee08b", 
                      `Baixo-Alto` = "#53c4a0", 
                      `Baixo-Baixo` = "#3288bd",
                      `p > 0,05` = "#e2f4ff"
          )
  ) +
  tm_borders(col="black",lwd=0.8) +
  tm_layout(legend.outside = F, legend.outside.position = "right",
            title.size = 0.8, title.position = c("left","bottom"))
cluster_pib
tmap_save(tm = cluster_pib, filename = "cluster_pib.jpeg", dpi = 600)
