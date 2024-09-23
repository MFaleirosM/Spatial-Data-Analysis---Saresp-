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

# Quantidade de muncipios no shapefile
nrow(as.data.frame(unique(shp_sp@data$NM_MUNICIP)))

# Observando a base de dados do shapefile carregado:
shp_sp %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 21)

### Uniformizando o shapefile para ter os nomes de cidades no mesmo formato da base do Saresp
colnames(df21)
df21_mat_media <- 
  df21 %>% 
  filter(Tem_Nec==0, SERIE_ANO=="EM-3ª série", validade == 1) %>% 
  subset(select=c("MUN","TOTAL_PONTO_MAT"))

nf_mat_21 <- nrow(df21_mat_media)
nf_mat_21

summary(df21_mat_media)

## Group_by médias por cidade
medias_cidade_mat21 <- df21_mat_media %>% 
  group_by(MUN) %>% 
  summarise(media = mean(TOTAL_PONTO_MAT, na.rm = T))

# checando o total de cidades no agrupamento
nrow(medias_cidade_mat21)
View(medias_cidade_mat21)

# medias_cidade_mat21 <- 
#   medias_cidade_mat21 %>%  filter(!row_number() %in% c(183))

nrow(unique(as.data.frame(medias_cidade_mat21$MUN)))

# Corrigindo nomes das cidades na base de origem para corresponder à base do shapefile
(cidades_df21 <- as.data.frame(unique(medias_cidade_mat21$MUN)))
writexl::write_xlsx(cidades_df21, 'cidades_df21_new.xlsx')

cidades_shp <- as.data.frame(shp_sp@data[1:2])
writexl::write_xlsx(cidades_shp, 'cidades_shp.xlsx')

# Base com as correspondências 
corresp21 <- readxl::read_xlsx("corresp21MAT.xlsx")

# Trocando os nomes dos municípios para corresponder ao shapefile
View(medias_cidade_mat21 <- medias_cidade_mat21 %>% cbind(corresp21))
medias_cidade_mat21 <- medias_cidade_mat21 %>% select(c(2,4,5))
colnames(medias_cidade_mat21) <- c("Média", "Cidade", "CD_GEOCMU")
View(medias_cidade_mat21)


## LEFT JOIN - Inserindo os dados de média por cidade na base do shapefile
shp_sp@data <- shp_sp@data %>% 
  left_join(medias_cidade_mat21, by = "CD_GEOCMU") 
nrow(shp_sp@data)

# Observando a nova base de dados do nosso shapefile:
shp_sp@data %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 21)

# new <- as.data.frame(shp_sp@data$NM_MUNICIP)
# writexl::write_xlsx(new, 'new.xlsx')
###################################################################################
###################################################################################
################################################################################################
# Fazendo a divisão das faixas de quantis
# shp_sp@data["Média"] 
# var <- get.var("Média",shp_sp@data)
# is.vector(var)
# class(var)
# percent <- c(0,.20,.40,.60,.80,1)
# var[!var %in% c(NA)]
# bperc <- quantile( var[!var %in% c(NA)], percent)
# bperc
# 
# # Mapa de quantis
# quantis_21_mat <- tm_shape(shp = shp_sp) +
#   tm_fill(col = "Média", breaks=bperc, 
#           palette = "Spectral",title= "Média em Matemática",
#           legend.hist=TRUE) +
#   tm_borders() +
#   tm_layout(legend.outside = TRUE, legend.outside.position = "right",
#             title = "Distribuição das médias", title.size = 0.8, title.position = c("left","bottom"))
# quantis_21_mat
###################################################################################
##### MAPA DEGRADÊ ##############################################################################

# Mapa de quantis por degradê
quantis_21_mat_degrade <- tm_shape(shp = shp_sp) +
  tm_fill(col="Média", style = "quantile", n = 5, 
          title="Quantis",
          palette = "YlOrBr",
          # c("#A0522D", "#FFD700", "#FA8072", "#8A2BE2", "#008000"),
          # sienna, gold, salmon, blue violet, green
          legend.hist=TRUE,
          legend.hist.title = "Histograma das médias"
          # labels=c("< 20%", "20% - 40%", "40% - 60%", "60% - 80%","> 80%")
  ) +
  tm_borders(col="black",lwd=0.8) +
  tm_layout(legend.outside = TRUE, legend.outside.position = "right",
            title.size = 0.8, title.position = c("left","bottom"))
quantis_21_mat_degrade
tmap_save(tm = quantis_21_mat_degrade, filename = "quantis_21_mat_degrade.jpeg", dpi = 600)

###################################################################################
###### I DE MORAN #############################################################################

# Estabelecendo vizinhanças por contiguidade, critério queen:
vizinhos <- poly2nb(pl = shp_sp,
                    queen = TRUE,
                    row.names = shp_sp@data$NM_MUNICIP)

# Dados sobre a vizinhança estabelecida:
summary(vizinhos)

# Mapa de vizinhos e conexões
plot.new()
plot(shp_sp, border = "grey60")
plot(vizinhos, 
     coordinates(shp_sp), 
     add = T, 
     col = "blue",
     lwd=1, pch=21, cex=0.6)

# Matriz W
W <- nb2mat(neighbours = vizinhos, style = "W", zero.policy = TRUE)
View(W)

# Inserindo os nomes das colunas na matriz W
colnames(W) <- shp_sp@data$NM_MUNICIP
View(W)

# vamos padronizar a variável Media pelo procedimento zscores:
shp_sp@data["Zmedia"] <- scale(shp_sp@data$Média)

#Checando a nova coluna criada com as médias padronizadas
summary(shp_sp@data$Zmedia)
sd(shp_sp@data$Zmedia, na.rm = T)
mean(shp_sp@data$Zmedia, na.rm = T)

## Removendo linhas e colunas da matriz de vizinhanças que não serão utilizadas
W_644 <- W[!rownames(W) %in% c("SANTANA DE PARNAÍBA"), 
           !colnames(W) %in% c("SANTANA DE PARNAÍBA")]

dim(W_644)
View(W_644)

# Cálculo da Estatística I de Moran - objeto de classe listw. 
W_644_listw <- mat2listw(W_644)
class(W_644_listw)
class(W_644)

W_645_listw <- mat2listw(W) # Quando houver 645 cidades no dataframe base

## Retirando da coluna de medias os valores NA e trasnformando a coluna 
# de médias em valores da classe numérico
medias <- as.numeric(na.omit(shp_sp@data$Zmedia))
summary(medias)
length(medias)
moran.test(x = medias, 
           listw = W_644_listw, 
           zero.policy = TRUE)

moran.test(x = medias, 
           listw = W_645_listw, 
           zero.policy = TRUE)
###################################################################################
####### GRÁFICO DE DISPERSÃO DE MORAN ############################################################################

# Gráfico de Dispersão de Moran automático
moran.plot(x = as.numeric(na.omit(shp_sp@data$Média)), 
           listw = W_644_listw, 
           zero.policy = TRUE,
           xlab = "Média em Matemática", 
           ylab = "Média em Matemática espacialmente defasada",
           pch = 21)

# Gráfico de Dispersão de Moran com ggplot2
defasagem <- lag.listw(x=W_644_listw,
                       var=na.omit(shp_sp@data$Média),
                       zero.policy = T)

length(defasagem)
mean(defasagem)
nrow(W_644 %*% as.numeric(na.omit(shp_sp@data$Média)))
defasagem[1]
(W_644 %*% as.numeric(na.omit(shp_sp@data$Média)))[1]

df <- data.frame(cidade = shp_sp@data$NM_MUNICIP[!shp_sp@data$NM_MUNICIP %in% c("SANTANA DE PARNAÍBA")],
                 Medias = na.omit(shp_sp@data$Média),
                 defasagem)


p <- df %>% ggplot(aes(label = cidade))+
  geom_point(aes(x = Medias, y = defasagem), alpha = 0.5, size = 2) +
  geom_smooth(aes(x = Medias, y = defasagem), color = "#FF8C00", method ='lm', se = F) +
  geom_hline(yintercept = mean(df$defasagem), lty = 2) +
  geom_vline(xintercept = mean(df$Medias), lty = 2) +
  scale_x_continuous(limits = c(3, 20)) +
  scale_y_continuous(limits = c(5, 21)) +
  xlab("Média em Matemática") +
  ylab("Média espacialmente defasada") +
  annotate("text", x = 21, y = 21, label = "Alto-Alto", size=4) +
  annotate("text", x = 21, y = 6, label = "Alto-Baixo", size=4) +
  annotate("text", x = 6, y = 21, label = "Baixo-Alto", size=4) +
  annotate("text", x = 6, y = 6, label = "Baixo-Baixo", size=4) +
  theme_bw() +
  theme(axis.text=element_text(size=21),
        axis.title=element_text(size=21))

plotly::ggplotly(p)
ggsave(p, filename = "scatterMoran21mat.jpeg", dpi = 600)

###################################################################################
####### MAPA I de MORAN LOCAL ############################################################################

# Moran Local já omitindo a cidade que não consta no ano de 2021 
moran_local <- localmoran(x = as.numeric(na.omit(shp_sp@data$Média)),
                          listw = W_644_listw, 
                          zero.policy = TRUE)

#Matriz com as estatísticas locais de Moran
View(moran_local) 

# Encontrando a linha do shapefile onde está localizada a cidade de SANTANA DE PARNAIBA
for (i in 1:nrow(shp_sp@data)){
  if (shp_sp@data$NM_MUNICIP[i] == 'SANTANA DE PARNAÍBA'){
    print (i)
  }
}
# Checando a linha encontrada
shp_sp@data$NM_MUNICIP[520] 
colnames(moran_local)
new_row <- c(NA, NA, NA, NA, NA)

#Adicionando uma linha com NA na posição do muncipio de SANTANA DE PARNAIBA
moran_local_645 <- rbind(moran_local[1:521,], new_row, moran_local[-(1:521),])
View(moran_local_645)

# Juntando os resultados da Estatística Moran Local no dataset do objeto shp_sp:
shp_sp <- cbind(shp_sp, moran_local_645)
View(shp_sp@data)

# Plotando a Estatística Moran Local de forma espacial:
# Ii_21_mat <- tm_shape(shp = shp_sp) +
#   tm_fill(col="Ii",  style = "quantile", n = 5, 
#           title="Quantis",
#           palette = "Spectral",
#           # c("#A0522D", "#FFD700", "#FA8072", "#8A2BE2", "#008000"),
#           # sienna, gold, salmon, blue violet, green
#           legend.hist=TRUE,
#           legend.hist.title = "Histograma das estatísticas de \nMoran locais Ii"
#           # labels=c("< 20%", "20% - 40%", "40% - 60%", "60% - 80%","> 80%")
#           ) +
#   tm_borders(col="black",lwd=0.8) +
#   tm_layout(legend.outside = TRUE, legend.outside.position = "right",
#             title.size = 0.8, title.position = c("left","bottom"))
# Ii_21_mat
# tmap_save(tm = Ii_21_mat, filename = "Ii_21_mat.jpeg", dpi = 600)


# Corrigindo erro na escala de cores (mesmo mapa acima de outro jeito)
# quantile(shp_sp@data$Ii, type = 5, probs = c(0,0.2,0.4,0.6,0.8,1), na.rm=T)
# shp_sp@data <- 
#   shp_sp@data %>% 
#   mutate(faixa_quantis = factor(quantcut(x = Ii, q = 5))) 
# View(shp_sp@data)
# tm_shape(shp = shp_sp) +
#   tm_fill(col = "faixa_quantis", palette = "-magma") +
#   tm_borders()

# Quadrantes no objeto moran_local (este objeto foi criado sem o NA)

##############################################################################
# Clusterização ##############################################################
AT <- attr(x = moran_local, which = "quadr")
View(AT)
class(AT)
linha_extra <- c(NA, NA, NA)
quadrantes <- rbind(AT[1:521,], linha_extra, AT[-(1:521),])  
class(quadrantes)
View(quadrantes)
shp_sp <- cbind(shp_sp, quadrantes[1])

# Observando a nova base de dados do nosso shapefile:
shp_sp@data %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 21)

A <- as.data.frame(shp_sp@data$mean) %>% 
  mutate("mean" = case_when(
    shp_sp@data$mean == "Low-Low" ~ "Baixo-Baixo",
    shp_sp@data$mean == "Low-High" ~ "Baixo-Alto",
    shp_sp@data$mean == "High-Low" ~ "Alto-Baixo",
    shp_sp@data$mean == "High-High" ~ "Alto-Alto"
  )
  )
View(A)
nrow(A)
shp_sp <- cbind(shp_sp, A[2])

# Observando a nova base de dados do nosso shapefile:
shp_sp@data %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 21)
mean(shp_sp@data$Zmedia, na.rm = T)
mean(shp_sp@data$Ii, na.rm = T)

# tm_shape(shp = shp_sp) +
#   tm_fill(col="mean.1", 
#           title="Quadrantes do gráfico de \ndispersão de Moran",
#           palette = c(`Alto-Alto` = "#d53e4f",
#                       `Alto-Baixo` = "#fee08b", 
#                       `Baixo-Alto` = "#53c4a0", 
#                       `Baixo-Baixo` = "#3288bd"
#                       )
#           ) +
#   tm_borders(col="black",lwd=0.8) +
#   tm_layout(legend.outside = F, legend.outside.position = "right",
#             title.size = 0.8, title.position = c("left","bottom"))

# Criando um vetor para armazenar os quadrantes AA, AB,...
quad <- vector(mode = "numeric", length = nrow(shp_sp))
length(quad)
quad[520] <- NA
length(quad)
is.vector(quad)

# Criando um vetor para armazenar as médias centralizadas
centro_media <- na.omit(shp_sp@data$Média) - mean(na.omit(shp_sp@data$Média))
centro_media
centro_media <- append(centro_media, NA, after = 510+9)
length(centro_media)
centro_media[510+9]
centro_media[520]
centro_media[521]

# Criando um vetor para armazenar as defasagens centralizadas
centro_lag <- df$defasagem - mean(df$defasagem)
length(centro_lag)
centro_lag <- append(centro_lag, NA, after = 510+9)
length(centro_lag)
View(as.data.frame(centro_lag))

# significância a ser adotada:
alpha <- 0.05

# Enquadrando nossas observações em seus respectivos quadrantes:
quad[centro_media > 0 & centro_lag > 0] <- "Alto-Alto"
quad[centro_media > 0 & centro_lag < 0] <- "Alto-Baixo"
quad[centro_media < 0 & centro_lag > 0] <- "Baixo-Alto"
quad[centro_media < 0 & centro_lag < 0] <- "Baixo-Baixo"
quad
pvalor <- append(moran_local[,5], NA, after = 510+9)

# Ajustando a presença da observação em razão de sua significância estatística:
quad[pvalor > alpha] <- "p > 0,05"
quad

# Juntando o objeto quadrantes ao objeto shp_sp
shp_sp@data["Quadrantes_novos"] <- factor(quad)
View(shp_sp@data)

mean(shp_sp@data$Média, na.rm = T)
mean(df$defasagem, na.rm = T)

# Clusters sem significância
cluster_total21mat <- tm_shape(shp = shp_sp) +
  tm_fill(col="mean.1", 
          title="Quadrantes do gráfico de \ndispersão de Moran",
          palette = c(`Alto-Alto` = "#d53e4f",
                      `Alto-Baixo` = "#fee08b", 
                      `Baixo-Alto` = "#53c4a0", 
                      `Baixo-Baixo` = "#3288bd"
          )
  ) +
  tm_borders(col="black",lwd=0.8) +
  tm_layout(legend.outside = F, legend.outside.position = "right",
            title.size = 0.8, title.position = c("left","bottom"))
cluster_total21mat

tmap_save(tm = cluster_total21mat, filename = "cluster_total21mat.jpeg", dpi = 600)


# Clusters com significância
cluster21mat <- tm_shape(shp = shp_sp) +
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

tmap_save(tm = cluster21mat, filename = "cluster21mat.jpeg", dpi = 600)
