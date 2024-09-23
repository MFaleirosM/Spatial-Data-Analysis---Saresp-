library(gridExtra)
library(tidyverse)
library(ggplot2)
install.packages("RColorBrewer")
library(RColorBrewer)
install.packages("ggpubr")
install.packages(c("FSA","patchwork"))
library(FSA)        # fitPlot() code may not run after >v0.9.0
library(patchwork)
pacotes <- c("plotly",
             "tidyverse",
             "ggrepel",
             "knitr", "kableExtra",
             "sjPlot",
             "FactoMineR",
             "amap", 
             "ade4")

lapply(pacotes, require, character.only = TRUE)


base_2021 <- read.csv('MICRODADOS_SARESP_2021.csv',sep=",", encoding = "UTF-8")
df21 <- base_2021 %>% select("MUN",            "SERIE_ANO",       "TP_SEXO", 
                             "PERIODO",        "Tem_Nec",         "TOTAL_PONTO_LP", 
                             "TOTAL_PONTO_MAT","nivel_profic_lp", "nivel_profic_mat",
                             "validade")
nt21 <- nrow(df21)
rm(base_2021)

base_2019 <- read.csv('MICRODADOS_SARESP_2019.csv',sep=";" ,encoding = "UTF-8")
colnames(base_2019)
df19 <- base_2019 %>% select("MUN",            "SERIE",           "TP_SEXO", 
                             "PERIODO",        "Tem_Nec",         "TOTAL_PONTO_LP", 
                             "TOTAL_PONTO_MAT","nivel_profic_lp", "nivel_profic_mat",
                             "VALIDADE")
nt19 <- nrow(df19)
rm(base_2019)


#### Montando histograma do percentual de alunos que atingiram proficiência avançada em Matemática
df21 %>% group_by(Tem_Nec,PERIODO,TP_SEXO) %>% summarise(n_distinct(nivel_profic_mat)) 
colnames(df21)

unique(df21$SERIE_ANO)

#Número de municípios
nrow(as.data.frame(unique(df21$MUN)))

## Aplicando um filtro para uma análise refinada por categorias
df21_filtrado <- 
  df21 %>% 
  #filter(Tem_Nec==0,PERIODO=="MANHÃ", TP_SEXO=="M", SERIE_ANO=="EM-3ª série", validade == 1) %>% 
  filter(Tem_Nec==0, SERIE_ANO=="EM-3ª série", validade == 1) %>% 
  select(TP_SEXO, PERIODO, nivel_profic_mat, nivel_profic_lp)

## Aplicando um filtro para excluir linhas vazias e considerar somente que fez ambas as provas português e matemática
df21_filtrado_semNA <- 
  df21_filtrado[df21_filtrado$nivel_profic_mat!="" &
                      df21_filtrado$nivel_profic_lp!="", ]

# Colocando fatores na tabela filtrada
df21_filtrado_semNA_fatorado <- 
  as.data.frame(unclass(df21_filtrado_semNA), stringsAsFactors=TRUE)

# Checando a ordem das categoriasl (níveis)
levels(df21_filtrado_semNA_fatorado$nivel_profic_mat)
levels(df21_filtrado_semNA_fatorado$nivel_profic_lp)
summary(df21_filtrado_semNA_fatorado$nivel_profic_mat)

### Corrigindo a ordem dos níveis para melhor visualização da tabela de contingencia
df21_filtrado_semNA_fatorado$nivel_profic_mat <- 
  df21_filtrado_semNA_fatorado$nivel_profic_mat %>% 
  factor(levels=c("Abaixo do Básico","Básico","Adequado","Avançado"))

### Corrigindo a ordem dos níveis para melhor visualização da tabela de contingencia
df21_filtrado_semNA_fatorado$nivel_profic_lp <- 
  df21_filtrado_semNA_fatorado$nivel_profic_lp %>% 
  factor(levels=c("Abaixo do Básico","Básico","Adequado","Avançado"))

# Tabelas de frequência das variáveis
summary(df21_filtrado_semNA_fatorado)

##################################################################
######################## Tabela de contingência ################################

# Tabela de contingência PERIDO x PROFIC_MAT
t_mat_periodo <- sjt.xtab(var.row = df21_filtrado_semNA_fatorado$PERIODO,
         var.col = df21_filtrado_semNA_fatorado$nivel_profic_mat,
         var.labels = c("Variáveis/ \n              PERIODO", "nivel_profic_mat"),
         show.na = F,
         show.exp = T,
         tdcol.expected = 'blue',
         encoding = "UTF-8",
         use.viewer = F)
t_mat_periodo

# Tabela de contingência PERIDO x PROFIC_LP
t_lp_periodo <- sjt.xtab(var.row = df21_filtrado_semNA_fatorado$PERIODO,
              var.col = df21_filtrado_semNA_fatorado$nivel_profic_lp,
              var.labels = c("Variáveis/ \n              PERIODO", "nivel_profic_lp"),
              show.na = F,
              show.exp = T,
              tdcol.expected = 'green',
              encoding = "UTF-8",
              use.viewer = F)
t_lp_periodo

# Tabela de contingência SEXO x PROFIC_MAT
t_mat_sexo <- sjt.xtab(var.row = df21_filtrado_semNA_fatorado$TP_SEXO,
                  var.col = df21_filtrado_semNA_fatorado$nivel_profic_mat,
                  var.labels = c("Variáveis/ \n              TP_SEXO", "nivel_profic_mat"),
                  show.na = F,
                  show.exp = T,
                  tdcol.expected = 'blue',
                  encoding = "UTF-8",
                  use.viewer = F)
t_mat_sexo

# Tabela de contingência SEXO x PROFIC_MAT
t_lp_sexo <- sjt.xtab(var.row = df21_filtrado_semNA_fatorado$TP_SEXO,
                       var.col = df21_filtrado_semNA_fatorado$nivel_profic_lp,
                       var.labels = c("Variáveis/ \n              TP_SEXO", "nivel_profic_lp"),
                       show.na = F,
                       show.exp = T,
                       tdcol.expected = 'green',
                       encoding = "UTF-8",
                       use.viewer = F)
t_lp_sexo
##################################################################
######################## ANOVA ################################

colnames(df21)
ANOVA_21_periodo_mat <- 
  df21 %>% 
  filter(Tem_Nec==0, SERIE_ANO=="EM-3ª série", validade == 1) %>% 
  subset(select=c("PERIODO","TP_SEXO","TOTAL_PONTO_MAT"))
nrow(ANOVA_21_periodo_mat)

ANOVA_21_periodo_lp <- 
  df21 %>% 
  filter(Tem_Nec==0, SERIE_ANO=="EM-3ª série", validade == 1) %>% 
  subset(select=c("PERIODO","TP_SEXO","TOTAL_PONTO_LP"))
nrow(ANOVA_21_periodo_lp)

## Removendo NA das pontuações 
ANOVA_21_periodo_mat <- ANOVA_21_periodo_mat %>% na.omit()
ANOVA_21_periodo_lp <- ANOVA_21_periodo_lp %>% na.omit()
nrow(ANOVA_21_periodo_mat)
nrow(ANOVA_21_periodo_lp)
str(ANOVA_21_periodo_mat$TOTAL_PONTO_MAT)

#############################################################################
#################### BOXPLOT #########################################################
box_21_mat <- ggplot(ANOVA_21_periodo_mat, 
       aes(x = PERIODO, y = TOTAL_PONTO_MAT, fill = TP_SEXO)) + 
  scale_fill_manual(values=c("#f46d43",
                             "#53c4a0"))+
  geom_boxplot(alpha=0.9) +
  theme_bw() 
ggsave(plot=box_21_mat, filename="box_21_mat.jpeg", dpi=600)

box_21_lp <- ggplot(ANOVA_21_periodo_lp, 
                     aes(x = PERIODO, y = TOTAL_PONTO_LP, fill = TP_SEXO)) + 
  scale_fill_manual(values=c("#f46d43",
                             "#53c4a0"))+
  geom_boxplot(alpha=0.9) +
  theme_bw() 
ggsave(plot=box_21_lp, filename="box_21_lp.jpeg", dpi=600)

ANOVA_21_periodo_mat$PERIODO <- as.factor(ANOVA_21_periodo_mat$PERIODO)
ANOVA_21_periodo_mat$TP_SEXO <- as.factor(ANOVA_21_periodo_mat$TP_SEXO)
ANOVA_21_periodo_lp$PERIODO <- as.factor(ANOVA_21_periodo_lp$PERIODO)
ANOVA_21_periodo_lp$TP_SEXO <- as.factor(ANOVA_21_periodo_lp$TP_SEXO)

anova <- aov(TOTAL_PONTO_MAT ~ TP_SEXO*PERIODO, data = ANOVA_21_periodo_mat)
summary(anova)

anova_lp <- aov(TOTAL_PONTO_LP ~ TP_SEXO*PERIODO, data = ANOVA_21_periodo_lp)
summary(anova_lp)

capture.output(summary(anova),file="test.doc")
plot(anova, 2)
# install.packages("ggfortify")
# library(ggfortify)
# autoplot(anova[[2]])
# Install from CRAN
ANOVA_21_periodo_lp$PERIODO <- 
  ANOVA_21_periodo_lp$PERIODO %>% 
  factor(levels=c("MANHÃ","TARDE","NOITE"))
str(ANOVA_21_periodo_lp)

ANOVA_21_periodo_mat$PERIODO <- 
  ANOVA_21_periodo_mat$PERIODO %>% 
  factor(levels=c("MANHÃ","TARDE","NOITE"))
str(ANOVA_21_periodo_mat)

install.packages("pwr")
library(CGPfunctions)
Plot2WayANOVA(formula = TOTAL_PONTO_LP ~ TP_SEXO*PERIODO, 
              dataframe = ANOVA_21_periodo_lp,
              confidence = .95,
              # title = "MPG by cylinders and type transmission",
              xlab = "TP_SEXO",
              ylab = "TOTAL_PONTO_LP",
              # mean.label = TRUE,
              mean.shape = 22,
              # # posthoc.method = "lsd",
              # errorbar.display = "SEM"
              plottype = "line",
              # overlay.type = "box",
              mean.label = TRUE,
              ggplot.component = theme(axis.text.x = element_text(size=15, color="black"),
                                       axis.text.y = element_text(size=15, color="black"))
)

Plot2WayANOVA(formula = TOTAL_PONTO_MAT ~ TP_SEXO*PERIODO, 
              dataframe = ANOVA_21_periodo_mat,
              confidence = .95,
              # title = "MPG by cylinders and type transmission",
              xlab = "TP_SEXO",
              ylab = "TOTAL_PONTO_MAT",
              # mean.label = TRUE,
              mean.shape = 22,
              # # posthoc.method = "lsd",
              # errorbar.display = "SEM"
              plottype = "line",
              # overlay.type = "box",
              mean.label = TRUE,
              ggplot.component = theme(axis.text.x = element_text(size=15, color="black"),
                                       axis.text.y = element_text(size=15, color="black"))
)
# ## Group_by período
# tabela_ANOVA_21_mat <- ANOVA_21_periodo_mat %>% 
#                        group_by(PERIODO, TP_SEXO)
##################################################################################
#### TRATAMENTO NO MINITAB######################################################
str(ANOVA_21_periodo_mat)
writexl::write_xlsx(ANOVA_21_periodo_mat, 'ANOVA_21_periodo_mat.xlsx')
writexl::write_xlsx(ANOVA_21_periodo_lp, 'ANOVA_21_periodo_lp.xlsx')
# str(ANOVA_21_periodo_mat)
# 
ANOVA_21_periodo_lp %>% group_by(PERIODO, TP_SEXO) %>% 
   summarise(media = mean(TOTAL_PONTO_LP), n=n())
# 
# View(t)
# colnames(ANOVA_21_periodo_mat)

###############################################################################
############ GRÁFICOS ###################################################################
# Nivel de proficiencia em Matematica para diferentes periodos
mf_21_mat_manha <- df21_filtrado[df21_filtrado$PERIODO=="MANHÃ",c(1,3)]
mf_21_mat_manha <- mf_21_mat_manha[mf_21_mat_manha$nivel_profic_mat!="" ,] # retirando as observações que não fizeram a prova de português 
summary(mf_21_mat_manha)
distinct(mf_21_mat_manha)
(contagem_mat_manha <- mf_21_mat_manha %>% group_by(TP_SEXO, nivel_profic_mat) %>% summarise(n = n()))

mf_21_mat_tarde <- df21_filtrado[df21_filtrado$PERIODO=="TARDE",c(1,3)]
mf_21_mat_tarde <- mf_21_mat_tarde[mf_21_mat_tarde$nivel_profic_mat!="" ,]
summary(mf_21_mat_tarde)
distinct(mf_21_mat_tarde)
(contagem_mat_tarde <- mf_21_mat_tarde %>% group_by(TP_SEXO, nivel_profic_mat) %>% summarise(n = n()))

mf_21_mat_noite <- df21_filtrado[df21_filtrado$PERIODO=="NOITE",c(1,3)]
mf_21_mat_noite <- mf_21_mat_noite[mf_21_mat_noite$nivel_profic_mat!="" ,]
summary(mf_21_mat_noite)
distinct(mf_21_mat_noite)
(contagem_mat_noite <- mf_21_mat_noite %>% group_by(TP_SEXO, nivel_profic_mat) %>% summarise(n = n()))

# Nivel de proficiencia em Portugues para diferentes periodos
mf_21_lp_manha <- df21_filtrado[df21_filtrado$PERIODO=="MANHÃ",c(1,4)]
mf_21_lp_manha <- mf_21_lp_manha[mf_21_lp_manha$nivel_profic_lp!="" ,]
summary(mf_21_lp_manha)
distinct(mf_21_lp_manha)
(contagem_lp_manha <- mf_21_lp_manha %>% group_by(TP_SEXO, nivel_profic_lp) %>% summarise(n = n()))

mf_21_lp_tarde <- df21_filtrado[df21_filtrado$PERIODO=="TARDE",c(1,4)]
mf_21_lp_tarde <- mf_21_lp_tarde[mf_21_lp_tarde$nivel_profic_lp!="" ,]
summary(mf_21_lp_tarde)
distinct(mf_21_lp_tarde)
(contagem_lp_tarde <- mf_21_lp_tarde %>% group_by(TP_SEXO, nivel_profic_lp) %>% summarise(n = n()))

mf_21_lp_noite <- df21_filtrado[df21_filtrado$PERIODO=="NOITE",c(1,4)]
mf_21_lp_noite <- mf_21_lp_noite[mf_21_lp_noite$nivel_profic_lp!="" ,]
summary(mf_21_lp_noite)
distinct(mf_21_lp_noite)
(contagem_lp_noite <- mf_21_lp_noite %>% group_by(TP_SEXO, nivel_profic_lp) %>% summarise(n = n()))

#### Criando gráfico comparativo por sexo #####################################
#Mat manha

mat_21_manha <- contagem_mat_manha %>% 
  ggplot(aes(fct_relevel(factor(nivel_profic_mat), c("Abaixo do Básico","Básico","Adequado","Avançado")), y= n, fill = TP_SEXO)) + 
  geom_bar(stat="identity", position = "dodge", color = "black") + 
  scale_fill_manual(values=c("#f46d43",
                             "#53c4a0")) +
  xlab("Nível de proficiência em Matemática") +
  ylab("Número de estudantes") +
  ggtitle("Período matutino") +
  theme_bw() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.background = element_rect(fill = "white", size = 4, colour = "white"),
        #legend.justification = c(0.5, 2),
        #legend.position = c(0, 1),
        panel.grid.major = element_line(colour = "grey70", size = 0.2),
        panel.grid.minor = element_blank()
        ) +
  guides(fill=guide_legend(title="Sexo")) +
  ylim(0, 25000) 
mat_21_manha 
# grid.arrange(mat_21_manha, mat_21_tarde, mat_21_noite, 
#              lp_21_manha, lp_21_tarde,lp_21_noite , nrow=2, ncol=3)

#Mat tarde
mat_21_tarde <- contagem_mat_tarde %>% 
  ggplot(aes(fct_relevel(factor(nivel_profic_mat), c("Abaixo do Básico","Básico","Adequado","Avançado")), y= n, fill = TP_SEXO)) + 
  geom_bar(stat="identity", position = "dodge", color = "black") + 
  scale_fill_manual(values=c("#f46d43",
                             "#53c4a0")) +
  xlab("Nível de proficiência em Matemática") +
  ylab("") +
  ggtitle("Período vespertino") +
  theme_bw() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.background = element_rect(fill = "white", size = 4, colour = "white"),
        legend.justification = c(2, -2),
        #legend.position = c(0, 1),
        panel.grid.major = element_line(colour = "grey70", size = 0.2),
        panel.grid.minor = element_blank()
  ) +
  guides(fill=guide_legend(title="Sexo")) +
  ylim(0, 25000)

#Mat noite
mat_21_noite <- contagem_mat_noite %>% 
  ggplot(aes(fct_relevel(factor(nivel_profic_mat), c("Abaixo do Básico","Básico","Adequado","Avançado")), y= n, fill = TP_SEXO)) + 
  geom_bar(stat="identity", position = "dodge", color = "black") + 
  scale_fill_manual(values=c("#f46d43",
                             "#53c4a0")) +
  xlab("Nível de proficiência em Matemática") +
  ylab("") +
  ggtitle(" Período noturno") +
  theme_bw() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.background = element_rect(fill = "white", size = 4, colour = "white"),
        legend.justification = c(2, -2),
        #legend.position = c(0, 1),
        panel.grid.major = element_line(colour = "grey70", size = 0.2),
        panel.grid.minor = element_blank()
  ) +
  guides(fill=guide_legend(title="Sexo")) +
  ylim(0, 25000)

# Generator RGB
# https://www.rapidtables.com/web/color/RGB_Color.html

#LP manha
lp_21_manha <- contagem_lp_manha %>% 
  ggplot(aes(fct_relevel(factor(nivel_profic_lp), c("Abaixo do Básico","Básico","Adequado","Avançado")), y= n, fill = TP_SEXO)) + 
  geom_bar(stat="identity", position = "dodge", color = "black") + 
  scale_fill_manual(values=c("#f46d43",
                             "#53c4a0")) +
  xlab("Nível de proficiência em Português") +
  ylab("Número de estudantes") +
  theme_bw() +
  # ggtitle("Português - Manhã") +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.background = element_rect(fill = "white", size = 4, colour = "white"),
        legend.justification = c(2, -2),
        #legend.position = c(0, 1),
        panel.grid.major = element_line(colour = "grey70", size = 0.2),
        panel.grid.minor = element_blank()
  ) +
  guides(fill=guide_legend(title="Sexo")) +
  ylim(0, 25000)

#LP tarde
lp_21_tarde <- contagem_lp_tarde %>% 
  ggplot(aes(fct_relevel(factor(nivel_profic_lp), c("Abaixo do Básico","Básico","Adequado","Avançado")), y= n, fill = TP_SEXO)) + 
  geom_bar(stat="identity", position = "dodge", color = "black") + 
  scale_fill_manual(values=c("#f46d43",
                             "#53c4a0")) +
  xlab("Nível de proficiência em Português") +
  ylab("") +
  theme_bw() +
  # ggtitle("Português - Tarde") +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.background = element_rect(fill = "white", size = 4, colour = "white"),
        legend.justification = c(2, -2),
        #legend.position = c(0, 1),
        panel.grid.major = element_line(colour = "grey70", size = 0.2),
        panel.grid.minor = element_blank()
  ) +
  guides(fill=guide_legend(title="Sexo")) +
  ylim(0, 25000)

#LP noite 
lp_21_noite <- contagem_lp_noite %>% 
  ggplot(aes(fct_relevel(factor(nivel_profic_lp), c("Abaixo do Básico","Básico","Adequado","Avançado")), y= n, fill = TP_SEXO)) + 
  geom_bar(stat="identity", position = "dodge", color = "black") + 
  scale_fill_manual(values=c("#f46d43",
                             "#53c4a0")) +
  xlab("Nível de proficiência em Português") +
  ylab("") +
  theme_bw() +
  # ggtitle("Português - Noite") +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.background = element_rect(fill = "white", size = 4, colour = "white"),
        legend.justification = c(2, -2),
        #legend.position = c(0, 1),
        panel.grid.major = element_line(colour = "grey70", size = 0.2),
        panel.grid.minor = element_blank() 
  ) +
  guides(fill=guide_legend(title="Sexo")) +
  ylim(0, 25000)

grid.arrange(mat_21_manha, mat_21_tarde, mat_21_noite, 
             lp_21_manha, lp_21_tarde,lp_21_noite , nrow=2, ncol=3)



#barplot(height=tabela$`2021`  , names=rownames(tabela), col="#69b3a2")
# #Removendo base completa para controle de memória
# rm(df21)
# 
# #Coletando o total de estudantes em cada categoria
# (nab_mat_2021 <- sum(mf_21$nivel_profic_mat == "Abaixo do Básico"))
# (nb_mat_2021 <- sum(mf_21$nivel_profic_mat == "Básico"))
# (nad_mat_2021 <- sum(mf_21$nivel_profic_mat == "Adequado"))
# (nav_mat_2021 <- sum(mf_21$nivel_profic_mat == "Avançado"))
# 
# (total <- nab_mat_2021 +
#          nb_mat_2021 +
#          nad_mat_2021 +
#          nav_mat_2021)
# 
# 
#2021 Criando a tabela com o percentual de proficiência para cada nível em matemática para cada ano
# tabela <- data.frame(matrix(0, ncol = 9, nrow = 4))
# anos <- c("2021", "2019", "2018", "2017", "2016", "2015", "2014", "2013", "2012")
# length(anos)
# 
# niveis <- c("Abaixo do Básico","Básico","Adequado","Avançado")
# rownames(tabela) <- niveis
# colnames(tabela) <- anos
# 
# tabela[,1] <- round(as.numeric(c(nab_mat_2021,
#                                  nb_mat_2021,
#                                  nad_mat_2021,
#                                  nav_mat_2021)/total*100),1)


### Número de Municípios presentes na planilha 
unique(df21$CODMUN)
