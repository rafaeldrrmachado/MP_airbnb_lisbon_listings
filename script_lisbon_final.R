library(dplyr)
library(gbm)
library(ggplot2)
library(psych)
library(lsr)
library(factoextra)
library(corrplot)
library(purrr)
library(caret)
library(Metrics)
library(lmtest)
library(partykit) # Toolkit with infrastructure for representing, summarizing and visualizing tree-strutured regression and classification models
library(rpart) # Recursive partitioning for classification , regression and survival trees
library(rpart.plot) # Plot 'rpart' models
library(e1071)
library(scales)
library(skedastic)

# -----------------------------------------------------------
# LER O FICHEIRO COMO TEXTO BRUTO
# -----------------------------------------------------------
raw_lines <- readLines("listings_Lisbon.csv", warn = FALSE)

# -----------------------------------------------------------
# FUNÇÃO PARA LIMPEZA DE UMA LINHA
# -----------------------------------------------------------
clean_row <- function(line) {
  # Remover ponto e vírgula no fim
  line <- sub(";+$", "", line)
  
  # Remover a primeira e última aspas
  if (startsWith(line, "\"") && endsWith(line, "\"")) {
    line <- substring(line, 2, nchar(line) - 1)
  }
  
  # Substituir vírgulas dentro de "" por hífen
  parts <- strsplit(line, "\"\"")[[1]]
  
  if (length(parts) > 1) {
    for (i in seq_along(parts)) {
      if (i %% 2 == 0) {
        parts[i] <- gsub(",", " -", parts[i])
      }
    }
    line <- paste(parts, collapse = "")
  }
  return(line)
}

# -----------------------------------------------------------
# APLICAR A LIMPEZA A TODAS AS LINHAS
# -----------------------------------------------------------
cleaned_lines <- sapply(raw_lines, clean_row, USE.NAMES = FALSE)

# -----------------------------------------------------------
# LER O CSV LIMPO PARA DATAFRAME
# -----------------------------------------------------------
Dados2 <- read.csv(text = cleaned_lines,
                   header = TRUE,
                   sep = ",",
                   dec = ".",
                   quote = "",
                   colClasses = c("character", rep(NA, 17)),
                   stringsAsFactors = TRUE)

# -----------------------------------------------------------
# REMOVER COLUNAS DESNECESSÁRIAS
# -----------------------------------------------------------
Dados2 <- subset(Dados2, select = -c(id, name, host_name, latitude, longitude,license))

# -----------------------------------------------------------
# REMOVER REGISTOS SEM PREÇO
# -----------------------------------------------------------
# Caso necessário este df guarda registos com NA
#Dados2_preco_na <- Dados2[(is.na(Dados2$price) | Dados2$price == ""), ]

Dados2 <- Dados2[!(is.na(Dados2$price) | Dados2$price == ""), ]

# -----------------------------------------------------------
# TRATAR REVIEWS POR MÊS - passar para 0 o que está a NA
# -----------------------------------------------------------
# Caso necessário este df guarda registos com NA
#Dados2_rpm_na <- Dados2[(is.na(Dados2$reviews_per_month) | Dados2$reviews_per_month == ""), ]
#unique(Dados2_rpm_na$number_of_reviews)

Dados2$reviews_per_month[is.na(Dados2$reviews_per_month)] <- 0

# -----------------------------------------------------------
# LIMPEZA DE last_review
# -----------------------------------------------------------
Dados2$last_review <- as.Date(Dados2$last_review, format = "%Y-%m-%d")

# Se falhar, tentar outro formato
if (any(is.na(Dados2$last_review))) {
  Dados2$last_review <- as.Date(Dados2$last_review, format = "%d/%m/%Y")
}

# Criar dias desde a última review
Dados2$days_since_last_review <- as.numeric(Sys.Date() - Dados2$last_review)

# Temos valores de NA, que passamos para 0, visto não existirem reviews
sum(is.na(Dados2$days_since_last_review))

Dados2$days_since_last_review[is.na(Dados2$days_since_last_review)] <- 0

#Retirar coluna de last_review
Dados2 <- subset(Dados2, select = -c(last_review))

# Remover fatores não utilizados
factors <- sapply(Dados2, is.factor)  # identifica colunas fator
Dados2[factors] <- lapply(Dados2[factors], droplevels)

# -----------------------------------------------------------
# VERIFICAÇÃO FINAL DE NA
# -----------------------------------------------------------
print(colSums(is.na(Dados2[c("price", "days_since_last_review", "number_of_reviews_ltm")])))

# -----------------------------------------------------------
# EXPORTAR O FICHEIRO FINAL
# -----------------------------------------------------------
#write.csv(Dados2,
#          "Listings_Lisbon_Cleaned.csv",
#          row.names = FALSE,
#          fileEncoding = "UTF-8")
#
#View(Dados2)

# -----------------------------------------------------------
# ANÁLISE EXPLORATÓRIA/IDENTIFICAÇÃO DE OUTLIERS
# -----------------------------------------------------------
summary(Dados2)
str(Dados2)

#Fomos olhar para os preços, vimos que tinhamos muitos preços na casa dos milhares de euros
Dados2 %>% 
  count(price) %>%      # conta quantas vezes cada preço aparece
  arrange(desc(price))  # ordena pelo preço decrescente

# Boxplot mostra que temos muitos casos de preços de vários milhares de euros 

ggplot(Dados2, aes(y = price)) +
  geom_boxplot(
    fill = "skyblue",
    width = 0.3,
    outlier.alpha = 0.3
  ) +
  scale_y_log10(
    labels = comma_format(big.mark = ".", decimal.mark = ",")
  ) +
  labs(
    title = "Distribuição de Preços (escala log)",
    y = "Preço (€)"
  ) +
  theme_minimal()

boxplot.stats(Dados2$price)$stats

ggplot(Dados2, aes(x = price)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black",
                 width = 0.3,
                 outlier.alpha = 0.3) +
  scale_x_log10(
    labels = comma_format(big.mark = ".", decimal.mark = ",")
  ) +
  labs(
    title = "Histograma (escala log)",
    x = "Preço (€)",
    y = "Frequência"
  ) +
  theme_minimal()



# Skewness
skewness(Dados2$price, na.rm = TRUE)

# Kurtosis
kurtosis(Dados2$price, na.rm = TRUE)

#Decidimos retirar os outliers para termos uma amostra mais compreensiva  dos quartos que queremos prever

Q <- quantile(Dados2$price, probs = c(0.25, 0.75), na.rm = TRUE)
IQR_value <- Q[2] - Q[1] #Q[2] representa limite superior Q3

# inicialmente o lower estava definido como lower <- Q[1] - 1.5 * IQR_value = -63
# como quartos não podem ser alugados abaixo de 0, alteramos o limite para 0

lower <- 0
upper <- Q[2] + 1.5 * IQR_value

Dados2_sem_outliers <- Dados2 %>%
  filter(price > lower & price <= upper)

summary(Dados2_sem_outliers$price)

# Boxplot após retirados os outliers

ggplot(Dados2_sem_outliers, aes(y = price)) +
  geom_boxplot(
    fill = "skyblue",
    width = 0.3,
    outlier.alpha = 0.3
  ) +
  labs(
    title = "Distribuição de Preços (retirados outliers)",
    y = "Preço (€)"
  ) +
  theme_minimal()

# Histograma após retirados os outliers
ggplot(Dados2_sem_outliers, aes(x = price)) +
  geom_histogram(bins=10, fill = "skyblue", color ="black",
                 width = 0.3,
                 outlier.alpha = 0.3) +
  labs(
    title = "Histograma de Preços (retirados outliers)",
    x = "Preço (€)",
    y = "Frequência"
  ) +
  theme_minimal()

# MÉDIA DE PREÇO POR NEIGHBOURHOOD GROUP
# Calcular media de preço e contagem de linhas por neighbourhood group
media_preco <- Dados2_sem_outliers %>%
  group_by(neighbourhood_group) %>%
  summarise(
    media_price = mean(price, na.rm = TRUE),
    count = n()
  ) %>%
  arrange(desc(media_price))

# Garantir a ordem no gráfico
media_preco$neighbourhood_group <- factor(media_preco$neighbourhood_group,
                                          levels = media_preco$neighbourhood_group)

# Gráfico de barras com labels na diagonal

ggplot(media_preco, aes(x = reorder(neighbourhood_group, -media_price), y = media_price)) +
  geom_col(fill = "skyblue") +
  geom_text(aes(label = paste0("n=", count)), 
            y = 0,           # posição na base do gráfico
            vjust = 1.5,     # desloca um pouco para cima do eixo
            size = 3) +
  geom_text(aes(label = round(media_price, 1)), 
            vjust = -0.5,    # mantém preço médio acima da barra
            size = 3) +
  labs(title = "Média de Preço por Neighbourhood Group",
       x = "Neighbourhood Group",
       y = "Preço Médio (€)") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()  # remove grelha
  )

# MÉDIA DE PREÇO POR NEIGHBOURHOOD
# Calcular media de preço e contagem de linhas por neighbourhood group
# Calcular média e contagem por neighbourhood
media_preco <- Dados2_sem_outliers %>%
  group_by(neighbourhood) %>%
  summarise(
    media_price = mean(price, na.rm = TRUE),
    count = n(),
    .groups = "drop"
  )

# Guardar Top 5 separado
top5 <- media_preco %>% slice_max(media_price, n = 5)
bottom5 <- media_preco %>% slice_min(media_price, n = 5)

# Juntar e criar coluna de grupo
top5_bottom5 <- bind_rows(top5, bottom5) %>%
  mutate(group = ifelse(neighbourhood %in% top5$neighbourhood, "Top 5", "Bottom 5"))

# Ordenar factor para o gráfico
top5_bottom5 <- top5_bottom5 %>%
  arrange(desc(media_price)) %>%
  mutate(neighbourhood = factor(neighbourhood, levels = neighbourhood))

# Gráfico
ggplot(top5_bottom5, aes(x = reorder(neighbourhood, -media_price), y = media_price, fill = group)) +
  geom_col() +
  # n de anúncios na base da barra
  geom_text(aes(label = paste0("n=", count)), 
            y = 0,        # na base
            vjust = 1.5,  # desloca ligeiramente para cima do eixo
            size = 3) +
  # preço médio acima da barra
  geom_text(aes(label = paste0(round(media_price, 1), "€")), 
            vjust = -0.5, 
            size = 3) +
  scale_fill_manual(values = c("Top 5" = "skyblue", "Bottom 5" = "salmon")) +
  labs(
    title = "Top 5 e Bottom 5 Neighbourhoods por Preço Médio",
    x = "Neighbourhood",
    y = "Preço Médio (€)",
    fill = ""
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()  # remove grelha
  )

# MÉDIA DE PREÇO POR ROOM TYPE
# Calcular media de preço e contagem por room_type
media_room <- Dados2_sem_outliers %>%
  group_by(room_type) %>%
  summarise(
    media_price = mean(price, na.rm = TRUE),
    count = n()
  ) %>%
  arrange(desc(media_price))

# Garantir a ordem no gráfico
media_room$room_type <- factor(media_room$room_type,
                               levels = media_room$room_type)

# Gráfico de barras
ggplot(media_room, aes(x = reorder(room_type, -media_price), y = media_price)) +
  geom_col(fill = "skyblue") +
  geom_text(aes(label = paste0("n=", count)), 
            y = 0,           # posição na base do gráfico
            vjust = 1.5,     # desloca um pouco para cima do eixo
            size = 3) +
  geom_text(aes(label = round(media_price, 1)), 
            vjust = -0.5,    # mantém preço médio acima da barra
            size = 3) +
  labs(title = "Média de Preço por Room Type",
       x = "Room Type",
       y = "Preço Médio (€)") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()  # remove grelha
  )

# Boxplot preço por room type
ggplot(Dados2_sem_outliers, aes(x = room_type, y = price)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Distribuição de Preços por Room Type",
       x = "Room Type",
       y = "Preço") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Violino preço por room type
ggplot(Dados2_sem_outliers, aes(x = room_type, y = price)) +
  geom_violin(fill = "skyblue", trim = FALSE) +
  labs(title = "Distribuição de Preços por Room Type",
       x = "Room Type",
       y = "Preço") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(Dados2_sem_outliers, aes(x = room_type, y = price)) +
  geom_violin(fill = "skyblue", trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white", outlier.alpha = 0.3) +
  labs(title = "Distribuição de Preços por Room Type",
       x = "Room Type",
       y = "Preço") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# PERCENTAGEM DE ANUNCIOS POR ROOM TYPE
# Calcular percentagem de cada room_type
room_type_pct <- Dados2_sem_outliers %>%
  group_by(room_type) %>%
  summarise(count = n()) %>%
  mutate(pct = count / sum(count) * 100)

# Gráfico de barras horizontais
ggplot(room_type_pct, aes(x = reorder(room_type, pct), y = pct, fill = room_type)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste0(round(pct,1), "%")), hjust = -0.1, size = 4) +
  labs(
    title = "Percentagem de Anúncios por Room Type",
    x = "Room Type",
    y = "Percentagem (%)"
  ) +
  coord_flip() +  # barras horizontais
  theme_minimal() +
  theme(panel.grid = element_blank())

# BOPXLOT PREÇO POR ROOM TYPE POR NEIGHBOURHOOD
# Facet wrap
ggplot(Dados2_sem_outliers, aes(x = room_type, y = price)) +
  geom_boxplot(fill = "skyblue") +
  scale_y_log10(labels = comma_format(big.mark = ".", decimal.mark = ",")) + # útil se preços assimétricos
  labs(title = "Distribuição de Preços por Room Type e Neighbourhood Group",
       x = "Room Type",
       y = "Preço (€)") +
  facet_wrap(~neighbourhood_group) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# PERCENTAGEM DE ANUNCIOS POR ROOM TYPE POR NEIGHBOURHOOD
# Contagem por grupo e tipo de quarto
room_group <- Dados2_sem_outliers %>%
  group_by(neighbourhood_group, room_type) %>%
  summarise(count = n(), .groups = "drop") 

# Top 10 grupos com mais anúncios
top10_groups <- room_group %>%
  group_by(neighbourhood_group) %>%
  summarise(total_count = sum(count), .groups = "drop") %>%
  arrange(desc(total_count)) %>%
  slice_head(n = 10)

# Filtrar apenas Top 10 e calcular percentagem **por grupo**
room_group_top10 <- room_group %>%
  filter(neighbourhood_group %in% top10_groups$neighbourhood_group) %>%
  left_join(top10_groups, by = "neighbourhood_group") %>%  # adiciona total_count
  group_by(neighbourhood_group) %>%
  mutate(pct = count / sum(count) * 100) %>%  # percentagem dentro do grupo
  ungroup()

# Criar labels do eixo X com número total de anúncios
room_group_top10 <- room_group_top10 %>%
  mutate(x_label = paste0(neighbourhood_group, "\n(n=", total_count, ")"))

# Gráfico de barras empilhadas
ggplot(room_group_top10, aes(x = reorder(x_label, -total_count), y = pct, fill = room_type)) +
  geom_col() +
  geom_text(aes(label = paste0(round(pct,1), "%")), 
            position = position_stack(vjust = 0.5), size = 3) +
  labs(
    title = "Distribuição de Room Type nos Top 10 Neighbourhood Groups com Mais Anúncios",
    x = "Neighbourhood Group (n total de anúncios)",
    y = "Percentagem (%)",
    fill = "Room Type"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

summary(Dados2_sem_outliers)

describe(select(Dados2_sem_outliers, 5:12), IQR = TRUE)

# Coeficiente de Eta entre room_type e price
anova_ <- aov (price ~ room_type, Dados2_sem_outliers)
(Eta_ <- sqrt(etaSquared(anova_ )[,1]))

# Coeficiente de Eta entre neighbourhood e price
anova_ <- aov (price ~ neighbourhood, Dados2_sem_outliers)
(Eta_ <- sqrt(etaSquared(anova_ )[,1]))

# Coeficiente de Eta entre neighbourhood_group e price
anova_ <- aov (price ~ neighbourhood_group, Dados2_sem_outliers)
(Eta_ <- sqrt(etaSquared(anova_ )[,1]))

# Coeficiente de Pearson entre days_since_last_review e price
cor(Dados2_sem_outliers$price ,Dados2_sem_outliers$days_since_last_review)

# Coeficiente de Pearson entre number_of_reviews_ltm e price
cor(Dados2_sem_outliers$price ,Dados2_sem_outliers$number_of_reviews_ltm)

# Coeficiente de Pearson entre availability_365 e price
cor(Dados2_sem_outliers$price ,Dados2_sem_outliers$availability_365)

# Coeficiente de Pearson entre calculated_host_listings_count e price
cor(Dados2_sem_outliers$price ,Dados2_sem_outliers$calculated_host_listings_count)

# Coeficiente de Pearson entre reviews_per_month e price
cor(Dados2_sem_outliers$price ,Dados2_sem_outliers$reviews_per_month)

# Coeficiente de Pearson entre number_of_reviews e price
cor(Dados2_sem_outliers$price ,Dados2_sem_outliers$number_of_reviews)

# Coeficiente de Pearson entre minimum_nights e price
cor(Dados2_sem_outliers$price ,Dados2_sem_outliers$minimum_nights)

round(cor(Dados2_sem_outliers[,5:12]),6)

(dist.cor <- round(1-cor(Dados2_sem_outliers[,5:12]),6))

# Dendograma
dist.cor<-as.dist(dist.cor)
hc.cor <- hclust(d = dist.cor, method = "average")
dist.coph <- cophenetic(hc.cor)
cor(dist.cor, dist.coph)# o dendrograma representa bem as correlações

fviz_dend(hc.cor, k = 3, # Cut in 3 groups
          cex = 0.5,k_colors = c("coral4","coral",
                                 "blue"),color_labels_by_k = TRUE,
          rect = TRUE )# retângulos a limitar grupos

# Matriz de distâncias
Dados2_sem_outliers.met<-as.matrix(Dados2_sem_outliers[,5:12])
Dados2_sem_outliers.met<-t(Dados2_sem_outliers.met) # transpose
dist.cor2<-get_dist(Dados2_sem_outliers.met,
                    method = "pearson")
# get_dist(): Computes a distance matrix between the rows of a data matrix.
fviz_dist(dist.cor2,order = FALSE,
          show_labels = TRUE,lab_size = NULL,
          gradient = list(low = "dodgerblue",
                          mid = "white", high = "maroon4"))

# CORRPLOT

# Criar matriz numérica
Dados2_met <- as.matrix(Dados2_sem_outliers[,5:12])

# Calcular matriz de correlação (Pearson por padrão)
cor_mat <- cor(Dados2_met, method = "pearson")

# Visualizar a matriz de correlação

corrplot(cor_mat, method = "color", 
         type = "upper", # mostrar apenas a metade superior
         order = "hclust", # ordenar por cluster hierárquico
         addCoef.col = "black", # adicionar valores da correlação
         tl.col = "black", tl.srt = 45, # cores e rotação das labels
         col = colorRampPalette(c("dodgerblue", "white", "maroon4"))(200))

corrplot(cor_mat, method = "ellipse", 
         type = "upper", # mostrar apenas a metade superior
         order = "hclust", # ordenar por cluster hierárquico
         addCoef.col = "black", # adicionar valores da correlação
         tl.col = "black", tl.srt = 45, # cores e rotação das labels
         col = colorRampPalette(c("dodgerblue", "white", "maroon4"))(200))

### Dividir o dataset em conjuntos de treino e de teste

# Remover fatores não utilizados
factors <- sapply(Dados2_sem_outliers, is.factor)  # identifica colunas fator
Dados2_sem_outliers[factors] <- lapply(Dados2_sem_outliers[factors], droplevels)

#Retirar coluna de host_id - demasiados níveis
Dados2_sem_outliers <- subset(Dados2_sem_outliers, select = -c(host_id))

set.seed(3900)

# 'neighbourhood' variável que queremos estratificar
train_index <- createDataPartition(Dados2_sem_outliers$neighbourhood, 
                                   p = 0.7,    # 70% treino
                                   list = FALSE)

train_strat <- Dados2_sem_outliers[train_index, ]
test_strat  <- Dados2_sem_outliers[-train_index, ]

### Criação e testes de Modelos

# Criação do Modelo de Regressão Linear Múltipla para a variável alvo (com os dados de treino)

modelo_reg_lin_mult <- lm(price ~ . -price, data = train_strat)
summary(modelo_reg_lin_mult)
coef(modelo_reg_lin_mult)

# Calculo de métricas Dados Treino
## Cálculo do RMSE do modelo 
(RMSE_train<-sigma(modelo_reg_lin_mult))
## Cálculo do MSE do modelo 
(MSE_train<-RMSE_train^2)
## Cálculo do MAE do modelo
(MAE_train<-mean(abs(resid(modelo_reg_lin_mult))))
## Coeficiente de Determinação R^2 
(R_Square_train<-1-(nrow(train_strat)*MSE_train/sum((train_strat$price-mean(train_strat$price))^2)))

# Teste do Modelo Regressão Linear Múltipla criado (com os dados de teste)
estimativas<-predict(modelo_reg_lin_mult,test_strat)

# Calculo de métricas Dados Teste 
## Cálculo do MSE 
(MSE_teste<-mse(test_strat$price,estimativas))
## Cálculo do RMSE 
(RMSE_teste<-sqrt(MSE_teste))
## Cálculo do MAE  
(MAE_teste<-mae(test_strat$price,estimativas))
## Coeficiente de Determinação R^2 
(R_Square_teste<-1-(nrow(test_strat)*MSE_teste/sum((test_strat$price-mean(test_strat$price))^2)))
# Validação do modelo
# Teste de Breusch-Pagan
bptest(modelo_reg_lin_mult)
## Teste de White
(teste_white<-white(mainlm =  modelo_reg_lin_mult,interactions = FALSE))
## Teste de Breusch Godfrey
bgtest(modelo_reg_lin_mult)
## Cálculo dos Resíduos
residuos<-resid(modelo_reg_lin_mult)
## Apresentação dos 6 primeiros resíduos
head(residuos)

## Validação de Preço estimado
min(estimativas)
#_______________________________________
# Criação do Modelo de Regressão Linear Múltipla Logarítmica (com os dados de treino)
modelo_reg_lin_mult_log <- lm(log(price) ~ . -price, data = train_strat)
summary(modelo_reg_lin_mult_log)
coef(modelo_reg_lin_mult_log)
# Calculo de métricas Dados Treino
## Cálculo do RMSE do modelo 
(RMSE_train<-sigma(modelo_reg_lin_mult_log))
## Cálculo do MSE do modelo 
(MSE_train<-RMSE_train^2)
## Cálculo do MAE do modelo
(MAE_train<-mean(abs(resid(modelo_reg_lin_mult_log))))
## Coeficiente de Determinação R^2 
(R_Square_train<-1-(nrow(train_strat)*MSE_train/sum((train_strat$price-mean(train_strat$price))^2)))
# Teste do Modelo Regressão Linear Múltipla Logarítmica criado (com os dados de teste)
estimativas_log<-exp(predict(modelo_reg_lin_mult_log,test_strat))
# Calculo de métricas Dados Teste 
## Cálculo do MSE 
(MSE_teste<-mse(test_strat$price,estimativas_log))
## Cálculo do RMSE 
(RMSE_teste<-sqrt(MSE_teste))
## Cálculo do MAE  
(MAE_teste<-mae(test_strat$price,estimativas_log))
## Coeficiente de Determinação R^2 
(R_Square_teste<-1-(nrow(test_strat)*MSE_teste/sum((test_strat$price-mean(test_strat$price))^2)))
# Validação do modelo
## Teste de Breusch-Pagan
bptest(modelo_reg_lin_mult_log)
## Teste de White
(teste_white_log<-white(mainlm =  modelo_reg_lin_mult_log,interactions = FALSE))
## Teste de Breusch-Godfrey
bgtest(modelo_reg_lin_mult_log)
## Cálculo dos Resíduos
residuos_log<-resid(modelo_reg_lin_mult_log)
## Apresentação dos 6 primeiros resíduos
head(residuos_log)
## Validação de Preço estimado
min(estimativas_log)   

# Grafico com dados reais vs previsão
df_prev <- data.frame(
  Real = test_strat$price,
  Previsto = estimativas_log
)
plot(df_prev$Real, df_prev$Previsto,
     xlab = "Preço Real (Log)",
     ylab = "Preço Previsto (Log)",
     main = "Valores Reais vs Valores Previstos",
     pch = 16,
     col = rgb(0,0,0.6,0.4)
)
abline(0, 1, col = "red", lwd = 2)

# ============================================================
# MODELO DE ÁRVORE DE DECISÃO (CART)
# ============================================================

# Criação do modelo de Árvore de Decisão para regressão
# - Algoritmo CART (rpart)
# - Método "anova" (regressão)
# - Validação cruzada com 10 folds (xval = 10)
# - A variável host_id é implicitamente ignorada por não ser informativa
model_tree <- rpart(
  formula  = price ~ . - price,
  data     = train_strat,
  method   = "anova",
  control  = rpart.control(xval = 10)
)

# ------------------------------------------------------------
# RESUMO E INSPEÇÃO DO MODELO
# ------------------------------------------------------------
model_tree

# ------------------------------------------------------------
# REPRESENTAÇÃO GRÁFICA DA ÁRVORE
# ------------------------------------------------------------

# Gráfico base da árvore
prp(
  model_tree,
  type = 1,
  extra = 101,
  fallen.leaves = TRUE,
  cex = 0.7,
  nn = TRUE,
  split.fun = function(x, labs, digits, varlen, faclen) rep("", length(labs))
)
title("Árvore de Decisão (CART) – Previsão do Preço", line = 3)

# ------------------------------------------------------------
# ANÁLISE DA COMPLEXIDADE E PODA
# ------------------------------------------------------------

# Tabela de complexidade (CP) e erro de validação cruzada
printcp(model_tree)

# Representação gráfica do erro em função de CP
oldpar <- par(no.readonly = TRUE)
par(mar = c(5, 4, 6, 2))
plotcp(model_tree)
title("Erro de Validação Cruzada vs Parâmetro de Complexidade (cp)", line = 4.5)
par(oldpar)

# Identificação do CP com menor erro de validação cruzada
cp_min <- which.min(model_tree$cptable[, "xerror"])
cp_min

# ------------------------------------------------------------
# AVALIAÇÃO DO MODELO (DADOS DE TESTE)
# ------------------------------------------------------------

# Previsões no conjunto de teste
model_tree_pred <- predict(model_tree, test_strat, type = "vector")

# Gráfico: valores reais vs previstos
plot(
  test_strat$price,
  model_tree_pred,
  main = "Árvore CART – Preços Previstos vs Reais",
  xlab = "Valores Reais",
  ylab = "Valores Previstos"
)
abline(0, 1)

# Cálculo e visualização dos resíduos
residuos <- test_strat$price - model_tree_pred
plot(
  test_strat$price,
  residuos,
  main = "Resíduos do Modelo CART em Função do Preço Real",
  xlab = "Preço (Valores Reais)",
  ylab = "Resíduos"
)
abline(h = 0)

# Métricas de erro
model_tree_rmse <- RMSE(pred = model_tree_pred, obs = test_strat$price)
model_tree_mae  <- MAE(pred = model_tree_pred,  obs = test_strat$price)

model_tree_rmse
model_tree_mae

# ------------------------------------------------------------
# MODELO PODADO (PRUNING)
# ------------------------------------------------------------

# Aplicação da poda com CP selecionado (cp = 0.01)
model_tree_prune <- prune(model_tree, cp = 0.01)

# Resumo do modelo podado
model_tree_prune_party <- as.party(model_tree_prune)
model_tree_prune_party

# Representação gráfica do modelo podado
prp(
  model_tree_prune,
  type = 1,
  extra = 101,
  fallen.leaves = TRUE,
  cex = 0.7,
  nn = TRUE,
  split.fun = function(x, labs, digits, varlen, faclen) rep("", length(labs))
)

title("Árvore de Decisão (CART) – Modelo Podado", line = 3)

# ------------------------------------------------------------
# AVALIAÇÃO DO MODELO PODADO (DADOS DE TESTE)
# ------------------------------------------------------------

# Previsões com o modelo podado
model_tree_prune_pred <- predict(model_tree_prune, test_strat, type = "vector")

# Gráfico: valores reais vs previstos
plot(
  test_strat$price,
  model_tree_prune_pred,
  main = "Árvore CART Podada – Previstos vs Reais",
  xlab = "Valores Reais",
  ylab = "Valores Previstos"
)
abline(0, 1)

# Resíduos do modelo podado
residuos_prune <- test_strat$price - model_tree_prune_pred
plot(
  test_strat$price,
  residuos_prune,
  main = "Resíduos do Modelo CART Podado em Função do Preço Real",
  xlab = "Preço (Valores Reais)",
  ylab = "Resíduos (Modelo Podado)"
)
abline(h = 0)

# Métricas de erro do modelo podado
model_tree_prune_rmse <- RMSE(pred = model_tree_prune_pred, obs = test_strat$price)
model_tree_prune_mae  <- MAE(pred = model_tree_prune_pred,  obs = test_strat$price)

model_tree_prune_rmse
model_tree_prune_mae


# Desenvolvimentos de restantes metodologias de árvores de decisão

# ============================================================
# MODELO BAGGING (treebag) - caret
# ============================================================

# ------------------------------------------------------------
# 1) Configuração da validação cruzada
# ------------------------------------------------------------
# - 10-fold CV
# - guardar previsões finais (útil para análise posterior)
cv.control <- trainControl(
  method = "cv",
  number = 10,
  savePredictions = "final"
)

# ------------------------------------------------------------
# 2) Treino do modelo Bagging
# ------------------------------------------------------------
# - treebag: bagging de árvores
# - nbagg = 100: número de árvores (bootstrap samples)
# - metric = "RMSE": métrica de otimização durante o treino
# - tuneLength = 5: exploração automática de hiperparâmetros (quando aplicável)
model_bag <- train(
  price ~ .,
  data = train_strat,
  method = "treebag",
  nbagg = 100,
  metric = "RMSE",
  tuneLength = 5,
  trControl = cv.control
)

# Resumo do modelo treinado
model_bag

# Guardar as árvores internas do modelo final (para inspeção/visualização)
trees <- model_bag$finalModel$mtrees

# ------------------------------------------------------------
# 3) Visualização de uma árvore individual do ensemble
# ------------------------------------------------------------
# Selecionar uma árvore específica (ex.: a 10ª árvore)
tree1 <- trees[[10]]$btree

# Verificar a classe do objeto (tipicamente "rpart")
class(tree1)

# Plotar a árvore selecionada
rpart.plot(
  tree1,
  main = "Exemplo de uma árvore do modelo Bagging (treebag)"
)

# ------------------------------------------------------------
# 4) Avaliação no conjunto de teste (dados não vistos)
# ------------------------------------------------------------
# Previsões do modelo no conjunto de teste
model_bag_previsao <- predict(model_bag, test_strat)

# Gráfico: valores reais vs previstos
plot(
  test_strat$price,
  model_bag_previsao,
  main = "Modelo Bagging (treebag): Previstos vs Reais",
  xlab = "Reais",
  ylab = "Previstos"
)
abline(0, 1)

# Métricas de erro no teste
model_bag_rmse <- RMSE(pred = model_bag_previsao, obs = test_strat$price)
model_bag_mae  <- MAE(pred = model_bag_previsao,  obs = test_strat$price)

model_bag_rmse
model_bag_mae

# ------------------------------------------------------------
# 5) Importância das variáveis (top 15)
# ------------------------------------------------------------
# Calcular importância completa
vimp <- varImp(model_bag)

# Ordenar por importância (decrescente) e selecionar top 15
vimp$importance <- vimp$importance[
  order(vimp$importance$Overall, decreasing = TRUE),
  , drop = FALSE
][1:15, , drop = FALSE]

# Plot da importância das 15 variáveis
plot(vimp, main = "Importância das 15 Variáveis – Bagging")


# Criação de um modelo com o método Florestas Aleatórias
#ESTES MODELOS ENCONTRAM-SE COMENTADOS DEVIDO AO FACTO DE DEMORAREM MAIS TEMPO A CARREGAR

#
#cv.control<-trainControl(method="cv",number=5,savePredictions="final")
#model_forest<-train(price ~.,data=train_strat,method="ranger",
#                    metric="RMSE",importance="impurity",
#                    tuneLength=5,trControl=cv.control)
#model_forest

#
# Teste ao Modelo criado (model_forest) com base nos dados não vistos do conjunto teste
#
# Cálculo das previsõses recorrendo ao modelo criado (model_forest)
#model_forest_previsao<-predict(model_forest,test_strat)
#plot(test_strat$price,model_forest_previsao,main="Modelo obtido com método Florestas Aleatórias: Previstos vs Reais",xlab="Reais",ylab="Previstos")
#abline(0,1)

# Cálculo do RMSE
#model_forest_rmse<-RMSE(pred=model_forest_previsao,obs=test_strat$price)
#model_forest_rmse

# Cálculo do MAE
#model_forest_mae<-MAE(pred=model_forest_previsao,obs=test_strat$price)
#model_forest_mae

# Criação de um modelo com o método Florestas Aleatórias 10 Folds
#ESTES MODELOS ENCONTRAM-SE COMENTADOS DEVIDO AO FACTO DE DEMORAREM MAIS TEMPO A CARREGAR

#cv.control<-trainControl(method="cv",number=10,savePredictions="final")
#model_forest<-train(price ~.,data=train_strat,method="ranger",
#                    metric="RMSE",importance="impurity",
#                    tuneLength=5,trControl=cv.control)
#model_forest

#
# Teste ao Modelo criado (model_forest) com base nos dados não vistos do conjunto teste
#
# Cálculo das previsõses recorrendo ao modelo criado (model_forest)
#model_forest_previsao<-predict(model_forest,test_strat)
#plot(test_strat$price,model_forest_previsao,main="Modelo obtido com método Florestas Aleatórias: Previstos vs Reais",xlab="Reais",ylab="Previstos")
#abline(0,1)

# Cálculo do RMSE
#model_forest_rmse<-RMSE(pred=model_forest_previsao,obs=test_strat$price)
#model_forest_rmse

# Cálculo do MAE
#model_forest_mae<-MAE(pred=model_forest_previsao,obs=test_strat$price)
#model_forest_mae

#Importância das variáveis
#plot(varImp(model_forest),main="Importância das Variáveis para o modelo obtido com Florestas Aleatórias")

# Criação de um modelo recorrendo ao método Florestas Aleatórias com Ajustamento dos Hiperparâmetros
#
cv.control<-trainControl(method="cv",number=5,savePredictions="final")

# Ajustamento dos hiperparâmetros 500 variance

tune_forest<-expand.grid(mtry =c(3,4,5,6),splitrule="variance",min.node.size=c(5,10,20))

model_forest_tune<-train(price ~.,data=train_strat,method="ranger",metric="RMSE",num.trees=500,
                         tuneGrid=tune_forest,importance="impurity",tuneLength=5,trControl=cv.control)
model_forest_tune

# Ajustamento dos hiperparâmetros 1000 variance

tune_forest<-expand.grid(mtry =c(3,4,5,6),splitrule="variance",min.node.size=c(5,10,20))

model_forest_tune<-train(price ~.,data=train_strat,method="ranger",metric="RMSE",num.trees=1000,
                         tuneGrid=tune_forest,importance="impurity",tuneLength=5,trControl=cv.control)
model_forest_tune

# Teste ao Modelo criado (model_forest_tune) com base nos dados não vistos do conjunto teste (teste_1)
#
# Cálculo das previsões recorrendo ao modelo criado (model_forest_tune)
model_forest_tune_previsao<-predict(model_forest_tune,test_strat)
plot(test_strat$price,model_forest_tune_previsao,main="Florestas Aleatórias com Ajustamento: Previstos vs Reais",xlab="Reais",ylab="Previstos")
abline(0,1, col = "red")

# Cálculo do RMSE
model_forest_tune_rmse<-RMSE(pred=model_forest_tune_previsao,obs=test_strat$price)
model_forest_tune_rmse

# Cálculo do MAE
model_forest_tune_mae<-MAE(pred=model_forest_tune_previsao,obs=test_strat$price)
model_forest_tune_mae

#Importância das variáveis
# Obter a importância completa
vimp <- varImp(model_forest_tune)

# Ordenar e ficar só com o top 15
vimp$importance <- vimp$importance[
  order(vimp$importance$Overall, decreasing = TRUE), , drop = FALSE
][1:15, , drop = FALSE]

# Plot com o mesmo comando que estavas a usar
plot(vimp, main="Importância das 15 Variáveis – Forest")


# Legenda: FA=Florestas Aleatórias
#Importância das variáveis
plot(varImp(model_forest_tune),main="Importância das Variáveis para o modelo obtido com FA considerando Ajustamento")

# Ajustamento dos hiperparâmetros com 500 extratrees
tune_forest<-expand.grid(mtry =c(3,4,5,6),splitrule="extratrees",min.node.size=c(5,10,20))

model_forest_tune<-train(price ~.,data=train_strat,method="ranger",metric="RMSE",num.trees=500,
                         tuneGrid=tune_forest,importance="impurity",tuneLength=5,trControl=cv.control)
model_forest_tune

# Teste ao Modelo criado (model_forest_tune) com base nos dados não vistos do conjunto teste (teste_1)
#
# Cálculo das previsões recorrendo ao modelo criado (model_forest_tune)
model_forest_tune_previsao<-predict(model_forest_tune,test_strat)
plot(test_strat$price,model_forest_tune_previsao,main="Florestas Aleatórias com Ajustamento: Previstos vs Reais",xlab="Reais",ylab="Previstos")
abline(0,1, col = "red")

# Cálculo do RMSE
model_forest_tune_rmse<-RMSE(pred=model_forest_tune_previsao,obs=test_strat$price)
model_forest_tune_rmse

# Cálculo do MAE
model_forest_tune_mae<-MAE(pred=model_forest_tune_previsao,obs=test_strat$price)
model_forest_tune_mae

#Importância das variáveis
# Obter a importância completa
vimp <- varImp(model_forest_tune)

# Ordenar e ficar só com o top 15
vimp$importance <- vimp$importance[
  order(vimp$importance$Overall, decreasing = TRUE), , drop = FALSE
][1:15, , drop = FALSE]

# Plot com o mesmo comando que estavas a usar
plot(vimp, main="Importância das 15 Variáveis – Forest")


# Legenda: FA=Florestas Aleatórias
#Importância das variáveis
plot(varImp(model_forest_tune),main="Importância das Variáveis para o modelo obtido com FA considerando Ajustamento")

# Ajustamento dos hiperparâmetros com 1000 extratrees
tune_forest<-expand.grid(mtry =c(3,4,5,6),splitrule="extratrees",min.node.size=c(5,10,20))

model_forest_tune<-train(price ~.,data=train_strat,method="ranger",metric="RMSE",num.trees=1000,
                         tuneGrid=tune_forest,importance="impurity",tuneLength=5,trControl=cv.control)
model_forest_tune

# Teste ao Modelo criado (model_forest_tune) com base nos dados não vistos do conjunto teste (teste_1)
#
# Cálculo das previsões recorrendo ao modelo criado (model_forest_tune)
model_forest_tune_previsao<-predict(model_forest_tune,test_strat)
plot(test_strat$price,model_forest_tune_previsao,main="Florestas Aleatórias com Ajustamento: Previstos vs Reais",xlab="Reais",ylab="Previstos")
abline(0,1, col = "red")

# Cálculo do RMSE
model_forest_tune_rmse<-RMSE(pred=model_forest_tune_previsao,obs=test_strat$price)
model_forest_tune_rmse

# Cálculo do MAE
model_forest_tune_mae<-MAE(pred=model_forest_tune_previsao,obs=test_strat$price)
model_forest_tune_mae

#Importância das variáveis
# Obter a importância completa
vimp <- varImp(model_forest_tune)

# Ordenar e ficar só com o top 15
vimp$importance <- vimp$importance[
  order(vimp$importance$Overall, decreasing = TRUE), , drop = FALSE
][1:15, , drop = FALSE]

# Plot com o mesmo comando que estavas a usar
plot(vimp, main="Importância das 15 Variáveis – Forest")


# Legenda: FA=Florestas Aleatórias
#Importância das variáveis
plot(varImp(model_forest_tune),main="Importância das Variáveis para o modelo obtido com FA considerando Ajustamento")

# Gradient Boosting Machine

# Criação de um modelo recorrendo ao Gradient Boosting Machine (GBM)

#set.seed(100)

model_boosting<-train(price ~.,data=test_strat,method="gbm",trControl=cv.control)
model_boosting

# Teste ao Modelo criado (model_boosting) com base nos dados não vistos do conjunto teste

# Cálculo das previsões recorrendo ao modelo criado (model_boosting)
model_boosting_previsao<-predict(model_boosting,test_strat)
plot(test_strat$price,model_boosting_previsao,main="Modelo obtido com método GBM: Previstos vs Reais",xlab="Reais",ylab="Previstos")
abline(0,1)

# Cálculo do RMSE
model_boosting_rmse<-RMSE(pred=model_boosting_previsao,obs=test_strat$price)
model_boosting_rmse

# Cálculo do MAE
model_boosting_mae<-MAE(pred=model_boosting_previsao,obs=test_strat$price)
model_boosting_mae

#Importância das variáveis
varImp(model_boosting)
plot(varImp(model_boosting),top = 15,main="Importância das Variáveis para o modelo obtido com GBM")

# Teste 2 - GBM com ajustamento de parâmetros
cv.control<-trainControl(method="cv",number=10,savePredictions="final")
tune_gbm<-expand.grid(interaction.depth = 2,n.trees = c(100,150,180,200),shrinkage = 0.1,n.minobsinnode = c(5,10)) 
model_boosting_tune<-train(price ~.,data=train_strat,method="gbm",tuneGrid=tune_gbm,tuneLength=5,trControl=cv.control)
model_boosting_tune

# Cálculo das previsões recorrendo ao modelo criado (model_boosting)
model_boosting_tune_previsao<-predict(model_boosting_tune,test_strat)
plot(test_strat$price,model_boosting_tune_previsao,main="GBM com Ajustamento: Previstos vs Reais",xlab="Reais",ylab="Previstos")
abline(0,1)

# Cálculo do RMSE
(model_boosting_tune_rmse<-RMSE(pred=model_boosting_tune_previsao,obs=test_strat$price))

# Cálculo do MAE
(model_boosting_tune_mae<-MAE(pred=model_boosting_tune_previsao,obs=test_strat$price))

#Importância das variáveis
varImp(model_boosting_tune)
plot(varImp(model_boosting_tune),top = 15,main="Importância das Variáveis para o modelo GBM com Ajustamento")

# Teste 3 - GBM com ajustamento de parâmetros
#ESTES MODELOS ENCONTRAM-SE COMENTADOS DEVIDO AO FACTO DE DEMORAREM MAIS TEMPO A CARREGAR
#tune_gbm_fast <- expand.grid(interaction.depth = c(2, 3), n.trees = c(200, 300), shrinkage = c(0.05, 0.1), n.minobsinnode = 5)

#model_boosting_fast <- train(price ~ ., data = train_strat, method = "gbm",tuneGrid = tune_gbm_fast, trControl = cv.control, verbose = FALSE)

#model_boosting_fast

#model_boosting_fast_previsao <- predict(model_boosting_fast, test_strat)

# Gráfico: Valores Reais vs Previstos
#plot( test_strat$price, model_boosting_fast_previsao, main = "GBM (Ajustamento Rápido): Previstos vs Reais", xlab = "Reais", ylab = "Previstos")
#abline(0, 1)

# Cálculo do RMSE
#(model_boosting_fast_rmse <- RMSE(pred = model_boosting_fast_previsao, obs = test_strat$price))

# Cálculo do MAE
#(model_boosting_fast_mae <- MAE( pred = model_boosting_fast_previsao, obs = test_strat$price))

# Importância das variáveis
#varImp(model_boosting_fast)

# Gráfico da importância das variáveis (top 15)
#plot(varImp(model_boosting_fast), top = 15, main = "Importância das Variáveis para o modelo GBM (Ajustamento Rápido)")