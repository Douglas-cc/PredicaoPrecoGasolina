library(dplyr)
library(caTools)
library(ggplot2)
library(ModelMetrics)

## Importando dados...
df <- read.csv("2004-2019.tsv", header = TRUE, sep = "\t", dec = ".")

## Filtragem
amapa <- filter(df,PRODUTO == "GASOLINA COMUM", ESTADO == "AMAPA")

## Teste e Treino
require(caTools)
set.seed(101) 
sample = sample.split(amapa$ANO, SplitRatio = .70)
train = subset(amapa, sample == TRUE)
test  = subset(amapa, sample == FALSE)

## Regressão Linear
mod_linear <- lm(PRECO_MEDIO_REVENDA ~ ANO, data = train)

##consultar as estimativas dos parâmetros:
mod_linear$coefficients

## predição
pred <- predict(mod_linear, newdata = test)

## Comparando erros quadraticos medios da raiz
rmse(pred, test[,"PRECO_MEDIO_REVENDA"])

## Modelo línear
ggplot(mod_linear, aes(x = ANO, y = PRECO_MEDIO_REVENDA)) +
geom_jitter() +
geom_smooth(method = lm, se = FALSE)

## Modelo não-línear
ggplot(mod_linear, aes(x = ANO, y = PRECO_MEDIO_REVENDA)) +
geom_jitter() +
geom_smooth(method = loess, se = FALSE)

## Comparando os dois modelos
ggplot(mod_linear, aes(x = ANO, y = PRECO_MEDIO_REVENDA)) +
geom_jitter() +
geom_smooth(method = lm, se = FALSE) +
geom_smooth(se = FALSE, colour = "red")

## Avaliando condições para um bom ajuste do modelo linear
par(mfrow = c(2,2))
plot(mod_linear, wich = c(1:4), pch = 20)