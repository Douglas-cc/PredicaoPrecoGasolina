library(dplyr)
library(ggplot2)

## importando dataset para o R
pdf("boxplot(preto).pdf")
df <- read.csv("2004-2019.tsv", header = TRUE, sep = "\t", dec = ".")

## gasolina nas Regiões brasilerias (boxplot)
regioes <- filter(df, PRODUTO == "GASOLINA COMUM")
ggplot(data = regioes, mapping = aes(x = REGIAO, y = PRECO_MEDIO_REVENDA)) +
  geom_boxplot() +
  ggtitle("Preço da Gasolina nas Regiões Brasileiras 2004 a 2019") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Regiões") + ylab("Preço médio de Revenda") 

## Estados da Região Norte
EstadosNorte <- filter(df, PRODUTO == "GASOLINA COMUM",
ESTADO %in% c("AMAPA","PARA","ACRE","AMAZONAS","RONDONIA","TOCANTINS","RORAIMA"))  
ggplot(EstadosNorte, aes(x = ESTADO, y = PRECO_MEDIO_REVENDA)) + 
  geom_boxplot() 

## Estados da Região sul
EstadosSul <- filter(df, PRODUTO == "GASOLINA COMUM", 
ESTADO %in% c("PARANA","SANTA CATARINA","RIO GRANDE DO SUL"))  
ggplot(EstadosSul, aes(x = ESTADO, y = PRECO_MEDIO_REVENDA)) + 
  geom_boxplot() 

## Estados da Região Nordeste
EstadosNordeste <- filter(df, PRODUTO == "GASOLINA COMUM", ESTADO %in% c("ALAGOAS","BAHIA","CEARA",
"MARANHAO","PARAIBA","PERNAMBUCO","PIAUI", "RIO GRANDE DO NORTE", "SERGIPE"))  
ggplot(EstadosNordeste, aes(x = ESTADO, y = PRECO_MEDIO_REVENDA)) + 
  geom_boxplot() + 
  coord_flip()

## Estados da Região Centro Oeste e DF
EstCoeste <- filter(df, PRODUTO == "GASOLINA COMUM",
ESTADO %in% c("DISTRITO FEDERAL","GOIAS","MATO GROSSO","MATO GROSSO DO SUL"))  
ggplot(EstCoeste, aes(x = ESTADO, y = PRECO_MEDIO_REVENDA)) + 
  geom_boxplot() 

## Estados da Região Sudeste
EstSudeste <- filter(df, PRODUTO == "GASOLINA COMUM",
ESTADO %in% c("ESPIRITO SANTO","MINAS GERAIS","RIO DE JANEIRO","SAO PAULO"))  
ggplot(EstSudeste, aes(x = ESTADO, y = PRECO_MEDIO_REVENDA)) + 
  geom_boxplot() 
dev.off()
