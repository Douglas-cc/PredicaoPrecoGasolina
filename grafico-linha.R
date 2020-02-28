library(readr)
library(dplyr)
library(ggplot2)

## importando dataset para o R
pdf("linha.pdf")
df <- read.csv("2004-2019.tsv", header = TRUE, sep = "\t", dec = ".")

## Aumento da gasolina comum no estado do Amapá 
AMAPA <- filter(df,PRODUTO == "GASOLINA COMUM",ESTADO == "AMAPA")
ggplot(data = AMAPA, aes(x = ANO, y = PRECO_MEDIO_REVENDA)) + 
  ggtitle("Aumento do Preço da Gasolina no Amapá") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Ano") + ylab("Preço médio de Revenda") +
  geom_jitter()+
  geom_smooth(se = FALSE) 

#gasolina nas Regiões brasilerias
regioes <- filter(df, PRODUTO == "GASOLINA COMUM")
ggplot(data = regioes, mapping = aes(x = ANO, y = PRECO_MEDIO_REVENDA, color = REGIAO)) +
geom_smooth(se = FALSE)

## facet
ggplot(data = regioes) +
geom_smooth(aes(x = ANO, y = PRECO_MEDIO_REVENDA, color = REGIAO, se = FALSE)) +
facet_wrap( ~ REGIAO)

## Estados da Região Norte
EstadosNorte <- filter(df, PRODUTO == "GASOLINA COMUM",
ESTADO %in% c("AMAPA","PARA","ACRE","AMAZONAS","RONDONIA","TOCANTINS","RORAIMA"))  
ggplot(EstadosNorte, aes(x = ANO, y = PRECO_MEDIO_REVENDA, color = ESTADO)) + 
  geom_smooth(se = FALSE) 

## facet  
ggplot(data = EstadosNorte) +  
 geom_smooth(aes(x = ANO, y = PRECO_MEDIO_REVENDA, color = ESTADO, se = FALSE)) +
 facet_wrap( ~ ESTADO)

## Estados da Região sul
EstadosSul <- filter(df, PRODUTO == "GASOLINA COMUM", 
ESTADO %in% c("PARANA","SANTA CATARINA","RIO GRANDE DO SUL"))  
ggplot(EstadosSul, aes(x = ANO, y = PRECO_MEDIO_REVENDA, color = ESTADO)) + 
  geom_smooth(se = FALSE) 

## facet  
ggplot(data = EstadosSul) +  
 geom_smooth(aes(x = ANO, y = PRECO_MEDIO_REVENDA, color = ESTADO, se = FALSE)) +
 facet_wrap( ~ ESTADO)

## Estados da Região Nordeste
EstadosNordeste <- filter(df, PRODUTO == "GASOLINA COMUM", ESTADO %in% c("ALAGOAS","BAHIA","CEARA",
"MARANHAO","PARAIBA","PERNAMBUCO","PIAUI", "RIO GRANDE DO NORTE", "SERGIPE"))  
ggplot(EstadosNordeste, aes(x = ANO, y = PRECO_MEDIO_REVENDA, color = ESTADO)) + 
  geom_smooth(se = FALSE) 

## facet  
ggplot(data = EstadosNordeste) +  
 geom_smooth(aes(x = ANO, y = PRECO_MEDIO_REVENDA, color = ESTADO, se = FALSE)) +
 facet_wrap( ~ ESTADO)

## Estados da Região Centro Oeste e DF
EstCoeste <- filter(df, PRODUTO == "GASOLINA COMUM",
ESTADO %in% c("DISTRITO FEDERAL","GOIAS","MATO GROSSO","MATO GROSSO DO SUL"))  
ggplot(EstCoeste, aes(x = ANO, y = PRECO_MEDIO_REVENDA, color = ESTADO)) + 
  geom_smooth(se = FALSE) 

## facet  
ggplot(data = EstCoeste) +  
 geom_smooth(aes(x = ANO, y = PRECO_MEDIO_REVENDA, color = ESTADO, se = FALSE)) +
 facet_wrap( ~ ESTADO)

# Estados da Região Sudeste
EstSudeste <- filter(df, PRODUTO == "GASOLINA COMUM",
ESTADO %in% c("ESPIRITO SANTO","MINAS GERAIS","RIO DE JANEIRO","SAO PAULO"))  
ggplot(EstSudeste, aes(x = ANO, y = PRECO_MEDIO_REVENDA, color = ESTADO)) + 
  geom_smooth(se = FALSE) 

## facet  
ggplot(data = EstSudeste) +  
 geom_smooth(aes(x = ANO, y = PRECO_MEDIO_REVENDA, color = ESTADO, se = FALSE)) +
 facet_wrap( ~ ESTADO)