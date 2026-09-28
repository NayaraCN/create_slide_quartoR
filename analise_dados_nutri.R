library(readxl)
install.packages("tidyverse") #contém no pacote tidyverse - library("dplyr") > library("glimpse")
library("dplyr")
library("tidyverse")
install.packages("glimpse2")
library("glimpse2")

#verificando onde estão os dados 
getwd()
dir()
setwd("D:/PosGraduacao_R/analise_dados")

#importar base de dados
dados <- read_excel("DadosNutri.xlsx")

dim(dados)
names(dados)
#glimpse(dados)
dplyr::glimpse(dados)

#verificar valores ausentes 
colSums(is.na(dados))

#conhecendo as categorias

table(dados$Dificuldade_Financeira)
table(dados$Alteracao_Pandemia)

table(dados$Frutas)
table(dados$Legumes_Verduras)
table(dados$Doces)
table(dados$Produtos_Industrializados)
table(dados$Refrigerante)
table(dados$Consumo_agua)

table(dados$Genero)
table(dados$Renda_familiar)

#cruzando dados 
table(
  dados$Dificuldade_Financeira,
  dados$Alteracao_Pandemia
)

prop.table(
  table(
    dados$Dificuldade_Financeira,
    dados$Alteracao_Pandemia
  ),
  margin = 1
) * 100

#realizando o teste qui-quadrado para verificar dependência entre as duas variáveis 
# Criar a tabela de contingência
tab_dificuldade_alteracao <- table(
  dados$Dificuldade_Financeira,
  dados$Alteracao_Pandemia
)

tab_dificuldade_alteracao


teste_chi <- chisq.test(tab_dificuldade_alteracao)

teste_chi

#consultando as caracteristicas das variaveis alimentares
table(dados$Frutas)
table(dados$Legumes_Verduras)
table(dados$Doces)
table(dados$Produtos_Industrializados)
table(dados$Refrigerante)
table(dados$Consumo_agua)

round(prop.table(table(dados$Frutas)) * 100, 1)
round(prop.table(table(dados$Legumes_Verduras)) * 100, 1)
round(prop.table(table(dados$Doces)) * 100, 1)
round(prop.table(table(dados$Produtos_Industrializados)) * 100, 1)
round(prop.table(table(dados$Refrigerante)) * 100, 1)
round(prop.table(table(dados$Consumo_agua)) * 100, 1)


# FRUTAS
round(
  prop.table(
    table(dados$Dificuldade_Financeira, dados$Frutas),
    margin = 1
  ) * 100,
  1
)

# DOCES
round(
  prop.table(
    table(dados$Dificuldade_Financeira, dados$Doces),
    margin = 1
  ) * 100,
  1
)

# PRODUTOS INDUSTRIALIZADOS
round(
  prop.table(
    table(dados$Dificuldade_Financeira,
          dados$Produtos_Industrializados),
    margin = 1
  ) * 100,
  1
)

#tabela de frutas 

frutas_grafico <- dados %>%
  count(Dificuldade_Financeira, Frutas) %>%
  group_by(Dificuldade_Financeira) %>%
  mutate(
    percentual = n / sum(n)
  ) %>%
  ungroup()

frutas_grafico

install.packages("ggplot2")
library("scales")
library("ggplot2")
#### grafico de fruta (%)
ggplot(
  frutas_grafico,
  aes(
    x = Dificuldade_Financeira,
    y = percentual,
    fill = Frutas
  )
) +
  geom_col() +
  
  geom_text(
    aes(
      label = percent(percentual, accuracy = 0.1)
    ),
    position = position_stack(vjust = 0.5),
    size = 3.5
  ) +
  
  scale_y_continuous(labels = percent) +
  
  labs(
    title = "Mudança no consumo de frutas segundo dificuldade financeira",
    x = "Dificuldade financeira",
    y = "Percentual",
    fill = "Consumo de frutas"
  ) +
  
  theme_minimal()

##### Teste sem o rótulo de percentual mesnor que 3%
ggplot(
  frutas_grafico,
  aes(
    x = Dificuldade_Financeira,
    y = percentual,
    fill = Frutas
  )
) +
  geom_col() +
  
  geom_text(
    aes(
      label = ifelse(
        percentual < 0.03,
        "",
        percent(percentual, accuracy = 0.1)
      )
    ),
    position = position_stack(vjust = 0.5),
    size = 3.5
  ) +
  
  scale_y_continuous(labels = percent) +
  
  labs(
    title = "Mudança no consumo de frutas segundo dificuldade financeira",
    x = "Dificuldade financeira",
    y = "Percentual",
    fill = "Consumo de frutas"
  ) +
  
  theme_minimal()

### a moda do professor
# Instale se ainda não tiver: install.packages("ggstats")
library(ggstats)
library(ggplot2)
ggplot(dados, aes(x = Dificuldade_Financeira, fill = Frutas, by= Dificuldade_Financeira)) +
  geom_bar(position = "fill") +
  geom_text(stat = "prop", position = position_fill(.5)) +
  scale_fill_brewer(name="Satisfação com a\nocomida e bebida",palette = "Dark2")+
  scale_y_continuous(labels = scales::percent) +
  xlab("Dificuldade financeira")+
  ylab("Proporção relativa (%)") +
  theme_bw()+
  theme(text = element_text(size = 14))

#### bibliotecas
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

### agrupando as tres variáveis alimentares
habitos_grafico <- dados %>%
  select(
    Dificuldade_Financeira,
    Frutas,
    Doces,
    Produtos_Industrializados
  ) %>%
  
  pivot_longer(
    cols = c(Frutas, Doces, Produtos_Industrializados),
    names_to = "Alimento",
    values_to = "Mudanca"
  ) %>%
  
  count(
    Dificuldade_Financeira,
    Alimento,
    Mudanca
  ) %>%
  
  group_by(
    Dificuldade_Financeira,
    Alimento
  ) %>%
  
  mutate(
    percentual = n / sum(n)
  ) %>%
  
  ungroup()

habitos_grafico

habitos_grafico <- habitos_grafico %>%
  mutate(
    Alimento = recode(
      Alimento,
      "Frutas" = "Frutas",
      "Doces" = "Doces",
      "Produtos_Industrializados" = "Produtos industrializados"
    )
  )

N <- habitos_grafico |>
  dplyr::count(Alimento, Dificuldade_Financeira)

ggplot(
  habitos_grafico,
  aes(
    x = Dificuldade_Financeira,
    y = percentual,
    fill = Mudanca
  )
) +
  
  geom_col() +
  
  geom_text(
    aes(
      label = ifelse(
        percentual < 0.05,
        "",
        percent(percentual, accuracy = 0.1)
      )
    ),
    position = position_stack(vjust = 0.5),
    size = 3
  ) +
  geom_text(
    data = N,
    aes(x = Dificuldade_Financeira, y = 1.02, label = paste0("n = ", n)),
    inherit.aes = FALSE
  ) +
  facet_wrap(~ Alimento) +
  
  scale_y_continuous(
    labels = percent,
    limits = c(0, 1)
  ) +
  
  labs(
    title = "Mudanças nos hábitos alimentares segundo dificuldade financeira",
    x = "Dificuldade financeira",
    y = "Proporção relativa (%)",
    fill = "Mudança no consumo"
  ) +
  
  theme_minimal()
################### versão do professor
N <- habitos_grafico |>
  dplyr::count(Alimento, Dificuldade_Financeira)

ggplot(habitos_grafico
, aes(x = Dificuldade_Financeira, fill = Mudanca, by = Dificuldade_Financeira)) +
  geom_bar(position = "fill") +
  geom_text(stat = "prop", position = position_fill(.5)) +
  geom_text(
    data = N,
    aes(x = Dificuldade_Financeira, y = 1.02, label = paste0("n = ", n)),
    inherit.aes = FALSE
  ) +
  scale_fill_brewer(
    palette = "Dark2",
    name = "Mudança no consumo"
  ) +
  scale_y_continuous(
    labels = scales::percent,
    breaks = seq(0, 1, 0.25),
    limits = c(0, 1.02)
  ) +
  xlab("Dificuldade financeira") +
  ylab("Proporção relativa (%)") +
  facet_wrap(~ Alimento, ncol = 3) +
  theme_bw() +
  theme(text = element_text(size = 14),
        legend.position = "bottom")


ggplot(
  habitos_grafico,
  aes(
    x = Grupo,
    y = percentual,
    fill = Mudanca
  )
) +
  
  geom_col() +
  
  geom_text(
    aes(
      label = ifelse(
        percentual < 0.05,
        "",
        percent(percentual, accuracy = 0.1)
      )
    ),
    position = position_stack(vjust = 0.5),
    size = 3
  ) +
  
  facet_wrap(~ Alimento) +
  
  scale_y_continuous(
    labels = percent,
    limits = c(0, 1)
  ) +
  
  labs(
    title = "Mudanças nos hábitos alimentares segundo dificuldade financeira",
    x = "Dificuldade financeira",
    y = "Percentual",
    fill = "Mudança no consumo"
  ) +
  
  theme_minimal()


###################
habitos_grafico <- habitos_grafico %>%
  mutate(
    Grupo = case_when(
      Dificuldade_Financeira == "Não" ~ "Não\n(n = 1923)",
      Dificuldade_Financeira == "Sim" ~ "Sim\n(n = 436)"
    )
  )

ggplot(
  habitos_grafico,
  aes(
    x = Grupo,
    y = percentual,
    fill = Mudanca
  )
) +
  
  geom_col() +
  
  geom_text(
    aes(
      label = ifelse(
        percentual < 0.05,
        "",
        percent(percentual, accuracy = 0.1)
      )
    ),
    position = position_stack(vjust = 0.5),
    size = 3
  ) +
  
  facet_wrap(~ Alimento) +
  
  scale_y_continuous(
    labels = percent,
    limits = c(0, 1)
  ) +
  
  labs(
    title = "Mudanças nos hábitos alimentares segundo dificuldade financeira",
    x = "Dificuldade financeira",
    y = "Percentual",
    fill = "Mudança no consumo"
  ) +
  
  theme_minimal()
###################################

dados %>%
  filter(Dificuldade_Financeira == "Sim") %>%
  select(Frutas, Doces, Produtos_Industrializados) %>%
  summarise(
    Total = n(),
    
    Frutas_aumentou = sum(Frutas == "Aumentou"),
    Frutas_diminuiu = sum(Frutas == "Diminuiu"),
    
    Doces_aumentou = sum(Doces == "Aumentou"),
    Doces_diminuiu = sum(Doces == "Diminuiu"),
    
    Industrializados_aumentou =
      sum(Produtos_Industrializados == "Aumentou"),
    Industrializados_diminuiu =
      sum(Produtos_Industrializados == "Diminuiu")
  )

dados %>%
  filter(
    Dificuldade_Financeira == "Sim",
    Frutas == "Aumentou",
    Doces == "Aumentou",
    Produtos_Industrializados == "Aumentou"
  ) %>%
  summarise(n = n())
################################
# caracterização dos participantes
#idade e IMC
dados %>%
  summarise(
    n = n(),
    
    idade_media = mean(Idade, na.rm = TRUE),
    idade_mediana = median(Idade, na.rm = TRUE),
    idade_dp = sd(Idade, na.rm = TRUE),
    idade_min = min(Idade, na.rm = TRUE),
    idade_max = max(Idade, na.rm = TRUE),
    
    imc_medio = mean(IMC, na.rm = TRUE),
    imc_mediano = median(IMC, na.rm = TRUE),
    imc_dp = sd(IMC, na.rm = TRUE),
    imc_min = min(IMC, na.rm = TRUE),
    imc_max = max(IMC, na.rm = TRUE)
  )

#caracterização para genero
dados %>%
  count(Genero) %>%
  mutate(
    percentual = round(n / sum(n) * 100, 1)
  )

#caracterização para renda 
dados %>%
  count(Renda_familiar) %>%
  mutate(
    percentual = round(n / sum(n) * 100, 1)
  )
#caracterização da dificuldade financeira
dados %>%
  count(Dificuldade_Financeira) %>%
  mutate(
    percentual = round(n / sum(n) * 100, 1)
  )

#Tabela 
library(tibble)
library(knitr)

tabela_caracterizacao <- tribble(
  ~Caracteristica, ~Resultado,
  "Participantes", "2.359",
  "Idade", "33,3 ± 12,9 anos",
  "IMC", "25,3 ± 5,05 kg/m²",
  "Gênero feminino", "1.775 (75,2%)",
  "Gênero masculino", "584 (24,8%)",
  "Renda até R$ 1.254", "290 (12,3%)",
  "Renda entre R$ 1.255 e R$ 8.640", "1.354 (57,4%)",
  "Renda acima de R$ 8.640", "715 (30,3%)",
  "Dificuldade financeira", "436 (18,5%)"
)

kable(
  tabela_caracterizacao,
  col.names = c("Característica", "Resultado"),
  align = c("l", "c")
)
