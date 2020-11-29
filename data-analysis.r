library(sampling)
library(ggplot2)

enem <- read.csv('MICRODADOS_ENEM_2019_FLORIANOPOLIS.csv')

set.seed(1); s <- srswor(n=2000, nrow(enem)) # função set.seed para conseguir replicar a mesma amostra posteriormente
amostra <- getdata(enem, s); amostra

amostra$TP_ESCOLA <- as.factor(amostra$TP_ESCOLA)
amostra$TP_SEXO <- as.factor(amostra$TP_SEXO)
amostra$TP_ESTADO_CIVIL <- as.factor(amostra$TP_ESTADO_CIVIL)
amostra$TP_COR_RACA <- as.factor(amostra$TP_COR_RACA)
amostra$TP_ST_CONCLUSAO <- as.factor(amostra$TP_ST_CONCLUSAO)
amostra$Q001 <- as.factor(amostra$Q001)
amostra$Q002 <- as.factor(amostra$Q002)
amostra$Q006 <- as.factor(amostra$Q006)
amostra$Q025 <- as.factor(amostra$Q025)
amostra$TP_ST_CONCLUSAO <- as.factor(amostra$TP_ST_CONCLUSAO)

# NOTA X TIPO ESCOLA
ggplot(data=amostra, aes(x=TP_ESCOLA,y=NOTA_MEDIA))+ 
  geom_boxplot() +
  labs(x = 'Tipo de escola', y = 'Nota média') +
  theme_minimal()

# NOTA X SEXO
ggplot(data=amostra, aes(x=TP_SEXO,y=NOTA_MEDIA))+
  geom_boxplot() +
  labs(x='Sexo', y='Nota média') +
  theme_minimal()

# NOTA X ESTADO CIVIL
ggplot(data=amostra, aes(x=TP_ESTADO_CIVIL, y=NOTA_MEDIA)) +
  geom_boxplot() +
  labs(x = 'Estado civil', y = 'Nota média') +
  theme_minimal()

# NOTA X RACA
ggplot(data=amostra, aes(x=TP_COR_RACA, y=NOTA_MEDIA)) + 
  geom_boxplot() +
  labs(x = 'Raça do participante', y = 'Nota média') + 
  theme_minimal()

# NOTA X IDADE
ggplot(data=amostra, aes(x=NU_IDADE, y=NOTA_MEDIA)) +
  geom_point() +
  labs(x = 'Idade do participante', y = 'Nota média') +
  theme_minimal()

# NOTA X ESCOLARIDADE PAI
ggplot(data=amostra, aes(x=Q001, y=NOTA_MEDIA)) +
  geom_boxplot() +
  labs(x = 'Escolaridade da Pai', y = 'Nota média') + 
  theme_minimal()

# NOTA X ESCOLARIDADE MAE
ggplot(data=amostra, aes(x=Q002, y=NOTA_MEDIA)) +
  geom_boxplot() +
  labs(x = 'Escolaridade da Mãe', y = 'Nota média') +
  theme_minimal()

# NOTA X RENDA
ggplot(data=amostra, aes(x=Q006, y=NOTA_MEDIA)) +
  geom_boxplot() +
  labs(x = 'Renda', y = 'Nota média') +
  theme_minimal()

# NOTA X INTERNET EM CASA
ggplot(data=amostra, aes(x=Q025, y=NOTA_MEDIA)) +
  geom_boxplot() +
  labs(x = 'Acesso a internet', y = 'Nota média') +
  theme_minimal()

# NOTA X SITUACAO DE CONCLUSAO
ggplot(data=amostra, aes(x=TP_ST_CONCLUSAO, y=NOTA_MEDIA)) +
  geom_boxplot() +
  labs(x = 'Situação de Conclusão do Ensino Médio', y = 'Nota média') + 
  theme_minimal()

#Ajuste dos dados para o modelo
amostra$TP_COR_RACA <- relevel(amostra$TP_COR_RACA, ref = '1')
amostra$TP_ESCOLA <- relevel(amostra$TP_ESCOLA, ref = '3')
amostra$TP_ST_CONCLUSAO <- relevel(amostra$TP_ST_CONCLUSAO, ref = '3')
amostra$Q006 <- relevel(amostra$Q006, ref = 'B')


# SELECAO DE VARIAVEIS
# AJUSTE DO MODELO NULO
m0 <- lm(NOTA_MEDIA~1, # '~1' indica que não consideramos nenhuma variável explicativa
         data=amostra) # nome do objeto onde encontram-se as variáveis
m1 <- step(m0, # m0 indica o modelo nulo
           list(lower=~1, # criamos uma lista: em lower indicamos o modelo nulo
                upper=~NU_IDADE+TP_ESCOLA+TP_SEXO+TP_ESTADO_CIVIL+TP_COR_RACA+TP_ST_CONCLUSAO+Q001+Q002+Q006+Q025), # em upper o modelo saturado (com todas as variáveis possíveis do modelo)
           direction="forward") # indica o método forward

# Ajuste do modelo de regressão linear múltipla
modelo <- lm(NOTA_MEDIA ~ Q006+Q002+TP_ST_CONCLUSAO+TP_COR_RACA+NU_IDADE+Q001+TP_ESCOLA+Q025, # após o '~' adicionamos todas as variáveis explicativas separadas por '+'
             data=amostra) # nome do objeto onde encontram-se as variáveis
summary(modelo) # imprimir o resumo do ajuste do modelo

# Análise de resíduos
# Valores preditos versus Resíduos padronizados
ggplot(data = modelo) + 
  geom_point(aes(x=.fitted, y=.stdresid)) +
  geom_hline(yintercept = 0) +
  labs(x = 'Valores preditos', y = 'Resíduos padronizados') +
  theme_minimal()

# Gráfico de probabilidade normal
ggplot(data = modelo, aes(sample = .stdresid)) +
  stat_qq() +
  stat_qq_line() +
  labs(x = 'Valores esperados pela normal', y = 'Resíduos padronizados') +
  theme_minimal()

# Histograma dos Resíduos padronizados
ggplot(data = modelo) + 
  geom_histogram(aes(x = .stdresid), 
                 bins = 5, 
                 fill = 'lightgrey',
                 colour = 'black') +
  labs(x = 'Resíduos padronizados', y = 'Frequência') +
  theme_minimal()

# Teste de Shapiro-Wilk
shapiro.test(rstandard(modelo))

#summary
summary(modelo)
