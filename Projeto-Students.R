### PROJETO ###

# Problema de negócio:
# Prevendo nota final de exame de alunos na escola

# Carregando pacotes
library(tidyverse) # Manipulação e limpeza dos dados
library(psych) # Painel de correlação
library(scales) # Porcentagem nos gráficos
library(lattice) # Map de correlação
library(randomForest) # Modelo de Machine Learning
library(caTools) # Dividir os dados em treino e teste
library(rpart)# Modelo de Machine Learning
library(caret)# Modelo de Machine Learning
library(e1071)# Modelo de Machine Learning

# Definindo diretório de trabalho
setwd('C:/DataScience/Projetos/Projeto3')

# Carregando os dados
df = read.csv('Students.csv')

#Visualizando o dataset
View(df)

# Informações do dataset
str(df)
summary(df)
head(df)

# Verificando se existem valores NA's
sum(is.na(df))

# Verificando os números da nossa variável preditora 'Class'
# Maioria dos alunos obtiveram uma nota média
table(df$Class)

# Gráfico
ggplot(df, aes(Class, fill = Class))+
  geom_bar()

# Transformando a variavel preditora em numérica ( 0 = L, 1 = M, 2 = H )
levels(df$Class) <- c(2,0,1)
df$Class = as.numeric(levels(df$Class))[df$Class]

# Mudando o nome da variavel preditora para Grade
df <- df%>%
  rename(Grade = Class)

# Média das notas
mean(df$Grade)

# Relação de quem mais levantou a mão com as notas
# Podemos ver que quem menos levanta a mão, a tendencia é ter notas menores
ggplot(df, aes(x=Grade, y = raisedhands, fill = as.factor(Grade)))+
  geom_bar(stat = 'identity')

# BoxPlot
boxplot(df$raisedhands)

# Relação das faltas com mãos levantadas
mean(df$raisedhands) # Média de vezes que os alunos levantaram a mão

df%>%
  group_by(StudentAbsenceDays)%>%
  summarise(total = sum(raisedhands))%>%
  ggplot(aes(x=StudentAbsenceDays, y=total, fill = StudentAbsenceDays))+
  geom_bar(stat = 'identity')+
  labs(title = 'Relação Faltas x Mãos levantadas', x = 'Faltas', y = 'X Mãos levantadas', fill = 'faltas' )+
  theme(plot.title = element_text(hjust = 0.5))

# Relação das faltas com as notas
table(df$Grade , df$StudentAbsenceDays)

prop.table(table(df$Grade))*100 # Proporção das notas dos alunos

# Podemos ver no gráfico uma forte relação das notas com as faltas
# Quanto maior a nota, menos faltas o aluno tem
ggplot(df, aes(x = Grade, fill = StudentAbsenceDays))+
  geom_bar(position = 'dodge')+
  geom_text(aes(label = ..count..), stat = 'count', position = position_dodge(width = 0.7), vjust=-0.2)+
  labs(title = 'Relação entre as faltas e as notas dos alunos', fill = 'Notas')+
  theme(plot.title = element_text(hjust = 0.5))

# Qual gênero tem as melhores notas?
ggplot(df, aes(x= Grade, fill = gender))+
  geom_bar(position = 'dodge')+
  geom_text(stat = 'count', aes(label = ..count..), position = position_dodge(width = 0.8), vjust = -0.2)

# Proporção de nacionalidade e local de nascimento:
prop.table(table(df$NationalITy))*100
prop.table(table(df$PlaceofBirth))*100
table(df$PlaceofBirth) # Quantidade de cada lugar

# As proporções da nacionalidade e do local de nascimento são praticamente as mesmas, 
# e não convem deixarmos 2 colunas com praticamente as mesmas informações,
# por isso vou remover a coluna Nationality
df$NationalITy = NULL

# Agora vamos ver a correlação do local de nascimento com as notas
# As notas variam bastante de país pra país, proporcionalmente não mudam muito.
ggplot(df, aes(PlaceofBirth, fill = as.factor(Grade)))+
  geom_bar(position = 'dodge')

# Section ID
table(df$SectionID)

# Média de notas por section ID
# Variam muito pouco. Section ID parece nao influenciar nas notas
df%>%
  group_by(SectionID)%>%
  summarise(media = mean(Grade))
  
### Média de notas por StageID, GradeID, Semester e Topic ###

# G-05 aparece apenas 3 vezes, e nas vezes a nota foi 0
table(df$GradeID)
df%>%
  group_by(GradeID)%>%
  summarise(media = mean(Grade))%>%
  arrange(desc(media)) 

# A média de quem está iniciando na escola é ligeiramente mais baixa
table(df$StageID)
df%>%
  group_by(StageID)%>%
  summarise(media = mean(Grade)) 

# A média do segundo semestre é ligeiramente mais baixa
table(df$Semester)
df%>%
  group_by(Semester)%>%
  summarise(media = mean(Grade))

# Como vemos no boxplot, as disciplinas Biology e IT fogem um pouco da média
table(df$Topic)
df%>%
  group_by(Topic)%>%
  summarise(media = mean(Grade))%>%
  ggplot(aes(x = '', y = media))+
  geom_boxplot()

# Vamos ver se ser Pai ou Mãe responsável pelo aluno influencia nas notas
prop.table(table(df$Relation)) # 59% Pai e 41% Mãe

# Surpreendentemente a média quase dobra quando o responsável pelo aluno é a mãe
df%>%
  group_by(Relation)%>%
  summarise(media= mean(Grade))%>%
  ggplot(aes(x = Relation, y = media, fill = Relation))+
  geom_bar(stat = 'identity')+
  scale_fill_manual("Relation", values = c("Father" = "deepskyblue", 'Mum' = 'Pink'))+
  geom_text(aes(label = round(media,2)), position = position_dodge(width = 0.7), vjust = -0.2)+
  labs(title = 'Responsável x Média da Nota', y = 'Média')+
  theme(plot.title = element_text(hjust = 0.5))
  
# Analisando os variaveis: VisITedResources, AnnouncementsView, Discussion

# Podemos ver que todas tem uma correlação positiva com a nota do aluno
cols = c('VisITedResources', 'AnnouncementsView', 'Discussion', 'Grade')
pairs.panels(df[,cols])

# Vamos ver a correlação da variavel ParentAnsweringSurvey com Grade
# Podemos ver que os pais que respondem as pesquisas na escola, provavelmente participam mais da vida academia dos filhos
# isso faz com que a nota seja melhor
ggplot(df, aes(Grade, fill = ParentAnsweringSurvey, label = percent(prop.table(stat(count)))))+
  geom_bar(aes(Grade, y = stat(prop), fill = ParentAnsweringSurvey),position = 'dodge')+
  labs(title = 'Proporção de pais que responderam perguntas x Notas dos alunos')+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_text(aes(label = percent(..prop..),y = ..prop..),stat = 'count', vjust = -.2, position = position_dodge(width = 0.8))+
  scale_y_continuous(labels = percent)+
  scale_fill_manual('ParentAnsweringSurvey', values = c('Yes' = 'green4', 'No' = 'red4'))

# Correlação entre ParentschoolSatisfaction e Grade
# Pais que acham que a escola nao tem boas condições, talvez não leve tao a sério o estudo dos filhos
# Oque também influencia na nota
ggplot(df,aes(x = Grade, y = stat(prop),fill = ParentschoolSatisfaction, label = percent(prop.table(stat(count)))))+
  geom_bar(position = 'dodge')+
  labs(title = 'Satisfação dos Pais com a Escola x Notas')+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_text(aes(label = percent(..prop..), y = ..prop..), stat = 'count', position = position_dodge(width = 0.8), vjust = -0.2)+
  scale_y_continuous(labels = percent)

# TRANSFORMANDO AS VARIAVEIS FATOR PARA NUMERICA, pois o modelo de machine learning aprende melhor com números
# e assim também podemos ver a correlação entre elas de uma maneira melhor

str(df) # Resumo dos dados

# Variavel Gender (F = 0, M = 1)
df$gender
levels(df$gender) <- c(0,1)
df$gender = as.numeric(levels(df$gender))[df$gender]

# Variavel PlaceofBirth (Egypt=1, Iran=2, Iraq=3, Jordan=4, KuwaIT=5, lebanon=6, Lybia=7, Morocco=8, 
# Palestine=9, SaudiArabia=10, Syria=11, Tunis=12, USA=13, venzuela=14) 
levels(df$PlaceofBirth)
levels(df$PlaceofBirth) <- c(1:14)
df$PlaceofBirth <- as.numeric(levels(df$PlaceofBirth))[df$PlaceofBirth]

# Variavel StageID (Lowerlevel = 0, MiddleSchool = 1, HighSchool = 2)
levels(df$StageID)
levels(df$StageID) <- c(2,0,1)
df$StageID <- as.numeric(levels(df$StageID))[df$StageID]

# GradeID (G-02=1, G-04=2, G-05=3, G-06=4, G-07=5, G-08=6, G-09=7, G-10=8, G-11=9, G-12=10)
levels(df$GradeID)
levels(df$GradeID) <- c(1:10)
df$GradeID <- as.numeric(levels(df$GradeID))[df$GradeID]

# SectionID (A=1, B=2, C=3)
levels(df$SectionID)
levels(df$SectionID) <- c(1:3)
df$SectionID <- as.numeric(levels(df$SectionID))[df$SectionID]

# Topic (Arabic=1, Biology=2, Chemistry=3, English=4, French=5, Geology=6,   
# History=7, IT=8, Math=9, Quran=10, Science=11, Spanish=12)
levels(df$Topic)
levels(df$Topic) <- c(1:12)
df$Topic <- as.numeric(levels(df$Topic))[df$Topic]

# Semester (F=0, S=1)
levels(df$Semester)
levels(df$Semester) <- c(0,1)
df$Semester = as.numeric(levels(df$Semester))[df$Semester]

# Relation (Father = 0, Mum = 1)
levels(df$Relation)
levels(df$Relation) <- c(0,1)
df$Relation = as.numeric(levels(df$Relation))[df$Relation]

# ParentAnsweringSurvey (No = 0, Yes = 1)
levels(df$ParentAnsweringSurvey)
levels(df$ParentAnsweringSurvey) <- c(0,1)
df$ParentAnsweringSurvey = as.numeric(levels(df$ParentAnsweringSurvey))[df$ParentAnsweringSurvey]

# ParentschoolSatisfaction (Bad = 0, Good = 1)
levels(df$ParentschoolSatisfaction)
levels(df$ParentschoolSatisfaction) <- c(0,1)
df$ParentschoolSatisfaction = as.numeric(levels(df$ParentschoolSatisfaction))[df$ParentschoolSatisfaction]

# StudentAbsenceDays (Under-7 = 0, Above-7 = 1)
levels(df$StudentAbsenceDays)
levels(df$StudentAbsenceDays) <- c(1,0)
df$StudentAbsenceDays = as.numeric(levels(df$StudentAbsenceDays))[df$StudentAbsenceDays]

# Visualizando o dataframe com todas as variaveis numericas
View(df)
head(df)

# ANALISE DE CORRELAÇÃO

# Métodos de Correlação
# Pearson - coeficiente usado para medir o grau de relacionamento entre duas variáveis com relação linear
# Spearman - teste não paramétrico, para medir o grau de relacionamento entre duas variaveis
# Kendall - teste não paramétrico, para medir a força de dependência entre duas variaveis

# Definindo as colunas para a análise de correlação
cols <- names(df)

# Vetor com os métodos
metodos <- c("pearson", "spearman")

# Aplicando os métodos de correlação com a função cor()
cors <- lapply(metodos, function(method)
  cor(df[,cols], method = method))

# Preprando o plot
plot.cors <- function(x, labs){
  diag(x) <- 0.0 
  plot(levelplot(x, 
                  main = paste("Plot de Correlação usando Método", labs),
                  scales = list(x = list(rot = 90), cex = 1.0)) )
}

# Mapa de Correlação
# Podemos ver que a variável StudentAbsencedays tem uma forte correlação com a nota
Map(plot.cors, cors, metodos)


# PREPARANDO MODELO DE MACHINE LEARNING

# Convertendo a varivel a ser prevista para tipo fator
df$Grade = as.factor(df$Grade)

# Copiando o dataframe
df1 = df

# Dividindo os dados para treino e teste
split = sample.split(df$Grade, SplitRatio = 0.75)
dados_treino = df[split == TRUE,]
dados_teste = df[split == FALSE,]

# Podemos usar o modelo Random Forest para ver quais são as variáveis mais importantes
# para prever a nota do aluno

# MODELO RANDOM FOREST
?randomForest
model = randomForest(Grade ~., df,
                     importance = TRUE,
                     ntree = 100,
                     nodesize = 1)
# As variaveis mais importantes são: StudentAbsenceDays + raisedhands + VisITedResources + 
# AnnouncementsView + Discussion + Relation + ParentAnsweringSurvey
varImpPlot(model)

# Criando um modelo com Random Forest
model1 = randomForest(Grade ~ StudentAbsenceDays + raisedhands + VisITedResources + 
                      AnnouncementsView + Discussion + Relation + ParentAnsweringSurvey,
                       dados_treino, ntree = 600, nodesize=4)

# Prevendo com os dados de teste
previsao1 = predict(model1, dados_teste, type = 'class')

# Matriz de Confusão para avaliar o modelo
confusionMatrix(table(previsao1, dados_teste$Grade))

# MODELO RPART
model2 = rpart(Grade ~ StudentAbsenceDays + raisedhands + VisITedResources + 
               AnnouncementsView + Discussion + Relation + ParentAnsweringSurvey,
               dados_treino)

# Prevendo com os dados de teste
previsao2 = predict(model2, dados_teste, type = 'class')

# Matriz de Confusão para avaliar o modelo
confusionMatrix(previsao2, dados_teste$Grade) 


# PACOTE CARET - MÉTODO svmPoly
model3 = train(Grade ~ StudentAbsenceDays + raisedhands + VisITedResources + 
                 AnnouncementsView + Discussion + Relation + ParentAnsweringSurvey, dados_treino, method='svmPoly')

# Prevendo o modelo com os dados de teste
previsao3 = predict(model3, dados_teste)

# Avaliando o modelo
confusionMatrix(previsao3, dados_teste$Grade)


# MODELO SVM
model4 = svm(Grade ~ StudentAbsenceDays + raisedhands + VisITedResources + 
              AnnouncementsView + Discussion + Relation + ParentAnsweringSurvey, dados_teste, type = 'C-classification')

# Fazendo as previsões com os dados de teste
previsao4 = predict(model4, dados_teste, type ='class')

# Avaliando o modelo
confusionMatrix(previsao4, dados_teste$Grade, positive = '1')

### CONCLUSÃO ###

# Após limparmos, organizarmos e visualizarmos os dados, usamos o modelo randomForest,
# para nos mostrar quais são as variaveis mais importantes para prever a nota.
# Depois usamos a matriz de confusão para avaliarmos os 4 modelos de machine learning,
# ela nos dá a acurácia do modelo em porcentagem.
# No final, podemos concluir que o nosso melhor modelo é o 'SVM'.






