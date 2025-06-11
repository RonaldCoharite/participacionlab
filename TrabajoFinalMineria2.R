

```{r}
install.packages("margins")
install.packages("pscl")

rm(list = ls())
library(dplyr)
library(labelled)

library(tm)
library(pdftools)
library(wordcloud)
library(SnowballC)
library(tidytext)
library(ggplot2)


library(haven)
library(margins)
library(pscl)
library(car)
library(mixlm)
library(caret)


pdf_files <- list.files(path = "_data/pdf", pattern = "*.pdf", full.names = TRUE)
text_data <- lapply(pdf_files, pdftools::pdf_text)
corpus <- Corpus(VectorSource(text_data))

clean_corpus <- function(corpus) {
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers) 
  corpus <- tm_map(corpus, stripWhitespace) 
  corpus <- tm_map(corpus, removeWords, stopwords("spanish")) 
  corpus <- tm_map(corpus, stemDocument, language = "spanish") 
  return(corpus)
}

clean_corp <- clean_corpus(corpus)
dtm <- DocumentTermMatrix(clean_corp)
dtm_matrix <- as.matrix(dtm)

word_freq <- colSums(dtm_matrix)
word_freq <- sort(word_freq, decreasing = TRUE)

wordcloud(names(word_freq), word_freq,
          max.words = 100, 
          random.order = FALSE, 
          colors = brewer.pal(8, "Dark2"),
          scale = c(3, 0.5))

freq_df <- data.frame(word = names(word_freq), freq = word_freq)
top_words <- head(freq_df, 20)

ggplot(top_words, aes(x = reorder(word, freq), y = freq)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Palabras más frecuentes", x = "Palabra", y = "Frecuencia") +
  theme_minimal()


load("_data/eh23.RData")

val_labels(eh23p$area)
val_labels(eh23p$s01a_02)
var_label(bd$s01a_03)
val_labels(bd$s01a_08)
val_labels(bd$s01a_05)
val_labels(bd$s01a_10)
val_labels(bd$s02a_01a)

print(table(bd$s02a_01a))


#Variables para el logit: ed, exp, exp2, jh, i.civ (estado civil), i.toc(tipo de ocupacion), esMadre 

bd <- eh23p %>% filter(area == 1 & s01a_02 == 2 & s01a_03 <= 48 & s01a_03 >= 24)
bd <- bd %>% mutate(ed = aestudio, exp = (s01a_03-ed-6), exp2 = (s01a_03-ed-6)*(s01a_03-ed-6))

# Variable s02b_07: cuantos hijos vivos ha tenido 
bd$s02b_07[is.na(bd$s02b_07)] <- 0 # Reemplazar NA por 0 

#creacion de la variable esMadre
bd <- bd %>% mutate(esMadre = ifelse(s02b_07 == 0, 0, 1)) #creacion de la variable esMadre


bd <- bd %>% mutate(nhijos_cat = case_when(
  s02b_07 == 0 ~ "Sin hijos",
  s02b_07 == 1 ~ "1 hijo",
  s02b_07 == 2 ~ "2 hijos",
  s02b_07 >= 3 & s02b_07 <= 4 ~ "3-4 hijos",
  s02b_07 >= 5 ~ "5 o más hijos"
)) %>%
  mutate(nhijos_cat = factor(nhijos_cat,
                             levels = c("Sin hijos", "1 hijo", "2 hijos", "3-4 hijos", "5 o más hijos")))



#variable caracteristica indigena (tomada del idioma aprendido en niñez)
bd <- bd %>% mutate(indigena = if_else(s01a_08 %in% c(1, 2, 3, 4, 5, 7, 8, 9, 10, 11, 12,
                   13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
                   25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36,
                   37, 39, 70, 71), 1, 0))
#variable si es jefe de hogar
bd <- bd %>% mutate(jh = if_else(s01a_05 == 1, 1, 0))

#variable estado civil (civ)
bd <- bd %>% mutate(civ = s01a_10) %>% mutate(civ = case_when(
  s01a_10 >= 3 & s01a_10 < Inf ~ 3, TRUE ~ civ))

# Otra variable unida
bd <- bd %>% mutate(unida = case_when(
  s01a_10 %in% c(2, 3) ~ 1, 
  TRUE ~ 0                  
))

#Variable de estado civil
bd <- bd %>% mutate(
  civ_cat = factor(s01a_10,
                   levels = 1:6,
                   labels = c("Soltera", "Casada", "Conviviente", 
                              "Separada", "Divorciada", "Viuda"))
)

#Variable de seguro de salud

bd <- bd %>%
  mutate(seguro = case_when(
    cobersalud == 3 ~ 2,
    cobersalud == 4 ~ 3, 
    TRUE ~ cobersalud)) %>%
  mutate(seguro = factor(seguro,
                         levels = c(1, 2, 3),
                         labels = c("Público", "Privado", "Ninguno")))

print(table(bd$seguro))



m1 <- glm(pea ~  ed + exp + exp2 + jh + civ_cat + esMadre + indigena + seguro + ynolab,
          data = bd,
          family = binomial(link = "logit"))

m2 <- glm(pea ~  ed + exp + exp2 + jh + civ_cat + esMadre + indigena + seguro + ynolab,
          data = bd,
          family = binomial(link = "probit"))


# EN vez de estado civil usar unida

m3 <- glm(pea ~ ed + exp + exp2 + jh + unida + esMadre + indigena + seguro + ynolab,
          data = bd,
          family = binomial(link = "logit"))

m4 <- glm(pea ~ ed + exp + exp2 + jh + unida + esMadre + indigena + seguro + ynolab,
          data = bd,
          family = binomial(link = "probit"))



me_logit <- margins(m1)
summary(me_logit)

me_probit <- margins(m2)
summary(me_probit)



me_logit2 <- margins(m3) # indigena, esMadre y seguro no son significativos
summary(me_logit2)

me_probit2 <- margins(m4) # indigena, esMadre y seguro no son significativos
summary(me_probit2)



round(pR2(m1), 3)
round(pR2(m2), 3)

round(pR2(m3), 3)
round(pR2(m4), 3)

# de m1 y m2 la variable indigena, esMadre, seguro y Nro de hijos no son significativas 

m1 <- glm(pea ~  ed + exp + exp2 + jh + civ_cat + ynolab,
          data = bd,
          family = binomial(link = "logit"))

m2 <- glm(pea ~  ed + exp + exp2 + jh + civ_cat + ynolab,
          data = bd,
          family = binomial(link = "probit"))

me_logit <- margins(m1)
summary(me_logit)

me_probit <- margins(m2)
summary(me_probit)

round(pR2(m1), 3)
round(pR2(m2), 3)



m3 <- glm(pea ~ ed + exp + exp2 + jh + unida + ynolab,
          data = bd,
          family = binomial(link = "logit"))

m4 <- glm(pea ~ ed + exp + exp2 + jh + unida + ynolab,
          data = bd,
          family = binomial(link = "probit"))


me_logit2 <- margins(m3)
summary(me_logit2)

me_probit2 <- margins(m4) 
summary(me_probit2)

round(pR2(m3), 3)
round(pR2(m4), 3)


bd$pea <- factor(bd$pea, levels = c(0,1), labels = c("No", "Si"))

# matriz de confusión 
conf_matrix <- function(modelo, data) {
  prob <- predict(modelo, type = "response")
  pred <- ifelse(prob >= 0.5, "Si", "No")
  pred <- factor(pred, levels = c("No", "Si"))
  confusionMatrix(pred, data$pea)
}

cm_m1 <- conf_matrix(m1, bd)
cm_m2 <- conf_matrix(m2, bd)
cm_m3 <- conf_matrix(m3, bd)
cm_m4 <- conf_matrix(m4, bd)


print(cm_m1)
print(cm_m2)
print(cm_m3)
print(cm_m4)


### Usando otros metodos de clasificacion

bd$id <- 1:nrow(bd)
aux <- createDataPartition(bd$pea, p = 0.7, list = FALSE)
bdtrain <- bd[aux, ] %>% select(-id)
bdtest <- bd[-aux, ] %>% select(-id)

# Arboles de decision 
library(C50)

m_arbol <- C5.0(pea ~ ed + exp + exp2 + jh + civ_cat + ynolab, data = bdtrain)
summary(m_arbol)


pred_arbol <- predict(m_arbol, bdtest)
confusionMatrix(pred_arbol, factor(bdtest$pea, labels = c("No", "Si")))

# KNN
library(class)
bdtrain_knn <- bdtrain %>% mutate(across(c(ed, exp, exp2), scale)) %>% select(ed, exp, exp2)
bdtest_knn <- bdtest %>% mutate(across(c(ed, exp, exp2), scale)) %>% select(ed, exp, exp2)

clase_train <- bdtrain$pea
clase_test <- bdtest$pea

k <- round(sqrt(nrow(bdtrain)), 0)
pred_knn <- knn(bdtrain_knn, bdtest_knn, clase_train, k)
confusionMatrix(pred_knn, factor(clase_test, labels = c("No", "Si")))

# Naive bayes
library(e1071)
m_nb <- naiveBayes(pea ~ ed + exp + exp2 + jh + civ_cat, data = bdtrain)
pred_nb <- predict(m_nb, bdtest)
confusionMatrix(pred_nb, factor(bdtest$pea, labels = c("No", "Si")))





```
