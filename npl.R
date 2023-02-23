# borrar workspace
rm(list = ls())


# manejar fechas 
library("lubridate")
# manejar plots 
library("ggplot2")
# manejar flujos de datos de manera cómoda
library("dplyr")
library("tidyr")
# análisis de textos 
library("tm")
library("tidytext")
library("stringr")
#Nube de palabras
install.packages("wordcloud")
#library(wordcloud)
#install.packages("RColorBrewer")
library(RColorBrewer)
#install.packages("wordcloud2")
library(wordcloud2)
#install.packages("text2vec")
library(text2vec)


# cargamos bases de datos, en este caso la extracción es de la página: 
# https://politicosentwitter.com/

tweetsMacri <- read.csv("mauriciomacri_timeline_database.csv")
tweetsCfk <- read.csv("CFKArgentina_timeline_database.csv")

tweets <- rbind(tweetsCfk,tweetsMacri)

# Visualización de los tweets y loa tipos de datos de sus columnas
#View(tweets)
str(tweets$created_at)

# cambiamos el tipo de las fechas

tweets <- tweets %>%
  mutate(created_at = ymd_hms(created_at))

str(tweets$created_at)


# visualización de los datos en un histograma 
ggplot(tweets)+ #datos
  aes(x = created_at, fill=screen_name) + #variables
  geom_histogram(bins = 40, show.legend = FALSE) + 
  facet_wrap(~screen_name,ncol = 1)

# Limpiamos los acentos, y carácteres en español 
#Cargo una lista con las letras a cambiar
esp_char = list('ñ'='n', 'Š'='S', 'š'='s', 'Ž'='Z', 'ž'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E',
                'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
                'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='Ss', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
                'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
                'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y' )


tweets$text <- chartr(paste(names(esp_char), collapse=''),
                      paste(esp_char, collapse=''),
                      tweets$text)


# Al no hacer ningún tipo de restricción ni nada por el estilo, lás palabras más usadas son stop words 
# conjunciones, artículos ... 
# definimos un diccionario, o buscamos en internet un diccionario de stopWords
# podríamos importar uno general de r, pero me quedé con uno que cogí en Internet
# spanish_stop_words <- stopwords(kind = "es")

# como stop_worsds por defectoes un dataframe con dos columnas words y lexicon, tenemos que incluirlo
# en nuestro dataframe propio para poder combinarlos

spanish_stop_words <- read.csv("spanish.txt")

spanish_stop_words$lexicon <- "ESP"
colnames(spanish_stop_words) <- c("word","lexicon")
stop_words <- rbind(stop_words,spanish_stop_words)

# hemos comprobado que aparecen muchas direcciones del estilo http/https que realmente son stop words
# en este contexto 
add_word <- c("http", "ESP")
stop_words <- rbind(stop_words,add_word)
add_word <- c("https", "ESP")
stop_words <- rbind(stop_words,add_word)


# Definimos unas expresiones regulares para limpiar un poco los textos
unnest_reg <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"

#limpio y separo por palabras
tidy_tweets <- tweets %>%
  filter(!str_detect(text, "^RT")) %>%
  unnest_tokens(word, text, token = "regex", pattern = unnest_reg) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))


# cuento el número de palabras usadas en los tweets, agrupadas por autor
freq <- tidy_tweets %>%
  group_by(screen_name) %>%
  count(word, sort = T) 


# nuevo dataframe donde tengo la suma total de palabras usadas en twets, agrupadas por autor
new_df<-tidy_tweets %>%
  group_by(screen_name) %>%
  summarise(total = n())

# View(new_df)

# junto todo los dataframes 
freq <- left_join(freq,new_df,"screen_name")

# hago el cálculo de la frecuencia 
freq<- freq %>%
  mutate(freq = n/total)

# spread lo que hace es poner columnas en base a los valores de una variable, "clasifica como screen_name"
freq2 <- freq %>%
    select(screen_name,word,freq) %>%
    spread(screen_name,freq) %>%
    arrange("CFKArgentina", "mauriciomacri")

# Para facilitar cálculos y para darle sentido lógico reemplazo los NA por 0 
freq2[is.na(freq2)] <- 0


# Para plotear la frecuencia de palabras en base a los 2 candidatos usaremos :
ggplot(freq2, aes(CFKArgentina, mauriciomacri)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels =scales::percent_format()) +
  scale_y_log10(labels = scales::percent_format()) +
  geom_abline(color = "red")

# Vamos a plotear una nube de palabras por usuario con la librería wordcloud
# Inicializamos semilla 
set.seed(1234)

wordcloud(words = freq2$word,freq = freq2$CFKArgentina,random.order = FALSE,rot.per = 0.35, 
          min.freq = 0.001,colors = brewer.pal(8,"Dark2"))

wordcloud(words = freq2$word,freq = freq2$mauriciomacri,random.order = FALSE,rot.per = 0.35, 
          min.freq = 0.001,colors = brewer.pal(8,"Dark2"))

wordcloud2(data = freq2, color ="random-dark")

bigrams<- tweets %>%
  unnest_tokens(bigram,text,token = "ngrams",n = 2)

#head(bigrams)

bigrams_separated<- bigrams %>%
    separate(bigram,c("word1","word2"),sep=" ")
    
#head(bigrams_separated)

bigrams_filtered<- bigrams_separated %>%
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word)

#head(bigrams_filtered)

# contamos los bigrams una vez filtrados por las stop words
bigrams_counts <- bigrams_filtered %>%
    count(word1, word2, sort=TRUE)

#head(bigrams_counts)

# volvemos a unir nuestros bigramas en una sola columna para manejarlo mejor
bigrams <- bigrams_filtered %>%
  unite(bigrams,word1,word2,sep = " ")

head(bigrams)

#calculo del tf-idf de la colección 
bigrams_tf_idf <- bigrams %>%
  count(screen_name,bigrams,sort = TRUE) %>%
  bind_tf_idf(bigrams,screen_name,n)%>%
  arrange(desc(tf_idf))
  
head(bigrams_tf_idf)


# plot de los bigramas por autor ploteamos los top15 
bigrams_tf_idf %>%
  top_n(15) %>%
  ggplot(aes(bigrams,tf_idf, fill = screen_name))+
  geom_col(show.legend = FALSE)+
  labs(x = NULL, y = "tdf_idf")+
  facet_wrap(~screen_name, ncol=1, scales="free")+
  coord_flip()
  

#Análisis de RTs 
remove_reg <- "&amp;|&lt;|&gt;"

tidy_tweets <- tweets %>%
  filter(!str_detect(text, "^(RT|@)")) %>%
  mutate(text = str_remove_all(text,remove_reg)) %>%
  unnest_tokens(word, text, token = "regex", pattern = unnest_reg) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))

head(tidy_tweets)

# cuenta el total de RTs de los users
totals <- tidy_tweets %>% 
  group_by(screen_name, status_id) %>% 
  summarise(rts = first(retweet_count)) %>% 
  group_by(screen_name) %>% 
  summarise(total_rts = sum(rts))

# agrupamos por palabra más retwiteada
word_by_rt <- tidy_tweets %>% 
  group_by(status_id, word, screen_name) %>% 
  summarise(rts = first(retweet_count)) %>% 
  group_by(screen_name,word) %>% 
  summarise(retweets = median(rts), uses=n())%>%
  left_join(totals) %>%
  filter(retweets != 0) %>%
  ungroup()

# filtramos las palabras más usadas y scamos un ranking
word_by_rt %>%
  filter(uses>=5) %>%
  arrange(desc(retweets))


# ploteamos que palabras fueron las más usadas y que fueron mas veces retweeteadas
word_by_rt %>%
  filter(uses >= 5) %>%
  group_by(screen_name) %>%
  top_n(10, retweets) %>%
  arrange(retweets) %>%
  ungroup() %>%
  mutate(word = factor(word, unique(word))) %>%
  ungroup() %>%
  ggplot(aes(word, retweets, fill = screen_name)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ screen_name, scales = "free", ncol = 2) +
  coord_flip() +
  labs(x = NULL, 
       y = "Mediana de la cantidad de retweets por cada palabra contenida en un tweet")


#Analizando favs, código análogo

word_by_favs <- bigrams %>% 
  group_by(status_id, bigrams, screen_name) %>% 
  summarise(favs = first(favorite_count)) %>% 
  group_by(screen_name, bigrams) %>% 
  summarise(favorites = median(favs), uses = n()) %>%
  left_join(totals) %>%
  filter(favorites != 0) %>%
  ungroup()

word_by_favs %>%
  filter(uses >= 5) %>%
  group_by(screen_name) %>%
  top_n(10, favorites) %>%
  arrange(favorites) %>%
  ungroup() %>%
  mutate(word = factor(bigrams, unique(bigrams))) %>%
  ungroup() %>%
  ggplot(aes(bigrams, favorites, fill = screen_name)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ screen_name, scales = "free", ncol = 2) +
  coord_flip() +
  labs(x = NULL, 
       y = "Mediana # de favoritos por tweets conteniendo par de palabras")

# comparacion del uso de palabras 
word_ratios<- tidy_tweets%>%
  filter(!str_detect(word,"^@"))%>%
  count(word,screen_name) %>%
  filter(sum(n)>=10)%>%
  ungroup() %>%
  spread(screen_name,n,fill=0)%>%
  mutate_if(is.numeric,funs((. +1)/sum(.+1)))%>%
  mutate(logratio=log(CFKArgentina/mauriciomacri))%>%
  arrange(desc(logratio))

# plot de comparacion del uso de las palabras
word_ratios %>%
  group_by(logratio<0) %>%
  top_n(n=15,abs(logratio))%>%
  ungroup()%>%
  mutate(word=reorder(word,logratio)) %>%
  ggplot(aes(word,logratio,fill=logratio<0))+
  geom_col(show.legend = TRUE)+
  coord_flip()+
  scale_fill_discrete(name="", labels=c("CFKArgentina","mauriciomacri"))


#Análisis de las tendencias en el tiempo 
words_by_time <- bigrams %>%
  filter(!str_detect(bigrams, "^@")) %>%
  # creo un ventana de timepo de un mes para calcular cuantos bigramas por ventana de tiempo
  mutate(time_floor = floor_date(created_at, unit = "1 month")) %>%
  count(time_floor, screen_name, bigrams) %>%
  ungroup() %>%
  group_by(screen_name, time_floor) %>%
  mutate(time_total = sum(n)) %>%
  group_by(bigrams) %>%
  mutate(word_total = sum(n)) %>%
  ungroup() %>%
  rename(count = n) %>%
  filter(word_total > 10)

head(words_by_time)


# inicializamos para que vaya entrenando el algoritmo word2vec
shakes_words_ls = list(tidy_tweets$word)
it = itoken(shakes_words_ls, progressbar = FALSE)
shakes_vocab = create_vocabulary(it)

# creamos los vectores de 5 
vectorizer = vocab_vectorizer(shakes_vocab)
shakes_tcm = create_tcm(it, vectorizer, skip_grams_window = 5)


glove <- GlobalVectors$new(x_max = 50, rank = 100)
shakes_wv_main <-  glove$fit_transform(shakes_tcm, n_iter = 100, convergence_tol = 0.0001)
shakes_wv_context = glove$components
shakes_word_vectors = shakes_wv_main + t(shakes_wv_context)

rom <- shakes_word_vectors["macri", , drop = F] 

cos_sim_rom = sim2(x = shakes_word_vectors, y = rom, method = "cosine", norm = "l2")
head(sort(cos_sim_rom[,1], decreasing = T), 10)