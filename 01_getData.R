#? Created on 2020-09-28 15:16:20 by Jyri Lillemets with R version 4.0.2 (2020-06-22).
#? This script ...

# Set options
#options(device = 'X11')

# Set working directory
setwd('/home/jrl/work/bioeastsup/horizon')
#setwd('C:/Users/lillemets/Google Drive/')

# Load packages
library('magrittr')
library('pdftools');library('stringr')
library('quanteda')


# Download and tidy documents (2020-09-28 15:20:10) ----------

## Load
if (length(list.files(pattern = '.pdf*')) == 0) {
  Urls <- c('https://ec.europa.eu/research/participants/data/ref/h2020/wp/2014_2015/main/h2020-wp1415-food_en.pdf', 
            'https://ec.europa.eu/research/participants/data/ref/h2020/wp/2016_2017/main/h2020-wp1617-food_en.pdf',
            'https://ec.europa.eu/research/participants/data/ref/h2020/wp/2018-2020/main/h2020-wp1820-food_en.pdf')
  lapply(Urls, function(x) download.file(x, sub('.*\\/','', x)))
}
Docs <- sapply(list.files(pattern = '.pdf*'), pdf_text)

## Remove some pages 
Pages <- list(6:79, 
              c(17:91, 98:122, 126:155, 159:169), 
              c(16:86, 93:120, 125:162, 167:199))
for (i in seq_along(Docs)) Docs[[i]] <- Docs[[i]][Pages[[i]]]

## Collapse pages
Docs %<>% sapply(paste, collapse = '')

## Split by activity
splitPat <- '\n\\s*\\p{Uppercase}{2,4}.*-\\d{1,2}-\\d{4,}.*(.*\n){1,5}\\s*Specific' # Define pattern to split by
str_extract_all(Docs[[3]], splitPat) # Examine
Acts <- str_split(Docs, splitPat) %>% lapply(`[`, -1) # Remove part before 1st activity

## Tidy
Acts <- unlist(Acts)
### Add period as an attribute
attributes(Acts) <- str_extract_all(Docs, splitPat) %>% sapply(length) %>% 
  rep(c("1415", "1617", "1820"), times = .) %>% list(period = .)
### Add document names
names(Acts) <- str_extract_all(Docs, splitPat) %>% unlist %>% 
  gsub('\n', ' ', .) %>% # Replcae newline characters with space
  gsub('\\s+', ' ', .) %>% # Replace spaces with one
  sub('\\d*\n*\\s*Specific$', '', .) %>% # Remove footnote numbers and "Specific"
  gsub('^\\s*|\\s*$', '', .) # Remove leading and trailing spaces
### Add colon to a missing case
names(Acts)[names(Acts) == "BG-13-2014 Ocean literacy – Engaging with society – Social Innovation"] <- "BG-13-2014: Ocean literacy – Engaging with society – Social Innovation"

## Save
saveRDS(Acts, 'texts.Rds')


# Make corpus, tokens, DFM and FCM (2020-09-28 17:38:58) ----------

## Read data
if (!exists('Acts')) Acts <- readRDS('texts.Rds')

## Make corpus
Corp <- corpus(Acts)

## Add docvars
Corp$period <- attr(Acts, 'period')
Corp$call <- sub('-\\d.*', '', names(Acts)) %>% 
  sub('^.*-', '', .)  %>% 
  factor(levels = sort(unique(.)))
Corp$year <- gsub('^.*-\\d{1,2}-|\\:.*$', '', names(Acts)) %>% 
  substring(1,4) %>% # Only first year
  gsub('/', '-', .) %>% factor(levels = sort(unique(.)))

## Tokenize
Toks <- tokens(Acts, remove_punct = T, remove_numbers = TRUE, padding = T) %>% 
  tokens_remove(pattern = stopwords("en"), padding = T) %>% ## Remove stopwords
  tokens_wordstem # Stem words by Martin Porter's stemming algorithm
### Compound multi-word expressions
Colls <- textstat_collocations(Toks, min_count = 10, size = 2, tolower = F) # Collocations
Toks <- tokens_compound(Toks, pattern = Colls[Colls$z > 3]) # 3 ~ p < 0.005
Toks %<>% tokens_remove('') # Remove padding

## Make Document Feature Matrix
dfMat <- dfm(Toks)
#dfMat %<>% dfm_trim(min_termfreq = 10) # Remove features that occur <10 times

## Make Feature Co-occurrence matrix
fcMat <- fcm(dfMat) #%>% fcm_select(pattern = names(topfeatures(fcm, 100)), selection = "keep") # Keep only top 100 features

## Save
save(Corp, Toks, dfMat, fcMat, file = 'corpus.Rda')