library(rvest)
library(stringr)
library(tidyverse)
library(tidytext)

# Do not uncomment unless you wish to reread the data from IMDB.
# ct = 100   # top {ct} movies from each year
# for (year in 1954:2017){
#   url <- paste("http://www.imdb.com/search/title?count=",ct,"&release_date=",year,",",year,"&title_type=feature",sep="")
# 
#   webpage <- read_html(url)
#   
#   # maybe clean this up later
#   imdb_data_html <- html_nodes(webpage,'.lister-item')
#   imdb_data <- html_text(imdb_data_html)
#   imdb_data <- str_trim(gsub('\n',"",str_to_lower(html_text(imdb_data_html))))
#   assign(paste("imdb_",year,sep=""),tibble(rank = as.numeric(str_extract(imdb_data,"[0-9]+")),
#                  title = str_trim(gsub("\\([ixv]+\\)","",gsub(paste("\\s+\\(",year,"\\)",sep=""),"",gsub("[0-9]+\\.\\s+","",str_extract(imdb_data,paste(".+\\(",year,"\\)",sep="")))))),
#                  release_year = year,
#                  #rating = str_extract(imdb_data,"not rated|pg-13|pg|nc-17|r|g"),
#                  rating = str_trim(gsub("\\|.+","",gsub(paste("\\(",year,"\\)",sep=""),"",str_extract(imdb_data,paste("\\(",year,"\\).+\\|\\s+[0-9]+\\smin",sep=""))))),
#                  runtime = as.numeric(gsub("\\smin","",str_extract(imdb_data,"[0-9]+\\smin"))),
#                  genres = str_trim(gsub("[0-9]\\.[0-9]|\\|","",str_extract(imdb_data,"\\|[\\s]+[a-z\\,\\s\\-]+[\\s]+[0-9]\\.[0-9]"))),
#                  movie_rating = as.numeric(str_trim(str_extract(str_extract(imdb_data,"\\|[\\s]+[a-z\\,\\s\\-]+[\\s]+[0-9]\\.[0-9]"),"[0-9]\\.[0-9]"))),
#                  metascore = str_extract(str_extract(imdb_data, "[0-9]+\\s+metascore"),"[0-9]+"),
#                  description = str_trim(gsub("\\s+director","",gsub("/10x","",gsub("[0-9]+\\s+metascore","",str_extract(imdb_data, "/10x.+director"))))),
#                  directors = str_trim(gsub("star[s]*","",gsub("\\|","",gsub("director[s]*:","",str_extract(imdb_data,"director[s]*:.+star[s]*"))))),
#                  actors = gsub("star[s]*:","",gsub("\\s+votes","",str_extract(imdb_data,"star[s]*:.+votes"))),
#                  votes = as.numeric(gsub("\\,","",gsub("votes:\\s+","",str_extract(imdb_data,"votes:\\s+[0-9]*[\\,]*[0-9]+")))),
#                  gross_in_millions = as.numeric(gsub("\\$","",gsub("M","",gsub("Gross:\\s+","",str_extract(imdb_data,"Gross:\\s+\\$[0-9\\.]+M")))))
#                  ))
#   
#   full_synopsis <- tibble(link = paste("http://www.imdb.com",
#         str_extract(html_children(imdb_data_html)[grepl("float-left",html_children(imdb_data_html))],
#               "/title/tt[0-9]+/"),
#         "plotsummary?ref_=tt_stry_pl#synopsis",sep=""),
#         synopsis = "",
#         summaries = "")
#   for (i in 1:dim(full_synopsis)[1]){
#     full_synopsis$synopsis[i] <- str_trim(gsub("\\n|Spoilers|Synopsis|See also|The synopsis below may give away important plot points\\.",
#                                "",
#                                str_extract(html_text(read_html(full_synopsis$link[i])),
#                                       "\\s+Spoilers[\\s\\S]+See also")))
#     full_synopsis$summaries[i] <- str_replace(
#       str_replace(str_extract(html_text(read_html(full_synopsis$link[i])),"Summaries[\\s\\S]+See also"),"Summaries[\\s\\S]+Summaries",""),
#       "It looks like we don't have a Synopsis for this title yet.[\\s\\S]+See also","")
#   }
#   assign(paste("full_synopsis_",year,sep=""),full_synopsis)
# }
# 
# # save data
# for (year in 1898:2017){
#   write.csv(get(paste("imdb_",year,sep="")),
#             paste("imdb_",year,".csv",sep=""))
#   write.csv(get(paste("full_synopsis_",year,sep="")),
#             paste("full_synopsis_",year,".csv",sep=""))
# }

# read data
for (year in 1898:2017){
  assign(paste0("imdb_",year,sep=""),
         read_csv(paste0("imdb_",year,".csv",sep="")))
  assign(paste0("full_synopsis_",year,sep=""),
         read_csv(paste0("full_synopsis_",year,".csv",sep="")))
}

imdb_top_100_since_1898 <- imdb_1898
for (year in 1899:2017){
  imdb_top_100_since_1898 <- rbind(imdb_top_100_since_1898, get(paste("imdb_",year,sep="")))
}

imdb_full_synopses <- full_synopsis_1898
for (year in 1899:2017){
  imdb_full_synopses <- rbind(imdb_full_synopses, get(paste("imdb_",year,sep="")))
}

imdb <- unnest_tokens(
  filter(imdb_top_100_since_1898,release_year >= 1920),
  word,
  description) %>%
  inner_join(get_sentiments("nrc"), by = "word")

sent_levs <- unique(get_sentiments("nrc")$sentiment)

imdb %>%
  group_by(title,release_year,sentiment) %>%
  summarize(num_sents = n()) %>%
  filter(sentiment %in% c("positive","negative")) %>%
  group_by(title,release_year) %>%
  summarize(overall = sum(ifelse(sentiment == "positive",num_sents,-num_sents))) %>%
  ggplot(aes(group = release_year)) + geom_boxplot(aes(x = release_year, y = overall))

imdb %>%
  group_by(title,release_year,sentiment) %>%
  summarize(num_sents = n()) %>%
  filter(sentiment %in% c("positive","negative")) %>%
  group_by(title,release_year) %>%
  summarize(overall = sum(ifelse(sentiment == "positive",num_sents,-num_sents))) %>%
  filter(release_year == 2017) %>%
  arrange(desc(abs(overall)))



