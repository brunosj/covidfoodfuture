# run this script to install/load all necessary packages

p_needed <- c(
  'rtweet',
  'twitteR',
  'tidyverse',
  'tidytext',
  'tm',
  'readxl',
  'writexl',
  'igraph',
  'wordcloud',
  'RColorBrewer',
  'ggthemes',
  'magick',
  'dplyr', 'tidyr', 'gapminder',
                 'ggplot2',  'ggalt',
                 'forcats', 'R.utils', 'png', 
                 'grid', 'ggpubr', 'scales',
                 'bbplot'
			)
packages <- rownames(installed.packages())
p_to_install <- p_needed[!(p_needed %in% packages)]
if (length(p_to_install) > 0) {
  install.packages(p_to_install)
}
lapply(p_needed, require, character.only = TRUE)


######## TWITTER SCRAPPING AND DATA FRAME PREP ######## 

# setup oauth token

## store api keys
api_key <- "O1l6Ht3YmIWBP2be2o0OAhCKj"
api_secret_key <- "yfbkjUjw4cix3PbF12gKYiZAGgRkSt5zIbufxq7Mj5jIrRHQXd"
access_token <- "1245981415784747008-voxU5hUTIogF3xsZbIqpqLXFgF6QMQ"
access_token_secret <- "JgQKXkd7aKcOLYdzFQZe4c3CLL1qCbAh8BiwIhGu7gVez"

## authenticate via web browser
token <- create_token(
  app = "tmg-twitteranalysis",
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_token_secret)

# retrieve tweets from timeline
yistweets <- get_timeline("CovidFoodFuture", n = 3200)
yistweets_true <- yistweets[yistweets$is_retweet==FALSE, ] #remove retweets
sum(yistweets_true$favorite_count)
sum(yistweets_true$retweet_count)

# text analysis 
yistweets_true$text <-  gsub("https\\S*", "", yistweets_true$text)
yistweets_true$text <-  gsub("@\\S*", "", yistweets_true$text) 
yistweets_true$text  <-  gsub("amp", "", yistweets_true$text) 
yistweets_true$text  <-  gsub("[\r\n]", "", yistweets_true$text)
yistweets_true$text  <-  gsub("[[:punct:]]", "", yistweets_true$text)

# create stop words objects
stop_french <- get_stopwords("fr","stopwords-iso")
stop_french <-  stop_french[,1] 

covidwords <- as.data.frame(matrix(c("covid19foodfuture", "covidfoodfuture", "benin", "burkina faso", "rdc", "ethiopia", "kenya", "madagascar", "malawi", "nigeria", "senegal", "south africa", "due"), ncol=1, byrow=TRUE))
colnames(covidwords) <- "word"

tweets_text <- yistweets_true %>%
  select(text) %>%
  unnest_tokens(word, text)
tweets_text <- tweets_text %>%
  anti_join(stop_words) %>% 
  anti_join(stop_french) %>% 
  anti_join(covidwords)

# plotting
sewohcolors <- c("#232e50", "#007991")
tweets_text %>% 
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "CovidFoodFuture most frequent words",
       subtitle = "Stop words removed from the list")


yistweets_true$hashtags <- as.character(yistweets_true$hashtags)
yistweets_true$hashtags <- gsub("c\\(", "", yistweets_true$hashtags)
set.seed(1234)
wordcloud(tweets_text$word, min.freq=15, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35, 
          colors=sewohcolors)



# followers and mentions
followers <- get_followers("CovidFoodFuture")
mentions <- get_mentions()

# tweets using #Covid19FoodFuture (can only retrive up to 6-9 days in the past, i.e we need to scrape data every week)
covid19foodfuture <- search_tweets("#Covid19FoodFuture", n = 5000)
covid19foodfuture_net <- network_data(covid19foodfuture, "retweet,mention,reply")
#write_xlsx(covid19foodfuture,"cff_hastag_202005051852-202005141226.xlsx") this one is written already

# 30-day search (maximum n per search is 100, which is problematic)
covidhastag <- search_30day(
  "#Covid19FoodFuture",
  n = 100,
  fromDate = "202004121300",
  toDate = "202004191300",
  env_name = "research",
  safedir = NULL,
  parse = TRUE,
  token = token
)

# plot time series of usage of #Covid19FoodFuture (only for the last 6-9 days sadly)
cff_ts <- ts_plot(
    covid19foodfuture, by = "day"
)

cff_ts <-  cff_ts +
    theme(legend.title = element_blank()) +
    labs(x = NULL, y = NULL,
         title = "Number of tweets with #Covid19FoodFuture (per day)",
         subtitle = "Tweets posted between 05.05.2020 - 14.05.2020\nTotal number of tweets = 1,924") +
    labs(x = "\nDate", y = "Number of tweets\n") +
    theme_classic() +
    theme(axis.text=element_text(size=9)) +
  theme(plot.title = element_text(size = 12))


# use data retrieved from the twitter dashboard to get # of impressions/engagements
twitteranalytics1 <- read_csv("tweet_activity_metrics_CovidFoodFuture_20200408_20200509_en.csv")
twitteranalytics2 <- read_csv("tweet_activity_metrics_CovidFoodFuture_20200508_20200515_en.csv")
twitterweb <- rbind(twitteranalytics1,twitteranalytics2)

nrow(twitterweb)
sum(twitterweb$impressions)  
sum(twitterweb$engagements)
sum(twitterweb$retweets)
sum(twitterweb$likes)
sum(twitterweb$`engagement rate`)

# there are discrepencies between the twitterweb and yistweets_true datasets (e.g. 889 vs 863 tweets)
# might need to investigate why


######## STRING MATCHING TO ADD VARIABLES ######## 


# add country and participants name
#countries <-  c("Benin", "Burkina Faso", "Congo DRC", "Ethiopia", "Kenya", "Madagascar", "Malawi", "Nigeria", "Senegal", "South Africa")


#countries

df$country <- ifelse(grepl("Benin", df$full_text, ignore.case = T), "Benin", 
              ifelse(grepl("BurkinaFaso|Burkina Faso|Burkina", df$full_text, ignore.case = T), "Burkina Faso", 
              ifelse(grepl("Ghana", df$full_text, ignore.case = T), "Ghana",
              ifelse(grepl("Nigeria", df$full_text, ignore.case = T), "Nigeria", 
              ifelse(grepl("Senegal", df$full_text, ignore.case = T), "Senegal", 
              ifelse(grepl("Congo|DRC|RDC", df$full_text, ignore.case = T), "DRC",
              ifelse(grepl("Malawi", df$full_text, ignore.case = T), "Malawi",
              ifelse(grepl("Madagascar", df$full_text, ignore.case = T), "Madagascar",
              ifelse(grepl("Kenya", df$full_text, ignore.case = T), "Kenya",
              ifelse(grepl("Ethiopia", df$full_text, ignore.case = T), "Ethiopia",
              ifelse(grepl("South Africa|SouthAfrica", df$full_text, ignore.case = T), "South Africa",
                     "Global")))))))))))
              #and africa wide?

#participants

df$participant <- ifelse(grepl("alexander_tmg", df$full_text, ignore.case = T), "alexander_tmg", 
              ifelse(grepl("AmandaNamayi", df$full_text, ignore.case = T), "AmandaNamayi", 
              ifelse(grepl("bruno__sj", df$full_text, ignore.case = T), "bruno__sj", 
              ifelse(grepl("EdoSango", df$full_text, ignore.case = T), "EdoSango", 
              ifelse(grepl("EMvuenga", df$full_text, ignore.case = T), "EMvuenga",
              ifelse(grepl("jes_tmg", df$full_text, ignore.case = T), "jes_tmg", 
              ifelse(grepl("JohnAgboola_", df$full_text, ignore.case = T), "JohnAgboola_", 
              ifelse(grepl("josherbert25", df$full_text, ignore.case = T), "josherbert25",
              ifelse(grepl("kanyemba_nellie", df$full_text, ignore.case = T), "kanyemba_nellie",        
              ifelse(grepl("lousvenja", df$full_text, ignore.case = T), "lousvenja",
              ifelse(grepl("MalalaOnisoa", df$full_text, ignore.case = T), "MalalaOnisoa",
              ifelse(grepl("mayadisraeli", df$full_text, ignore.case = T), "mayadisraeli",
              ifelse(grepl("miss_abimbola", df$full_text, ignore.case = T), "miss_abimbola",
              ifelse(grepl("ndomfuh_1", df$full_text, ignore.case = T), "ndomfuh_1",
              ifelse(grepl("olareeh", df$full_text, ignore.case = T), "olareeh",
              ifelse(grepl("sarahdhaen", df$full_text, ignore.case = T), "sarahdhaen",
              ifelse(grepl("maya_disraeli", df$full_text, ignore.case = T), "mayadisraeli",
              ifelse(grepl("serahKiragu_tmg", df$full_text, ignore.case = T), "serahKiragu_tmg",
              ifelse(grepl("sharonjcheboi", df$full_text, ignore.case = T), "sharonjcheboi",
              ifelse(grepl("wangumwangi", df$full_text, ignore.case = T), "wangumwangi",
              ifelse(grepl("YadeYacine", df$full_text, ignore.case = T), "YadeYacine",
              ifelse(grepl("YaredTesema1", df$full_text, ignore.case = T), "YaredTesema1",
                     "Other")))))))))))))))))))))) 

# add columns for the different relevant categories (affected/targeted groups, region, producer/consumer side etc.)
# here we sometimes need to develop lexicons (group of words) to help us adequately capture the information

# return strings to quickly verify if info is captured
# return binaries for discrete output / visualisation

#target_strings

df$target <- 
              ifelse(grepl("farmer|farmers|farms | farming| producer|producers |peasants|smallholder
                            |smallholders|small-scale | small scale | agriculteur|agriculteurs|
                             producteur|producteurs|paysans|exploitants | petit exploitant|petits exploitants
                             ", df$full_text, ignore.case = T), "farmers",
              ifelse(grepl("consumer|consumers | employees", df$full_text, ignore.case = T), "consumers",
              ifelse(grepl("distributor|distributors |distribution |transport | suppliers |food-suppliers | food-supply", df$full_text, ignore.case = T), "distributors",
                 ifelse(grepl("vulnerable|poor|poorer|poorest", df$full_text, ignore.case = T), "vulnerable",
                     ifelse(grepl("youth|young|younger|students", df$full_text, ignore.case = T), "youth",
              ifelse(grepl("vendor|vendors | food market| food markets| street market |street markets", df$full_text, ignore.case = T), "vendors",
              ifelse(grepl("woman|women|mother|mothers|female", df$full_text, ignore.case = T), "women",
              ifelse(grepl("child|children|kid|kids|pupils |school|schools| youngest", df$full_text, ignore.case = T), "children",
              ifelse(grepl("informal|street|", df$full_text, ignore.case = T), "informal",
                     "other")))))))))

#target_binaries

            df$target_farmers_producers <-  ifelse(grepl("farmer|farmers|producer|producers", df$full_text, ignore.case = T), 1, 0)
            df$target_consumers <-   ifelse(grepl("consumer|consumers", df$full_text, ignore.case = T), 1, 0)
            df$target_distributors <-  ifelse(grepl("distributor|distributors |distribution |transport | markets |suppliers |food-suppliers | food-supply", df$full_text, ignore.case = T), 1, 0)
            df$target_vendors <-  ifelse(grepl("vendor|vendors", df$full_text, ignore.case = T), 1, 0)
            df$target_youth <-  ifelse(grepl("youth|young", df$full_text, ignore.case = T), 1, 0)
            df$target_women <-  ifelse(grepl("woman|women", df$full_text, ignore.case = T), 1, 0)
            df$target_children <-  ifelse(grepl("child|children|kid|kids", df$full_text, ignore.case = T), 1, 0)
            df$target_informal <-  ifelse(grepl("informal|street", df$full_text, ignore.case = T), 1, 0)
            df$target_vulnerable <-  ifelse(grepl("vulnerable|poor|poorer|poorest", df$full_text, ignore.case = T), 1, 0)
            df$private_sector <- ifelse(grepl("private sector", df$full_text, ignore.case = T), 1, 0)

  
# area_strings

df$area <-       
              ifelse(grepl("urban|city|cities|settlements|slums", df$full_text, ignore.case = T), "urban",
              ifelse(grepl("rural", df$full_text, ignore.case = T), "rural",
              ifelse(grepl("urban .* rural | region | county", df$full_text, ignore.case = T), "linkages",
                         "other")))

#area_binaries

              df$area_urban <- ifelse(grepl("urban|city|cities|settlements|slums| metropolitan|capital", df$full_text, ignore.case = T), 1,0)
              df$area_rural <-   ifelse(grepl("rural", df$full_text, ignore.case = T), 1,0)
              df$area_linkages <-    ifelse(grepl("urban .* rural | region | county", df$full_text, ignore.case = T), 1,0)


#production_strings

df$production <-  
                ifelse (grepl("crops|produce|harvest|yields|products", df$full_text, ignore.case = T),"crops",
                ifelse (grepl("staples|grains|rice|millet|wheat |cassava | tubercule | racine | manioc | céréales", df$full_text, ignore.case = T), "staples",
                ifelse (grepl("fruit|fruits|vegetables|légumes", df$full_text, ignore.case = T), "fruit and veg",
                            "other")))    


#production_binaries

              df$produ_crops <- ifelse (grepl("crops|produce|harvest|yields|products", df$full_text, ignore.case = T),1,0)
              df$produ_staples <-  ifelse (grepl("staples|grains|rice|millet|wheat|cassava| tubercule | racine | manioc | céréales", df$full_text, ignore.case = T), 1,0)
              df$produfruitandveg <-  ifelse (grepl("fruit|fruits|vegetables|légumes", df$full_text, ignore.case = T), 1,0)
              


#CONSUMPTION

#consumption_strings

df$consumption <- 
                    ifelse (grepl("access | availability | accès| disponibilité ", df$full_text, ignore.case = T),"access",
                    ifelse (grepl("buying power| buy| income to buy |income to eat| 
                                income to feed| money to buy | money to eat| money to feed 
                                | purchasing power | pouvoir d'achat | pouvoirs d'achat ", df$full_text, ignore.case = T), "buying power",
                    ifelse (grepl("meals", df$full_text, ignore.case = T),"missing meals",
                    ifelse (grepl("price | prices | food cost | food costs |inflation", df$full_text, ignore.case = T),"food prices",
                    ifelse (grepl("diet | dietary | nutrition | immune system | système immunitaire", df$full_text, ignore.case = T),"dietary",
                    ifelse (grepl("middle men | intermediate | intermediaries ", df$full_text, ignore.case = T),"middle men corruption",
                                "other"))))))


#consumption_binaries

              df$consu_buying_power <- ifelse (grepl("buying power| buy| income to buy |income to eat| 
                                income to feed| money to buy | money to eat| money to feed 
                                | purchasing power | pouvoir d'achat | pouvoirs d'achat ", df$full_text, ignore.case = T), 1 ,0)
              df$consu_missing_meals <- ifelse (grepl("missing meals", df$full_text, ignore.case = T),1,0)
              df$consu_access <-  ifelse (grepl("access | availability | accès| disponibilité ", df$full_text, ignore.case = T),1,0)
              df$consu_food_prices <- ifelse (grepl("price | prices | food costs |inflation", df$full_text, ignore.case = T),1,0)
              df$consu_dietary <- ifelse (grepl("diet | dietary | nutrition | immune system | système immunitaire", df$full_text, ignore.case = T),1,0)

           
# separate ENG and FR tweets

#df$language <- ifelse(grepl("é | è | à | ê | ç", df$full_text, ignore.case = T), "French", "English")
#df_FR <- df %>% filter(country %in% c("Benin", "Burkina Faso", "DRC", "Madagascar", "Senegal") | language == "French")

df_FR <- df %>% 
  filter(participant %in% c("josherbert25", "EMvuenga", "mayadisraeli", "MalalaOnisoa", "EdoSango", "ed_sango", "YadeYacine"))

df_ENG <- df %>% 
    anti_join(df_FR)

count(df_FR, participant) # tweets per countries
count(df_ENG, participant)

# filter df by week 
df_FR_w23 <- filter(df_FR, week == "2-3" )

df_ENG_w23 <- filter(df_ENG, week == "2-3" )

######## EXPORT TO EXCEL ######## 

write_xlsx(df_FR_w23,"df_FR_w23.xlsx")
write_xlsx(df_ENG_w23,"df_ENG_w23.xlsx")




df_ENG_w23 <- filter(df_ENG, week == "2-3" )

######## EXPORT TO EXCEL ######## 

#write_xlsx(df_FR_w23,"df_FR_w23.xlsx")
#write_xlsx(df_ENG_w23,"df_ENG_w23.xlsx")

df_ENG_w23 <- filter(df_ENG, week == "2-3" )


# new columns based on Sarah´s text for WHH


#df_ENG CHALLENGES


  #  
  df_ENG$consu_access <-  ifelse (grepl("access ", df_ENG$full_text, ignore.case = T),1,0)

  # post harvest losses
  df_ENG$consu_post_harvest_losses <- ifelse (grepl ("post-harvest losses | post harvest losses |loss|losses |storage |refrigeration
                                      | spoilage | wastage | throw away | food waste ", df_ENG$full_text, ignore.case = T), 1,0)
 #logistics
  df_ENG$consu_logistics <- ifelse (grepl ("logistics | agrologistics | agrilogistics | agro-logistics | agri-logistics |barriers|trade
                                      |supply chains | disrupted chain| mobility | transport |transportation |food trucks | trucks | driver |
                                           drivers |sending food |from farm to market| farm to markets |field to markets | distribution ", df_ENG$full_text, ignore.case = T), 1,0)

  #price hikes
    df_ENG$consu_price_hikes <- ifelse (grepl(" price | prices | food hikes | hiked prices | cost | costs| hike | hikes |
                                      |food cost | food costs |inflation |  fluctuating| fluctuation ", df_ENG$full_text, ignore.case = T),1,0)

  #aggregated sourcing : small scale producers and vendors are at a disadvantage  
    
    df_ENG$aggregated_sourcing  <- ifelse (grepl(" limited offer | vendors ", df_ENG$full_text, ignore.case = T),1,0)
    
    
#df_ENG RESPONSES


    df_ENG$responses_all<- ifelse (grepl(" response | responses | solution | solutions |
    food packages .* vulnerable households | solidarity funds | parcels | relief | NGO | NGOs | aid |monitor | monitoring
      | food banks | digital | app |software | online | platform |website | data | ICT ", df_ENG$full_text, ignore.case = T),1,0)
    
    df_ENG$response_digital <- ifelse (grepl(" digital | app |software | online | platform |website | data | ICT ", df_ENG$full_text, ignore.case = T),1,0)
    
    df_ENG$response_monitoring  <- ifelse (grepl(" monitor | monitoring |data ", df_ENG$full_text, ignore.case = T),1,0)
    
    df_ENG$response_solidarity_funds  <- ifelse (grepl(" food packages .* vulnerable households | solidarity funds | parcels | relief 
                                                       NGO | NGOs | aid ", df_ENG$full_text, ignore.case = T),1,0)
    
    df_ENG$extension_services <- ifelse (grepl(" extension ", df_ENG$full_text, ignore.case = T),1,0)
    
    
    df_ENG$government <-  ifelse (grepl(" laws | legislation ", df_ENG$full_text, ignore.case = T),1,0)
    
  
 
#create a table with responses
    
df_RESPONSES <- filter(df_ENG, responses_all == 1)
    
    

####### EXPORT TO EXCEL ######## 

#write_xlsx(df_FR_w23,"df_FR_w23.xlsx")
#write_xlsx(df_ENG_w23,"df_ENG_w23.xlsx")
#write_xlsx(df_RESPONSES,"df_RESPONSES.xlsx")


      
