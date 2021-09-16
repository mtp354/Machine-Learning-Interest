#Tweet scraper
rm(list = ls())
dev.off()
cat("\014")
library(tidytext)
library(rtweet)
library(ggpubr)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)


MP_handles <- c("KiriAllan", "ginnyandersen", "jacindaardern", "bayly_andrew", "Camilla_Belich",
                "DavidBennettMP", "bennett4np", "cjsbishop", "rachelboyack", "simonjbridges",
                "BrookingRachel", "SimeonBrownMP", "GerryBrownleeMP", "NaisiChen",
                "DavidClarkNZ", "tamaticoffey", "JudithCollinsMP", "SimonCourtACT",
                "MaramaDavidson", "NgatiBird", "JacquiDeanMP", "pauleaglenz",
                "BarbEdmondsMana", "KrisinMana", "JulieAnneGenter", "golrizghahraman",
                "PaulGoldsmithMP", "shananhalbert", "PeeniHenare", "HareteHipangoMP",
                "chrishipkins", "WillieJLabour", "AAnahila", "EKerekere", "BarbaraKuriger",
                "IngridLeary", "melissaleemp", "stephwhanganui", "AndrewLittleMP",
                "janlogie", "raising5girls", "MarjaLubeck", "chrisluxonmp", "joluxx",
                "NanaiaMahuta", "SpeakerTrevor", "Kieran_McAnulty",
                "mcdowallact","ianmckelviemp", "traceymclellan", "RMarchNZ",
                "MarkMitchellMP", "JosephMooneyMP", "toddmullerBoP", "Stuart_NashMP",
                "whaeadeb", "NgobiTerisa", "DamienOConnorMP", "GregOhariu", "IbrahimomerNZ",
                "sarah4ilam", "DavidParkerMP", "ChrisPenknz", "WillowPrime", "MaureenPughNat",
                "priyancanzlp", "DrShaneRetiMP", "AS_Roberts", "grantrobertson1",
                "adrianrurawhe", "BeeFaerie", "EugenieSage", "JennySalesa", "CarmelSepuloni",
                "SeverinToni", "dbseymour", "gmsharmanz", "jamespeshaw", "ScottSimpsonMP",
                "AupitoWSio_MP", "stuartsmithmp", "EricaStanfordMP", "jamiestrangenz",
                "_chloeswarbrick", "jantinetti", "RinoTirikatene", "teanau_tuiono", "PhilTwyford",
                "TangiU", "TimvandeMolenMP", "BrookevanVelden", "drayeshaverrall",
                "Rawiri_Waititi", "vanushi_walters", "angewarrenclark", "SimonWattsMP", "Duncan_Webb_",
                "mekawhaitiri", "arenaa", "PotoChchEast", "NicolaWillisMP",
                "michaelwoodnz", "WoodhouseMP", "Megan_Woods")

MP_handles <- paste("@", MP_handles, sep = "") # formatting correctly for rtweet
# want to map between screen_name, actual name and party affiliation
MP_names <- c("Kiritapu Allan", "Ginny Anderson", "Jacinda Ardern", "Andrew Bayly", "Camilla Belich", "David Bennett", "Glen Bennett",
              "Chris Bishop", "Rachel Boyack", "Simon Bridges", "Rachel Brooking", "Simeon Brown", "Gerry Brownlee", "Naisi Chen",
              "David Clark", "Tamati Coffey", "Judith Collins", "Simon Court", "Marama Davidson", "Kelvin Davis", "Jacqui Dean",
              "Paul Eagle", "Barbara Edmonds", "Kris Faafoi", "Julie Anne Genter", "Golriz Ghahraman", "Paul Goldsmith",
              "Shanan Halbert", "Peeni Henare", "Harete Hipango", "Chris Hipkins", "Willie Jackson", "Anahila Kanonggata'a-Suisuiki",
              "Elizabeth Kerekere", "Barbara Kuriger", "Ingrid Leary", "Melissa Lee", "Steph Lewis", "Andrew Little", "Jan Logie",
              "Anna Lorck", "Marja Lubeck", "Chris Luxon", "Jo Luxton", "Nanaia Mahuta", "Trevor Mallard", "Kieran McAnulty",
              "James McDowall", "Ian McKelvie", "Tracey Mclellan", "Ricardo Menendez March", " Mark Mitchell", "Joseph Mooney",
              "Todd Muller", "Stuart Nash", "Debbie Ngarewa-Packer", "Terisa Ngobi", "Damien O'Connor", "Greg O'Connor",
              "Ibrahim Omer", "Sarah Pallett", "David Parker", "Chris Penk", "Willow-Jean Prime", "Mauren Pugh",
              "Priyanca Radhakrishnan", "Shane Reti", "Angela Roberts", "Grant Robertson", "Adrian Rurawhe", "Deborah Russell",
              "Eugenie Sage", "Jenny Salesa", "Carmel Spuloni", "Toni Severin", "David Seymour", "Gaurav Sharma", "James Shaw",
              "Scott Simpson", "Aupito William Sio", "Stuart Smith", "Erica Stanford", "Jamie Strange", "Chlöe Swarbrick",
              "Jan Tinetti", "Rino Tirikatene", "Teanau Tuiono", "Phil Twyford", "Tangi Utikere", "Tim van de Molen", "Brook van Velden",
              "Ayesha Verrall", "Rawiri Waititi", "Vanushi Walters", "Angie Warren-Clark", "Simon Watts", "Duncan Webb",
              "Meka Whaitiri", "Arena Williams", "Poto Williams", "Nicola Willis", "Michael Wood", "Michael Woodhouse", "Megan Woods")

L <- "Labour"
N <- "National"
G <- "Greens"
A <- "Act"
M <- "Maori"

MP_party <- c(L, L, L, N, L, N, L, N, L, N, L, N, N, L, L, L, N, A, G, L, N, L, L, L, G, G, N, L, L, N, L, L, L, G, N, L, N,
              L, L, G, L, L, N, L, L, L, L, A, N, L, G, N, N, N, L, M, L, L, L, L, L, L, N, L, N, L, N, L, L, L, L, G, L, L, 
              A, A, L, G, N, L, N, N, L, G, L, L, G, L, L, N, A, L, M, L, L, N, L, L, L, L, N, L, N, L)



MP_timelines_list <- list() # empty list which will contain DFs of timeline variables for each MP

for (val in MP_handles){
  dummy <- get_timeline(val, n=3200)
  dummy <- dummy %>% filter(is_retweet==F)
  dummy <- dummy %>% select(screen_name, reply_to_screen_name, status_id, reply_to_status_id, text)
  MP_timelines_list <- append(MP_timelines_list, dummy)}

MP_handles <- c("KiriAllan", "ginnyandersen", "jacindaardern", "bayly_andrew", "Camilla_Belich",
                "DavidBennettMP", "bennett4np", "cjsbishop", "rachelboyack", "simonjbridges",
                "BrookingRachel", "SimeonBrownMP", "GerryBrownleeMP", "NaisiChen",
                "DavidClarkNZ", "tamaticoffey", "JudithCollinsMP", "SimonCourtACT",
                "MaramaDavidson", "NgatiBird", "JacquiDeanMP", "pauleaglenz",
                "BarbEdmondsMana", "KrisinMana", "JulieAnneGenter", "golrizghahraman",
                "PaulGoldsmithMP", "shananhalbert", "PeeniHenare", "HareteHipangoMP",
                "chrishipkins", "WillieJLabour", "AAnahila", "EKerekere", "BarbaraKuriger",
                "IngridLeary", "melissaleemp", "stephwhanganui", "AndrewLittleMP",
                "janlogie", "raising5girls", "MarjaLubeck", "chrisluxonmp", "joluxx",
                "NanaiaMahuta", "SpeakerTrevor", "Kieran_McAnulty",
                "mcdowallact","ianmckelviemp", "traceymclellan", "RMarchNZ",
                "MarkMitchellMP", "JosephMooneyMP", "toddmullerBoP", "Stuart_NashMP",
                "whaeadeb", "NgobiTerisa", "DamienOConnorMP", "GregOhariu", "IbrahimomerNZ",
                "sarah4ilam", "DavidParkerMP", "ChrisPenknz", "WillowPrime", "MaureenPughNat",
                "priyancanzlp", "DrShaneRetiMP", "AS_Roberts", "grantrobertson1",
                "adrianrurawhe", "BeeFaerie", "EugenieSage", "JennySalesa", "CarmelSepuloni",
                "SeverinToni", "dbseymour", "gmsharmanz", "jamespeshaw", "ScottSimpsonMP",
                "AupitoWSio_MP", "stuartsmithmp", "EricaStanfordMP", "jamiestrangenz",
                "_chloeswarbrick", "jantinetti", "RinoTirikatene", "teanau_tuiono", "PhilTwyford",
                "TangiU", "TimvandeMolenMP", "BrookevanVelden", "drayeshaverrall",
                "Rawiri_Waititi", "vanushi_walters", "angewarrenclark", "SimonWattsMP", "Duncan_Webb_",
                "mekawhaitiri", "arenaa", "PotoChchEast", "NicolaWillisMP",
                "michaelwoodnz", "WoodhouseMP", "Megan_Woods")
MP_df <- data.frame(MP_names, MP_party, MP_handles)
rm(dummy, val, MP_handles, MP_names, MP_party, L, N, A, G, M)

# cleaning and reformatting MP_timelines_list

i <- seq(from = 0, to = 515, by = 5)
MP_tweet_df <- data.frame(handle=character(), reply_handle= character(), status_id=character(), reply_to_status_id=character(), text=character())

for (val in i){
  handle <- unlist(MP_timelines_list[i+1])
  reply_handle <- unlist(MP_timelines_list[i+2])
  status_id <- unlist(MP_timelines_list[i+3])
  reply_to_status_id <- unlist(MP_timelines_list[i+4])
  text <- unlist(MP_timelines_list[i+5])
  dummy_df <- data.frame(handle, reply_handle, status_id, reply_to_status_id, text)
  names(dummy_df) <- c("handle", "reply_handle", "status_id", "reply_to_status_id", "text")
  MP_tweet_df <- rbind(MP_tweet_df, dummy_df)
}
rm(i, handle, reply_handle, status_id, reply_to_status_id, text, dummy_df, val)


MP_names <- c("Kiritapu Allan", "Ginny Anderson", "Jacinda Ardern", "Andrew Bayly", "Camilla Belich", "David Bennett", "Glen Bennett",
              "Chris Bishop", "Rachel Boyack", "Simon Bridges", "Rachel Brooking", "Simeon Brown", "Gerry Brownlee", "Naisi Chen",
              "David Clark", "Tamati Coffey", "Judith Collins", "Simon Court", "Marama Davidson", "Kelvin Davis", "Jacqui Dean",
              "Paul Eagle", "Barbara Edmonds", "Kris Faafoi", "Julie Anne Genter", "Golriz Ghahraman", "Paul Goldsmith",
              "Shanan Halbert", "Peeni Henare", "Harete Hipango", "Chris Hipkins", "Willie Jackson", "Anahila Kanonggata'a-Suisuiki",
              "Elizabeth Kerekere", "Barbara Kuriger", "Ingrid Leary", "Melissa Lee", "Steph Lewis", "Andrew Little", "Jan Logie",
              "Anna Lorck", "Marja Lubeck", "Chris Luxon", "Jo Luxton", "Nanaia Mahuta", "Trevor Mallard", "Kieran McAnulty",
              "James McDowall", "Ian McKelvie", "Tracey Mclellan", "Ricardo Menendez March", " Mark Mitchell", "Joseph Mooney",
              "Todd Muller", "Stuart Nash", "Debbie Ngarewa-Packer", "Terisa Ngobi", "Damien O'Connor", "Greg O'Connor",
              "Ibrahim Omer", "Sarah Pallett", "David Parker", "Chris Penk", "Willow-Jean Prime", "Mauren Pugh",
              "Priyanca Radhakrishnan", "Shane Reti", "Angela Roberts", "Grant Robertson", "Adrian Rurawhe", "Deborah Russell",
              "Eugenie Sage", "Jenny Salesa", "Carmel Spuloni", "Toni Severin", "David Seymour", "Gaurav Sharma", "James Shaw",
              "Scott Simpson", "Aupito William Sio", "Stuart Smith", "Erica Stanford", "Jamie Strange", "Chlöe Swarbrick",
              "Jan Tinetti", "Rino Tirikatene", "Teanau Tuiono", "Phil Twyford", "Tangi Utikere", "Tim van de Molen", "Brook van Velden",
              "Ayesha Verrall", "Rawiri Waititi", "Vanushi Walters", "Angie Warren-Clark", "Simon Watts", "Duncan Webb",
              "Meka Whaitiri", "Arena Williams", "Poto Williams", "Nicola Willis", "Michael Wood", "Michael Woodhouse", "Megan Woods")
L <- "Labour"
N <- "National"
G <- "Greens"
A <- "Act"
M <- "Maori"

MP_party <- c(L, L, L, N, L, N, L, N, L, N, L, N, N, L, L, L, N, A, G, L, N, L, L, L, G, G, N, L, L, N, L, L, L, G, N, L, N,
              L, L, G, L, L, N, L, L, L, L, A, N, L, G, N, N, N, L, M, L, L, L, L, L, L, N, L, N, L, N, L, L, L, L, G, L, L, 
              A, A, L, G, N, L, N, N, L, G, L, L, G, L, L, N, A, L, M, L, L, N, L, L, L, L, N, L, N, L)
MP_handles <- c("KiriAllan", "ginnyandersen", "jacindaardern", "bayly_andrew", "Camilla_Belich",
                "DavidBennettMP", "bennett4np", "cjsbishop", "rachelboyack", "simonjbridges",
                "BrookingRachel", "SimeonBrownMP", "GerryBrownleeMP", "NaisiChen",
                "DavidClarkNZ", "tamaticoffey", "JudithCollinsMP", "SimonCourtACT",
                "MaramaDavidson", "NgatiBird", "JacquiDeanMP", "pauleaglenz",
                "BarbEdmondsMana", "KrisinMana", "JulieAnneGenter", "golrizghahraman",
                "PaulGoldsmithMP", "shananhalbert", "PeeniHenare", "HareteHipangoMP",
                "chrishipkins", "WillieJLabour", "AAnahila", "EKerekere", "BarbaraKuriger",
                "IngridLeary", "melissaleemp", "stephwhanganui", "AndrewLittleMP",
                "janlogie", "raising5girls", "MarjaLubeck", "chrisluxonmp", "joluxx",
                "NanaiaMahuta", "SpeakerTrevor", "Kieran_McAnulty",
                "mcdowallact","ianmckelviemp", "traceymclellan", "RMarchNZ",
                "MarkMitchellMP", "JosephMooneyMP", "toddmullerBoP", "Stuart_NashMP",
                "whaeadeb", "NgobiTerisa", "DamienOConnorMP", "GregOhariu", "IbrahimomerNZ",
                "sarah4ilam", "DavidParkerMP", "ChrisPenknz", "WillowPrime", "MaureenPughNat",
                "priyancanzlp", "DrShaneRetiMP", "AS_Roberts", "grantrobertson1",
                "adrianrurawhe", "BeeFaerie", "EugenieSage", "JennySalesa", "CarmelSepuloni",
                "SeverinToni", "dbseymour", "gmsharmanz", "jamespeshaw", "ScottSimpsonMP",
                "AupitoWSio_MP", "stuartsmithmp", "EricaStanfordMP", "jamiestrangenz",
                "_chloeswarbrick", "jantinetti", "RinoTirikatene", "teanau_tuiono", "PhilTwyford",
                "TangiU", "TimvandeMolenMP", "BrookevanVelden", "drayeshaverrall",
                "Rawiri_Waititi", "vanushi_walters", "angewarrenclark", "SimonWattsMP", "Duncan_Webb_",
                "mekawhaitiri", "arenaa", "PotoChchEast", "NicolaWillisMP",
                "michaelwoodnz", "WoodhouseMP", "Megan_Woods")
MP_df <- data.frame(MP_names, MP_party, MP_handles)

# adding party affiliation column
labour <- MP_df %>% filter(MP_party == "Labour")
national <- MP_df %>% filter(MP_party == "National")
act <- MP_df %>% filter(MP_party == "Act")
greens <- MP_df %>% filter(MP_party == "Greens")
maori <- MP_df %>% filter(MP_party == "Maori")
rm(A, G, L, M, N, MP_party, MP_timelines_list)

MP_tweet_df <- MP_tweet_df %>% mutate(party = ifelse(handle %in% labour$MP_handles, "Labour", 
                                       ifelse(handle %in% national$MP_handles, "National", 
                                              ifelse(handle %in% act$MP_handles, "Act",
                                                     ifelse(handle %in% greens$MP_handles, "Greens",
                                                            ifelse(handle %in% maori$MP_handles, "Maori","00000"))))))
rm(labour, national, greens, act, maori)

MP_tweet_df <- data.frame(as.character(MP_tweet_df$handle), as.character(MP_tweet_df$reply_handle), MP_tweet_df$status_id, 
                          MP_tweet_df$reply_to_status_id, MP_tweet_df$text, MP_tweet_df$party, stringsAsFactors = F)

i <- seq(1, 104, 1)
for (val in i){
  MP_tweet_df[MP_tweet_df == MP_handles[val]] <- MP_names[val]
}
rm(i, val, MP_handles, MP_names, MP_df)
names(MP_tweet_df) <- c("Name", "Reply_Name", "Status_ID", "Reply_Status_ID", "Text", "Party")


write.csv(MP_tweet_df, "MP_Tweets_Compiled", row.names = F)







