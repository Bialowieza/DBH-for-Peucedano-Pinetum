#load libraries

library("readxl")
library("ggplot2")
library("tidyr")
library("plyr")
library("dplyr")


# tidyr s?uzy do obr?bki danych z wielkich arkuszy do ma?ych


#load data
d1990 <- read_excel("c:/KlimaVeg/Peucedano-Pinetum/Andrzej Keczy?ski dane.xlsx", sheet = "48_90")
d1998 <- read_excel("c:/KlimaVeg/Peucedano-Pinetum/Andrzej Keczy?ski dane.xlsx", sheet = " 48_98")
d2005 <- read_excel("c:/KlimaVeg/Peucedano-Pinetum/Andrzej Keczy?ski dane.xlsx", sheet = "48_2005")
d2016 <- read_excel("c:/KlimaVeg/Peucedano-Pinetum/Andrzej Keczy?ski dane.xlsx", sheet = "48_2016")

names(d1998)[1] <- "DBH"
names(d2005)[1] <- "DBH"
names(d2016)[1] <- "DBH"

d1990$year <- 1990
d1998$year <- 1998
d2005$year <- 2005
d2016$year <- 2016



#combine datasets

dall <- rbind.fill(d1990, d1998, d2005, d2016)
head(dall)


#fat to thin

dall2 <- gather(dall, key = species, value  = number, -year, -DBH) %>% 
  filter(!is.na(number)) %>%
  mutate(DBH = as.numeric(gsub("DBH ", "", DBH))) %>%
  mutate(species = gsub(" 2005", "", species)) %>%
  mutate(species = gsub(" 2016", "", species)) %>%
  mutate(stage = ifelse(grepl("SNAG", species), "snag", "ls")) %>%
  mutate(species = sub(" SNAG", "", species)) %>%
  mutate(species = sub(" LS", "", species)) %>%
  filter(!species %in% c("Frangula alnus", "Populus tremula", "Sorbus aucuparia")) %>%
  mutate(DBHclass = (DBH - 2) %/% 3 * 3 + 2) %>%
  group_by(DBHclass, species, year, stage) %>%
  summarise(number = sum(number))


dall2 %>% as.data.frame()
summary(dall2)


##plot

ggplot(dall2, aes(x = DBHclass, y = number, colour  = species, linetype = stage)) +
  geom_line()+scale_y_log10()+facet_wrap(~ year)

ggplot(dall2, aes(x = DBHclass, y = number, colour  = as.factor(year), linetype = stage)) +
  geom_line() + 
  scale_y_log10() +
  facet_wrap(~ species)

ggplot(dall2, aes(x = DBHclass, y = number, colour  = as.factor(year), linetype = stage)) +
  geom_line() + 
  scale_y_log10() +
  facet_grid(stage~species) +
  theme_bw()




