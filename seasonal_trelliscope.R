library(tidyverse)
library(RColorBrewer)
library(trelliscopejs)
library(stringr)

#df <- read_delim("data/raw/2008-2018_eng_river_names.csv", delim = ";") 

#(not run!)
# Prepare data for seasonal charts 
df0 <- df %>% mutate_at(vars(Azot_zahalnyj__mh_per_dm3:Symazyn__mkh_per_dm3f), funs(ifelse(is.na(.), 0, .))) %>% 
  mutate_at(.vars = c('Fitoplankton__tys_klityn_per_dm3', 'Symazyn__mkh_per_dm3f'), as.numeric) %>% 
  mutate_at('Data_sposterezhen', as.Date, "%d.%m.%Y") %>% 
  select(-(1:3), -(5:7)) %>%
  mutate(Year = factor(year(Data_sposterezhen), ordered = T, levels = seq(2017, 2008, -1)),
         Month = month(Data_sposterezhen, label = T)) %>% 
  dplyr::filter(Data_sposterezhen < "2018-01-01") %>% 
  select(1, 2, Year, Month, everything()) %>% 
  gather(key = Parameter, value = Value, -(1:4)) %>% 
  select(-Data_sposterezhen) %>% 
  group_by(Parameter, Rajon_richkovoho_basejnu, Year, Month) %>% 
  summarise(Value = mean(Value))

#==============================================================================
# Get data
df <- readRDS("data/processing/season4trelliscope.Rds")

# Use ggplot2 
ggplot(df, aes(x = Month, y = Value)) +
  geom_area(aes(fill = Year, group = Year), color = "white", alpha = 0.8) +
  facet_wrap(~ Parameter + Rajon_richkovoho_basejnu, nrow = 8, scales = "free_y") +
  scale_color_manual(values = brewer.pal(10, "Set1")) +
  labs(title = "Зміна середньомісячного значення Розчиненого кисню впродовж кожного року",
       x = NULL, y = NULL) +
  theme_classic() +
  theme(
    panel.grid.major.x = element_line(colour="black")
  )

# Use trelliscopejs
qplot(data = df, Month, Value) +
  geom_area(aes(fill = Year, group = Year), color = "white", alpha = 0.8) +
  facet_trelliscope(~ Parameter + Rajon_richkovoho_basejnu, nrow = 4, ncol = 2) +
  theme_bw()
  #labs(x = NULL, y = NULL) 
  #scale_color_manual(values = brewer.pal(10, "Set1")) +
  #theme_classic() +
  #theme(panel.grid.major.x = element_line(colour="black"))

### TEST ZONE ###
aa <- df %>% 
  group_by(Parameter, Rajon_richkovoho_basejnu) %>% 
  nest() 

gg <- aa[49:56,] %>% 
  mutate(panel = map_plot(data, function(x) 
                          ggplot(x, aes(Month, Value)) +
                            geom_area(aes(fill = Year, group = Year), color = "white", alpha = 0.8) +
                            scale_x_discrete(expand = c(0.01, 0.01)) +
                            scale_color_manual(values = brewer.pal(10, "Set1")) +
                            labs(x = NULL, y = 'Mean Value') +
                            theme_classic() +
                            theme(panel.grid.major.x = element_line(colour="black"),
                                  axis.text = element_text(size = 16, colour = "black"),
                                  legend.position = "right",
                                  legend.title = element_text(face = "bold", size = 17),
                                  legend.text = element_text(size = 16),
                                  axis.title = element_text(size = 17, face = "bold"))))

gg %>% 
  trelliscope(name = "Seasonal chart. Changing of the mean value of each water quality parameter by river basin during years 2008-2017", nrow = 2, ncol = 4, width = 1000, height = 750)

##################
### Main plot!####
##################
df %>% 
  group_by(Parameter, Rajon_richkovoho_basejnu) %>% 
  nest() %>%
  mutate(panel = map_plot(data,
                          ~ ggplot(data = .x, aes(x = Month, y = Value)) +
                            geom_area(aes(fill = Year, group = Year), color = "white", alpha = 0.8) +
                            scale_x_discrete(expand = c(0.01, 0.01)) +
                            scale_color_manual(values = brewer.pal(10, "Set1")) +
                            labs(x = NULL, y = 'Mean Value') +
                            theme_classic() +
                            theme(panel.grid.major.x = element_line(colour="black"),
                                  axis.text = element_text(size = 16, colour = "black"),
                                  legend.position = "right",
                                  legend.title = element_text(face = "bold", size = 17),
                                  legend.text = element_text(size = 16),
                                  axis.title = element_text(size = 17, face = "bold")))) %>% 
  trelliscope(name = "Seasonal chart. Changing of the mean value of each water quality parameter by river basin during years 2008-2017", nrow = 2, ncol = 4, width = 1000, height = 750, path = 'displays')
