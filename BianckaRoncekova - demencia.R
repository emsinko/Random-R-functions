# Import kniznic
library(tidyverse)
library(readxl)
library(stringr)
library(lubridate)

# Nacitanie dat
data <- read_excel("Data_demencia.xls")
#View(Data_demencia)

# Struktura dat
glimpse(data)

# Premenovanie stlpcov
data <- 
  data %>%
    rename(
      gender = `F/M`,
      datum_narodenia = geboren,
      vek = alter,
      datum_vysetrenia = LP,
      bunky = zellen,
      laktat = Laktat,
      cb = CB,
      albumin = albumín,
      Qalb = Qalb,
      OligoIg = `OligoIg (IgG CSF/IgGs)`,
      MRZ = MRZ,
      tau = TAU,
      p_tau = `p-TAU`,
      A_beta_1_42 = `Aβ 1-42`,
      A_beta_1_40 = `Aβ 1-40`,
      A_beta_ratio = `Aβ ratio`,
      s_storung = `S-storung`,
      Dg = Dg,
      NSE = NSE,
      s100B = S100B
      )

# Uprava hodnot / 

data <- 
  data %>%
    transmute(
      pohlavie = case_when(gender %in% c("F","w") ~ "muz", 
                            gender %in% c("m","M") ~ "zena",
                         TRUE ~ "??"
                         ),
      vek = floor(interval(data$datum_narodenia,data$datum_vysetrenia) / years(1)),
      laktat,
      cb,
      albumin,
      bunky,
      laktat,
      cb,
      albumin,
      Qalb,
      #OligoIg,
      #MRZ,
      tau = as.numeric(str_extract(tau, pattern = "[0-9]+")),
      p_tau = as.numeric(str_extract(p_tau, pattern = "[0-9]+")),
      A_beta_1_42,
      A_beta_1_40,
      A_beta_ratio,
      #s_storung,
      typ_demencie = ifelse(str_starts(Dg, pattern = '^[Vv]'), "vaskularna", "zmiesana"),
      skore = str_extract(Dg, pattern = '(MMSE|MoCA).[0-9]+')
    )


# Vizualizacia dat

data %>%
  ggplot(aes(y = tau, fill = typ_demencie)) +
  geom_boxplot() + 
  theme_bw()

# Boxploty pre typ demencie
data %>% 
  select(typ_demencie,laktat,cb,albumin,Qalb,tau, p_tau, A_beta_1_42, A_beta_1_40, A_beta_ratio) %>%
  gather(key = "premenna", value = "hodnota", -typ_demencie) %>%
  ggplot(aes(y = hodnota, fill = typ_demencie)) +
  geom_boxplot() + 
  facet_wrap(~premenna, scales = 'free_y') + 
  theme_bw()



#  geom_histogram(binwidth = function(x) 2 * IQR(x) / (length(x)^(1/3)))

vyber_premennej <- "p_tau"

# Historgramy rozdelenia dát pre danu premennu
data %>% 
  select(typ_demencie,laktat,cb,albumin,Qalb,tau, p_tau, A_beta_1_42, A_beta_1_40, A_beta_ratio) %>%
  gather(key = "premenna", value = "hodnota", -typ_demencie) %>%
  filter(premenna == vyber_premennej) %>%
  ggplot(aes(x = hodnota, fill = typ_demencie)) +
  #geom_histogram(bins = 7, colour = "white") + 
  geom_histogram(colour = "white", binwidth = function(x) 2 * IQR(x) / (length(x)^(1/3))) + # Friedman-Diaconis width
  facet_wrap(premenna ~ typ_demencie, ncol = 2, scale = "free") + 
  theme_bw()

# Tabulka pocetnosti pre vybranu premennu :

data %>% 
  select(typ_demencie, vyber_premennej) %>%
  group_by(typ_demencie) %>%
  summarise(
    n = n(),
    mean = mean(p_tau, na.rm = T),
    sd = sd(p_tau, na.rm = T)
  )

# Test normality:



