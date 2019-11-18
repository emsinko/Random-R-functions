# Import kniznic
library(tidyverse)
library(readxl)
library(stringr)
library(lubridate)
library(glue)

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
      albumin = albumĂ­n,
      Qalb = Qalb,
      OligoIg = `OligoIg (IgG CSF/IgGs)`,
      MRZ = MRZ,
      tau = TAU,
      p_tau = `p-TAU`,
      A_beta_1_42 = `AÎ˛ 1-42`,
      A_beta_1_40 = `AÎ˛ 1-40`,
      A_beta_ratio = `AÎ˛ ratio`,
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

#Premenne, ktore z boxplotov vykazuju na opko rozdielne mediany: "A_beta_1_40", "A_beta-1_42", "A_beta_ratio","laktat","p_tau", "tau"

#####
## Analyza rozdielnosti premennych pre dva druhy demencii

# Urcenie pozorovanej premennej_
vyber_premennej <- "p_tau"

# Historgramy rozdelenia dĂˇt pre danu premennu
data %>% 
  select(typ_demencie,laktat,cb,albumin,Qalb,tau, p_tau, A_beta_1_42, A_beta_1_40, A_beta_ratio) %>%
  gather(key = "premenna", value = "hodnota", -typ_demencie) %>%
  filter(premenna == vyber_premennej) %>%
  ggplot(aes(x = hodnota, fill = typ_demencie)) +
  #geom_histogram(bins = 7, colour = "white") + 
  geom_histogram(colour = "white", binwidth = function(x) 2 * IQR(x) / (length(x)^(1/3))) + # Friedman-Diaconis width
  facet_wrap(premenna ~ typ_demencie, ncol = 2, scale = "free") + 
  theme_bw()

# ZĂˇkladnĂˇ sumĂˇrna tabuÄľka pre vybranĂş premennĂş :

data %>% 
  select(typ_demencie, vyber_premennej) %>%
  group_by(typ_demencie) %>%
  summarise(
    pocet = n(),
    min = min(!!as.symbol(vyber_premennej), na.rm = TRUE),
    Q1 = quantile(!!as.symbol(vyber_premennej), 0.25, na.rm = TRUE),
    mean = mean(!!as.symbol(vyber_premennej), na.rm = TRUE),
    median = median(!!as.symbol(vyber_premennej), na.rm = TRUE),
    Q3 = quantile(!!as.symbol(vyber_premennej), 0.25, na.rm = TRUE),
    max = max(!!as.symbol(vyber_premennej), na.rm = TRUE),
    sd = sd(!!as.symbol(vyber_premennej), na.rm = TRUE),
    var = var(!!as.symbol(vyber_premennej), na.rm = TRUE)
  )

# QQ-plot. Vizualny pohlad na normalitu

data %>%
  select(vyber_premennej, typ_demencie) %>%
  ggplot(aes(sample = !!as.symbol(vyber_premennej))) + 
  stat_qq() + stat_qq_line() + 
  facet_wrap(~typ_demencie) + 
  theme_bw()

# Test normality vypoctovy: Shapiro-test

data %>%
select(vyber_premennej, typ_demencie) %>%
group_by(typ_demencie) %>%
summarise(#p_value = scales::percent(shapiro.test(p_tau)$p.value),
          p_value = shapiro.test(!!as.symbol(vyber_premennej))$p.value,
          test_statistic = shapiro.test(!!as.symbol(vyber_premennej))$statistic) %>%
mutate(result = ifelse(p_value < 0.05, "Zamietame normalitu dat", "Nezamietame normalitu dat"))

# Rucne shapiro: 
#shapiro.test(as.numeric(data[data$typ_demencie == "zmiesana","p_tau"]$p_tau))


# KedĹľe pri jednom zo sĂşborov sme zamietli normalitu dĂˇt, pouĹľijeme neparametrickĂ˝ test. 

# Boxplot 
data %>% 
  select(vyber_premennej, typ_demencie) %>% 
  ggplot(aes(y = !!as.symbol(vyber_premennej), fill = typ_demencie)) +
  geom_boxplot() + 
  theme_bw() +
  ggtitle(glue::glue("Boxploty premennej {vyber_premennej} podÄľa typu demencie"))



# Hypotezy:
# H0: zmiesana demencia mĂˇ menĹˇie rovnĂ© p_tau ako vaskulĂˇrna demencia vs.
# H1: zmiesana demencia mĂˇ vyĹˇĹˇie p_tau ako vaskulĂˇrna demencia

x <- data %>% filter(typ_demencie == "zmiesana") %>% select(vyber_premennej) %>% pull(vyber_premennej)
y <- data %>% filter(typ_demencie == "vaskularna") %>% select(vyber_premennej) %>% pull(vyber_premennej)

wilcox.test(x, y, paired = FALSE, alternative = "greater")

testova_statistika <- wilcox.test(x, y, paired = FALSE, alternative = "greater")$statistic
p_value <- wilcox.test(x, y, paired = FALSE, alternative = "greater")$p.value 

ifelse(p_value < 0.05, "HypotĂ©zu H0 zamietame a prijĂ­mame hypotĂ©zu H1", "HypotĂ©zu H0 nezamietame")
