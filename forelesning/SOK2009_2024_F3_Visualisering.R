#################################################################
# SOK2009_2024_F3_Visualisering
#################################################################

#### Start up #####
rm(list = ls()) # Tommer listen

options(scipen=10) # skriver ut 10 siffer (foran komma)
options(digits=5) # skriver ut 5 desimaler (etter komma...)

library(tidyverse)
library(readr)
library(janitor)
library(mosaic)
library(NHANES)
library(gt)

NHANES <- NHANES

# Kode for a kunne bruke norske bokstaver
Sys.setlocale(locale="no_NO")

# Kaller opp datasettet NHANES
NHANES %>% 
  # Grupperer på den variablen vi er interesert i
  select(SleepHrsNight) %>% 
  # Fjerne alle observasjoner som har NA eller ikke observasjoner
  na.omit() %>%
  # Skriver ut antall timer søvn for de 10 første observasjoene.
  head(10) %>%
  # Denne funksjonen gjor tabellen litt penere
  gt()


#### Frekvenstabell #### 

# Kaller opp datasettet NHANES
NHANES %>% 
  # Grupperer på den variablen vi er interesert i
  select(SleepHrsNight) %>% 
  # Fjerne alle observasjoner som har NA eller ikke observasjoner
  na.omit() %>%
  # grupper etter antall timer
  group_by(SleepHrsNight) %>%
  # Teller opp antallet i hver gruppe
  summarise(frekvens = n()) %>%
  # Gjør plottet penere
  gt()


# Plotter antall timer personer sover

# Bruker datasettet 'NHANES' til å lage en graf
NHANES %>% 
  # Starter et ggplot-objekt med 'SleepHrsNight' som x-akse variabel
  ggplot(aes(x = SleepHrsNight)) +
  # Legger til et stolpediagram (bar plot) for å vise frekvensen av søvntimer
  geom_bar() +
  # Legger til en x-akse label som forklarer hva 'SleepHrsNight' representerer
  xlab("Antall timer søvn") +
  # Legger til en y-akse label som forklarer antallet (frekvensen) av observasjoner
  ylab("Antall")

#### Relativ frekvens #### 

# Kaller opp datasettet NHANES
NHANES %>% 
  # Grupperer på den variablen vi er interesert i
  select(SleepHrsNight) %>% 
  # Fjerne alle observasjoner som har NA eller ikke observasjoner
  na.omit() %>%
  # grupper etter antall timer
  group_by(SleepHrsNight) %>%
  # Teller opp antallet i hver gruppe
  summarise(frekvens = n()) %>%
  # lager en ny variabel som heter relative frekvens
  # round her runder av tallet til to desimaler
  mutate(relative_frekvens = round(  sum( frekvens), 2 )  ) %>%
  # Denne funksjonen gjør tabellen litt penere (vi trenger ikke bruke denne koden)
  gt()


#### Kumulativ frekvens ####

# Kaller opp datasettet NHANES
NHANES %>% 
  # Velger den variabelen vi er interessert i
  select(SleepHrsNight) %>% 
  # Fjerner observasjoner som mangler data
  na.omit() %>%
  # Grupperer data basert på antall timer søvn
  group_by(SleepHrsNight) %>%
  # Teller opp antallet i hver gruppe
  summarise(frekvens = n()) %>%
  # Legger til en kolonne for relativ frekvens, rundet av til to desimaler
  mutate(relative_frekvens = round(frekvens / sum(frekvens), 2)) %>%
  # Beregner den kumulative frekvensen ved hjelp av cumsum-funksjonen
  mutate(kumulative_frekvens = cumsum(frekvens)) %>%
  # Presenterer tabellen på en oversiktlig måte
  gt()

#### Kumulativ relativ frekvens ####

# Kaller opp datasettet NHANES
NHANES %>% 
  # Grupperer på den variablen vi er interesert i
  select(SleepHrsNight) %>% 
  # Fjerne alle observasjoner som har NA eller ikke observasjoner
  na.omit() %>%
  # grupper etter antall timer
  group_by(SleepHrsNight) %>%
  # Teller opp antallet i hver gruppe
  summarise(frekvens = n()) %>%
  # lager en ny variabel som heter relative frekvens
  # round her runder av tallet til to desimaler
  mutate(relative_frekvens = round( frekvens / sum( frekvens), 2 )  ) %>%
  # Her bruker vi coden cumsum for å regne ut den kumulative frekvensen
  mutate(kumulative_frekvens = cumsum(frekvens) ) %>%
  # En siste variable som regner ut kumulative relative frekevns
  mutate(kumulativ_relative_frekvens = round( kumulative_frekvens / sum( frekvens), 2 ) ) %>%
  # Denne funksjonen gjør tabellen litt penere (vi trenger ikke bruke denne koden)
  gt()


#### Frekvenstabell for nominalnivå ####

# Kaller opp datasettet NHANES
NHANES %>% 
  # Grupperer på den variablen vi er interesert i
  select(Race3) %>% 
  # Fjerne alle observasjoner som har NA eller ikke observasjoner
  na.omit() %>%
  # grupper etter antall timer
  group_by(Race3) %>%
  # Teller opp antallet i hver gruppe
  summarise(frekvens = n()) %>%
  # lager en ny variabel som heter relative frekvens
  # round her runder av tallet til to desimaler
  mutate(relative_frekvens = round( frekvens / sum( frekvens), 2 )  ) %>%
  # Denne funksjonen gjør tabellen litt penere (vi trenger ikke bruke denne koden)
  gt()


#### Stolpediagram #### 

# Kaller objektet NHANES
NHANES %>%
  # velger kun sivil status, vi ønsker å fjære NA fra denne variablen. Hvis vi gjennomfører "na.omit()" for hele datasette er det ingen observasjoner igjen. Alle observasjonen har misnt en NA i en variabel.
  select(MaritalStatus) %>%
  # fjerner alle NA i datasettet ( som nå er kun sivilstatus)
  na.omit() %>%
  # Vi lager et ggplot, når vi bruker pipes trenger vi ikke spesifisere datasettet vi bruker
  ggplot(aes(x=MaritalStatus)) +
  # legger til bars
  geom_bar() +
  # Lager en tittle til plottet
  ggtitle("Antall persjoner i forsjellige sivilstatus grupper i NHANES datasettet") +
  # Legger til navn på x-aksen
  xlab("Sivil status") +
  # Legger til navn på y-aksen
  ylab("Frekvens")


#### Deskripitv statistikk tabell ####
# Beregn deskriptiv statistikk for variabelen 'Height' i datasettet 'NHANES'
NHANES %>%
  # Oppsummerer dataen
  summarize(
    # n() gir antall observasjoner i datasettet
    n = sum(!is.na(Height)),
    
    # quantile(Height, na.rm = TRUE)[1] gir minimumsverdien (0% kvantilen)
    min = quantile(Height, na.rm = TRUE)[1], 
    
    # quantile(Height, na.rm = TRUE)[2] gir 1. kvartil (25% kvantilen)
    Q1 = quantile(Height, na.rm = TRUE)[2],
    
    # quantile(Height, na.rm = TRUE)[3] gir medianen (50% kvantilen)
    Q2 = quantile(Height, na.rm = TRUE)[3],
    
    # quantile(Height, na.rm = TRUE)[4] gir 3. kvartil (75% kvantilen)
    Q3 = quantile(Height, na.rm = TRUE)[4],
    
    # quantile(Height, na.rm = TRUE)[5] gir maksimumsverdien (100% kvantilen)
    max = quantile(Height, na.rm = TRUE)[5],
    
    # mean(Height, na.rm = TRUE) beregner gjennomsnittet av 'Height'
    mean = mean(Height, na.rm = TRUE),
    
    # IQR beregnes som forskjellen mellom 3. kvartil (Q3) og 1. kvartil (Q1)
    IQR = quantile(Height, na.rm = TRUE)[4] - 
      quantile(Height, na.rm = TRUE)[2],
    
    # sd(Height, na.rm = TRUE) beregner standardavviket for 'Height'
    sd = sd(Height, na.rm = TRUE)
  ) %>%
  
  # round() runder av resultatene til 2 desimaler for bedre lesbarhet
  round(digits = 2) %>%
  gt()


#### Stople diagram ####

# Kaller objektet NHANES
NHANES %>%
  # velger kun sivil status, vi ønsker å fjære NA fra denne variablen. Hvis vi gjennomfører "na.omit()" for hele datasette er det ingen observasjoner igjen. Alle observasjonen har misnt en NA i en variabel.
  select(MaritalStatus) %>%
  # fjerner alle NA i datasettet ( som nå er kun sivilstatus)
  na.omit() %>%
  # Vi lager et ggplot, når vi bruker pipes trenger vi ikke spesifisere datasettet vi bruker
  ggplot(aes(x=MaritalStatus)) +
  # legger til bars
  geom_bar() +
  # Lager en tittle til plottet
  ggtitle("Antall persjoner i forsjellige sivilstatus grupper i NHANES datasettet") +
  # Legger til navn på x-aksen
  xlab("Sivil status") +
  # Legger til navn på y-aksen
  ylab("Frekvens")






# Kaller objektet NHANES
NHANES %>%
  # velger kun sivil status, vi ønsker å fjære NA fra denne variablen. Hvis vi gjennomfører "na.omit()" for hele datasette er det ingen observasjoner igjen. Alle observasjonen har misnt en NA i en variabel.
  select(MaritalStatus,HealthGen) %>%
  # fjerner alle NA i datasettet ( som nå er kun sivilstatus)
  na.omit() %>%
  # Vi lager et ggplot, når vi bruker pipes trenger vi ikke spesifisere datasettet vi bruker
  ggplot(aes(x=MaritalStatus, fill=HealthGen)) +
  # legger til bars
  geom_bar() +
  # Lager en tittle til plottet
  ggtitle("Antall persjoner i forsjellige sivilstatus grupper i NHANES datasettet") +
  # Legger til navn på x-aksen
  xlab("Sivil status") +
  # Legger til navn på y-aksen
  ylab("Frekvens")


#### Histogramm ####

# Kaller objektet NHANES
NHANES %>%
  # Vi fjerner alle som er under 20  år
  filter(Age>=20) %>%
  # Vi lager et ggplot, når vi bruker pipes trenger vi ikke spesifisere datasettet vi bruker
  ggplot(aes(x=Weight)) +
  # legger til bars
  geom_histogram(bins=20) +
  # Lager en tittle til plottet
  ggtitle("Fordeling av vekt i NHANES datasettet") +
  # Legger til navn på x-aksen
  xlab("Vekt") +
  # Legger til navn på y-aksen
  ylab("Frekvens") +
  # Lager en linje for gjennomsnitts høyde
  geom_vline(
    aes(xintercept = mean(Weight, na.rm = TRUE)),
    color = "red", linetype = "dashed", size =1  ) +
  # Lager en linje for median høyde
  geom_vline(aes(xintercept = median(Weight, na.rm = TRUE)), 
             color = "green", linetype = "dotted", size = 1)


#### Tetthetsplott ####

# Kaller objektet NHANES
NHANES %>%
  # Vi fjerner alle som er under 20  år
  filter(Age>=20) %>%
  # Vi lager et ggplot, når vi bruker pipes trenger vi ikke spesifisere datasettet vi bruker
  ggplot(aes(x=Weight)) +
  # legger til bars
  geom_density(fill = "blue", alpha = 0.5) +
  # Lager en tittle til plottet
  ggtitle("Fordeling av vekt i NHANES datasettet") +
  # Legger til navn på x-aksen
  xlab("Vekt") +
  # Legger til navn på y-aksen
  ylab("Frekvens") +
  # Lager en linje for gjennomsnitts høyde
  geom_vline(
    aes(xintercept = mean(Weight, na.rm = TRUE)),
    color = "red", linetype = "dashed", size =1  ) +
  # Lager en linje for median høyde
  geom_vline(aes(xintercept = median(Weight, na.rm = TRUE)), 
             color = "green", linetype = "dotted", size = 1)


#### Krysstabell ####


# Lager et nytt datasett som inneholder kun den dataen jeg ønsker
NHANES_temp <- NHANES %>%
  # Kun de som er 20 år og eldre
  filter(Age >= 20) %>%
  # selekterer de variablene jeg ønsker å se på
  # Dette er strengktatt ikke nødvendig, men gjør objektene litt mindre
  select(HealthGen, MaritalStatus)

min_tabell <- table(NHANES_temp$HealthGen , NHANES_temp$MaritalStatus)

min_tabell 

#### Mariner og relativ frekvens data  #### 

addmargins(
  prop.table(min_tabell,2)
)  %>% round(digits = 2)

#### Stolpediagram med forskjellige farger ####

# Kaller objektet NHANES
NHANES %>%
  # velger kun sivil status, vi ønsker å fjære NA fra denne variablen. Hvis vi gjennomfører "na.omit()" for hele datasette er det ingen observasjoner igjen. Alle observasjonen har misnt en NA i en variabel.
  select(MaritalStatus,HealthGen) %>%
  # fjerner alle NA i datasettet ( som nå er kun sivilstatus)
  na.omit() %>%
  # Vi lager et ggplot, når vi bruker pipes trenger vi ikke spesifisere datasettet vi bruker
  ggplot(aes(x=MaritalStatus, fill=HealthGen)) +
  # legger til bars
  geom_bar() +
  # Lager en tittle til plottet
  ggtitle("Antall persjoner i forsjellige sivilstatus grupper i NHANES datasettet") +
  # Legger til navn på x-aksen
  xlab("Sivil status") +
  # Legger til navn på y-aksen
  ylab("Frekvens")


#### Stople diagram side ved side ####

# Kaller objektet NHANES
NHANES %>%
  # velger kun sivil status, vi ønsker å fjære NA fra denne variablen. Hvis vi gjennomfører "na.omit()" for hele datasette er det ingen observasjoner igjen. Alle observasjonen har misnt en NA i en variabel.
  select(MaritalStatus,HealthGen) %>%
  # fjerner alle NA i datasettet ( som nå er kun sivilstatus)
  na.omit() %>%
  # Vi lager et ggplot, når vi bruker pipes trenger vi ikke spesifisere datasettet vi bruker
  ggplot(aes(x=MaritalStatus, fill=HealthGen)) +
  # legger til bars
  geom_bar(position = "dodge") +
  # Lager en tittle til plottet
  ggtitle("Antall persjoner i forsjellige sivilstatus grupper i NHANES datasettet") +
  # Legger til navn på x-aksen
  xlab("Sivil status") +
  # Legger til navn på y-aksen
  ylab("Frekvens")

#### Stople diagram frekvens ####

# Kaller objektet NHANES
NHANES %>%
  # velger kun sivil status, vi ønsker å fjære NA fra denne variablen. Hvis vi gjennomfører "na.omit()" for hele datasette er det ingen observasjoner igjen. Alle observasjonen har misnt en NA i en variabel.
  select(MaritalStatus,HealthGen) %>%
  # fjerner alle NA i datasettet ( som nå er kun sivilstatus)
  na.omit() %>%
  # Vi lager et ggplot, når vi bruker pipes trenger vi ikke spesifisere datasettet vi bruker
  ggplot(aes(x=MaritalStatus, fill=HealthGen)) +
  # legger til bars
  geom_bar(position = "fill") +
  # Lager en tittle til plottet
  ggtitle("Antall persjoner i forsjellige sivilstatus grupper i NHANES datasettet") +
  # Legger til navn på x-aksen
  xlab("Sivil status") +
  # Legger til navn på y-aksen
  ylab("Frekvens")





#### Statistikk tabell to variabler ####

NHANES %>%
  # filtrerer vekk de uten heles score
  filter(!is.na(HealthGen)) %>%
  group_by(HealthGen) %>%
  # Oppsummerer dataen
  summarize(
    # n() gir antall observasjoner i datasettet
    n = sum(!is.na(BMI)),
    
    # quantile(BMI, na.rm = TRUE)[1] gir minimumsverdien (0% kvantilen)
    min = quantile(BMI, na.rm = TRUE)[1], 
    
    # quantile(BMI, na.rm = TRUE)[2] gir 1. kvartil (25% kvantilen)
    Q1 = quantile(BMI, na.rm = TRUE)[2],
    
    # quantile(BMI, na.rm = TRUE)[3] gir medianen (50% kvantilen)
    Q2 = quantile(BMI, na.rm = TRUE)[3],
    
    # quantile(BMI, na.rm = TRUE)[4] gir 3. kvartil (75% kvantilen)
    Q3 = quantile(BMI, na.rm = TRUE)[4],
    
    # quantile(BMI, na.rm = TRUE)[5] gir maksimumsverdien (100% kvantilen)
    max = quantile(BMI, na.rm = TRUE)[5],
    
    # mean(BMI, na.rm = TRUE) beregner gjennomsnittet av 'BMI'
    mean = mean(BMI, na.rm = TRUE),
    
    # IQR beregnes som forskjellen mellom 3. kvartil (Q3) og 1. kvartil (Q1)
    IQR = quantile(BMI, na.rm = TRUE)[4] - 
      quantile(BMI, na.rm = TRUE)[2],
    
    # sd(BMI, na.rm = TRUE) beregner standardavviket for 'BMI'
    sd = sd(BMI, na.rm = TRUE)
  )  %>%
  gt()


#### Boxplot ####

# Kaller objektet NHANES
NHANES %>%
  # filtrerer vekk de uten heles score
  filter(!is.na(HealthGen)) %>%
  # Kun de som er 20 år og eldre
  filter(Age >= 20) %>%
  # Vi lager et ggplot, når vi bruker pipes trenger vi ikke spesifisere datasettet vi bruker
  ggplot(aes(x = HealthGen, y=BMI)) +
  # legger til bars
  geom_boxplot() +
  # Lager en tittle til plottet
  ggtitle("Fordeling mellom hoyde og vekt") +
  # Legger til navn på x-aksen
  xlab("Helse karakter") +
  # Legger til navn på y-aksen
  ylab("BMI")


#### Tetthets plot ####

# Kaller objektet NHANES
NHANES %>%
  # filtrerer vekk de uten heles score
  filter(!is.na(HealthGen)) %>%
  # Vi fjerner alle som er under 20  år
  filter(Age>=20) %>%
  # Vi lager et ggplot, når vi bruker pipes trenger vi ikke spesifisere datasettet vi bruker
  ggplot(aes(x=BMI, fill = HealthGen)) +
  # legger til bars
  geom_density( alpha = 0.3) +
  # Lager en tittle til plottet
  ggtitle("Fordeling av BMI i NHANES datasettet") +
  # Legger til navn på x-aksen
  xlab("BMI") +
  # Legger til navn på y-aksen
  ylab("Frekvens") 
  
  
  
#### Scatter plott ####
  
  
# Kaller objektet NHANES
NHANES %>%
    # Vi fjerner alle som er under 20  år
    filter(Age>=20) %>%
    # Vi lager et ggplot, når vi bruker pipes trenger vi ikke spesifisere datasettet vi bruker
    ggplot(aes(x=Height, y = Weight)) +
    # legger til bars
    geom_point()+
    # Lager en tittle til plottet
    ggtitle("Plotter høyde og vekt") +
    # Legger til navn på x-aksen
    xlab("Høyde") +
    # Legger til navn på y-aksen
    ylab("Vekt") 
  

#### Bare fantasien som setter grenser ####

# Kaller objektet NHANES
NHANES %>%
  # Vi fjerner alle som er under 20  år
  filter(Age>=20) %>%
  sample_n(1000, replace = FALSE) %>%
  # Vi lager et ggplot, når vi bruker pipes trenger vi ikke spesifisere datasettet vi bruker
  ggplot(aes(x=Height, y = Weight, colour = HealthGen, shape = Gender)) +
  # legger til bars
  geom_point()+
  # Legger til en lineær regresjonslinje
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  # Lager en tittle til plottet
  ggtitle("Plotter høyde og vekt") +
  # Legger til navn på x-aksen
  xlab("Høyde") +
  # Legger til navn på y-aksen
  ylab("Vekt") 

view(NHANES)

#### Oppgave ####

# Datasett info
# https://cran.r-project.org/web/packages/NHANES/index.html
