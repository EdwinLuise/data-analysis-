library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)

data_Meteo<- dt %>% group_by(STAGIONALITA_CLIENTE) %>% summarise(n=n(), ordineMedio= mean(ORD_TOTALE),somma=sum(ORD_TOTALE)) %>% mutate(perc=n/sum(n) ) %>% 
  filter(STAGIONALITA_CLIENTE!="")
data_Meteo1<- data_Meteo%>% mutate(STAGIONALITA_CLIENTE= recode(STAGIONALITA_CLIENTE,
                                                                "S1",
                                                                "S2",
                                                                "S3",
                                                                 "S4",
                                                     
))

ggplot(data_Meteo1, aes(x=STAGIONALITA_CLIENTE,y=perc, fill=STAGIONALITA_CLIENTE)) + geom_bar(stat="identity") + 
  labs(x="STAGIONALITÀ CLIENTE", y="PERCENTUALE",fill="STAGIONALITA_CLIENTE") + scale_y_continuous(labels=percent) + theme(axis.text.x = element_text(angle=45,hjust=1))
ggplot(data_Meteo1, aes(x=STAGIONALITA_CLIENTE,y=n, fill=STAGIONALITA_CLIENTE)) + geom_bar(stat="identity") + 
  labs(x="STAGIONALITÀ CLIENTE", y="FREQUENZA",fill="STAGIONALITA_CLIENTE") +  theme(axis.text.x = element_text(angle=45,hjust=1))
ggplot(data_Meteo1, aes(x=STAGIONALITA_CLIENTE,y=ordineMedio, fill=STAGIONALITA_CLIENTE,group=1)) +
  geom_line() + geom_point(size=2) +
  labs(x="STAGIONALITÀCLIENTE", y="ORDINE MEDIO",fill="STAGIONALITA_CLIENTE") + theme(axis.text.x = element_text(angle=45,hjust=1))
