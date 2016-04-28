library(rvest)
#loobime veebist andmed
tulem=c()
for (i in 1:175) { #tuleb käsitsi vaadata, kas ehk on juurde tulnud
  url=paste0("http://register.fin.ee/register/index.php?&asuttyyp=&regname=&tunnus=aruanded&regkoodfrom=&regkoodto=&aadr=&korgkood=&action=searchnow&out=&slimit=3000&report=72&page=",
  i)
  html_source =url
  page = read_html(html_source)
  tekst=page %>% 
    html_nodes(".table_system td") %>%
    html_text()
  tulem=c(tulem, tekst)
  tekst=c()#järgmiseks loobiks nulli ära
}

#puhastame sodi välja
tulem=gsub("\n", "", tulem)
eemalda=c("Asutuse registrikood", "Asutuse tĆ¤isnimi", "Maakond, linn/vald, kĆ¼la",
          "TĆ¤nav/maja", "Arhiv. kp", "LĆµp.akt.nimi", "LĆµpet. nr",
          "LĆµpetamise akti kuupĆ¤ev", "LĆµpet.kp", "Arhiivis", "E-mail")
tulempuhas=tulem[!tulem %in% eemalda]
tulempuhas=gsub("Ā", "", tulempuhas)
library(stringr)
tulempuhas=str_trim(tulempuhas, side = c("both"))

#teeme dataframe'i andmetest
rkoarr=data.frame(NULL)
#loobime väärtused dataframe
for (i in 1:(length(tulempuhas)/11)) {
  for (j in 1:11) {
    rkoarr[i, j]=tulempuhas[(i-1)*11+j]
  }
}
#anname veergudele nimed
names(rkoarr)=c("Asutuse registrikood", "Asutuse täisnimi", "Maakond, linn/vald, kĆ¼la",
                "Tänav/maja", "Arhiv. kp", "Lõp.akt.nimi", "Lõpet. nr",
                "Lõpetamise akti kuupäev", "Lõpet.kp", "Arhiivis", "E-mail")
#täpitähed korrektseks
rkoarr=as.data.frame(lapply(rkoarr, function(y) gsub("Ć¼", "ü", y)))
rkoarr=as.data.frame(lapply(rkoarr, function(y) gsub("Ćµ", "õ", y)))
rkoarr=as.data.frame(lapply(rkoarr, function(y) gsub("Ć¤", "ä", y)))
rkoarr=as.data.frame(lapply(rkoarr, function(y) gsub("Ć¶", "ö", y)))
rkoarr=as.data.frame(lapply(rkoarr, function(y) gsub("Ć„", "Ä", y)))
rkoarr=as.data.frame(lapply(rkoarr, function(y) gsub("Ć„", "Ä", y)))
rkoarr=as.data.frame(lapply(rkoarr, function(y) gsub("Ć", "Ü", y)))
rkoarr=as.data.frame(lapply(rkoarr, function(y) gsub("Ü•", "Õ", y)))

#salvestame
write.table(rkoarr, "rkoarr.csv", row.names = F, sep=";")


