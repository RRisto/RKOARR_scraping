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
names(rkoarr)=c("Asutuse registrikood", "Asutuse täisnimi", "Maakond, linn/vald, küla",
                "Tänav/maja", "Arhiv. kp", "Lõp.akt.nimi", "Lõpet. nr",
                "Lõpetamise akti kuupäev", "Lõpet.kp", "Arhiivis", "E-mail")
#täpitähed korrektseks
rkoarr=as.data.frame(lapply(rkoarr, function(y) gsub("Ć¼", "ü", y)))
rkoarr=as.data.frame(lapply(rkoarr, function(y) gsub("Ćµ", "õ", y)))
rkoarr=as.data.frame(lapply(rkoarr, function(y) gsub("Ć¤", "ä", y)))
rkoarr=as.data.frame(lapply(rkoarr, function(y) gsub("Ć¶", "ö", y)))
rkoarr=as.data.frame(lapply(rkoarr, function(y) gsub("Ć„", "Ä", y)))
rkoarr=as.data.frame(lapply(rkoarr, function(y) gsub("Ć", "Ü", y)))
rkoarr=as.data.frame(lapply(rkoarr, function(y) gsub("Ü•", "Õ", y)))

#salvestame
write.table(rkoarr, "rkoarr.csv", row.names = F, sep=";")

######Iga asutuse andmete juurde kraapimine

#aadress tab
rkoarr2=rkoarr
rkoarr2$asut_liik=NA
rkoarr2$maakond=NA
rkoarr2$postikood=NA
rkoarr2$telefon=NA
rkoarr2$faks=NA
rkoarr2$tegevuala_kood=NA
rkoarr2$tegevusala=NA

tekst=c()
for (i in 1:nrow(rkoarr)) { #tuleb käsitsi vaadata, kas ehk on juurde tulnud
  kood=rkoarr$`Asutuse registrikood`[i]
  url=paste0("http://register.fin.ee/register/viewasutus.php?regkood=", kood,
             "&strip=aadress")
  html_source =url
  page = read_html(html_source)
  tekst=page %>% 
    html_nodes(".table_system_viewfield") %>%
    html_text()
  
  rkoarr2$asut_liik[i]=tekst[3]
  rkoarr2$maakond[i]=tekst[7]
  rkoarr2$postikood[i]=tekst[10]
  rkoarr2$telefon[i]=tekst[11]
  rkoarr2$faks[i]=tekst[12]
  rkoarr2$tegevuala_kood[i]=tekst[14]
  rkoarr2$tegevusala[i]=tekst[15]
  tekst=c()#uueks loobiks nulli
}

#moodustamine/register tab
rkoarr2$Moodust_akti_nr=NA
rkoarr2$Moodust_akti_kuupäev=NA
rkoarr2$Asutamise_kuupäev=NA
rkoarr2$Põhimääruse_akti_nr=NA
rkoarr2$Põhimääruse_kinnitamise_kuupäev=NA
rkoarr2$Kande_kuupäev=NA
rkoarr2$Ettevõtte_registri_kood=NA
rkoarr2$Kõrgemalseisva_asutuse_number=NA

tekst=c()
for (i in 1:nrow(rkoarr)) { #tuleb käsitsi vaadata, kas ehk on juurde tulnud
  kood=rkoarr$`Asutuse registrikood`[i]
  url=paste0("http://register.fin.ee/register/viewasutus.php?regkood=", kood,
             "&strip=register")
  html_source =url
  page = read_html(html_source)
  tekst=page %>% 
    html_nodes(".table_system_viewfield") %>%
    html_text()
  
  rkoarr2$Moodust_akti_nr[i]=tekst[4]
  rkoarr2$Moodust_akti_kuupäev[i]=tekst[5]
  rkoarr2$Asutamise_kuupäev[i]=tekst[6]
  rkoarr2$Põhimääruse_akti_nr[i]=tekst[7]
  rkoarr2$Põhimääruse_kinnitamise_kuupäev[i]=tekst[8]
  rkoarr2$Kande_kuupäev[i]=tekst[9]
  rkoarr2$Ettevõtte_registri_kood[i]=tekst[10]
  rkoarr2$Kõrgemalseisva_asutuse_number[i]=tekst[11]
  tekst=c()#uueks loobiks nulli
}

##moodustamise aktid ja põhimäärused
rkoarr2$Moodustamisakt=NA
rkoarr2$Põhimääruse_akt=NA

tekst=c()
for (i in 1:nrow(rkoarr)) { #tuleb käsitsi vaadata, kas ehk on juurde tulnud
  kood=rkoarr$`Asutuse registrikood`[i]
  url=paste0("http://register.fin.ee/register/viewasutus.php?regkood=", kood,
             "&strip=register")
  html_source =url
  page = read_html(html_source)
  tekst=page %>% 
    html_nodes("td+ td .table_system_fieldset td") %>%
    html_text()
  
  rkoarr2$Moodustamisakt[i]=tekst[2]
  rkoarr2$Põhimääruse_akt[i]=tekst[4]
  tekst=c()#uueks loobiks nulli
}

#tegevusala tab
rkoarr2$Tegevusala_kirjeldus=NA

tekst=c()
for (i in 1:nrow(rkoarr)) { #tuleb käsitsi vaadata, kas ehk on juurde tulnud
  kood=rkoarr$`Asutuse registrikood`[i]
  url=paste0("http://register.fin.ee/register/viewasutus.php?regkood=", kood,
             "&strip=tegevusala")
  html_source =url
  page = read_html(html_source)
  tekst=page %>% 
    html_nodes("li") %>%
    html_text()
  
  rkoarr2$Tegevusala_kirjeldus[i]=tekst[1]
  tekst=c()#uueks loobiks nulli
}

#teeme täpitähed korda ja sodi välja
rkoarr2=as.data.frame(lapply(rkoarr2, function(y) gsub("\r", "", y)))
rkoarr2=as.data.frame(lapply(rkoarr2, function(y) gsub("\n", "", y)))
rkoarr2=as.data.frame(lapply(rkoarr2, function(y) gsub("Ā", "", y)))
rkoarr2=as.data.frame(lapply(rkoarr2, function(y) gsub("Ć¼", "ü", y)))
rkoarr2=as.data.frame(lapply(rkoarr2, function(y) gsub("Ćµ", "õ", y)))
rkoarr2=as.data.frame(lapply(rkoarr2, function(y) gsub("Ć¤", "ä", y)))
rkoarr2=as.data.frame(lapply(rkoarr2, function(y) gsub("Ć¶", "ö", y)))
rkoarr2=as.data.frame(lapply(rkoarr2, function(y) gsub("Ć„", "Ä", y)))
rkoarr2=as.data.frame(lapply(rkoarr2, function(y) gsub("Ć", "Ü", y)))
rkoarr2=as.data.frame(lapply(rkoarr2, function(y) gsub("Ü•", "Õ", y)))

#salvestame
write.table(rkoarr2, "rkoarr_detailinfo.csv", sep=";", row.names = F)