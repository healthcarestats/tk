# Konyvtarak beolvasasa
library(mailR)
library(RSQLite)
library(readxl)

### Ujonnan torzskonyvezett keszitmenyek
setwd("~/gergo/tk")

### adatgyujtes - hazai torzskonyvezes
download.file("https://www.ogyei.gov.hu/generalt_listak/tk_lista.csv", "adatok/tk_lista.csv")
adat <- read.csv("adatok/tk_lista.csv", sep=";", fileEncoding="latin1")
adat <- adat[,-ncol(adat)]
colnames(adat) <- c('tkszam', 'nev', 'kiszereles', 'inn', 'atc', 'forgeng', 'kiadhatosag', 'kiad_alcs', 'biztonsag', 'tk_datum', 'veglegminta', 'alaki_hiba', 'termekhiany')
tkszam <- adat$tkszam

### adatgyujtes - europai torzskonyvezes
download.file("https://www.ema.europa.eu/sites/default/files/Medicines_output_european_public_assessment_reports.xlsx", "adatok/epar.xlsx", mode="wb")
adat2 <- as.data.frame(read_excel("adatok/epar.xlsx", skip=8, col_names=T, sheet=1))
adat2 <- adat2[which(adat2$Category == "Human"),]
colnames(adat2) <- gsub(" ", ".", colnames(adat2))
pnumber <- as.character(adat2$Product.number)

# adatbazis letrehozasa
# mydb <- dbConnect(SQLite(), "tkmonitor.sqlite")
# dbWriteTable(mydb, "main", adat)
# dbWriteTable(mydb, "epar", adat2)
# dbListTables(mydb)
###

# check if links are already in the database
conn        <- dbConnect(SQLite(),"tkmonitor.sqlite")
tkszam_upd  <- tkszam[which(!(tkszam %in% dbReadTable(conn, "main", row.names=F)$tkszam))]
pnumber_upd <- pnumber[which(!(pnumber %in% dbReadTable(conn, "epar", row.names=F)$Product.number))]

### European Medicines Agency

db_download <- dbReadTable(conn, "epar", row.names=F)
collect_revisions <- NULL

for (i in levels(as.factor(pnumber)))
{
  uj_revnumber <- adat2$Revision.number[which(adat2$Product.number %in% i)]
  regi_revnumber <- db_download$Revision.number[which(db_download$Product.number %in% i)]
  
  if(is.na(uj_revnumber) | is.na(regi_revnumber)) {next}
  
  if(uj_revnumber > regi_revnumber)
  {
    a <- as.character(adat2$Condition...indication[which(adat2$Product.number %in% i)])
    b <- as.character(db_download$Condition...indication[which(db_download$Product.number %in% i)])
    
    indikacio <- Reduce(setdiff, strsplit(c(a, b), split = ""))
    if(length(indikacio)>0)
    {
      collect_revisions <- rbind(collect_revisions, cbind(as.character(adat2$Medicine.name[which(adat2$Product.number %in% i)]), indikacio, as.character(adat2$URL[which(adat2$Product.number %in% i)])))
    } else {
      indikacio <- "nem indikacios valtozas"
      collect_revisions <- rbind(collect_revisions, cbind(as.character(adat2$Medicine.name[which(adat2$Product.number %in% i)]), indikacio, as.character(adat2$URL[which(adat2$Product.number %in% i)])))
    }
  }
  write.csv(collect_revisions, paste0("uj_reviziok_", Sys.Date(), ".csv"))
}

if(is.null(collect_revisions) == F)
{colnames(collect_revisions) <- c("INN", "indikacios valtozas?", "link")}

db_upload <- adat2[which(adat2$Product.number %in% pnumber_upd),]
dbWriteTable(conn, "epar", db_upload, row.names=FALSE, append=TRUE)
output_data <- adat2[which(adat2$pnumber %in% pnumber_upd),]
write.csv(output_data, paste0("uj_epar_", Sys.Date(), ".csv"))

output_data <- NULL

sender <- "XXX"
recipients <- "XXX"

if(length(pnumber_upd) != 0 & is.null(collect_revisions) == F )
  {
    send.mail(from = sender,
              to = recipients,
              subject = "EMA Törzskönyvezési riport",
              body = paste0("Módosítás történt az EPAR-ok listáján. Csatolva található az újonnan törzskönyvezett termékek és a további megváltozott EPAR-ok listája"),
              smtp = list(host.name = "smtp.gmail.com", user.name = "XXX", port=465, passwd = "XXX", ssl = T),
              authenticate = TRUE, send = TRUE,
              attach.files = c(paste0("uj_epar_", Sys.Date(), ".csv"), paste0("uj_reviziok_", Sys.Date(), ".csv")))

} else if (length(pnumber_upd) == 0 & is.null(collect_revisions) == F )
  {
    send.mail(from = sender,
              to = recipients,
              subject = "EMA Törzskönyvezési riport",
              body = paste0("Módosítás történt az EPAR-ok listáján. Csatolva található az újonnan törzskönyvezett termékek listája."),
              smtp = list(host.name = "smtp.gmail.com", user.name = "XXX", port=465, passwd = "XXX", ssl = T),
              authenticate = TRUE, send = TRUE,
              attach.files = paste0("uj_reviziok_", Sys.Date(), ".csv"))

} else if (length(pnumber_upd) != 0 & is.null(collect_revisions) == T )
  {
    send.mail(from = sender,
              to = recipients,
              subject = "EMA Törzskönyvezési riport",
              body = paste0("Módosítás történt az EPAR-ok listáján. Csatolva talólható a megváltozott EPAR-ok listája."),
              smtp = list(host.name = "smtp.gmail.com", user.name = "XXX", port=465, passwd = "XXX", ssl = T),
              authenticate = TRUE, send = TRUE,
              attach.files = paste0("uj_epar_", Sys.Date(), ".csv"))
} else
  {
    send.mail(from = sender,
              to = recipients,
              subject = "EMA Törzskönyvezési riport",
              body = "Nem történt változás az EMA oldalain.",
              smtp = list(host.name = "smtp.gmail.com", user.name = "XXX", port=465, passwd = "XXX", ssl = T),
              authenticate = TRUE, send = TRUE)
}

### OGYEI torzskonyvezes
if(length(tkszam_upd) != 0)
  {
    db_upload <- adat[which(adat$tkszam %in% tkszam_upd),]
    dbWriteTable(conn, "main", db_upload, row.names=FALSE, append=TRUE)
    output_data <- adat[which(adat$tkszam %in% tkszam_upd),]
    write.csv(output_data, paste0("uj_torzskonyvek_", Sys.Date(), ".csv"))
  
    send.mail(from = sender,
              to = recipients,
              subject = "OGYÉI Törzskönyvezési riport",
              body = "Módosítás történt a törzskönyvezett szerek listáján. Csatolva található az újonnan törzskönyvezett termékek listája.",
              smtp = list(host.name = "smtp.gmail.com", user.name = "XXX", port=465, passwd = "XXX", ssl = T),
              authenticate = TRUE, send = TRUE,
              attach.files = paste0("uj_torzskonyvek_", Sys.Date(), ".csv"))
    }      
