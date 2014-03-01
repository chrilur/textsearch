## Skript for å lete gjennom tekst og finne match

# To teksteksempler
tekst <- "Jeg heter Christian og jobber i Bergens Tidende."
tekst2 <- readline("Skriv tekst, minst to bokstaver: ")


#Splitte strengen i sine minste bestanddeler. Sett den sammen igjen
#bokstav for bokstav og legg alt i en liste.
atom <- unlist(strsplit(tekst, ""))
biter <- list()
lengde <- length(atom)
    for (i in 1:lengde) {
        biter <- c(biter, paste(atom[1:i], collapse=""))
    }
    for (i in 1:lengde) {
        biter <- c(biter, paste(atom[i:lengde], collapse=""))
    }
deler <- length(biter)/2
   
#Finnes teksten vi leter etter i liste?
#Tell opp alle forekomster der hele tekst2 er i listen.
res <- grepl(tekst2, biter)
res2 <- length(res[res==TRUE])

##Hvis det er ingen forekomster, skal svare være 0%
## Hvis det er to forekomster, skal svare være 100%
## ifelse(<condition>,ifelse(<condition>,<yes>,<no>),<no>)

resultat <- ifelse(res2 !=2 , ifelse(res2 !=0, deler-res2, 0),deler)

# Regner ut prosentvis hvor godt tekstene stemmer overens.
# deler = 100%
# resultat = ?%

svar <- 100 * resultat / deler

paste(cat(dQuote(tekst2),'samsvarer',svar,'% med teksten.'), collapse="")
