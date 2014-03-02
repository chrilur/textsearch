## Skript for å lete gjennom tekst og finne match

# Hent inn kildetekst fra filen text.txt
kildetekst <- scan(file = "text.txt", what = 'character')

# Få kildeteksten inn i én streng.
kildetekst <- paste(kildetekst, sep = " ", collapse = " ")

# Spør etter tekst som skal sjekkes mot kildetekst
inputtekst <- readline("Skriv søketekst: ")

#Sjekker at søketeksten ikke er tom. I så fall, prøv på nytt.
if (inputtekst=="") source('tekst.R') 

# Hvor lange er kildetekst og inputtekst?
lengde_kildetekst <- nchar(kildetekst)
lengde_inputtekst <- nchar(inputtekst)

# Hvor lang er inputtekst i forhold til kildetekst?
input_vs_kilde <- round(100 * lengde_inputtekst / lengde_kildetekst, digits=2)

#Ordanalyse:
#Splitt kildetekst i sine minste bestanddeler. Sett den sammen igjen
#bokstav for bokstav og legg alt i en liste.
atom <- unlist(strsplit(kildetekst, ""))
biter <- list()
lengde <- length(atom)
    for (i in 1:lengde) {
        biter <- c(biter, paste(atom[1:i], collapse=""), paste(atom[i:lengde], collapse=""))
    }
       
#Finnes teksten vi leter etter i listen biter?
#Tell opp alle forekomster der hele inputtekst er i listen.
res <- grepl(inputtekst, biter)
res2 <- length(res[res==TRUE])

#Sjekk om inputtekst faktisk er i kildetekst.
#Hvis res2 >0, finnes inputteksten i kildeteksten.
# input_vs_kilde forteller hvor godt inputteksten matcher kildeteksten.
ja <- 'Teksten du søker etter, finnes i kildeteksten.\n'
nei <- 'Teksten du søker etter, finnes ikke i kildeteksten.\n'
svar <- paste(dQuote(inputtekst),'utgjør',input_vs_kilde,'% av kildeteksten.', collapse="")

if (res2>0) paste(cat(ja,svar)) else cat(nei)
source('tekst.R')