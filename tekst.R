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

# Hvor lang er inputtekst i forhold til kildetekst? Prosentregning
input_vs_kilde <- round(100 * lengde_inputtekst / lengde_kildetekst, digits=2)
   
#Funksjon for ordanalyse.
#Lag en liste av kildetekst der første element er fra første bokstav til lengde_inputtekst.
#Gå deretter gjennom kildetekst ved å flytte en plass mot høyre i hver loop.

    sjekktekst <- function(x) {
        ord <- list()
        diff <- lengde_kildetekst-x+1
        atom <- unlist(strsplit(kildetekst, ""))
        for (i in 1:diff) {
            ord <- c(ord, paste(atom[i:(i+(x-1))], collapse=""))
                }
                return(ord)
        }

#Ordanalyse: Sjekk hvor mange ganger strengen dukker opp i kildeteksten
#Regn ut omfanget søkestrengen har i teksten: Prosent av lengde på kildetekst * antall treff.
ordliste <- sjekktekst(lengde_inputtekst)
treff <- length(which(ordliste == inputtekst))
omfang <- input_vs_kilde * treff

#Tekster til rapport
nei <- 'Teksten du søker etter, finnes ikke i kildeteksten.\n\n'
svar1 <- paste(dQuote(inputtekst),'utgjør',omfang,'% av kildeteksten.\n', collapse="")
svar2 <- paste('Søketeksten finnes', treff, 'steder i kildeteksten.\n\n', collapse="")

#Sjekk om inputtekst faktisk er i kildetekst.
#Hvis treff>0, finnes inputteksten i kildeteksten.

if (treff > 0) paste(cat(svar1,svar2)) else cat(nei)

source('tekst.R')