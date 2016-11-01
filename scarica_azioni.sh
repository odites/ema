#!/bin/sh

# Scarica da yahoo il csv di una particolare azione e poi la converte nel formato della lista di haskell


codice_azione="PMI.MI"
giorno_inizio="3"
# non si capisce come mai il mese sia un numero in meno rispetto al numero vero
mese_inizio="00"
anno_inizio="2000"
giorno_fine="11"
# non si capisce come mai il mese sia un numero in meno rispetto al numero vero
mese_fine="09"
anno_fine="2016"

# d sta per giornaliero, w settimana, m mese, v solo dividendi
tipo="d"

wget "http://real-chart.finance.yahoo.com/table.csv?s=${codice_azione}&a=${mese_inizio}&b=${giorno_inizio}&c=${anno_inizio}&d=${mese_fine}&e=${giorno_fine}&f=${anno_fine}&g=${tipo}&ignore=.csv"


# Prendo la colonna cinque trasformando la virgola in spazio, poi elimino la prima riga, poi trasformo il carattere di fine linea in virgola, aggiungo un [ all'inizio, aggiungo un ] alla fine
awk -F "\"*,\"*" '{print $5}' "table.csv?s=${codice_azione}&a=${mese_inizio}&b=${giorno_inizio}&c=${anno_inizio}&d=${mese_fine}&e=${giorno_fine}&f=${anno_fine}&g=${tipo}&ignore=.csv" | sed '1d' | sed ':a;N;$!ba;s/\n/,/g' | sed "1s/^/[/" | sed '$s/$/]/' > ${codice_azione}.txt

# Cancello il csv perché è inutile
rm "table.csv?s=${codice_azione}&a=${mese_inizio}&b=${giorno_inizio}&c=${anno_inizio}&d=${mese_fine}&e=${giorno_fine}&f=${anno_fine}&g=${tipo}&ignore=.csv"
