import Data.List
import Data.Ord

-- Crea la tupla con (valore_massimo, posizione)
maxi xs = maximumBy (comparing fst) (zip xs [1..])

media x = sum x / (fromIntegral $ length x)

corpo [] _ _ = []
corpo (x:xs) y costante = valore : corpo xs valore costante
    where valore = x * costante + y * (1 - costante)    

ema lista ampiezza = testa : corpo sottostante testa costante
    where testa = media $ take ampiezza lista
          sottostante = drop ampiezza lista
          costante = 2 / fromIntegral (ampiezza + 1)
 
-- Lista contente i vari acquisti in negativo, e vendite in positivo
incrocia (x:[]) _ = []
incrocia _ (x:[]) = []
incrocia (x0:x1:xs) (y0:y1:ys)
    | (x0 < y0) && (x1 > y1) = -x1 : incrocia (x1:xs) (y1:ys)
    | (x0 > y0) && (x1 < y1) = x1 : incrocia (x1:xs) (y1:ys)
    | otherwise = incrocia (x1:xs) (y1:ys)

-- Toglie il positivo, ovvero non si può partire con una vendita
pulisci_testa [] = []
pulisci_testa (x:xs)
    | x > 0     = xs
    | otherwise = (x:xs)
-- Toglie l'ultimo elemento negativo, ovvero non si finisce una lista con un acquisto
pulisci_coda [] = []
pulisci_coda lista
    | x < 0     = init lista
    | otherwise = lista
    where x = last lista
          
-- Tupla che ha come primo valore il fattore moltiplicativo dell'investimento e come secondo la lunghezza, ovvero il numero delle operazioni
moltiplicatore [] (y,z) = (0,0)
moltiplicatore (x:[]) (y,z) = (y,z)
moltiplicatore (x:xs) (y,z)
    | x < 0     = moltiplicatore xs (y / (-x) , z + 1)
    | otherwise = moltiplicatore xs (y *   x  , z + 1)

-- Calcola quanto si guadagna realmente
calcolatore volume (fattore,lunghezza) = (volume * fattore - commissione_fissa * fromIntegral(lunghezza)) * gain

-- Lista contente l'icroncio tra l'ema e i dati reali
risultante lista ampiezza = pulisci_testa $ pulisci_coda $ incrocia (drop (ampiezza - 1) lista) (ema lista ampiezza)

-- Valore finale del calcolo del netto relativo ad una spefica ampiezza su cui calcolare la media mobile
netto volume lista ampiezza = calcolatore volume (moltiplicatore (risultante lista ampiezza) (1,0))

 
tupla_massimale volume lista = maxi $ map (netto volume lista) [1..mole_dati]

risultatoz = tupla_massimale soldi dati
ampiezzaz =  snd $ risultatoz
ultimimovimenti n = take n $ reverse $ incrocia (drop (ampiezzaz - 1) dati) (ema dati ampiezzaz)
 
main = do
    print $ mole_dati
    print $ risultatoz
    print $ ultimimovimenti 3
    
-- Dati realtivi alla banca
gain = 1 - 0.26
commissione_fissa = 2.5

-- Dati di input!
soldi = 500
mole_dati = length dati
dati = reverse datum
datum =  
