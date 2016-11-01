import Data.List
import Data.Ord
 
-- Crea la tupla con (valore_massimo, posizione)
maxi xs = maximumBy (comparing fst) (zip xs [1..])
 
media x = sum x / (fromIntegral $ length x)
 
corpo [] _ _ = []
corpo (x:xs) y costante = valore : corpo xs valore costante
    where valore = x * costante + y * (1 - costante)    
 
ema ampiezza lista = testa : corpo sottostante testa costante
    where testa = media $ take ampiezza lista
          sottostante = drop ampiezza lista
          costante = 2 / fromIntegral (ampiezza + 1)
 
-- Esempi ricorrenti di medie mobili quotidianamente usate
ema20 = ema 20
ema50 = ema 50
ema200 = ema 200
 
 
 
-- Lista contente i vari acquisti in negativo, e vendite in positivo
incrocia (x:[]) _ = []
incrocia _ (x:[]) = []
incrocia grezzo media
    | (x0 <= y0) && (x1 >= y1) = -x1 : incrocia (tail grezzo) (tail media)
    | (x0 >= y0) && (x1 <= y1) = x1 : incrocia (tail grezzo) (tail media)
    | otherwise = incrocia (tail grezzo) (tail media)
    where x0 = grezzo !! 0
          x1 = grezzo !! 1
          y0 = media !! 0
          y1 = media !! 1
 
-- Lista contente l'icroncio tra l'ema e i dati reali
risultante ampiezza lista = incrocia (drop (ampiezza - 1) lista) (ema ampiezza lista)


-- Partiamo comprando le azioni! Quindi con un valore negativo
pulitore (x:xs)
    | x < 0     = (x:xs)
    | otherwise = xs
    
pulizia_risultante ampiezza lista = pulitore $ risultante ampiezza lista


-- Ipot calcola il fattore moltiplicativo del mio investimento
ipot [] = 1
ipot (x:[]) = 1
ipot (x1:x2:xs) = (x2 / x1) * ipot xs


-- Calcola il volume finale dopo tutti gli investimenti
ipotesi volume lista = abs $ volume * ipot lista

-- Versione rifatta di incrocia per ottimizzare gli ipot

incrociaB (x:[]) _ = 1
incrociaB _ (x:[]) = 1
incrociaB grezzo media
    | (x0 <= y0) && (x1 >= y1) = incrociaB (tail grezzo) (tail media) / x1
    | (x0 >= y0) && (x1 <= y1) = incrociaB (tail grezzo) (tail media) * x1
    | otherwise = incrociaB (tail grezzo) (tail media)
    where x0 = grezzo !! 0
          x1 = grezzo !! 1
          y0 = media !! 0
          y1 = media !! 1

          
-- Risultante rifatta
risultanteB ampiezza lista = incrociaB (drop (ampiezza - 1) lista) (ema ampiezza lista)

          
          
          
          
 
-- Sarebbe da capire se fare finta di tenere le commissioni fisse o variabili
commissioni ampiezza lista = commissione_fissa * fromIntegral(length $ risultante ampiezza lista)
 
lordo ampiezza lista = sum $ risultante ampiezza lista
 
netto volume ampiezza lista = (volume * (lordo ampiezza lista) - (commissioni ampiezza lista) ) * gain
-- inverto l'ordine per usarlo con map
netto' volume lista ampiezza = netto volume ampiezza lista
 
 
 
tupla_massimale volume lista = maxi $ map (netto' volume lista) [1..223]
 
profitto_massimale volume lista = fst $ tupla_massimale volume lista
ampiezza_massimale volume lista = snd $ tupla_massimale volume lista
 
 
-- Dati realtivi alla banca
gain = 1 - 0.26
commissione_fissa = 2.5
 
-- Dati di input!
dati = [48.20,47.90,47.70,47.67,47.35,46.47,46.23,45.50,45.45,44.80,44.67,45.14,44.86,43.79,42.46,42.82,41.56,42.50,42.30,42.35,42.94,43.33,43.80,44.48,43.17,43.27,43.30,42.83,43.20,43.40,43.25,43.02,42.80,43.05,43.13,42.83,42.53,42.69,42.60,42.37,43.45,43.45,43.05,43.05,42.95,42.75,42.45,41.63,41.94,41.52,40.50,40.44,39.76,40.97,39.72,39.33,39.09,38.95,38.27,38.45,38.33,38.36,38.62,37.85,38.78,38.26,37.55,35.85,36.03,36.49,37.00,37.25,36.65,35.83,36.19,35.72,36.72,38.59,37.59,37.90,38.26,37.33,36.75,36.91,36.63,36.58,37.44,38.24,38.42,38.80,37.94,37.89,38.54,38.00,38.10,38.18,37.57,37.57,37.85,36.87,35.98,36.72,36.44,37.61,37.81,38.65,39.02,38.04,38.15,38.33,37.71,38.00,37.85,38.56,38.06,39.00,39.32,39.75,38.89,38.70,38.13,38.60,39.17,39.54,39.30,39.03,38.70,37.50,37.00,36.02,36.36,36.31,35.55,36.20,35.39,36.29,35.90,36.50,37.22,36.66,36.53,36.53,36.53,36.78,37.30,37.58,37.80,37.92,37.69,37.37,38.17,37.89,36.81,38.10,37.79,36.68,36.00,35.92,36.10,35.92,35.59,35.29,34.80,34.41,35.25,35.97,34.36,34.66,34.45,31.21,31.09,30.04,28.00,30.29,30.40,32.32,34.00,34.03,32.95,33.00,36.50,36.60,36.31,37.91,37.85,37.45,38.62,37.50,36.20,38.80,35.73,36.95,38.20,40.05,40.51,39.32,40.54,43.15,44.60,44.59,43.67]
soldi = 10000
 
 
main = do
    print $ length dati
    print $ tupla_massimale soldi dati
