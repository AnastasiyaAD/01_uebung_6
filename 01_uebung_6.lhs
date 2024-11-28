> module Mengen where
> type Fehlermeldung = String
> type MengeAlsZeichenreihe = String
> newtype MT1 e = MT1 [e] 
> data MT2 e = Nichts
>              | VerlaengereUm e (MT2 e)
> newtype MT3 e = MT3 (e -> Bool)

> class Menge m where
>   leereMenge :: m
>   allMenge :: m
>   istMenge :: m -> Bool
>   vereinige :: m -> m -> m
>   schneide :: m -> m -> m
>   zieheab :: m -> m -> m -- Nimm nur die Elemente der ersten Menge, die nicht in der Zweiten vorkommen
>   komplementiere :: m -> m
>   sindGleich :: m -> m -> Bool
>   sindUngleich :: m -> m -> Bool
>   istTeilmenge :: m -> m -> Bool
>   istObermenge :: m -> m -> Bool
>   istEchteTeilmenge :: m -> m -> Bool
>   istEchteObermenge :: m -> m -> Bool
>   sindElementeFremd :: m -> m -> Bool
>   sindQuerUeberlappend :: m -> m -> Bool
>   istKeinGueltigerMengenwert :: Fehlermeldung -> m
>   nichtImplementierbar :: Fehlermeldung -> m
>   zeige :: m -> MengeAlsZeichenreihe
> -------------------------------------------------------------------- A.1 --------------------------------------------------------------
> -- Prototypische Implementierungen (A.1)
>   leereMenge = nichtImplementierbar "leereMenge: Diese Funktion muss in einer Instanz implementiert werden."
>   allMenge = nichtImplementierbar "allMenge: Diese Funktion kann nicht allgemein implementiert werden."
>   vereinige _ _ = nichtImplementierbar "vereinige: Diese Funktion kann nicht allgemein implementiert werden."
>   schneide _ _ = nichtImplementierbar "schneide: Diese Funktion kann nicht allgemein implementiert werden."
>   zieheab _ _ = nichtImplementierbar "zieheab: Diese Funktion kann nicht allgemein implementiert werden."
>   komplementiere = zieheab allMenge
>   istTeilmenge _ _ = error "istTeilmenge: Diese Funktion kann nicht allgemein implementiert werden."
>   sindGleich m1 m2 = istTeilmenge m1 m2 && istTeilmenge m2 m1
>   sindUngleich m1 m2 = not (sindGleich m1 m2)
>   istObermenge m1 m2 = istTeilmenge m2 m1
>   istEchteTeilmenge m1 m2 = istTeilmenge m1 m2 && not (sindGleich m1 m2)
>   istEchteObermenge m1 m2 = istEchteTeilmenge m2 m1 
>   sindElementeFremd m1 = sindGleich leereMenge . schneide m1
>   sindQuerUeberlappend m1 m2 = not (sindElementeFremd m1 m2) && not (istTeilmenge m1 m2) && not (istTeilmenge m2 m1)
>   istKeinGueltigerMengenwert fehlermeldung = error fehlermeldung
>   nichtImplementierbar fehlermeldung = error fehlermeldung
>   zeige _ = error "zeige: Diese Funktion muss in einer Instanz implementiert werden."
> -------------------------------------------------------------------- A.2 --------------------------------------------------------------
> -------------------------------------------------------------------- MT1 --------------------------------------------------------------
> instance Menge (MT1 Char) where
>   leereMenge = leereMengeMT1
>   allMenge = MT1 (['a'..'z'] ++ ['A'..'Z'])
>   istMenge = istMengeMT1
>   vereinige = vereinigeMT1
>   schneide = schneideMT1
>   zieheab = zieheabMT1
>   istTeilmenge = istTeilmengeMT1
>   zeige = zeigeMT1


> instance Menge (MT1 Int) where
>   leereMenge = leereMengeMT1
>   allMenge = MT1 [(-100)..100]
>   istMenge = istMengeMT1
>   vereinige = vereinigeMT1
>   schneide = schneideMT1
>   zieheab = zieheabMT1
>   istTeilmenge = istTeilmengeMT1
>   zeige = zeigeMT1


> leereMengeMT1 :: MT1 e 
> leereMengeMT1 = MT1 []

> istMengeMT1 :: (Eq e) => MT1 e -> Bool
> istMengeMT1 (MT1 []) = True
> istMengeMT1 (MT1 list)  = noDuplicates list 

> vereinigeMT1 :: Eq e => MT1 e -> MT1 e -> MT1 e
> vereinigeMT1 m1 m2
>       | istMengeMT1 m1 && istMengeMT1 m2 = MT1 . nub $ toList m1 ++ toList m2
>       | otherwise                        = fehlermeldung

> schneideMT1 :: Eq e => MT1 e -> MT1 e -> MT1 e
> schneideMT1 m1 m2
>       | istMengeMT1 m1 && istMengeMT1 m2 = MT1 . dup $ toList m1 ++ toList m2
>       | otherwise                        = fehlermeldung

> zieheabMT1 :: Eq e => MT1 e -> MT1 e -> MT1 e
> zieheabMT1 m1 m2
>       | istMengeMT1 m1 && istMengeMT1 m2 = MT1 $ [e | e <- toList m1, e `notElem` toList m2]
>       | otherwise                        = fehlermeldung


> istTeilmengeMT1 :: Eq e => MT1 e -> MT1 e -> Bool
> istTeilmengeMT1 m1 m2
>       | istMengeMT1 m1 && istMengeMT1 m2 = all (`elem` toList m2) (toList m1)
>       | otherwise                        = fehlermeldung

> zeigeMT1 :: Show e => MT1 e -> MengeAlsZeichenreihe
> zeigeMT1 (MT1 elems) = "{" ++ formatElems elems ++ "}"




> -- Fehlermeldung fuer wenn ein oder mehrere Argumente nicht Menge sind.
> fehlermeldung :: a
> fehlermeldung = error "Argument muss Menge sein (keine Duplikate)"
> -- ob es Duplikate einer Liste gibt.
> noDuplicates :: Eq a => [a] -> Bool
> noDuplicates [] = True
> noDuplicates (x:xs) = notElem x xs && noDuplicates xs

> -- Entferne Duplikate einer Liste.
> nub :: Eq a => [a] -> [a]
> nub [] = []
> nub (e:es) = e : (nub $ filter (/= e) es)

> -- Lasse nur Duplikate einer Liste bleiben.
> dup :: Eq a => [a] -> [a]
> dup [] = []
> dup (e:es)
>     | e `elem` es = e : (dup es)
>     | otherwise   = dup es

> toList :: MT1 e -> [e]
> toList (MT1 list) = list

> -- Formatiere Elemente, um sie auszudrucken.
> formatElems :: Show a => [a] -> String
> formatElems []     = ""
> formatElems [e]    = show e
> formatElems (e:es) = show e ++ ", " ++ formatElems es

> -------------------------------------------------------------------- MT2 --------------------------------------------------------------
> instance Menge (MT2 Char) where
>   leereMenge = Nichts
>   allMenge = createMT2 (['a'..'z'] ++ ['A'..'Z'])
>   istMenge = istMengeMT2
>   vereinige = vereinigeMT2
>   schneide = schneideMT2
>   zieheab = zieheabMT2
>   istTeilmenge = istTeilmengeMT2
>   zeige = zeigeMT2


> instance Menge (MT2 Int) where
>   leereMenge = Nichts
>   allMenge = createMT2 [(-100)..100]
>   istMenge = istMengeMT2
>   vereinige = vereinigeMT2
>   schneide = schneideMT2
>   zieheab = zieheabMT2
>   istTeilmenge = istTeilmengeMT2
>   zeige = zeigeMT2



> istMengeMT2 :: (Eq e) => MT2 e -> Bool
> istMengeMT2 Nichts = True
> istMengeMT2 m  = noDuplicates (toListMT2 m) 

> vereinigeMT2 :: Eq e => MT2 e -> MT2 e -> MT2 e
> vereinigeMT2 m1 m2
>       | istMengeMT2 m1 && istMengeMT2 m2 = createMT2 . nub $ toListMT2 m1 ++ toListMT2 m2
>       | otherwise                        = fehlermeldung

> schneideMT2 :: Eq e => MT2 e -> MT2 e -> MT2 e
> schneideMT2 m1 m2
>       | istMengeMT2 m1 && istMengeMT2 m2 = createMT2 . dup $ toListMT2 m1 ++ toListMT2 m2
>       | otherwise                        = fehlermeldung

> zieheabMT2 :: Eq e => MT2 e -> MT2 e -> MT2 e
> zieheabMT2 m1 m2
>       | istMengeMT2 m1 && istMengeMT2 m2 = createMT2 [e | e <- toListMT2 m1, e `notElem` toListMT2 m2]
>       | otherwise                        = fehlermeldung


> istTeilmengeMT2 :: Eq e => MT2 e -> MT2 e -> Bool
> istTeilmengeMT2 m1 m2
>       | istMengeMT2 m1 && istMengeMT2 m2 = all (`elem` toListMT2 m2) (toListMT2 m1)
>       | otherwise                        = fehlermeldung

> zeigeMT2 :: Show e => MT2 e -> MengeAlsZeichenreihe
> zeigeMT2 elems = "{" ++ formatElems (toListMT2 elems) ++ "}"




> createMT2 :: [e] -> MT2 e
> createMT2 [] = Nichts
> createMT2 [x] = VerlaengereUm x Nichts
> createMT2 (x:xs) = VerlaengereUm x (createMT2(xs))


> toListMT2 :: MT2 e -> [e]
> toListMT2 x = reverse (toListMT2' x)

> toListMT2' :: MT2 e -> [e]
> toListMT2' (VerlaengereUm z n) = toListMT2' n ++ [z]
> toListMT2' (Nichts) = []

> main = do
>   putStrLn "------------------------------Char------------------------------"
>   putStrLn ""
>   putStrLn $ "leereMenge: " ++ zeige (leereMenge :: MT1 Char)
>   putStrLn $ "allMenge  : " ++ zeige (allMenge   :: MT1 Char)
>   putStrLn ""
>   putStrLn $ "istMenge         {}: " ++ (show $ istMenge (leereMenge :: MT1 Char))
>   putStrLn $ "istMenge {'a', 'a'}: " ++ (show $ istMenge $ MT1 "aa")
>   putStrLn $ "istMenge {'a', 'b'}: " ++ (show $ istMenge $ MT1 "ab")
>   putStrLn ""
>   putStrLn $ "vereinige    {} {'a'}: " ++ (zeige . vereinige leereMenge $ MT1 "a")
>   putStrLn $ "vereinige {'a'} {'a'}: " ++ (zeige $ vereinige (MT1 "a") (MT1 "a"))
>   putStrLn $ "vereinige {'a'} {'b'}: " ++ (zeige $ vereinige (MT1 "a") (MT1 "b"))
>   putStrLn ""
>   putStrLn $ "schneide         {} {'a'}: " ++ (zeige . schneide leereMenge $ MT1 "a")
>   putStrLn $ "schneide      {'a'} {'a'}: " ++ (zeige $ schneide (MT1 "a") (MT1 "a"))
>   putStrLn $ "schneide {'a'} {'a', 'b'}: " ++ (zeige $ schneide (MT1 "a") (MT1 "ab"))
>   putStrLn ""
>   putStrLn $ "zieheab         {} {'a'}: " ++ (zeige . zieheab leereMenge $ MT1 "a")
>   putStrLn $ "zieheab      {'a'} {'a'}: " ++ (zeige $ zieheab (MT1 "a") (MT1 "a"))
>   putStrLn $ "zieheab {'a', 'b'} {'a'}: " ++ (zeige $ zieheab (MT1 "ab") (MT1 "a"))
>   putStrLn ""
>   putStrLn $ "komplementiere . zieheab allMenge $ {'a'}: " ++ (zeige . komplementiere . zieheab allMenge $ MT1 "a")
>   putStrLn ""
>   putStrLn $ "sindGleich           {'a'} {'a'}: " ++ (show $ sindGleich (MT1  "a") (MT1  "a"))
>   putStrLn $ "sindGleich           {'a'} {'b'}: " ++ (show $ sindGleich (MT1  "a") (MT1  "b"))
>   putStrLn $ "sindGleich {'a', 'b'} {'b', 'a'}: " ++ (show $ sindGleich (MT1 "ab") (MT1 "ba"))
>   putStrLn ""
>   putStrLn $ "sindUngleich           {'a'} {'a'}: " ++ (show $ sindUngleich (MT1  "a") (MT1  "a"))
>   putStrLn $ "sindUngleich           {'a'} {'b'}: " ++ (show $ sindUngleich (MT1  "a") (MT1  "b"))
>   putStrLn $ "sindUngleich {'a', 'b'} {'b', 'a'}: " ++ (show $ sindUngleich (MT1 "ab") (MT1 "ba"))
>   putStrLn ""
>   putStrLn $ "istTeilmenge           {'a'} {'a'}: " ++ (show $ istTeilmenge (MT1  "a") (MT1  "a"))
>   putStrLn $ "istTeilmenge {'a', 'b'} {'b', 'a'}: " ++ (show $ istTeilmenge (MT1 "ab") (MT1 "ba"))
>   putStrLn $ "istTeilmenge      {'a'} {'a', 'b'}: " ++ (show $ istTeilmenge (MT1  "a") (MT1 "ab"))
>   putStrLn $ "istTeilmenge      {'a', 'b'} {'a'}: " ++ (show $ istTeilmenge (MT1 "ab") (MT1  "a"))
>   putStrLn ""
>   putStrLn $ "istEchteTeilmenge           {'a'} {'a'}: " ++ (show $ istEchteTeilmenge (MT1  "a") (MT1  "a"))
>   putStrLn $ "istEchteTeilmenge {'a', 'b'} {'b', 'a'}: " ++ (show $ istEchteTeilmenge (MT1 "ab") (MT1 "ba"))
>   putStrLn $ "istEchteTeilmenge      {'a'} {'a', 'b'}: " ++ (show $ istEchteTeilmenge (MT1  "a") (MT1 "ab"))
>   putStrLn $ "istEchteTeilmenge      {'a', 'b'} {'a'}: " ++ (show $ istEchteTeilmenge (MT1 "ab") (MT1  "a"))
>   putStrLn ""
>   putStrLn $ "istObermenge           {'a'} {'a'}: " ++ (show $ istObermenge (MT1  "a") (MT1  "a"))
>   putStrLn $ "istObermenge {'a', 'b'} {'b', 'a'}: " ++ (show $ istObermenge (MT1 "ab") (MT1 "ba"))
>   putStrLn $ "istObermenge      {'a'} {'a', 'b'}: " ++ (show $ istObermenge (MT1  "a") (MT1 "ab"))
>   putStrLn $ "istObermenge      {'a', 'b'} {'a'}: " ++ (show $ istObermenge (MT1 "ab") (MT1  "a"))
>   putStrLn ""
>   putStrLn $ "istEchteObermenge           {'a'} {'a'}: " ++ (show $ istEchteObermenge (MT1  "a") (MT1  "a"))
>   putStrLn $ "istEchteObermenge {'a', 'b'} {'b', 'a'}: " ++ (show $ istEchteObermenge (MT1 "ab") (MT1 "ba"))
>   putStrLn $ "istEchteObermenge      {'a'} {'a', 'b'}: " ++ (show $ istEchteObermenge (MT1  "a") (MT1 "ab"))
>   putStrLn $ "istEchteObermenge      {'a', 'b'} {'a'}: " ++ (show $ istEchteObermenge (MT1 "ab") (MT1  "a"))
>   putStrLn ""
>   putStrLn $ "sindElementeFremd           {'a'} {'a', 'b'}: " ++ (show $ istEchteObermenge (MT1  "a") (MT1  "ab"))
>   putStrLn $ "sindElementeFremd {'a', 'b', 'c'} {'d', 'e', 'f'}: " ++ (show $ istEchteObermenge (MT1 "abc") (MT1 "def"))
>   putStrLn ""
>   putStrLn $ "sindQuerUeberlappend           {'a'} {'a'}: " ++ (show $ istEchteObermenge (MT1  "a") (MT1  "a"))
>   putStrLn $ "sindQuerUeberlappend {'a', 'b', 'c'} {'b', 'a', 'd'}: " ++ (show $ istEchteObermenge (MT1 "abc") (MT1 "bad"))
>   putStrLn $ "sindQuerUeberlappend      {'a'} {'a', 'b'}: " ++ (show $ istEchteObermenge (MT1  "a") (MT1 "ab"))
>   putStrLn $ "sindQuerUeberlappend      {'a', 'b'} {'a'}: " ++ (show $ istEchteObermenge (MT1 "ab") (MT1  "a"))
>   putStrLn ""
>   putStrLn ""
>   putStrLn "------------------------------Int------------------------------"
>   putStrLn ""
>   putStrLn $ "leereMenge: " ++ zeige (leereMenge :: MT1 Int)
>   putStrLn $ "allMenge  : " ++ zeige (allMenge   :: MT1 Int)
>   putStrLn ""
>   putStrLn $ "istMenge     {}: " ++ (show $ istMenge (leereMenge :: MT1 Int))
>   putStrLn $ "istMenge {1, 1}: " ++ (show $ istMenge $ MT1 [1, 1 :: Int])
>   putStrLn $ "istMenge {1, 2}: " ++ (show $ istMenge $ MT1 [1, 2 :: Int])
>   putStrLn ""
>   putStrLn $ "vereinige  {} {1}: " ++ (zeige . vereinige leereMenge $ MT1 [1 :: Int])
>   putStrLn $ "vereinige {1} {1}: " ++ (zeige $ vereinige (MT1 [1]) (MT1 [1 :: Int]))
>   putStrLn $ "vereinige {1} {2}: " ++ (zeige $ vereinige (MT1 [1]) (MT1 [2 :: Int]))
>   putStrLn ""
>   putStrLn $ "schneide     {} {1}: " ++ (zeige . schneide leereMenge $ MT1 [1 :: Int])
>   putStrLn $ "schneide    {1} {1}: " ++ (zeige $ schneide (MT1 [1]) (MT1    [1 :: Int]))
>   putStrLn $ "schneide {1} {1, 2}: " ++ (zeige $ schneide (MT1 [1]) (MT1 [1, 2 :: Int]))
>   putStrLn ""
>   putStrLn $ "zieheab     {} {1}: " ++ (zeige . zieheab leereMenge $ MT1 [1 :: Int])
>   putStrLn $ "zieheab    {1} {1}: " ++ (zeige $ zieheab (MT1    [1]) (MT1 [1 :: Int]))
>   putStrLn $ "zieheab {1, 2} {1}: " ++ (zeige $ zieheab (MT1 [1, 2]) (MT1 [1 :: Int]))
>   putStrLn ""
>   putStrLn $ "komplementiere . zieheab allMenge $ {1}: " ++ (zeige . komplementiere . zieheab allMenge $ MT1 [1 :: Int])
>   putStrLn ""
>   putStrLn $ "sindGleich       {1} {1}: " ++ (show $ sindGleich (MT1    [1]) (MT1    [1 :: Int]))
>   putStrLn $ "sindGleich       {1} {2}: " ++ (show $ sindGleich (MT1    [1]) (MT1    [2 :: Int]))
>   putStrLn $ "sindGleich {1, 2} {2, 1}: " ++ (show $ sindGleich (MT1 [1, 2]) (MT1 [2, 1 :: Int]))
>   putStrLn ""
>   putStrLn $ "sindUngleich       {1} {1}: " ++ (show $ sindUngleich (MT1    [1]) (MT1    [1 :: Int]))
>   putStrLn $ "sindUngleich       {1} {2}: " ++ (show $ sindUngleich (MT1    [1]) (MT1    [2 :: Int]))
>   putStrLn $ "sindUngleich {1, 2} {2, 1}: " ++ (show $ sindUngleich (MT1 [1, 2]) (MT1 [2, 1 :: Int]))
>   putStrLn ""
>   putStrLn $ "istTeilmenge       {1} {1}: " ++ (show $ istTeilmenge (MT1    [1]) (MT1    [1 :: Int]))
>   putStrLn $ "istTeilmenge {1, 2} {2, 1}: " ++ (show $ istTeilmenge (MT1 [1, 2]) (MT1 [2, 1 :: Int]))
>   putStrLn $ "istTeilmenge    {1} {1, 2}: " ++ (show $ istTeilmenge (MT1    [1]) (MT1 [1, 2 :: Int]))
>   putStrLn $ "istTeilmenge    {1, 2} {1}: " ++ (show $ istTeilmenge (MT1 [1, 2]) (MT1    [1 :: Int]))
>   putStrLn ""
>   putStrLn $ "istEchteTeilmenge       {1} {1}: " ++ (show $ istEchteTeilmenge (MT1    [1]) (MT1    [1 :: Int]))
>   putStrLn $ "istEchteTeilmenge {1, 2} {2, 1}: " ++ (show $ istEchteTeilmenge (MT1 [1, 2]) (MT1 [2, 1 :: Int]))
>   putStrLn $ "istEchteTeilmenge    {1} {1, 2}: " ++ (show $ istEchteTeilmenge (MT1    [1]) (MT1 [1, 2 :: Int]))
>   putStrLn $ "istEchteTeilmenge    {1, 2} {1}: " ++ (show $ istEchteTeilmenge (MT1 [1, 2]) (MT1    [1 :: Int]))
>   putStrLn ""
>   putStrLn $ "istObermenge       {1} {1}: " ++ (show $ istObermenge (MT1    [1]) (MT1    [1 :: Int]))
>   putStrLn $ "istObermenge {1, 2} {2, 1}: " ++ (show $ istObermenge (MT1 [1, 2]) (MT1 [2, 1 :: Int]))
>   putStrLn $ "istObermenge    {1} {1, 2}: " ++ (show $ istObermenge (MT1    [1]) (MT1 [1, 2 :: Int]))
>   putStrLn $ "istObermenge    {1, 2} {1}: " ++ (show $ istObermenge (MT1 [1, 2]) (MT1    [1 :: Int]))
>   putStrLn ""
>   putStrLn $ "istEchteObermenge       {1} {1}: " ++ (show $ istEchteObermenge (MT1    [1]) (MT1    [1 :: Int]))
>   putStrLn $ "istEchteObermenge {1, 2} {2, 1}: " ++ (show $ istEchteObermenge (MT1 [1, 2]) (MT1 [2, 1 :: Int]))
>   putStrLn $ "istEchteObermenge    {1} {1, 2}: " ++ (show $ istEchteObermenge (MT1    [1]) (MT1 [1, 2 :: Int]))
>   putStrLn $ "istEchteObermenge    {1, 2} {1}: " ++ (show $ istEchteObermenge (MT1 [1, 2]) (MT1    [1 :: Int]))
>   putStrLn ""
>   putStrLn $ "sindElementeFremd       {1} {1, 2}: " ++ (show $ sindElementeFremd (MT1    [1]) (MT1    [1, 2 :: Int]))
>   putStrLn $ "sindElementeFremd {1, 2, 3} {4, 5, 6}: " ++ (show $ sindElementeFremd (MT1 [1, 2, 3]) (MT1 [4, 5, 6 :: Int]))
>   putStrLn ""
>   putStrLn $ "sindQuerUeberlappend       {1} {1}: " ++ (show $ sindQuerUeberlappend (MT1    [1]) (MT1    [1 :: Int]))
>   putStrLn $ "sindQuerUeberlappend {1, 2, 3} {2, 1, 4}: " ++ (show $ sindQuerUeberlappend (MT1 [1, 2, 3]) (MT1 [2, 1, 4 :: Int]))
>   putStrLn $ "sindQuerUeberlappend    {1} {1, 2}: " ++ (show $ sindQuerUeberlappend (MT1    [1]) (MT1 [1, 2 :: Int]))
>   putStrLn $ "sindQuerUeberlappend    {1, 2} {1}: " ++ (show $ sindQuerUeberlappend (MT1 [1, 2]) (MT1    [1 :: Int]))
>   putStrLn ""
>   putStrLn ""
>   putStrLn "------------------------------MT2------------------------------"
>   putStrLn ""
>   putStrLn ""
>   putStrLn "------------------------------Char------------------------------"
>   putStrLn ""
>   putStrLn $ "leereMenge: " ++ zeige (leereMenge :: MT2 Char)
>   putStrLn $ "allMenge  : " ++ zeige (allMenge   :: MT2 Char)
>   putStrLn ""
>   putStrLn $ "istMenge         {}: " ++ (show $ istMenge (leereMenge :: MT2 Char))
>   putStrLn $ "istMenge {'a', 'a'}: " ++ (show $ istMenge $ createMT2 "aa")
>   putStrLn $ "istMenge {'a', 'b'}: " ++ (show $ istMenge $ createMT2 "ab")
>   putStrLn ""
>   putStrLn $ "vereinige    {} {'a'}: " ++ (zeige . vereinige leereMenge $ createMT2 "a")
>   putStrLn $ "vereinige {'a'} {'a'}: " ++ (zeige $ vereinige (createMT2 "a") (createMT2 "a"))
>   putStrLn $ "vereinige {'a'} {'b'}: " ++ (zeige $ vereinige (createMT2 "a") (createMT2 "b"))
>   putStrLn ""
>   putStrLn $ "schneide         {} {'a'}: " ++ (zeige . schneide leereMenge $ createMT2 "a")
>   putStrLn $ "schneide      {'a'} {'a'}: " ++ (zeige $ schneide (createMT2 "a") (createMT2 "a"))
>   putStrLn $ "schneide {'a'} {'a', 'b'}: " ++ (zeige $ schneide (createMT2 "a") (createMT2 "ab"))
>   putStrLn ""
>   putStrLn $ "zieheab         {} {'a'}: " ++ (zeige . zieheab leereMenge $ createMT2 "a")
>   putStrLn $ "zieheab      {'a'} {'a'}: " ++ (zeige $ zieheab (createMT2 "a") (createMT2 "a"))
>   putStrLn $ "zieheab {'a', 'b'} {'a'}: " ++ (zeige $ zieheab (createMT2 "ab") (createMT2 "a"))
>   putStrLn ""
>   putStrLn $ "komplementiere . zieheab allMenge $ {'a'}: " ++ (zeige . komplementiere . zieheab allMenge $ createMT2 "a")
>   putStrLn ""
>   putStrLn $ "sindGleich           {'a'} {'a'}: " ++ (show $ sindGleich (createMT2  "a") (createMT2  "a"))
>   putStrLn $ "sindGleich           {'a'} {'b'}: " ++ (show $ sindGleich (createMT2  "a") (createMT2  "b"))
>   putStrLn $ "sindGleich {'a', 'b'} {'b', 'a'}: " ++ (show $ sindGleich (createMT2 "ab") (createMT2 "ba"))
>   putStrLn ""
>   putStrLn $ "sindUngleich           {'a'} {'a'}: " ++ (show $ sindUngleich (createMT2  "a") (createMT2  "a"))
>   putStrLn $ "sindUngleich           {'a'} {'b'}: " ++ (show $ sindUngleich (createMT2  "a") (createMT2  "b"))
>   putStrLn $ "sindUngleich {'a', 'b'} {'b', 'a'}: " ++ (show $ sindUngleich (createMT2 "ab") (createMT2 "ba"))
>   putStrLn ""
>   putStrLn $ "istTeilmenge           {'a'} {'a'}: " ++ (show $ istTeilmenge (createMT2  "a") (createMT2  "a"))
>   putStrLn $ "istTeilmenge {'a', 'b'} {'b', 'a'}: " ++ (show $ istTeilmenge (createMT2 "ab") (createMT2 "ba"))
>   putStrLn $ "istTeilmenge      {'a'} {'a', 'b'}: " ++ (show $ istTeilmenge (createMT2  "a") (createMT2 "ab"))
>   putStrLn $ "istTeilmenge      {'a', 'b'} {'a'}: " ++ (show $ istTeilmenge (createMT2 "ab") (createMT2  "a"))
>   putStrLn ""
>   putStrLn $ "istEchteTeilmenge           {'a'} {'a'}: " ++ (show $ istEchteTeilmenge (createMT2  "a") (createMT2  "a"))
>   putStrLn $ "istEchteTeilmenge {'a', 'b'} {'b', 'a'}: " ++ (show $ istEchteTeilmenge (createMT2 "ab") (createMT2 "ba"))
>   putStrLn $ "istEchteTeilmenge      {'a'} {'a', 'b'}: " ++ (show $ istEchteTeilmenge (createMT2  "a") (createMT2 "ab"))
>   putStrLn $ "istEchteTeilmenge      {'a', 'b'} {'a'}: " ++ (show $ istEchteTeilmenge (createMT2 "ab") (createMT2  "a"))
>   putStrLn ""
>   putStrLn $ "istObermenge           {'a'} {'a'}: " ++ (show $ istObermenge (createMT2  "a") (createMT2  "a"))
>   putStrLn $ "istObermenge {'a', 'b'} {'b', 'a'}: " ++ (show $ istObermenge (createMT2 "ab") (createMT2 "ba"))
>   putStrLn $ "istObermenge      {'a'} {'a', 'b'}: " ++ (show $ istObermenge (createMT2  "a") (createMT2 "ab"))
>   putStrLn $ "istObermenge      {'a', 'b'} {'a'}: " ++ (show $ istObermenge (createMT2 "ab") (createMT2  "a"))
>   putStrLn ""
>   putStrLn $ "istEchteObermenge           {'a'} {'a'}: " ++ (show $ istEchteObermenge (createMT2  "a") (createMT2  "a"))
>   putStrLn $ "istEchteObermenge {'a', 'b'} {'b', 'a'}: " ++ (show $ istEchteObermenge (createMT2 "ab") (createMT2 "ba"))
>   putStrLn $ "istEchteObermenge      {'a'} {'a', 'b'}: " ++ (show $ istEchteObermenge (createMT2  "a") (createMT2 "ab"))
>   putStrLn $ "istEchteObermenge      {'a', 'b'} {'a'}: " ++ (show $ istEchteObermenge (createMT2 "ab") (createMT2  "a"))
>   putStrLn ""
>   putStrLn $ "sindElementeFremd           {'a'} {'a', 'b'}: " ++ (show $ istEchteObermenge (createMT2  "a") (createMT2  "ab"))
>   putStrLn $ "sindElementeFremd {'a', 'b', 'c'} {'d', 'e', 'f'}: " ++ (show $ istEchteObermenge (createMT2 "abc") (createMT2 "def"))
>   putStrLn ""
>   putStrLn $ "sindQuerUeberlappend           {'a'} {'a'}: " ++ (show $ istEchteObermenge (createMT2  "a") (createMT2  "a"))
>   putStrLn $ "sindQuerUeberlappend {'a', 'b', 'c'} {'b', 'a', 'd'}: " ++ (show $ istEchteObermenge (createMT2 "abc") (createMT2 "bad"))
>   putStrLn $ "sindQuerUeberlappend      {'a'} {'a', 'b'}: " ++ (show $ istEchteObermenge (createMT2  "a") (createMT2 "ab"))
>   putStrLn $ "sindQuerUeberlappend      {'a', 'b'} {'a'}: " ++ (show $ istEchteObermenge (createMT2 "ab") (createMT2  "a"))
>   putStrLn ""
>   putStrLn ""
>   putStrLn "------------------------------Int------------------------------"
>   putStrLn ""
>   putStrLn $ "leereMenge: " ++ zeige (leereMenge :: MT2 Int)
>   putStrLn $ "allMenge  : " ++ zeige (allMenge   :: MT2 Int)
>   putStrLn ""
>   putStrLn $ "istMenge     {}: " ++ (show $ istMenge (leereMenge :: MT2 Int))
>   putStrLn $ "istMenge {1, 1}: " ++ (show $ istMenge $ createMT2 [1, 1 :: Int])
>   putStrLn $ "istMenge {1, 2}: " ++ (show $ istMenge $ createMT2 [1, 2 :: Int])
>   putStrLn ""
>   putStrLn $ "vereinige  {} {1}: " ++ (zeige . vereinige leereMenge $ createMT2 [1 :: Int])
>   putStrLn $ "vereinige {1} {1}: " ++ (zeige $ vereinige (createMT2 [1]) (createMT2 [1 :: Int]))
>   putStrLn $ "vereinige {1} {2}: " ++ (zeige $ vereinige (createMT2 [1]) (createMT2 [2 :: Int]))
>   putStrLn ""
>   putStrLn $ "schneide     {} {1}: " ++ (zeige . schneide leereMenge $ createMT2 [1 :: Int])
>   putStrLn $ "schneide    {1} {1}: " ++ (zeige $ schneide (createMT2 [1]) (createMT2    [1 :: Int]))
>   putStrLn $ "schneide {1} {1, 2}: " ++ (zeige $ schneide (createMT2 [1]) (createMT2 [1, 2 :: Int]))
>   putStrLn ""
>   putStrLn $ "zieheab     {} {1}: " ++ (zeige . zieheab leereMenge $ createMT2 [1 :: Int])
>   putStrLn $ "zieheab    {1} {1}: " ++ (zeige $ zieheab (createMT2    [1]) (createMT2 [1 :: Int]))
>   putStrLn $ "zieheab {1, 2} {1}: " ++ (zeige $ zieheab (createMT2 [1, 2]) (createMT2 [1 :: Int]))
>   putStrLn ""
>   putStrLn $ "komplementiere . zieheab allMenge $ {1}: " ++ (zeige . komplementiere . zieheab allMenge $ createMT2 [1 :: Int])
>   putStrLn ""
>   putStrLn $ "sindGleich       {1} {1}: " ++ (show $ sindGleich (createMT2    [1]) (createMT2    [1 :: Int]))
>   putStrLn $ "sindGleich       {1} {2}: " ++ (show $ sindGleich (createMT2    [1]) (createMT2    [2 :: Int]))
>   putStrLn $ "sindGleich {1, 2} {2, 1}: " ++ (show $ sindGleich (createMT2 [1, 2]) (createMT2 [2, 1 :: Int]))
>   putStrLn ""
>   putStrLn $ "sindUngleich       {1} {1}: " ++ (show $ sindUngleich (createMT2    [1]) (createMT2    [1 :: Int]))
>   putStrLn $ "sindUngleich       {1} {2}: " ++ (show $ sindUngleich (createMT2    [1]) (createMT2    [2 :: Int]))
>   putStrLn $ "sindUngleich {1, 2} {2, 1}: " ++ (show $ sindUngleich (createMT2 [1, 2]) (createMT2 [2, 1 :: Int]))
>   putStrLn ""
>   putStrLn $ "istTeilmenge       {1} {1}: " ++ (show $ istTeilmenge (createMT2    [1]) (createMT2    [1 :: Int]))
>   putStrLn $ "istTeilmenge {1, 2} {2, 1}: " ++ (show $ istTeilmenge (createMT2 [1, 2]) (createMT2 [2, 1 :: Int]))
>   putStrLn $ "istTeilmenge    {1} {1, 2}: " ++ (show $ istTeilmenge (createMT2    [1]) (createMT2 [1, 2 :: Int]))
>   putStrLn $ "istTeilmenge    {1, 2} {1}: " ++ (show $ istTeilmenge (createMT2 [1, 2]) (createMT2    [1 :: Int]))
>   putStrLn ""
>   putStrLn $ "istEchteTeilmenge       {1} {1}: " ++ (show $ istEchteTeilmenge (createMT2    [1]) (createMT2    [1 :: Int]))
>   putStrLn $ "istEchteTeilmenge {1, 2} {2, 1}: " ++ (show $ istEchteTeilmenge (createMT2 [1, 2]) (createMT2 [2, 1 :: Int]))
>   putStrLn $ "istEchteTeilmenge    {1} {1, 2}: " ++ (show $ istEchteTeilmenge (createMT2    [1]) (createMT2 [1, 2 :: Int]))
>   putStrLn $ "istEchteTeilmenge    {1, 2} {1}: " ++ (show $ istEchteTeilmenge (createMT2 [1, 2]) (createMT2    [1 :: Int]))
>   putStrLn ""
>   putStrLn $ "istObermenge       {1} {1}: " ++ (show $ istObermenge (createMT2    [1]) (createMT2    [1 :: Int]))
>   putStrLn $ "istObermenge {1, 2} {2, 1}: " ++ (show $ istObermenge (createMT2 [1, 2]) (createMT2 [2, 1 :: Int]))
>   putStrLn $ "istObermenge    {1} {1, 2}: " ++ (show $ istObermenge (createMT2    [1]) (createMT2 [1, 2 :: Int]))
>   putStrLn $ "istObermenge    {1, 2} {1}: " ++ (show $ istObermenge (createMT2 [1, 2]) (createMT2    [1 :: Int]))
>   putStrLn ""
>   putStrLn $ "istEchteObermenge       {1} {1}: " ++ (show $ istEchteObermenge (createMT2    [1]) (createMT2    [1 :: Int]))
>   putStrLn $ "istEchteObermenge {1, 2} {2, 1}: " ++ (show $ istEchteObermenge (createMT2 [1, 2]) (createMT2 [2, 1 :: Int]))
>   putStrLn $ "istEchteObermenge    {1} {1, 2}: " ++ (show $ istEchteObermenge (createMT2    [1]) (createMT2 [1, 2 :: Int]))
>   putStrLn $ "istEchteObermenge    {1, 2} {1}: " ++ (show $ istEchteObermenge (createMT2 [1, 2]) (createMT2    [1 :: Int]))
>   putStrLn ""
>   putStrLn $ "sindElementeFremd       {1} {1, 2}: " ++ (show $ sindElementeFremd (createMT2    [1]) (createMT2    [1, 2 :: Int]))
>   putStrLn $ "sindElementeFremd {1, 2, 3} {4, 5, 6}: " ++ (show $ sindElementeFremd (createMT2 [1, 2, 3]) (createMT2 [4, 5, 6 :: Int]))
>   putStrLn ""
>   putStrLn $ "sindQuerUeberlappend       {1} {1}: " ++ (show $ sindQuerUeberlappend (createMT2    [1]) (createMT2    [1 :: Int]))
>   putStrLn $ "sindQuerUeberlappend {1, 2, 3} {2, 1, 4}: " ++ (show $ sindQuerUeberlappend (createMT2 [1, 2, 3]) (createMT2 [2, 1, 4 :: Int]))
>   putStrLn $ "sindQuerUeberlappend    {1} {1, 2}: " ++ (show $ sindQuerUeberlappend (createMT2    [1]) (createMT2 [1, 2 :: Int]))
>   putStrLn $ "sindQuerUeberlappend    {1, 2} {1}: " ++ (show $ sindQuerUeberlappend (createMT2 [1, 2]) (createMT2    [1 :: Int]))
>   putStrLn ""
>   putStrLn ""