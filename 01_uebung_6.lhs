> module Mengen where
> type Fehlermeldung = String
> type MengeAlsZeichenreihe = String
> newtype MT1 e = MT1 [e] 
> data MT2 e = Nichts
>              | VerlaengereUm e (MT2 e)
> newtype MT3 e = MT3 (e -> Bool)


Klasse Defaultable
Definiert eine Typklasse für Typen, die einen Standardwert (eine Liste von 
 Werten) liefern können.

> class Defaultable a where
>   defaultValue :: [a] -- liefert eine Liste von Standardwerten des Typs 'a'.

> instance Defaultable Int where
>   defaultValue = [(-100)..100]

> instance Defaultable Char where
>   defaultValue = ['a'..'z'] ++ ['A'..'Z']


> class Menge m where
>   leereMenge :: m
>   allMenge :: m
>   istMenge :: m -> Bool
>   vereinige :: m -> m -> m
>   schneide :: m -> m -> m
>   zieheab :: m -> m -> m
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


--------------------------------------------------------- A.1 ----------------------------------------------------------

PROTOIMPLEMENTIERUNGEN

>   komplementiere = zieheab allMenge

Zwei Mengen sind gleich, wenn sie Teilmengen voneinander sind

>   sindGleich m1 m2 = istTeilmenge m1 m2 && istTeilmenge m2 m1
>   sindUngleich m1 m2 = not (sindGleich m1 m2)

Wenn A (echte) Obermenge von B ist, ist dann B (echte) Teilmenge von A

>   istObermenge m1 m2 = istTeilmenge m2 m1
>   istEchteObermenge m1 m2 = istEchteTeilmenge m2 m1 

Eine Menge ist echte Teilmenge einer Anderen, wenn sie Teilmenge aber nicht 
 gleich ist

>   istEchteTeilmenge m1 m2 = istTeilmenge m1 m2 && not (sindGleich m1 m2)

Zwei Mengen sind elementefremd, wenn ihrer Schnitt die Leeremenge ist

>   sindElementeFremd m1 = sindGleich leereMenge . schneide m1

Zwei Mengen sind quer-ueberlappend, wenn sie...
  ... mindestens ein Element gemeinsam haben
  ... jeweils keine Teilmenge voneinander sind

>   sindQuerUeberlappend m1 m2 =
>       not (sindElementeFremd m1 m2)
>       && not (istTeilmenge m1 m2)
>       && not (istTeilmenge m2 m1)
>   istKeinGueltigerMengenwert = error
>   nichtImplementierbar = error


--------------------------------------------------------- A.2 ----------------------------------------------------------

--------------------------------------------------------- MT1 ----------------------------------------------------------

> instance Menge (MT1 Char) where
>   leereMenge = MT1 []
>   allMenge = MT1 defaultValue
>   istMenge = istMengeMT1
>   vereinige = vereinigeMT1
>   schneide = schneideMT1
>   zieheab = zieheabMT1
>   istTeilmenge = istTeilmengeMT1
>   zeige = zeigeMT1


> instance Menge (MT1 Int) where
>   leereMenge = MT1 []
>   allMenge = MT1 defaultValue
>   istMenge = istMengeMT1
>   vereinige = vereinigeMT1
>   schneide = schneideMT1
>   zieheab = zieheabMT1
>   istTeilmenge = istTeilmengeMT1
>   zeige = zeigeMT1


Allgemeine Funktionen fuer MT1

> istMengeMT1 :: Eq e => MT1 e -> Bool
> istMengeMT1 (MT1     []) = True
> istMengeMT1 (MT1 (e:es)) = all (/= e) es && (istMengeMT1 . MT1) es

> vereinigeMT1 :: Eq e => MT1 e -> MT1 e -> MT1 e
> vereinigeMT1 m1@(MT1 list1) m2@(MT1 list2)
>       | istMengeMT1 m1 && istMengeMT1 m2 = MT1 . nub $ list1 ++ list2
>       | otherwise                        = fehlermeldung

> schneideMT1 :: Eq e => MT1 e -> MT1 e -> MT1 e
> schneideMT1 m1@(MT1 list1) m2@(MT1 list2)
>       | istMengeMT1 m1 && istMengeMT1 m2 = MT1 $ [e | e <- list1, e `elem` list2]
>       | otherwise                        = fehlermeldung

> zieheabMT1 :: Eq e => MT1 e -> MT1 e -> MT1 e
> zieheabMT1 m1@(MT1 list1) m2@(MT1 list2)
>       | istMengeMT1 m1 && istMengeMT1 m2 = MT1 $ [e | e <- list1, e `notElem` list2]
>       | otherwise                        = fehlermeldung


> istTeilmengeMT1 :: Eq e => MT1 e -> MT1 e -> Bool
> istTeilmengeMT1 m1@(MT1 list1) m2@(MT1 list2)
>       | istMengeMT1 m1 && istMengeMT1 m2 = all (`elem` list2) list1
>       | otherwise                        = fehlermeldung

> zeigeMT1 :: Show e => MT1 e -> MengeAlsZeichenreihe
> zeigeMT1 (MT1 elems) = "{" ++ formatElems elems ++ "}"


Hilffunktionen fuer MT1.

Fehlermeldung fuer wenn ein oder mehrere Argumente nicht Menge sind.

> fehlermeldung :: a
> fehlermeldung = error "Argument muss Menge sein (keine Duplikate)"

Entferne Duplikate einer Liste.

> nub :: Eq a => [a] -> [a]
> nub     [] = []
> nub (e:es) = e : (nub $ filter (/= e) es)

Formatiere Elemente, um sie auszudrucken.

> formatElems :: Show a => [a] -> String
> formatElems []     = ""
> formatElems [e]    = show e
> formatElems (e:es) = show e ++ ", " ++ formatElems es


--------------------------------------------------------- MT2 ----------------------------------------------------------

> instance Menge (MT2 Char) where
>   leereMenge = Nichts
>   allMenge = createMT2 defaultValue
>   istMenge = istMengeMT2
>   vereinige = vereinigeMT2
>   schneide = schneideMT2
>   zieheab = zieheabMT2
>   istTeilmenge = istTeilmengeMT2
>   zeige = zeigeMT2


> instance Menge (MT2 Int) where
>   leereMenge = Nichts
>   allMenge = createMT2 defaultValue
>   istMenge = istMengeMT2
>   vereinige = vereinigeMT2
>   schneide = schneideMT2
>   zieheab = zieheabMT2
>   istTeilmenge = istTeilmengeMT2
>   zeige = zeigeMT2


Allgemeine Funktionen fuer MT2.

> istMengeMT2 :: Eq e => MT2 e -> Bool
> istMengeMT2              Nichts = True
> istMengeMT2 (VerlaengereUm e m) = (not . isElem e) m && istMengeMT2 m

> vereinigeMT2 :: Eq e => MT2 e -> MT2 e -> MT2 e
> vereinigeMT2 m1 m2
>       | istMengeMT2 m1 && istMengeMT2 m2 = join m1 m2
>       | otherwise                        = fehlermeldung

> schneideMT2 :: Eq e => MT2 e -> MT2 e -> MT2 e
> schneideMT2 m1 m2
>       | istMengeMT2 m1 && istMengeMT2 m2 = union m1 m2
>       | otherwise                        = fehlermeldung

> zieheabMT2 :: Eq e => MT2 e -> MT2 e -> MT2 e
> zieheabMT2 m1 m2
>       | istMengeMT2 m1 && istMengeMT2 m2 = sub m1 m2
>       | otherwise                        = fehlermeldung


> istTeilmengeMT2 :: Eq e => MT2 e -> MT2 e -> Bool
> istTeilmengeMT2 m1 m2
>       | istMengeMT2 m1 && istMengeMT2 m2 = isSubset m1 m2
>       | otherwise                        = fehlermeldung

> zeigeMT2 :: Show e => MT2 e -> MengeAlsZeichenreihe
> zeigeMT2 elems = "{" ++ (formatElems . toListMT2) elems ++ "}"


Hilffunktionen fuer MT2.

Wandle eine Liste von Dingen in einem MT2 um.

> createMT2 :: [e] -> MT2 e
> createMT2     [] = Nichts
> createMT2 (x:xs) = VerlaengereUm x $ createMT2 xs

Wandle ein MT2 in einer Liste seiner Elemente.

> toListMT2 :: MT2 e -> [e]
> toListMT2              Nichts = []
> toListMT2 (VerlaengereUm e m) = e : (toListMT2 m)

Ob ein Ding Element eines MT2s ist.

> isElem :: Eq e => e -> MT2 e -> Bool
> isElem _ Nichts = False
> isElem x (VerlaengereUm e m)
>     | x == e    = True
>     | otherwise = isElem x m

Vereinige zwei MT2, wobei man akkumuliert das Ergebnis im ersten Argument.

> join :: Eq e => MT2 e -> MT2 e -> MT2 e
> join      m Nichts = m
> join m1 (VerlaengereUm e m2)
>     -- wenn e in m1, dann ueberspringe ihn
>     | isElem e m1 = join m1 m2
>     | otherwise   = join (VerlaengereUm e m1) m2

Schneide zwei MT2, wobei man akkumuliert das Ergebnis am Rueckweg der Rekursion.

> union :: Eq e => MT2 e -> MT2 e -> MT2 e
> union Nichts      _ = Nichts
> union (VerlaengereUm e m1) m2
>     -- wenn e in m2, dann behalte es
>     | isElem e m2 = VerlaengereUm e $ union m1 m2
>     | otherwise   = union m1 m2

Ziehe den zweiten MT2 vom Ersten ab, wobei man akkumuliert das Ergebnis am
 Rueckweg der Rekursion.

> sub :: Eq e => MT2 e -> MT2 e -> MT2 e
> sub Nichts      _ = Nichts
> sub (VerlaengereUm e m1) m2
>     -- wenn e in m2, dann ueberspringe ihn
>     | isElem e m2 = sub m1 m2
>     | otherwise   = VerlaengereUm e $ sub m1 m2

Ob der erste MT2 Teilmenge vom Zweiten ist.

> isSubset :: Eq e => MT2 e -> MT2 e -> Bool
> isSubset Nichts _ = True
> isSubset (VerlaengereUm e m1) m2
>     -- wenn e in m2, pruefe m1 weiter
>     | isElem e m2 = isSubset m1 m2
>     | otherwise   = False


--------------------------------------------------------- MT3 ----------------------------------------------------------

> instance Menge (MT3 Char) where
>   leereMenge = MT3 (\_ -> False)
>   allMenge = MT3 (\_ -> True )
>   istMenge = \_ -> True
>   vereinige = vereinigeMT3
>   schneide = schneideMT3
>   zieheab = zieheabMT3
>   istTeilmenge = istTeilmengeMT3
>   zeige = zeigeMT3


> instance Menge (MT3 Int) where
>   leereMenge = MT3 (\_ -> False)
>   allMenge = MT3 (\_ -> True )
>   istMenge = \_ -> True
>   vereinige = vereinigeMT3
>   schneide = schneideMT3
>   zieheab = zieheabMT3
>   istTeilmenge = istTeilmengeMT3
>   zeige = zeigeMT3


Allgemeine Funktionen fuer MT3.

> vereinigeMT3 :: Eq e => MT3 e -> MT3 e -> MT3 e
> vereinigeMT3 (MT3 f1) (MT3 f2) = MT3 $ \elem -> f1 elem || f2 elem

> schneideMT3 :: Eq e => MT3 e -> MT3 e -> MT3 e
> schneideMT3 (MT3 f1) (MT3 f2) = MT3 $ \elem -> f1 elem && f2 elem

> zieheabMT3 :: Eq e => MT3 e -> MT3 e -> MT3 e
> zieheabMT3(MT3 f1) (MT3 f2) = MT3 $ \elem -> f1 elem && (not . f2) elem

> istTeilmengeMT3 :: (Eq e, Defaultable e) => MT3 e -> MT3 e -> Bool
> istTeilmengeMT3 m1 (MT3 f) =
>     let elems1 = toListMT3 m1
>     in all f elems1

> zeigeMT3 m = "{" ++ (formatElems . toListMT3) m ++ "}"


Diese Funktion wandelt einen Wert des Typs MT3 in eine Liste vom Typ e um.
 Sie benoetigt die Typklassebeschraenkung (Defaultable e), um sicherzustellen, 
 dass der Typ e eine defaultValue-Funktion besitzt.  
Haette man nicht diese Beschaenkung, muesste man Bound nutzen, ist es aber sehr
 gross bei Int und Char.

> toListMT3 :: (Defaultable e) => MT3 e -> [e]
> toListMT3 (MT3 f) = filter f defaultValue


--------------------------------------------------------- A.3 ----------------------------------------------------------

Ueberpruefe ob ein Char Element einer Menge ueber Chars ist.
Man kann hier bei der Ueberpruefung, ob die Eingaben gueltig sind, nicht 
 istKeinGueltigerMengenwert nutzen, weil der Rueckgabetyp dieser Funktion ein 
 Bool ist, und von istKeinGueltigerMengenwert eine Menge.

> istElement :: Menge m => Char -> m -> Bool
> istElement c m
>     | not (isInt [c] || isMChar [c]) = error "Ungueltiger Charakter"
>     | otherwise = isInString c (zeige m)

Ueberpruefe ob ein Char kein Element einer Menge ueber Chars ist.

> istKeinElement :: Menge m => Char -> m -> Bool
> istKeinElement c m = not (istElement c m)

> isInString :: Char -> String -> Bool
> isInString char str = elem char str

> isInt :: String -> Bool
> isInt str = all (`elem` ['1' .. '9']) str

> isMChar :: String -> Bool
> isMChar str = all (`elem` ['a'..'z'] ++ ['A'..'Z']) str


-------------------------------------------------------- Tests ---------------------------------------------------------

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
>   putStrLn "------------------------------MT3------------------------------"
>   let a3  = \e -> if e == 'a'                          then True else False
>       b3  = \e -> if e == 'b'                          then True else False
>       ab3 = \e -> if e == 'a' || e == 'b'              then True else False
>       ba3 = \e -> if e == 'b' || e == 'a'              then True else False
>       abc3 = \e -> if e == 'a' || e == 'b' || e == 'c' then True else False
>       def3 = \e -> if e == 'd' || e == 'e' || e == 'f' then True else False
>       bad3 = \e -> if e == 'b' || e == 'a' || e == 'd' then True else False
>   putStrLn ""
>   putStrLn ""
>   putStrLn "------------------------------Char------------------------------"
>   putStrLn ""
>   putStrLn $ "leereMenge: " ++ zeige (leereMenge :: MT3 Char)
>   putStrLn $ "allMenge  : " ++ zeige (allMenge   :: MT3 Char)
>   putStrLn ""
>   putStrLn "istMenge ist die Protoimplementierung"
>   putStrLn ""
>   putStrLn $ "vereinige    {} {'a'}: " ++ (zeige . vereinige leereMenge $ MT3 a3)
>   putStrLn $ "vereinige {'a'} {'a'}: " ++ (zeige $ vereinige (MT3 a3) (MT3 a3))
>   putStrLn $ "vereinige {'a'} {'b'}: " ++ (zeige $ vereinige (MT3 a3) (MT3 b3))
>   putStrLn ""
>   putStrLn $ "schneide         {} {'a'}: " ++ (zeige . schneide leereMenge $ MT3 a3)
>   putStrLn $ "schneide      {'a'} {'a'}: " ++ (zeige $ schneide (MT3 a3) (MT3  a3))
>   putStrLn $ "schneide {'a'} {'a', 'b'}: " ++ (zeige $ schneide (MT3 a3) (MT3 ab3))
>   putStrLn ""
>   putStrLn $ "zieheab         {} {'a'}: " ++ (zeige . zieheab leereMenge $ MT3 a3)
>   putStrLn $ "zieheab      {'a'} {'a'}: " ++ (zeige $ zieheab (MT3  a3) (MT3 a3))
>   putStrLn $ "zieheab {'a', 'b'} {'a'}: " ++ (zeige $ zieheab (MT3 ab3) (MT3 a3))
>   putStrLn ""
>   putStrLn $ "komplementiere . zieheab allMenge $ {'a'}: " ++ (zeige . komplementiere . zieheab allMenge $ MT3 a3)
>   putStrLn ""
>   putStrLn $ "sindGleich           {'a'} {'a'}: " ++ (show $ sindGleich (MT3  a3) (MT3  a3))
>   putStrLn $ "sindGleich           {'a'} {'b'}: " ++ (show $ sindGleich (MT3  a3) (MT3  b3))
>   putStrLn $ "sindGleich {'a', 'b'} {'b', 'a'}: " ++ (show $ sindGleich (MT3 ab3) (MT3 ba3))
>   putStrLn ""
>   putStrLn $ "istTeilmenge           {'a'} {'a'}: " ++ (show $ istTeilmenge (MT3  a3) (MT3  a3))
>   putStrLn $ "istTeilmenge {'a', 'b'} {'b', 'a'}: " ++ (show $ istTeilmenge (MT3 ab3) (MT3 ba3))
>   putStrLn $ "istTeilmenge      {'a'} {'a', 'b'}: " ++ (show $ istTeilmenge (MT3  a3) (MT3 ab3))
>   putStrLn $ "istTeilmenge      {'a', 'b'} {'a'}: " ++ (show $ istTeilmenge (MT3 ab3) (MT3  a3))
>   putStrLn ""
>   putStrLn $ "istEchteTeilmenge           {'a'} {'a'}: " ++ (show $ istEchteTeilmenge (MT3  a3) (MT3  a3))
>   putStrLn $ "istEchteTeilmenge {'a', 'b'} {'b', 'a'}: " ++ (show $ istEchteTeilmenge (MT3 ab3) (MT3 ba3))
>   putStrLn $ "istEchteTeilmenge      {'a'} {'a', 'b'}: " ++ (show $ istEchteTeilmenge (MT3  a3) (MT3 ab3))
>   putStrLn $ "istEchteTeilmenge      {'a', 'b'} {'a'}: " ++ (show $ istEchteTeilmenge (MT3 ab3) (MT3  a3))
>   putStrLn ""
>   putStrLn $ "istObermenge           {'a'} {'a'}: " ++ (show $ istObermenge (MT3  a3) (MT3  a3))
>   putStrLn $ "istObermenge {'a', 'b'} {'b', 'a'}: " ++ (show $ istObermenge (MT3 ab3) (MT3 ba3))
>   putStrLn $ "istObermenge      {'a'} {'a', 'b'}: " ++ (show $ istObermenge (MT3  a3) (MT3 ab3))
>   putStrLn $ "istObermenge      {'a', 'b'} {'a'}: " ++ (show $ istObermenge (MT3 ab3) (MT3  a3))
>   putStrLn ""
>   putStrLn $ "istEchteObermenge           {'a'} {'a'}: " ++ (show $ istEchteObermenge (MT3  a3) (MT3  a3))
>   putStrLn $ "istEchteObermenge {'a', 'b'} {'b', 'a'}: " ++ (show $ istEchteObermenge (MT3 ab3) (MT3 ba3))
>   putStrLn $ "istEchteObermenge      {'a'} {'a', 'b'}: " ++ (show $ istEchteObermenge (MT3  a3) (MT3 ab3))
>   putStrLn $ "istEchteObermenge      {'a', 'b'} {'a'}: " ++ (show $ istEchteObermenge (MT3 ab3) (MT3  a3))
>   putStrLn ""
>   putStrLn $ "sindElementeFremd           {'a'} {'a', 'b'}: " ++ (show $ istEchteObermenge (MT3  a3) (MT3 ab3))
>   putStrLn $ "sindElementeFremd {'a', 'b', 'c'} {'d', 'e', 'f'}: " ++ (show $ istEchteObermenge (MT3  abc3) (MT3  def3))
>   putStrLn ""
>   putStrLn $ "sindQuerUeberlappend           {'a'} {'a'}: " ++ (show $ istEchteObermenge (MT3  a3) (MT3 a3))
>   putStrLn $ "sindQuerUeberlappend {'a', 'b', 'c'} {'b', 'a', 'd'}: " ++ (show $ istEchteObermenge (MT3  abc3) (MT3  bad3))
>   putStrLn $ "sindQuerUeberlappend      {'a'} {'a', 'b'}: " ++ (show $ istEchteObermenge (MT3  a3) (MT3 ab3))
>   putStrLn $ "sindQuerUeberlappend      {'a', 'b'} {'a'}: " ++ (show $ istEchteObermenge (MT3  ab3) (MT3 a3))
>   putStrLn ""
>   putStrLn ""
>   putStrLn "------------------------------Int------------------------------"
>   let a3'  = \e -> if e == '1'                             then True else False
>       b3'  = \e -> if e == '2'                             then True else False
>       ab3' = \e -> if e == '1' || e == '2'                 then True else False
>       ba3' = \e -> if e == '2' || e == '1'                 then True else False
>       abc3' = \e -> if e == '1' || e == '2' || e == '3'    then True else False
>       def3' = \e -> if e == '4' || e == '5' || e == '6'    then True else False
>       bad3' = \e -> if e == '2' || e == '1' || e == '4'    then True else False
>   putStrLn ""
>   putStrLn $ "leereMenge: " ++ zeige (leereMenge :: MT3 Char)
>   putStrLn $ "allMenge  : " ++ zeige (allMenge   :: MT3 Char)
>   putStrLn ""
>   putStrLn "istMenge ist die Protoimplementierung"
>   putStrLn ""
>   putStrLn $ "vereinige    {} {'1'}: " ++ (zeige . vereinige leereMenge $ MT3 a3')
>   putStrLn $ "vereinige {'1'} {'1'}: " ++ (zeige $ vereinige (MT3 a3') (MT3 a3'))
>   putStrLn $ "vereinige {'1'} {'2'}: " ++ (zeige $ vereinige (MT3 a3') (MT3 b3'))
>   putStrLn ""
>   putStrLn $ "schneide         {} {1}: " ++ (zeige . schneide leereMenge $ MT3 a3')
>   putStrLn $ "schneide      {1} {1}: " ++ (zeige $ schneide (MT3 a3') (MT3  a3'))
>   putStrLn $ "schneide {1} {1, 2}: " ++ (zeige $ schneide (MT3 a3') (MT3 ab3'))
>   putStrLn ""
>   putStrLn $ "zieheab         {} {1}: " ++ (zeige . zieheab leereMenge $ MT3 a3')
>   putStrLn $ "zieheab      {1} {1}: " ++ (zeige $ zieheab (MT3  a3') (MT3 a3'))
>   putStrLn $ "zieheab {1, 2} {1}: " ++ (zeige $ zieheab (MT3 ab3') (MT3 a3'))
>   putStrLn ""
>   putStrLn $ "komplementiere . zieheab allMenge $ {1}: " ++ (zeige . komplementiere . zieheab allMenge $ MT3 a3')
>   putStrLn ""
>   putStrLn $ "sindGleich           {1} {1}: " ++ (show $ sindGleich (MT3  a3') (MT3  a3'))
>   putStrLn $ "sindGleich           {1} {2}: " ++ (show $ sindGleich (MT3  a3') (MT3  b3'))
>   putStrLn $ "sindGleich {1, 2} {2, 1}: " ++ (show $ sindGleich (MT3 ab3') (MT3 ba3'))
>   putStrLn ""
>   putStrLn $ "istTeilmenge           {1} {1}: " ++ (show $ istTeilmenge (MT3  a3') (MT3  a3'))
>   putStrLn $ "istTeilmenge {1, 2} {2, 1}: " ++ (show $ istTeilmenge (MT3 ab3') (MT3 ba3'))
>   putStrLn $ "istTeilmenge      {1} {1, 2}: " ++ (show $ istTeilmenge (MT3  a3') (MT3 ab3'))
>   putStrLn $ "istTeilmenge      {1, 2} {1}: " ++ (show $ istTeilmenge (MT3 ab3') (MT3  a3'))
>   putStrLn ""
>   putStrLn $ "istEchteTeilmenge           {1} {1}: " ++ (show $ istEchteTeilmenge (MT3  a3') (MT3  a3'))
>   putStrLn $ "istEchteTeilmenge {1, 2} {2, 1}: " ++ (show $ istEchteTeilmenge (MT3 ab3') (MT3 ba3'))
>   putStrLn $ "istEchteTeilmenge      {1} {1, 2}: " ++ (show $ istEchteTeilmenge (MT3  a3') (MT3 ab3'))
>   putStrLn $ "istEchteTeilmenge      {1, 2} {1}: " ++ (show $ istEchteTeilmenge (MT3 ab3') (MT3  a3'))
>   putStrLn ""
>   putStrLn $ "istObermenge           {1} {1}: " ++ (show $ istObermenge (MT3  a3') (MT3  a3'))
>   putStrLn $ "istObermenge {1, 2} {2, 1}: " ++ (show $ istObermenge (MT3 ab3') (MT3 ba3'))
>   putStrLn $ "istObermenge      {1} {1, 2}: " ++ (show $ istObermenge (MT3  a3') (MT3 ab3'))
>   putStrLn $ "istObermenge      {1, 2} {1}: " ++ (show $ istObermenge (MT3 ab3') (MT3  a3'))
>   putStrLn ""
>   putStrLn $ "istEchteObermenge           {1} {1}: " ++ (show $ istEchteObermenge (MT3  a3') (MT3  a3'))
>   putStrLn $ "istEchteObermenge {1, 2} {2, 1}: " ++ (show $ istEchteObermenge (MT3 ab3') (MT3 ba3'))
>   putStrLn $ "istEchteObermenge      {1} {1, 2}: " ++ (show $ istEchteObermenge (MT3  a3') (MT3 ab3'))
>   putStrLn $ "istEchteObermenge      {1, 2} {1}: " ++ (show $ istEchteObermenge (MT3 ab3') (MT3  a3'))
>   putStrLn ""
>   putStrLn $ "sindElementeFremd           {1} {1, 2}: " ++ (show $ istEchteObermenge (MT3  a3') (MT3 ab3'))
>   putStrLn $ "sindElementeFremd {1, 2, 3} {4, 5, 6}: " ++ (show $ istEchteObermenge (MT3  abc3') (MT3  def3'))
>   putStrLn ""
>   putStrLn $ "sindQuerUeberlappend           {1} {1}: " ++ (show $ istEchteObermenge (MT3  a3') (MT3 a3'))
>   putStrLn $ "sindQuerUeberlappend {1, 2, 3} {2, 1, 4}: " ++ (show $ istEchteObermenge (MT3  abc3') (MT3  bad3'))
>   putStrLn $ "sindQuerUeberlappend      {1} {1, 2}: " ++ (show $ istEchteObermenge (MT3  a3') (MT3 ab3'))
>   putStrLn $ "sindQuerUeberlappend      {1, 2} {1}: " ++ (show $ istEchteObermenge (MT3  ab3') (MT3 a3'))
>   putStrLn ""
>   putStrLn ""
>   putStrLn "------------------------------A.3------------------------------"
>   let mt1 = MT1 ['a', 'b', 'c']
>       mt2 = VerlaengereUm 'a' ( VerlaengereUm 'b' ( VerlaengereUm 'c' Nichts))
>       mt3 = MT3 (\e -> if e == 'a' || e == 'b' || e == 'c' then True else False)
>   putStrLn "------------------------------MT1------------------------------"
>   putStrLn $ "istElement      'b' {'a', 'b', 'c'}: " ++ (show $ istElement 'b' mt1)
>   putStrLn $ "istElement      'd' {'a', 'b', 'c'}: " ++ (show $ istElement 'd' mt1)
>   putStrLn ""
>   putStrLn $ "istKeinElement  'b' {'a', 'b', 'c'}: " ++ (show $ istKeinElement 'b' mt1)
>   putStrLn $ "istKeinElement  'd' {'a', 'b', 'c'}: " ++ (show $ istKeinElement 'd' mt1)
>   putStrLn "------------------------------MT2------------------------------"
>   putStrLn $ "istElement      'b' {'a', 'b', 'c'}: " ++ (show $ istElement 'b' mt2)
>   putStrLn $ "istElement      'd' {'a', 'b', 'c'}: " ++ (show $ istElement 'd' mt2)
>   putStrLn ""
>   putStrLn $ "istKeinElement  'b' {'a', 'b', 'c'}: " ++ (show $ istKeinElement 'b' mt2)
>   putStrLn $ "istKeinElement  'd' {'a', 'b', 'c'}: " ++ (show $ istKeinElement 'd' mt2)
>   putStrLn "------------------------------MT3------------------------------"
>   putStrLn $ "istElement      'b' {'a', 'b', 'c'}: " ++ (show $ istElement 'b' mt3)
>   putStrLn $ "istElement      'd' {'a', 'b', 'c'}: " ++ (show $ istElement 'd' mt3)
>   putStrLn ""
>   putStrLn $ "istKeinElement  'b' {'a', 'b', 'c'}: " ++ (show $ istKeinElement 'b' mt3)
>   putStrLn $ "istKeinElement  'd' {'a', 'b', 'c'}: " ++ (show $ istKeinElement 'd' mt3)
