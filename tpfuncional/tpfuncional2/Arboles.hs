module Arboles (Componente(Madera, Hoja, Fruto, Flor), Arbol(Rama, Brote), Dirección(Izquierda, Derecha), TipoHambre(Gula, Hambre, Inanicion), Animal, foldArbol, peso, perfume, puedeVivir, mismosComponentes, masPesado,componentesPorNivel, dimensiones, ultimaPrimavera) where

data Componente = Madera | Hoja | Fruto | Flor deriving (Eq, Show)

data Arbol = Rama Componente Arbol Arbol | Brote Componente deriving Eq

data Dirección = Izquierda | Derecha deriving Eq

data TipoHambre = Gula | Hambre | Inanicion deriving Eq

type Animal = (Dirección, Int, TipoHambre)

instance Show Arbol where
  show = ("\n" ++) . (padArbol 0 0 False)

padArbol nivel acum doPad (Brote c) = (if doPad then pad (4*nivel + acum) else "") ++ show c
padArbol nivel acum doPad (Rama x i d) = (if doPad then pad (4*nivel + acum) else "") ++ show x ++
					  pad 4 ++ padArbol (nivel+1) (acum+l) False i ++ "\n" ++
					  padArbol (nivel+1) (acum+l) True d where l = length $ show x

getAlt :: Animal -> Int
getAlt (dir, alt, th) = alt

getDir :: Animal -> Dirección
getDir (dir, alt, th) = dir

getTh :: Animal -> TipoHambre
getTh (dir, alt, th) = th

pad :: Int -> String
pad i = replicate i ' '

--Ejercicio 1
foldArbol :: ( Componente -> b ->  b ->  b) -> ( Componente ->  b) -> Arbol -> b
foldArbol casoR casoB a = case a of Rama c sub1 sub2 -> casoR c (rec sub1) (rec sub2)
                                    Brote c -> casoB c
                                    where rec = foldArbol casoR casoB

--Ejercicio 2
peso :: Arbol -> Int
peso = foldArbol (\c res1 res2 -> case c of Madera -> 1 + res1 + res2
                                            x -> res1 + res2) (\c -> case c of Madera -> 1 
                                                                               j -> 0) 
perfume :: Arbol -> Int
perfume = foldArbol (\c res1 res2 -> case c of Flor -> 1 + res1 + res2
                                               x -> res1 + res2) (\c -> case c of Flor -> 1 
                                                                                  j -> 0)
puedeVivir :: Arbol -> Bool
puedeVivir = foldArbol (\c res1 res2 -> case c of Hoja -> True
                                                  x -> res1 || res2) (\c -> case c of Hoja -> True 
                                                                                      j -> False)
mismosComponentes :: Arbol -> Arbol -> Bool
mismosComponentes a1 a2 = if peso a1 == peso a2 && perfume a1 == perfume a2 && hojas a1 == hojas a2 then True else False

hojas :: Arbol -> Int
hojas = foldArbol (\c res1 res2 -> case c of Hoja -> 1 + res1 + res2
                                             x -> res1 + res2) (\c -> case c of Hoja -> 1 
                                                                                j -> 0)
--Ejercicio 3

masPesado :: [Arbol] -> Arbol
masPesado ls = (foldr1 (\x res -> if (peso x) > (peso res) then x else res) ls)

--Ejercicio 4

crecer :: (Componente -> Componente) -> Arbol -> Arbol
crecer f a = foldArbol (\c res1 res2 -> Rama (f c) res1 res2) (\c -> Brote (f c)) a



ultimaPrimavera :: Arbol -> Arbol
ultimaPrimavera a = crecer (\c -> if c == Hoja then Flor else c) a 

subArbolP :: Int -> Int -> Dirección -> Arbol -> Arbol
subArbolP j i d (Rama c a1 a2) = if j == i then Rama c a1 a2 else if d == Izquierda then subArbolP (j+1) i d a1 else subArbolP (j+1)i d a2
subArbolP j i d (Brote c) = Brote c

subArbol :: Int -> Dirección -> Arbol -> Arbol
subArbol i d a = subArbolP 0 i d a

achicarP :: Int -> Int -> Dirección -> Arbol -> Arbol
achicarP j i d (Rama c a1 a2) = if i == j then Rama c (Brote Madera) (Brote Madera) else if d == Izquierda then achicarP (j+1) i d a1 else achicarP (j+1) i d a2 
achicarP j i d (Brote c) = Brote c

achicar :: Int -> Dirección -> Arbol -> Arbol
achicar i d a = achicarP 0 i d a

-- Ejercicio 5
comer :: Animal -> Arbol -> Arbol
comer an ar = if getTh an == Gula then ar
			  else if getTh an == Hambre then
			  	      if perfume (subArbol (getAlt an) (getDir an) ar) > 0 then ar else achicar (getAlt an) (getDir an) ar
			  else achicar (getAlt an) (getDir an) ar

-- Ejercicio 6
alimentar :: Arbol -> [Animal] -> Arbol
alimentar ar ls = foldr (\an res -> comer an res) ar ls

-- Ejercicio 7
sobrevivientes :: [Animal] -> [Arbol] -> [Arbol]
sobrevivientes ans ars = foldr (\ar res -> if (puedeVivir (alimentar ar ans)) == True then ar:res else res) [] ars

componentesPorNivelP :: Arbol -> Int -> Int -> Int
componentesPorNivelP (Rama c a1 a2) j i = if j == i then 2 else (componentesPorNivelP a1 (j+1) i) + (componentesPorNivelP a2 (j+1) i)
componentesPorNivelP (Brote c) j i = if j == i then 1 else 0

-- Ejercicio 8a
componentesPorNivel :: Arbol -> Int -> Int
componentesPorNivel ar i = componentesPorNivelP ar i 0 

-- Ejercicio 8b
dimensiones :: Arbol -> (Int, Int)
dimensiones ar = foldArbol (\c res1 res2 -> (((max (fst res1) (fst res2)) + 1), max (componentesPorNivel ar ((max (fst res1) (fst res2)) + 1)) (max (snd res1) (snd res2)))) (\c -> (0, 1)) ar
