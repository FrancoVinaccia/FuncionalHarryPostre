module Library where
import PdePreludat
import GHC.Base (BCO)


-- A)
data Postre = UnPostre {
    sabores :: [String],
    peso :: Number,
    temperatura :: Number
} deriving (Show, Eq)

bizcocho = UnPostre ["fruta", "crema"] 100 25


-- B)
-- HECHIZOS
type Hechizo = Postre -> Postre

incendio :: Hechizo
incendio = restarPesoPorcentaje 5 . modificarTemperatura 1

immobulus :: Hechizo
immobulus postre = postre {temperatura = 0}

wingardumLeviosa :: Hechizo
wingardumLeviosa = agregarSabor "concentrado" . restarPesoPorcentaje 10

diffindo :: Number -> Hechizo
diffindo = restarPesoPorcentaje  

riddikulus :: String -> Hechizo
riddikulus = agregarSabor  

avadaKedavra :: Hechizo
avadaKedavra = immobulus . removerSabores


-- FUNCIONES AYUDA
modificarTemperatura :: Number -> Postre -> Postre
modificarTemperatura cant postre = postre {temperatura = temperatura postre + cant}

restarPesoPorcentaje :: Number -> Postre -> Postre
restarPesoPorcentaje porc postre = postre {peso = peso postre - peso postre * porc / 100}

agregarSabor :: String -> Postre -> Postre
agregarSabor sabor postre = postre {sabores = sabor : sabores postre}

removerSabores :: Postre -> Postre
removerSabores postre = postre {sabores = []}


-- C)
estanListos :: [Postre] -> Bool
estanListos = all estaListo

estaListo :: Postre -> Bool
estaListo postre = peso postre > 0 && tieneSabor postre && not (estaCongelado postre)

tieneSabor :: Postre -> Bool
tieneSabor = not . null . sabores

estaCongelado :: Postre -> Bool
estaCongelado = (<= 0) . temperatura

-- D)
postresListos :: [Postre] -> [Postre]
postresListos = filter estaListo

promedioPesoListos :: [Postre] -> Number
promedioPesoListos postres = promedio (map peso (postresListos postres))

promedio :: [Number] -> Number
promedio numeros = sum numeros / length numeros

-- 2 A)

data Mago = UnMago{
    hechizos :: [Hechizo],
    cantHorrorcrux :: Number
} deriving (Show, Eq)

practicarHechizo :: Mago -> Hechizo -> Postre -> Mago
practicarHechizo mago hechizo postre 
    | esComoAvadaKedavra mago hechizo postre = aniadirHorrorcrux (aniadirHechizo hechizo mago)
    | otherwise = aniadirHechizo hechizo mago

aniadirHechizo :: Hechizo -> Mago -> Mago
aniadirHechizo hechizo mago = mago {hechizos = hechizo : hechizos mago}

aniadirHorrorcrux :: Mago -> Mago
aniadirHorrorcrux mago = mago {cantHorrorcrux = cantHorrorcrux mago + 1}

esComoAvadaKedavra :: Mago -> Hechizo -> Postre -> Bool
esComoAvadaKedavra mago hechizo postre = postre == avadaKedavra postre

-- B)

--3 A)Construir una lista infinita de postres, y construir un mago con infinitos hechizos.

postresInfinitos :: [Postre]
postresInfinitos = repeat bizcocho

magoInfinito :: Mago
magoInfinito = UnMago (repeat avadaKedavra)  0

-- B) pregunto si algún hechizo los deja listos  ¿Existe alguna consulta que pueda hacer para que me sepa dar una respuesta? Justificar conceptualmente.
-- Si, ya que a la que encuentra un caso de un postre que no este listo, no se cumple el all, por lo tanto devuelve false, ya que usa lazy eval

-- C) Suponiendo que un mago tiene infinitos hechizos ¿Existe algún caso en el que se puede encontrar al mejor hechizo? Justificar conceptualmente.
-- No, ya que para deteminar el mejor hay que comparar todos.