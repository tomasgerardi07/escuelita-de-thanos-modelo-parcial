module Library where
import PdePreludat

-- Primera Parte

-- Punto 1
data Guantelete = Guantelete {
    material :: String,
    gemas    :: [Gema]
}

data Personaje = Personaje {
    edad        :: Number,
    energia     :: Number,
    habilidades :: [String],
    nombre      :: String,
    planeta     :: String
}

type Universo = [Personaje]

reducirUniverso :: Universo -> Universo
reducirUniverso universo = take (length universo `div` 2) universo

sePuedeUsar :: Guantelete -> Bool
sePuedeUsar guantelete = (length . gemas) guantelete == 6 && material guantelete == "uru"

cantidadDeHabilidades :: Personaje -> Number
cantidadDeHabilidades = length . habilidades

chasquido :: Guantelete -> Universo -> Universo
chasquido guantelete universo 
 | sePuedeUsar guantelete = reducirUniverso universo
 | otherwise              = universo

-- Punto 2
esAptoParaPendex :: Universo -> Bool
esAptoParaPendex universo = any ((<=45) . edad) universo

energiaTotal :: Universo -> Number
energiaTotal = sum . map energia . filter ((>1). cantidadDeHabilidades)

-- Segunda parte
-- Punto 3
type Gema = Personaje -> Personaje

debilitar :: Number -> Gema
debilitar daño personaje = personaje {
    energia = energia personaje - daño
}  

eliminarHabilidad :: String -> Gema
eliminarHabilidad habilidadAQuitar personaje = personaje {
    habilidades = filter (/= habilidadAQuitar) (habilidades personaje) 
}

transportar :: String -> Gema
transportar nuevoPlaneta personaje = personaje {
    planeta = nuevoPlaneta
} 

acortarEdad :: Gema
acortarEdad personaje = personaje {
    edad = max 18 (edad personaje `div` 2)
}

restarHabilidades :: Gema
restarHabilidades personaje
 | cantidadDeHabilidades personaje <=2 = quitarTodasLasHabilidades personaje
 | otherwise = personaje

quitarTodasLasHabilidades :: Gema
quitarTodasLasHabilidades personaje = personaje {
    habilidades = []
}

laMente :: Number -> Gema
laMente = debilitar  

elAlma :: String -> Gema
elAlma habilidadAQuitar = debilitar 10 . eliminarHabilidad habilidadAQuitar 

elEspacio :: String -> Gema
elEspacio nuevoPlaneta = debilitar 20 . transportar nuevoPlaneta

elPoder :: Gema
elPoder personaje = restarHabilidades . debilitar (energia personaje) $ personaje

elTiempo :: Gema
elTiempo = debilitar 50 . acortarEdad

laGemaLoca :: Gema -> Gema
laGemaLoca gema = gema . gema

-- Punto 4
guanteleteDeGoma :: [Gema]
guanteleteDeGoma = [elTiempo, elAlma "usar Mjolnir", laGemaLoca (elAlma "programacion en Haskell")]

-- Punto 5
utilizar :: [Gema] -> Gema
utilizar gemas personaje = foldr ($) personaje gemas

-- Punto 6
-- gemaMasPoderosa :: Guantelete -> Personaje -> Personaje
-- gemaMasPoderosa guantelete personaje 
