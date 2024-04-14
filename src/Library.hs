module Library where
import PdePreludat


data Cancion = Cancion {
    nombre :: String,
    duracion :: Number,
    instrumentos :: [Instrumento]
} deriving (Show, Eq)


-- Canciones
patternMatching :: Cancion
patternMatching = Cancion "Pattern Matching" 4 [Guitarra, Bajo, Bateria]

seisDieciocho :: Cancion
seisDieciocho = Cancion "Seis dieciocho" 3 [Teclado, Guitarra]

laVidaEnHaskell :: Cancion
laVidaEnHaskell = Cancion "La vida en Haskell" 5 []

-- Instrumentos
data Instrumento = Guitarra | Bajo | Bateria | Teclado | Saxofon deriving (Show, Eq)


-- Aceptación
aceptacion :: Cancion -> Number
aceptacion cancion
    | head (nombre cancion) == 'M' = 500
    | even (duracion cancion) = length (nombre cancion) * 10
    | esAcapella cancion = 10
    | otherwise = 0


-- Repertorio
type Repertorio = [Cancion]
repertorio :: Repertorio
repertorio = [patternMatching, seisDieciocho, laVidaEnHaskell, melodiasFuncionales, haskellEsAmor]


-- SE PIDE --
-- 1) Definir al menos 2 canciones más para la banda y agregarlas al repertorio.
melodiasFuncionales :: Cancion
melodiasFuncionales = Cancion "Melodias Funcionales" 2 [Guitarra]

haskellEsAmor :: Cancion
haskellEsAmor = Cancion "Haskell es amor" 6 [Saxofon, Bajo, Bateria]


-- 2) PdePop tiene la costumbre de tocar sus canciones por orden alfabético.
--    Dadas dos canciones, determinar cuál viene antes en el repertorio.
vieneAntes :: Cancion -> Cancion -> Cancion
vieneAntes cancion1 cancion2
    | nombre cancion1 < nombre cancion2 = cancion1
    | otherwise = cancion2


-- 3) Determinar si una canción es acapella.
esAcapella :: Cancion -> Bool
esAcapella cancion = null (instrumentos cancion)


-- 4) Averiguar si una canción es aceptada por el público, esto ocurre cuando su índice de aceptación es mayor a 60. 
esAceptada :: Cancion -> Bool
esAceptada cancion = aceptacion cancion > 60


-- 5) Dado un instrumento y una canción, determinar si la canción necesita al instrumento para ser interpretada. 
llevaInstrumento :: Instrumento -> Cancion -> Bool
llevaInstrumento _ (Cancion _ _ []) = False
llevaInstrumento instrumento cancion = elem instrumento (instrumentos cancion)


-- 6) Tocar una canción, esto implica que, si la canción es aceptada por el público, se la toca tal cual es,
--    en caso contrario, se la toca con la duración reducida a la mitad.
tocar :: Cancion -> Cancion
tocar cancion
    | esAceptada cancion = cancion
    | otherwise = cancion {duracion = duracion cancion / 2}