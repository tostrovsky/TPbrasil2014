import Text.Show.Functions()
-- import Data.Char
-- import Data.List (genericLength)

type Caracteristica = Jugador -> Bool

data Jugador = Jugador{
    nombre :: String,
    edad :: Int,
    promedioDeGol :: Double,
    habilidad :: Int,
    cansancio :: Double
} deriving (Show, Eq)

data Equipo = Equipo {
    nombreEquipo :: String,
    grupo :: Char,
    jugadores :: [Jugador]
} deriving Show

{-
martin = Jugador "Martin" 26 0.0 50 35.0
juan = Jugador "Juancho" 30 0.2 50 40.0
maxi = Jugador "Maxi Lopez" 27 0.4 68 30.0

jonathan = Jugador "Chueco" 20 1.5 80 99.0
lean = Jugador "Hacha" 23 0.01 50 35.0
brian = Jugador "Panadero" 21 5 80 15.0

garcia = Jugador "Sargento" 30 1 80 13.0
messi = Jugador "Pulga" 26 10 99 43.0
aguero = Jugador "Aguero" 24 5 90 5.0

equipo1 = Equipo "Lo Que Vale Es El Intento" 'F' [martin, juan, maxi]
losDeSiempre = Equipo "Los De Siempre" 'F' [jonathan, lean, brian]
restoDelMundo = Equipo "Resto del Mundo" 'A' [garcia, messi, aguero]

-}
quickSort :: (a -> a -> Bool) -> [a] -> [a]
quickSort _ [] = [] 
quickSort criterio (x:xs) = (quickSort criterio . filter (not . criterio x)) xs ++ [x] ++ (quickSort criterio . filter (criterio x)) xs


--Punto 1
granHabilidad :: Caracteristica 
granHabilidad = (>75).habilidad 

granPromedio :: Caracteristica 
granPromedio = (>0).promedioDeGol

esFigura :: Caracteristica
esFigura unJugador = granPromedio unJugador && granHabilidad unJugador  

figuras :: Equipo -> [Jugador]
figuras = filter esFigura.jugadores
 
--Punto 2
jugadoresFaranduleros :: [String]
jugadoresFaranduleros = ["Maxi Lopez", "Icardi", "Aguero", "Caniggia", "Demichelis"]

esJugadorFarandulero :: Caracteristica
esJugadorFarandulero unJugador = elem (nombre unJugador) jugadoresFaranduleros

tieneFarandulero :: Equipo -> Bool
tieneFarandulero = (>0).length.filter esJugadorFarandulero. jugadores

--Punto 3
esJoven :: Caracteristica
esJoven = (<27).edad

esDificil :: Caracteristica
esDificil unJugador= esFigura unJugador && esJoven unJugador && not (esJugadorFarandulero unJugador) 

filtrarEquiposPorGrupo :: [Equipo] -> Char -> [Equipo]
filtrarEquiposPorGrupo equipos unGrupo = filter ((== unGrupo).grupo) equipos  

jugadoresDificiles :: [Equipo] -> Char -> [Jugador]
jugadoresDificiles equipos unGrupo= concat (map jugadores (filtrarEquiposPorGrupo equipos unGrupo))

--Punto 4
cambiarCansancio :: Double -> Jugador -> Jugador
cambiarCansancio valor unJugador = unJugador {cansancio = valor}


modificarJugador :: Jugador -> Jugador
modificarJugador unJugador
    | not (esJugadorFarandulero unJugador) && esJoven unJugador && esFigura unJugador = cambiarCansancio 50 unJugador 
    | esJoven unJugador = cambiarCansancio (cansancio unJugador *1.1) unJugador
    | esFigura unJugador = cambiarCansancio (cansancio unJugador +20) unJugador
    | otherwise = cambiarCansancio (cansancio unJugador *2) unJugador

modificarEquipo :: [Jugador] -> [Jugador]
modificarEquipo = map modificarJugador 

jugarPartido :: Equipo -> Equipo
jugarPartido unEquipo = unEquipo {jugadores = modificarEquipo (jugadores unEquipo)}

--Punto 5
segunPromedio :: Jugador -> Caracteristica
segunPromedio unJugador otroJugador= promedioDeGol unJugador > promedioDeGol otroJugador

losQueJuegan :: Equipo -> [Jugador]
losQueJuegan = take 11.quickSort (segunPromedio).jugadores

losQueNoJuegan :: Equipo -> [Jugador]
losQueNoJuegan = drop 11.quickSort (segunPromedio).jugadores 

promedioLosQueJuegan :: Equipo -> Double
promedioLosQueJuegan = sum.map promedioDeGol. losQueJuegan

ganaPrimero :: (Equipo, Equipo) -> Bool
ganaPrimero (primero, segundo) = promedioLosQueJuegan primero > promedioLosQueJuegan segundo

enfrentarEquipos :: (Equipo, Equipo) -> Equipo
enfrentarEquipos (primero, segundo)
    | ganaPrimero (primero, segundo) = primero
    | otherwise = segundo

cansarJugadores :: Equipo -> Equipo
cansarJugadores unEquipo = unEquipo {jugadores= (modificarEquipo (losQueJuegan unEquipo)) ++ (losQueNoJuegan unEquipo)  } 

equipoGanador :: Equipo -> Equipo -> Equipo
equipoGanador primero segundo = cansarJugadores (enfrentarEquipos (primero, segundo))

--Punto 6
torneo :: [Equipo] -> Equipo
torneo (equipoUnico:[]) = equipoUnico
torneo (equipoUnico: equipos) = equipoGanador equipoUnico (torneo equipos)

torneo' :: [Equipo] -> Equipo
torneo' (equipoUnico:[]) = equipoUnico 
torneo' equipos = equipoGanador (head equipos) (torneo' (tail equipos))   

--Punto 7 

jugadorMasGroso :: [Equipo] -> String
jugadorMasGroso = nombre.head.figuras.torneo' 


{-  La funcion quickSort es de orden superior porque criterio es una
funcion que parametriza a cualquiera del tipo (a -> a -> Bool).
Todos los filters tambien lo son.

    Gracias a la Lazy Evaluation, si la lista de jugadores fuese infinita,
los torneos se jugarían igual. Sin embargo, la funcion quickSort nunca
terminaría de ordenar a los jugadores por orden de promedio de goles
(es ansiosa), por lo cual nunca se sabrá qué jugadores jugarán cada partido. 
A demás, el banco de suplentes quedaría chico.
-}