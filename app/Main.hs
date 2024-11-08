module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.List

-- Tamaño de la ventana
windowWidth, windowHeight :: Int
windowWidth = 800
windowHeight = 600

-- Offset inicial de los tanques respecto al centro de la ventana
initialTankOffset :: Float
initialTankOffset = 300

-- Posición vertical para colocar los tanques al nivel del "piso"
groundLevel :: Float
groundLevel = - fromIntegral windowHeight / 2 + 20

-- Velocidad de movimiento de los tanques y ángulo de rotación del cañón
moveSpeed, angleSpeed :: Float
moveSpeed = 10
angleSpeed = 5

-- Velocidad inicial de los proyectiles y gravedad
initialProjectileSpeed :: Float
initialProjectileSpeed = 220
gravity :: Float
gravity = 70

data Turno = Izq | Der deriving (Eq)

-- Estado del juego: ahora incluye los rectángulos de gasolina
data Estado = Estado
  { posTanqueIzq     :: Float
  , posTanqueDer     :: Float
  , anguloCanonIzq   :: Float
  , anguloCanonDer   :: Float
  , proyectiles      :: [Proyectil]
  , vidaIzq          :: Int
  , vidaDer          :: Int
  , gasolinaIzq      :: Int
  , gasolinaDer      :: Int
  , turno            :: Turno  -- Agregado para saber de quién es el turno
  }

-- Representación de un proyectil
data Proyectil = Proyectil
  { posX :: Float
  , posY :: Float
  , velX :: Float
  , velY :: Float
  , haImpactado :: Bool  -- Nuevo campo para indicar si el proyectil ha impactado
  } deriving (Eq)

-- Representación de un rectángulo de gasolina
data Gasolina = Gasolina
  { posGasolinaX :: Float
  , posGasolinaY :: Float
  }

-- Estado inicial
estadoInicial :: Estado
estadoInicial = Estado 
  { posTanqueIzq = -initialTankOffset
  , posTanqueDer = initialTankOffset
  , anguloCanonIzq = 0
  , anguloCanonDer = 0
  , proyectiles = []
  , vidaIzq = 30
  , vidaDer = 30
  , gasolinaIzq = 10
  , gasolinaDer = 10
  , turno = Izq  -- El tanque izquierdo empieza
  }

-- Función principal
main :: IO ()
main = play
    (InWindow "CANOWARS" (windowWidth, windowHeight) (100, 100))
    white
    60
    estadoInicial
    dibujarEscena
    manejarEvento
    actualizar

dibujarEscena :: Estado -> Picture
dibujarEscena estado
  | otherwise = pictures
    [ dibujarTanque (posTanqueIzq estado) (anguloCanonIzq estado) green
    , dibujarTanque (posTanqueDer estado) (anguloCanonDer estado) blue
    , paredDivisoria
    , dibujarProyectiles (proyectiles estado)
    , dibujarVida estado
    , dibujarTurno (turno estado)  -- Indicador del turno actual
    ]

dibujarTurno :: Turno -> Picture
dibujarTurno Izq = translate (- fromIntegral windowWidth / 2 + 20) (fromIntegral windowHeight / 2 - 50) (color green (text "Turno del Tanque Izquierdo"))
dibujarTurno Der = translate (- fromIntegral windowWidth / 2 + 20) (fromIntegral windowHeight / 2 - 50) (color blue (text "Turno del Tanque Derecho"))

-- Función para dibujar un tanque en una posición dada con un ángulo para el cañón
dibujarTanque :: Float -> Float -> Color -> Picture
dibujarTanque x angulo colorTanque = translate x groundLevel (pictures [cuerpoTanque, canonRotado])
  where
    cuerpoTanque = color colorTanque (rectangleSolid 60 40)
    canonRotado  = rotate angulo (translate (if colorTanque == green then 30 else -30) 0 (color black (rectangleSolid 40 10)))

-- Función para dibujar la pared divisoria
paredDivisoria :: Picture
paredDivisoria = translate 0 (- fromIntegral windowHeight / 3) (color (greyN 0.5) (rectangleSolid 5 (fromIntegral windowHeight / 2)))

-- Función para dibujar todos los proyectiles
dibujarProyectiles :: [Proyectil] -> Picture
dibujarProyectiles = pictures . map dibujarProyectil

-- Función para dibujar un proyectil individual
dibujarProyectil :: Proyectil -> Picture
dibujarProyectil proyectil = translate (posX proyectil) (posY proyectil) (color red (circleSolid 5))

-- Función para dibujar la vida y gasolina de los tanques
dibujarVida :: Estado -> Picture
dibujarVida estado = pictures
  [ translate (-fromIntegral windowWidth / 2 + 20) (fromIntegral windowHeight / 2 - 30) (text ("Vida: " ++ show (vidaIzq estado)))
  , translate (-fromIntegral windowWidth / 2 + 20) (fromIntegral windowHeight / 2 - 60) (text ("Gasolina: " ++ show (gasolinaIzq estado)))
  , translate (fromIntegral windowWidth / 2 - 100) (fromIntegral windowHeight / 2 - 30) (text ("Vida: " ++ show (vidaDer estado)))
  , translate (fromIntegral windowWidth / 2 - 100) (fromIntegral windowHeight / 2 - 60) (text ("Gasolina: " ++ show (gasolinaDer estado)))
  ]

-- Manejar eventos de teclado
manejarEvento :: Event -> Estado -> Estado
manejarEvento (EventKey (Char 'd') Down _ _) estado
  | turno estado == Izq && gasolinaIzq estado > 0 = estado { posTanqueIzq = posTanqueIzq estado + moveSpeed, gasolinaIzq = gasolinaIzq estado - 1 }
  | turno estado == Der && gasolinaDer estado > 0 = estado { posTanqueDer = posTanqueDer estado + moveSpeed, gasolinaDer = gasolinaDer estado - 1 }
  | otherwise = estado
manejarEvento (EventKey (Char 'a') Down _ _) estado
  | turno estado == Izq && gasolinaIzq estado > 0 = estado { posTanqueIzq = posTanqueIzq estado - moveSpeed, gasolinaIzq = gasolinaIzq estado - 1 }
  | turno estado == Der && gasolinaDer estado > 0 = estado { posTanqueDer = posTanqueDer estado - moveSpeed, gasolinaDer = gasolinaDer estado - 1 }
  | otherwise = estado
manejarEvento (EventKey (Char 'w') Down _ _) estado
  | turno estado == Izq && gasolinaIzq estado > 0 = estado { anguloCanonIzq = anguloCanonIzq estado - angleSpeed, gasolinaIzq = gasolinaIzq estado - 1 }
  | turno estado == Der && gasolinaDer estado > 0 = estado { anguloCanonDer = anguloCanonDer estado + angleSpeed, gasolinaDer = gasolinaDer estado - 1 }
  | otherwise = estado
manejarEvento (EventKey (Char 's') Down _ _) estado
  | turno estado == Izq && gasolinaIzq estado > 0 = estado { anguloCanonIzq = anguloCanonIzq estado + angleSpeed, gasolinaIzq = gasolinaIzq estado - 1 }
  | turno estado == Der && gasolinaDer estado > 0 = estado { anguloCanonDer = anguloCanonDer estado - angleSpeed, gasolinaDer = gasolinaDer estado - 1 }
  | otherwise = estado
manejarEvento (EventKey (Char 'f') Down _ _) estado
  | turno estado == Izq && gasolinaIzq estado > 0 = dispararProyectilIzq estado
  | turno estado == Der && gasolinaDer estado > 0 = dispararProyectilDer estado
  | otherwise = estado
manejarEvento _ estado = estado

-- Crear y añadir un proyectil desde el tanque izquierdo
dispararProyectilIzq :: Estado -> Estado
dispararProyectilIzq estado = estado { proyectiles = proyectil : proyectiles estado }
  where
    anguloRad = anguloCanonIzq estado * pi / 180
    proyectil = Proyectil 
      { posX = posTanqueIzq estado + 30 * cos anguloRad
      , posY = groundLevel - 30 * sin anguloRad
      , velX = initialProjectileSpeed * cos anguloRad
      , velY = -initialProjectileSpeed * sin anguloRad
      , haImpactado = False
      }

-- Crear y añadir un proyectil desde el tanque derecho
dispararProyectilDer :: Estado -> Estado
dispararProyectilDer estado = estado { proyectiles = proyectil : proyectiles estado }
  where
    anguloRad = anguloCanonDer estado * pi / 180
    proyectil = Proyectil 
      { posX = posTanqueDer estado - 30 * cos anguloRad  -- Ajuste para la posición inicial
      , posY = groundLevel + 30 * sin anguloRad
      , velX = -initialProjectileSpeed * cos anguloRad   -- Velocidad negativa para dirección izquierda
      , velY = initialProjectileSpeed * sin anguloRad
      , haImpactado = False
      }

actualizar :: Float -> Estado -> Estado
actualizar tiempo estado
  | gasolinaIzq estado <= 0 && turno estado == Izq = estado { turno = Der, gasolinaIzq = 10, gasolinaDer = gasolinaDer estado }  -- El tanque izquierdo terminó su turno
  | gasolinaDer estado <= 0 && turno estado == Der = estado { turno = Izq, gasolinaIzq = gasolinaIzq estado, gasolinaDer = 10 }  -- El tanque derecho terminó su turno
  | otherwise = estado { 
      proyectiles = map (actualizarProyectil tiempo) (proyectiles estado)
    , vidaIzq = vidaIzq (procesarImpactos estado)
    , vidaDer = vidaDer (procesarImpactos estado)
  }
-- Actualizar la posición de un proyectil considerando la gravedad
actualizarProyectil :: Float -> Proyectil -> Proyectil
actualizarProyectil tiempo proyectil = proyectil
  { posX = posX proyectil + velX proyectil * tiempo
  , posY = posY proyectil + velY proyectil * tiempo - 0.5 * gravity * tiempo ^ 2
  , velY = velY proyectil - gravity * tiempo
  }

-- Procesar impactos de los proyectiles con los tanques
procesarImpactos :: Estado -> Estado
procesarImpactos estado = estado
  { vidaIzq = vidaIzqActualizada
  , vidaDer = vidaDerActualizada
  , proyectiles = proyectilesActualizados
  }
  where
    -- Filtrar proyectiles que colisionan con cada tanque y no han impactado aún
    (impactosIzq, proyectilesRestantes) = partition (\p -> colisionaConTanque (posTanqueIzq estado) p && not (haImpactado p)) (proyectiles estado)
    (impactosDer, proyectilesFinales) = partition (\p -> colisionaConTanque (posTanqueDer estado) p && not (haImpactado p)) proyectilesRestantes

    -- Vida actualizada considerando solo un impacto de 10 puntos por proyectil
    vidaIzqActualizada = vidaIzq estado - (10 * length impactosIzq)
    vidaDerActualizada = vidaDer estado - (10 * length impactosDer)

    -- Actualizamos `haImpactado` para todos los proyectiles que colisionaron
    proyectilesActualizados = map (\p -> if p `elem` (impactosIzq ++ impactosDer)
                                         then p { haImpactado = True }
                                         else p) proyectilesFinales

-- Función para eliminar un proyectil de la lista si colisiona con un tanque
filtrarProyectiles :: Estado -> [Proyectil] -> Proyectil -> [Proyectil]
filtrarProyectiles estado proyectiles proyectilImpactado
  = filter (\p -> not (colisionaConTanque (posTanqueIzq estado) p || colisionaConTanque (posTanqueDer estado) p)) proyectiles

-- Función para verificar si un proyectil ha colisionado con un tanque
colisionaConTanque :: Float -> Proyectil -> Bool
colisionaConTanque posTanque proyectil = 
    let rangoColisionHorizontal = 30    -- Rango de colisión en el eje X (ajusta según el tamaño del tanque)
        rangoColisionVertical = 20      -- Rango de colisión en el eje Y (ajusta según la altura del tanque)
    in abs (posX proyectil - posTanque) < rangoColisionHorizontal
       && posY proyectil >= groundLevel - rangoColisionVertical
       && posY proyectil <= groundLevel + rangoColisionVertical


-- Función para mostrar fin de juego
renderFinDeJuego :: Picture
renderFinDeJuego = color red (text "¡Juego Terminado!")