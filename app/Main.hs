module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

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

-- Estado del juego: posiciones y ángulos de los cañones de los tanques y proyectiles activos
data Estado = Estado
  { posTanqueIzq :: Float
  , posTanqueDer :: Float
  , anguloCanonIzq :: Float
  , anguloCanonDer :: Float
  , proyectiles :: [Proyectil]
  , vidaIzq :: Int   -- Vida del tanque izquierdo
  , vidaDer :: Int   -- Vida del tanque derecho
  }

-- Representación de un proyectil
data Proyectil = Proyectil
  { posX :: Float
  , posY :: Float
  , velX :: Float
  , velY :: Float
  } deriving (Eq)

-- Estado inicial
estadoInicial :: Estado
estadoInicial = Estado 
  { posTanqueIzq = -initialTankOffset
  , posTanqueDer = initialTankOffset
  , anguloCanonIzq = 0
  , anguloCanonDer = 0
  , proyectiles = []
  , vidaIzq = 30  -- Tanque izquierdo comienza con 30 puntos de vida
  , vidaDer = 30  -- Tanque derecho comienza con 30 puntos de vida
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

-- Función para dibujar la escena
dibujarEscena :: Estado -> Picture
dibujarEscena estado
--  | vidaIzq estado <= 0 || vidaDer estado <= 0 = renderFinDeJuego
  | otherwise = pictures
    [ dibujarTanque (posTanqueIzq estado) (anguloCanonIzq estado) green  -- Tanque izquierdo
    , dibujarTanque (posTanqueDer estado) (anguloCanonDer estado) blue   -- Tanque derecho
    , paredDivisoria
    , dibujarProyectiles (proyectiles estado)
    , dibujarVida estado
    ]

-- Función para dibujar la vida de los tanques
dibujarVida :: Estado -> Picture
dibujarVida estado = pictures
  [ translate (-fromIntegral windowWidth / 2 + 20) (fromIntegral windowHeight / 2 - 30) (text ("Vida: " ++ show (vidaIzq estado)))
  , translate (fromIntegral windowWidth / 2 - 100) (fromIntegral windowHeight / 2 - 30) (text ("Vida: " ++ show (vidaDer estado)))
  ]

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

-- Manejar eventos de teclado
manejarEvento :: Event -> Estado -> Estado
manejarEvento (EventKey (Char 'd') Down _ _) estado = estado { posTanqueIzq = posTanqueIzq estado + moveSpeed }
manejarEvento (EventKey (Char 'a') Down _ _) estado = estado { posTanqueIzq = posTanqueIzq estado - moveSpeed }
manejarEvento (EventKey (Char 'w') Down _ _) estado = estado { anguloCanonIzq = anguloCanonIzq estado - angleSpeed }
manejarEvento (EventKey (Char 's') Down _ _) estado = estado { anguloCanonIzq = anguloCanonIzq estado + angleSpeed }
manejarEvento (EventKey (Char 'l') Down _ _) estado = estado { posTanqueDer = posTanqueDer estado + moveSpeed }
manejarEvento (EventKey (Char 'j') Down _ _) estado = estado { posTanqueDer = posTanqueDer estado - moveSpeed }
manejarEvento (EventKey (Char 'i') Down _ _) estado = estado { anguloCanonDer = anguloCanonDer estado + angleSpeed }
manejarEvento (EventKey (Char 'k') Down _ _) estado = estado { anguloCanonDer = anguloCanonDer estado - angleSpeed }
-- Disparar proyectiles
manejarEvento (EventKey (Char 'p') Down _ _) estado = dispararProyectilDer estado
manejarEvento (EventKey (Char 'f') Down _ _) estado = dispararProyectilIzq estado
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
      }

actualizar :: Float -> Estado -> Estado
actualizar tiempo estado
--  | vidaIzq estado <= 0 || vidaDer estado <= 0 = estado  -- No actualizamos el juego si ha terminado
  | otherwise = estado
    { proyectiles = map (actualizarProyectil tiempo) (proyectiles estado)
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
procesarImpactos estado = foldl procesarImpacto estado (proyectiles estado)

-- Procesar un solo impacto
procesarImpacto :: Estado -> Proyectil -> Estado
procesarImpacto estado proyectil
  | impactoTanqueIzq = estado { vidaIzq = vidaIzq estado - 10, proyectiles = filtrarProyectiles estado (proyectiles estado) proyectil }
  | impactoTanqueDer = estado { vidaDer = vidaDer estado - 10, proyectiles = filtrarProyectiles estado (proyectiles estado) proyectil }
  | otherwise = estado
  where
    impactoTanqueIzq = colisionaConTanque (posTanqueIzq estado) proyectil
    impactoTanqueDer = colisionaConTanque (posTanqueDer estado) proyectil

-- Función para eliminar un proyectil de la lista si colisiona con un tanque
filtrarProyectiles :: Estado -> [Proyectil] -> Proyectil -> [Proyectil]
filtrarProyectiles estado proyectiles proyectilImpactado
  = filter (\p -> not (colisionaConTanque (posTanqueIzq estado) p || colisionaConTanque (posTanqueDer estado) p)) proyectiles

-- Función para verificar si un proyectil ha colisionado con un tanque
colisionaConTanque :: Float -> Proyectil -> Bool
colisionaConTanque posTanque proyectil = abs (posX proyectil - posTanque) < 30 && posY proyectil < groundLevel + 20

-- Función para mostrar fin de juego
renderFinDeJuego :: Picture
renderFinDeJuego = color red (text "¡Juego Terminado!")