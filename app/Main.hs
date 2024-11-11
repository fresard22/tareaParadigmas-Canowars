module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.List
import System.Random (randomR, mkStdGen)


-- Tamaño de la ventana
windowWidth, windowHeight :: Int
windowWidth = 1280
windowHeight = 720

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
gravity = 60

-- Estado del juego: posiciones y ángulos de los cañones de los tanques y proyectiles activos
data Estado = Estado
  { posTanqueIzq :: Float
  , posTanqueDer :: Float
  , anguloCanonIzq :: Float
  , anguloCanonDer :: Float
  , proyectiles :: [Proyectil]
  , vidaIzq :: Int      -- Vida del tanque izquierdo
  , vidaDer :: Int      -- Vida del tanque derecho
  , gasolinaIzq :: Int  -- Gasolina del tanque izquierdo
  , gasolinaDer :: Int  -- Gasolina del tanque derecho
  , juegoTerminado :: Bool
  }

-- Representación de un proyectil
data Proyectil = Proyectil
  { posX :: Float       -- Posición horizontal del proyectil
  , posY :: Float       -- Posición vertical del proyectil
  , velX :: Float       -- Velocidad horizontal del proyectil
  , velY :: Float       -- Velocidad vertical del proyectil
  , haImpactado :: Bool  -- Nuevo campo para indicar si el proyectil ha impactado
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
  , gasolinaIzq = 100  -- Tanque izquierdo comienza con 100 puntos de gasolina
  , gasolinaDer = 100  -- Tanque derecho comienza con 100 puntos de gasolina
  , juegoTerminado = False
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
  | juegoTerminado estado = renderFinDeJuego estado
  | otherwise = pictures
      [ dibujarTanque (posTanqueIzq estado) (anguloCanonIzq estado) green
      , dibujarTanque (posTanqueDer estado) (anguloCanonDer estado) blue
      , paredDivisoria
      , dibujarProyectiles (proyectiles estado)
      , dibujarVida estado
      ]

-- Renderizar el mensaje de fin de juego, mostrando si fue empate o quién ganó
renderFinDeJuego :: Estado -> Picture
renderFinDeJuego estado = 
  pictures 
    [ translate (-300) 0 $ color red $
        if (vidaIzq estado <= 0 && vidaDer estado <= 0) || (gasolinaIzq estado == 0 && gasolinaDer estado == 0)
          then scale 0.5 0.5 (text "¡Empate!")
          else if vidaIzq estado <= 0
            then scale 0.5 0.5 (color blue (text "¡Tanque Derecho Gana!"))
            else scale 0.5 0.5 (color green (text "¡Tanque Izquierdo Gana!"))
    , translate (-200) (-50) $ color black $ scale 0.3 0.3 (text "Presiona 'R' para reiniciar")
    ]


-- Agrega una función para reiniciar el juego
reiniciarJuego :: Estado -> Estado
reiniciarJuego _ = estadoInicial

-- Función para dibujar un tanque en una posición dada con un ángulo para el cañón
dibujarTanque :: Float -> Float -> Color -> Picture
dibujarTanque x angulo colorTanque = translate x groundLevel (pictures [cuerpoTanque, canonRotado])
  where
    cuerpoTanque = color colorTanque (rectangleSolid 60 40)
    canonRotado  = rotate angulo (translate (if colorTanque == green then 30 else -30) 0 (color black (rectangleSolid 40 10)))

-- Función para dibujar la pared divisoria
paredDivisoria :: Picture
paredDivisoria = translate 0 (groundLevel+124) (color (greyN 0.5) (rectangleSolid 30 (fromIntegral windowHeight / 2.5)))

-- Ancho de la pared divisoria
anchoParedDivisoria :: Float
anchoParedDivisoria = 30 / 2  -- Mitad del ancho de la pared

-- Altura de la pared divisoria
alturaParedDivisoria :: Float
alturaParedDivisoria = fromIntegral windowHeight / 2.5

-- Función de colisión mejorada
colisionaConParedDivisoria :: Proyectil -> Bool
colisionaConParedDivisoria proyectil =
    abs (posX proyectil) <= anchoParedDivisoria &&  -- Dentro del rango horizontal de la pared
    posY proyectil <= groundLevel + alturaParedDivisoria  -- Solo detecta colisión si está debajo de la parte superior de la pared





-- Función para dibujar todos los proyectiles
dibujarProyectiles :: [Proyectil] -> Picture
dibujarProyectiles = pictures . map dibujarProyectil

-- Función para dibujar un proyectil individual
dibujarProyectil :: Proyectil -> Picture
dibujarProyectil proyectil = translate (posX proyectil) (posY proyectil) (color red (circleSolid 5))

-- Función para dibujar la vida y gasolina de los tanques
dibujarVida :: Estado -> Picture
dibujarVida estado = pictures
  [ -- Vida y gasolina del tanque izquierdo en la esquina superior izquierda
    translate (-fromIntegral windowWidth / 2 + 20) (fromIntegral windowHeight / 2 - 30) 
        (scale 0.2 0.2 $ color green (text ("Vida: " ++ show (vidaIzq estado)))),
    translate (-fromIntegral windowWidth / 2 + 20) (fromIntegral windowHeight / 2 - 60) 
        (scale 0.2 0.2 $ color green (text ("Gasolina: " ++ show (gasolinaIzq estado)))),
    -- Vida y gasolina del tanque derecho en la esquina superior derecha
    translate (fromIntegral windowWidth / 2 - 180) (fromIntegral windowHeight / 2 - 30) 
        (scale 0.2 0.2 $ color blue (text ("Vida: " ++ show (vidaDer estado)))),
    translate (fromIntegral windowWidth / 2 - 180) (fromIntegral windowHeight / 2 - 60) 
        (scale 0.2 0.2 $ color blue (text ("Gasolina: " ++ show (gasolinaDer estado))))
  ]


-- Límite de movimiento para cada tanque
limiteTanqueIzq :: Float
limiteTanqueIzq = -anchoParedDivisoria - 30  -- Ajusta según el tamaño de tu tanque

limiteTanqueDer :: Float
limiteTanqueDer = anchoParedDivisoria + 30  -- Ajusta según el tamaño de tu tanque


-- Nuevos límites para los movimientos hacia atrás de los tanques
limiteTanqueIzqRetroceder :: Float
limiteTanqueIzqRetroceder = - fromIntegral windowWidth / 2 + 30  -- Límite izquierdo considerando el tamaño del tanque

limiteTanqueDerRetroceder :: Float
limiteTanqueDerRetroceder = fromIntegral windowWidth / 2 - 30  -- Límite derecho considerando el tamaño del tanque


-- Manejar eventos de teclado para ambos tanques
manejarEvento :: Event -> Estado -> Estado

-- Eventos para el tanque izquierdo
manejarEvento (EventKey (Char 'd') Down _ _) estado
  | gasolinaIzq estado > 0 && posTanqueIzq estado + moveSpeed < limiteTanqueIzq
      = estado { posTanqueIzq = posTanqueIzq estado + moveSpeed, gasolinaIzq = gasolinaIzq estado - 1 }
  | otherwise = estado

manejarEvento (EventKey (Char 'a') Down _ _) estado
  | gasolinaIzq estado > 0 && posTanqueIzq estado - moveSpeed > limiteTanqueIzqRetroceder
      = estado { posTanqueIzq = posTanqueIzq estado - moveSpeed, gasolinaIzq = gasolinaIzq estado - 1 }
  | otherwise = estado

manejarEvento (EventKey (Char 'w') Down _ _) estado
  | gasolinaIzq estado > 0
      = estado { anguloCanonIzq = anguloCanonIzq estado - angleSpeed, gasolinaIzq = gasolinaIzq estado - 1 }
  | otherwise = estado

manejarEvento (EventKey (Char 's') Down _ _) estado
  | gasolinaIzq estado > 0
      = estado { anguloCanonIzq = anguloCanonIzq estado + angleSpeed, gasolinaIzq = gasolinaIzq estado - 1 }
  | otherwise = estado

  

-- Eventos para el tanque derecho
manejarEvento (EventKey (Char 'j') Down _ _) estado
  | gasolinaDer estado > 0 && posTanqueDer estado - moveSpeed > limiteTanqueDer
      = estado { posTanqueDer = posTanqueDer estado - moveSpeed, gasolinaDer = gasolinaDer estado - 1 }
  | otherwise = estado

manejarEvento (EventKey (Char 'l') Down _ _) estado
  | gasolinaDer estado > 0 && posTanqueDer estado + moveSpeed < limiteTanqueDerRetroceder
      = estado { posTanqueDer = posTanqueDer estado + moveSpeed, gasolinaDer = gasolinaDer estado - 1 }
  | otherwise = estado

manejarEvento (EventKey (Char 'i') Down _ _) estado
  | gasolinaDer estado > 0
      = estado { anguloCanonDer = anguloCanonDer estado + angleSpeed, gasolinaDer = gasolinaDer estado - 1 }
  | otherwise = estado

manejarEvento (EventKey (Char 'k') Down _ _) estado
  | gasolinaDer estado > 0
      = estado { anguloCanonDer = anguloCanonDer estado - angleSpeed, gasolinaDer = gasolinaDer estado - 1 }
  | otherwise = estado




-- Disparar proyectiles si hay gasolina
manejarEvento (EventKey (Char 'p') Down _ _) estado
  | gasolinaDer estado > 0 = dispararProyectilDer estado { gasolinaDer = gasolinaDer estado - 1 }
  | otherwise = estado

manejarEvento (EventKey (Char 'f') Down _ _) estado
  | gasolinaIzq estado > 0 = dispararProyectilIzq estado { gasolinaIzq = gasolinaIzq estado - 1 }
  | otherwise = estado

manejarEvento (EventKey (Char 'r') Down _ _) estado
  | juegoTerminado estado = reiniciarJuego estado
manejarEvento evento estado
  | not (juegoTerminado estado) = manejarEventoJuego evento estado
  | otherwise = estado

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

-- Usamos fmap para aplicar actualizarProyectil a cada proyectil en la lista
actualizar :: Float -> Estado -> Estado
actualizar tiempo estado
  | juegoTerminado estado = estado
  | vidaIzq estado <= 0 || vidaDer estado <= 0 || (gasolinaIzq estado == 0 && gasolinaDer estado == 0) = 
      estado { juegoTerminado = True }  -- El juego termina en empate si ambos tanques se quedan sin gasolina
  | otherwise = estado
      { proyectiles = proyectilesFinales
      , vidaIzq = vidaIzqActualizada
      , vidaDer = vidaDerActualizada
      }
  where
    -- Usamos fmap para actualizar todos los proyectiles
    proyectilesActualizados = fmap (actualizarProyectil tiempo) (proyectiles estado)
    
    (vidaIzqActualizada, vidaDerActualizada, proyectilesFinales) = procesarImpactos estado


-- Actualizar posición de proyectiles con lógica de impacto
actualizarProyectil :: Float -> Proyectil -> Proyectil
actualizarProyectil tiempo p
  | haImpactado p = p
  | colisionaConParedDivisoria p = p { haImpactado = True }  -- Marcar como impactado si colisiona con la pared divisoria
  | otherwise = p 
      { posX = posX p + velX p * tiempo
      , posY = posY p + velY p * tiempo - 0.5 * gravity * tiempo ** 2
      , velY = velY p - gravity * tiempo
      }

-- Procesar impactos de los proyectiles
procesarImpactos :: Estado -> (Int, Int, [Proyectil])
procesarImpactos estado = (vidaIzqNueva, vidaDerNueva, proyectilesFinales)
  where
    -- Proyectiles que impactan al tanque izquierdo
    (impactosIzq, proyectilesRestantes) = partition (\p -> colisionaConTanque (posTanqueIzq estado) p && not (haImpactado p)) (map (actualizarProyectil 0.1) (proyectiles estado))
    
    -- Proyectiles que impactan al tanque derecho
    (impactosDer, proyectilesRestantes2) = partition (\p -> colisionaConTanque (posTanqueDer estado) p && not (haImpactado p)) proyectilesRestantes

    -- Filtrar proyectiles que no han impactado ni con tanques ni con la pared divisoria
    proyectilesFinales = filter (not . haImpactado) proyectilesRestantes2

    -- Función para calcular el daño de cada proyectil con probabilidad de daño crítico
    calculateDamage :: Int -> Int -> Int
    calculateDamage baseDamage seed =
      let gen = mkStdGen seed
          (prob, gen') = randomR (0 :: Int, 99 :: Int) gen  -- Para calcular probabilidad de crítico
          (critDamage, _) = randomR (7 :: Int, 9 :: Int) gen'  -- Daño crítico aleatorio entre 7 y 9
      in if prob < 5
            then critDamage  -- Aplicar un daño crítico entre 7 y 9
            else baseDamage  -- Si no es crítico, usar el daño base
      -- La semilla lamentablemente se reinicia por partida y no por tiro, ya que al querer usar un metodo
      -- impuro para la generacion de numeros aleatorios con randomRIO, al usar la libreria gloss nos daba problemas en compilacion,
      -- ya que la libreria Gloss trabaja con datos inmutables a comparacion de como lo es randomRIO (dinamico),
      -- por este motivo decidimos usar un metodo puro como StdGen para evitar problemas, la "desventaja de esto" es que
      -- el daño critico por partida va a ser el mismo si un proyectil realiza un daño critico.

    -- Usar el índice de cada proyectil como semilla para generar valores aleatorios diferentes
    dañosIzq = zipWith (\p idx -> calculateDamage 3 idx) impactosIzq [1..]
    dañosDer = zipWith (\p idx -> calculateDamage 3 idx) impactosDer [1..]

    -- Actualizar vida de los tanques en función de los impactos
    vidaIzqNueva = vidaIzq estado - sum dañosIzq
    vidaDerNueva = vidaDer estado - sum dañosDer


-- Función para eliminar un proyectil de la lista si colisiona con un tanque
filtrarProyectiles :: Estado -> [Proyectil] -> Proyectil -> [Proyectil]
filtrarProyectiles estado proyectiles proyectilImpactado
  = filter (\p -> not (colisionaConTanque (posTanqueIzq estado) p || colisionaConTanque (posTanqueDer estado) p)) proyectiles

-- Colisiones con tanques
colisionaConTanque :: Float -> Proyectil -> Bool
colisionaConTanque posTanque p = abs (posX p - posTanque) < 30 && posY p < groundLevel + 20 && posY p > groundLevel - 20


-- Función para verificar si un tanque está colisionando con la pared divisoria
colisionaConParedTanque :: Float -> Bool
colisionaConParedTanque posTanque =
    abs posTanque <= anchoParedDivisoria + 30  -- Checa si el tanque está a la izquierda o derecha de la pared, a su ancho más el del tanque.

-- Procesa los eventos del juego normal, evitando que los tanques pasen la pared
manejarEventoJuego :: Event -> Estado -> Estado
manejarEventoJuego (EventKey (Char 'd') Down _ _) estado
  | gasolinaIzq estado > 0 && not (colisionaConParedTanque (posTanqueIzq estado + moveSpeed)) = 
      estado { posTanqueIzq = posTanqueIzq estado + moveSpeed, gasolinaIzq = gasolinaIzq estado - 1 }
  | otherwise = estado

manejarEventoJuego (EventKey (Char 'a') Down _ _) estado
  | gasolinaIzq estado > 0 && not (colisionaConParedTanque (posTanqueIzq estado - moveSpeed)) = 
      estado { posTanqueIzq = posTanqueIzq estado - moveSpeed, gasolinaIzq = gasolinaIzq estado - 1 }
  | otherwise = estado

manejarEventoJuego (EventKey (Char 'l') Down _ _) estado
  | gasolinaDer estado > 0 && not (colisionaConParedTanque (posTanqueDer estado + moveSpeed)) = 
      estado { posTanqueDer = posTanqueDer estado + moveSpeed, gasolinaDer = gasolinaDer estado - 1 }
  | otherwise = estado

manejarEventoJuego (EventKey (Char 'j') Down _ _) estado
  | gasolinaDer estado > 0 && not (colisionaConParedTanque (posTanqueDer estado - moveSpeed)) = 
      estado { posTanqueDer = posTanqueDer estado - moveSpeed, gasolinaDer = gasolinaDer estado - 1 }
  | otherwise = estado

manejarEventoJuego _ estado = estado
