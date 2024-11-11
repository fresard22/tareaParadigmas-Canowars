# Instrucciones de uso

## Compilacion

Para compilar y ejecutar este proyecto, necesitas tener instaladas las siguientes herramientas:

- **Haskell Platform** (incluye el compilador `GHC`)
- **Gloss**: Biblioteca para gráficos y animaciones.
- **Random**: Biblioteca para la generación de números aleatorios.

### Instalación de Librerías

Si no tienes **Gloss** y **Random** instaladas, puedes hacerlo de la siguiente manera:

1. Con **cabal**:

    ```bash
    cabal update
    cabal install gloss random
    ```

## Compilación y Ejecución

1. **Compilación**: Para compilar el proyecto, usa el comando `make` en la terminal:

    ```bash
    make
    ```

    Esto generará un archivo ejecutable llamado `main`.

2. **Ejecución**: Ejecuta el juego con el siguiente comando:

    ```bash
    ./main
    ```

## Dependencias

El proyecto utiliza las siguientes bibliotecas de Haskell:

- **Graphics.Gloss**: Proporciona herramientas para gráficos y animación, permitiendo la creación rápida de aplicaciones visuales en Haskell.
- **Graphics.Gloss.Interface.Pure.Game**: Ofrece herramientas adicionales para manejar la interacción del usuario en juegos, como eventos de teclado y ratón.
- **Data.List**: Módulo para manipulación de listas.
- **System.Random**: Proporciona funciones para la generación de números aleatorios, útiles para introducir variabilidad en el juego.
- **Graphics.Gloss** y **Graphics.Gloss.Interface.Pure.Game** se utilizan para gráficos y manejo de eventos en el juego.
- **System.Random** permite la creación de elementos aleatorios.