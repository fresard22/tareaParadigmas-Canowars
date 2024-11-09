# Nombre del archivo fuente
SRC = Main.hs

# Nombre del archivo ejecutable
EXEC = main

# Opciones de compilación
GHC_OPTS = -threaded -rtsopts -with-rtsopts=-N

# Objetivo principal: compilar el archivo Main.hs
$(EXEC): $(SRC)
	ghc $(GHC_OPTS) -o $(EXEC) $(SRC)

# Ejecutar el programa
run: $(EXEC)
	./$(EXEC)

# Limpiar archivos generados
clean:
	rm -f $(EXEC) *.hi *.o

# Limpiar y recompilar
rebuild: clean $(EXEC)

.PHONY: run clean rebuild
