# Imagen oficial de Haskell
FROM haskell:9.4

WORKDIR /opt/app

# Copiamos primero el archivo de configuracion para descargar dependencias
# (Esto ayuda a que Docker guarde las dependencias en cache)
COPY codigo1.cabal /opt/app/
RUN cabal update
RUN cabal build --only-dependencies -j4

# Copiamos el resto del codigo fuente
COPY . /opt/app/

# Compilamos el proyecto
RUN cabal build

# Exponemos el puerto de la interfaz grafica
EXPOSE 8023

# El comando por defecto levantara el servidor web local
CMD ["cabal", "run"]
