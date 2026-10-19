# Proyecto de Criptografía - Teoría de Números II

Este proyecto implementa algoritmos de cifrado clásicos y asimétricos modernos vistos durante el curso de Teoría de Números II, diseñados funcionalmente en Haskell con una interfaz gráfica web (basada en Threepenny-GUI).

## Algoritmos Implementados
- **Cifrados Monalfabéticos:** César, Decimado, Afín.
- **Cifrados Polialfabéticos:** Vigenère, Discos de Alberti (por letras y por números).
- **Cifrados de Bloque / Álgebra Lineal:** Hill.
- **Cifrados Asimétricos:** RSA, ElGamal.

---

## Documentación (Haddock)

El código fuente ha sido documentado detallando los procesos y las relaciones con otras funciones en cada implementación.
Para leer la documentación técnica de manera gráfica y navegar por los módulos:

1. Abra la carpeta **`documentacion_html/`** incluida en este proyecto.
2. Abra el archivo **`index.html`** con cualquier navegador web (Chrome, Firefox, Safari).

---

##  Cómo ejecutar el proyecto

Existen dos maneras de levantar la interfaz gráfica y probar los cifrados:

### Opción 1: Ejecución nativa (Requiere Haskell)
Si cuenta con el ecosistema de Haskell (GHC y Cabal) instalado:

1. Abra una terminal en la raíz de esta carpeta.
2. Ejecute el comando:
   ```bash
   cabal run
   ```
3. Abra su navegador web en: **http://localhost:8023**

### Opción 2: Ejecución mediante Docker
Si tiene Docker instalado y no desea instalar las dependencias de Haskell:

1. Construya la imagen (ejecutar solo la primera vez):
   ```bash
   docker build -t cifrados-haskell .
   ```
2. Ejecute el contenedor, mapeando el puerto:
   ```bash
   docker run -p 8023:8023 cifrados-haskell
   ```
3. Abra su navegador web en: **http://localhost:8023**

