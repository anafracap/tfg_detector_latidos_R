# HeartbeatDetector

## Título

**Implementación de un detector de latidos en lenguaje R**

------------------------------------------------------------------------

## Autores

-   **Ana Fraile Caparrós**\
    Autora del proyecto
-   **Abraham Otero Quintana**\
    Tutor

------------------------------------------------------------------------

## Descripción del proyecto

`heartbeatDetector` es un programa diseñado para detectar complejos QRS en señales ECG de un único canal, aplicando un filtrado paso-base. Permite la carga de datos desde un `.csv` y desde ficheros estilo WFDB con formato `.hea` y `.dat`.

Para este programa se tomó como referencia la herramienta SQRS puesta a disposición por WFDB, y que a su vez se inspiró en el algoritmo desarrollado por Engelse y Zeelenberg, “A single scan algorithm for QRS detection and feature extraction,” Computers in Cardiology, 1979

------------------------------------------------------------------------

## Instalación y ejecución

El paquete se encuentra en el entorno local, y se descargará desde el propio repositorio github:

Instalar devtools si es preciso:

`install.packages("devtools")`

`devtools::install_local("path/to/detectorLatidos")`

Cargar las funciones del paquete con `devtools::load_all()`

Hay un archivo de ejemplo con cómo se ejecutarían las funciones, disponible en R/example/example_script.R
