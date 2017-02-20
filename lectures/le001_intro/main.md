---
title: "Introducción a Haskell y la programación funcional"
author: Francisco J. A. Casas B.
date: 14 de marzo del 2017
output: beamer_presentation
theme: Warsaw
highlight: tango
colortheme: seagull
---

## ¿Por qué Haskell?

* Parte de **saber programar** es saber **manejar la complejidad**, más aun en el software moderno.

* Parte de **programar en equipo** es **modularizar** y diseñar la **interfaz** de cada componente.

* Esto es determinar (e informar, o mejor aun, que sea obvio) el alcance de los **efectos** de nuetros componentes.

* Esto se evidencia mucho al diseñar **programas paralelos**.

* Haskell es un lenguaje que tiene varias características especiales y presenta un **enfoque diferente** ante estos problemas.

## ¿Por qué Haskell?

Haskell proveé:

* Código de alto nivel mantenible.
* Alto desempeño.
* Buenas herramientas de profiling.
* Código **cross-platform**.
* Paralelización *"gratuita"*.

## El camino de Haskell

![The central challenge - *Simon Peython Jones*](media/side_effects.png)

## El plan A (todo el mundo)



## Haskell

* Haskell es un lenguaje de programación **puramente funcional**.

* La **programación funcional** es un tipo de **programación declarativa**.

## Programación declarativa

* Suele corresponderse con la **lógica matemática**.
* Uno NO le dice al computador **qué hacer** para obtener el resultado.
* Uno indica **qué es** el resultado, tal como se hace con una definición matemática.
* Las instrucciones se deducen a partir de las expresiones.
* Carece de **side effects** (cambios de estado fuera de la **ejecución** de la función).

### Pregunta

*¿Qué ventajas y desventajas nos puede traer esto?*

## Transparencia referencial

* La **transparencia referencial** es un resultado de la ausencia de **side effects**, significa que al llamar una función con los mismos argumentos, siempre entregará el mismo resultado.

### Pregunta

*¿Las variables static implican side effects?*

## Transparencia referencial

* La ausencia de **side effects** nos permite utilizar una función abstrayéndonos del contexto de dicha función y su historial de usos para predecir su comportamiento.

* O como les gusta decír a la gente de Haskell, *saber que no disparará los misiles*.

### Pregunta

*¿Es posible que un programa esté 100% libre de side effects?*

## Programación funcional

* Una vez se define algo, jamás cambia de estado (**data inmutable**).
* Trata la computación como la evaluación de **funciones matemáticas**.
* Tiene sus orígenes en el **cálculo lambda**.

## Programación funcional

Para poder programar funcionalmente tenemos que tener en cuenta:

* Nunca vamos a *modificar* algo, si necesitamos cambiar una sola celda en una matriz, usaremos una función que a partir de la matriz, nos entregará otra con el valor modificado.

* Debemos pensar en las funciones como **tubos**, entra algo y sale algo diferente, no cambia el estado del tubo ni nada fuera de él.

* El único efecto que puede tener nuestras funciones está en el valor de **retorno**, y se puede escribir con una sola expresión (aunque podemos usar *alias*).

* Generalmente reemplazaremso **iteración** con **recursión**.

## Ventajas de la programación funcional

+ Las funciones están completamente encapsuladas, por definición.
+ Asegura determinismo.
+ Simplifica programas paralelos.

## Experiencia personal

+ El programador debe pensar más para resolver los problemas, pero las soluciones resultan **robustas**.
+ Usualmente el código es compacto y puede resultar poco legible, pero es completamente **modular**.
+ El **tipificado fuerte** y **estático** de Haskell, junto con la programación funcional, atrapa la mayor parte de los errores en tiempo de compilación.
+ La ausencia de **side effects** permite un código más seguro.
+ El **sistema de tipos** de Haskell permite realizar **generic programming**, siendo una herramienta muy poderosa.
+ Generalmente es posible optimizar Haskell a una velocidad comparable a la de C (depende mucho de la programación).
+ El lenguaje es rápido y cuenta con **garbage collection** automática.

## Experiencia personal

- La **lazyness** complica conocer la complejidad real de los programas y la comunicación entre threads y es fuente de *sorpresas desagradables*, relacionadas con el uso de memoria. Esto se puede evitar con **evaluación estricta**.
- El concepto de ``**monad**'', usado por muchas soluciones de Haskell, es difícil de entender, pero se puede evitar o usar superficialmente.
- Aunque la comunidad es amigable, está más relacionada con la academia, las librerías son limitadas.

## Videos extra

* [**Haskell is useless**](https://www.youtube.com/watch?v=iSmkqocn0oQ)

* [Lambda Calculus - Computerphile](https://www.youtube.com/watch?v=eis11j_iGMs)
