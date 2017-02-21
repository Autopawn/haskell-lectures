---
title: "Introducción a Haskell y la programación funcional"
author: Francisco J. A. Casas B.
date: 14 de marzo del 2017
output: beamer_presentation
theme: Warsaw
highlight: tango
colortheme: seagull
---

# ¿Por qué Haskell?

## ¿Por qué Haskell?

Haskell proveé:

* Código de alto nivel **mantenible**.
* Alto **desempeño**.
* Código **cross-platform**.
* Lenguaje **interpretado** o **compilado**.
* Paralelización *"gratuita"*.
* La mayoría de los **errores se detectan en tiempo de compilación**.
* Buenas herramientas de profiling y debug.
* Tiene **garbage collection**.

Pero principalmente:

* Haskell es un lenguaje que tiene varias características especiales y presenta un **enfoque diferente** ante los siguientes **problemas**...

## El problema de la seguridad

![The central challenge - *Simon Peython Jones*](media/side_effects.png)

## El problema de la complejidad

* Parte de **saber programar** es saber **manejar la complejidad**, más aun en el software moderno.

* Parte de **programar en equipo** es **modularizar** y diseñar la **interfaz** de cada componente.

* Esto es determinar (e informar, o mejor aun, que sea obvio) el alcance de los **efectos** de nuetros componentes.

* Esto se evidencia mucho al diseñar **programas paralelos** y código **reusable** y **mantenible**.

## Plan A (todo el mundo)

* Los lenguajes convencionales parten de la idea de permitirle al programador hacer todo (*efectos arbitrarios*), por lo tanto son **útiles** pero **inseguros**.

* La **inseguridad** implica también que se deben realizar esfuerzos por manejar la **complejidad**, porque un programador no puede asumir cosas sobre el comportamiento del código.

### El plan

* **Añadir restricciones**: *scope*, *modulos*, el *typesystem*, etc.

* Se utilizan paradigmas y constructos del lenguaje, por ejemplo, **funciones**, **programación orientada a objetos** y **patrones de diseño**, pero su uso correcto siempre depende del programador.

## Plan B (Haskell)

*Se necesita ver algunos conceptos primero...*

## Haskell

Haskell es un lenguaje:

* **Puramente funcional**.
* Tiene **tipificado fuerte**, pero **inferencia de tipo**.
* Permite **programación genérica**.
* Tiene **evaluación no estricta**.

*Veamos qué significa cada una de estas cosa...*

## Paradigmas
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

*¿Qué ventajas y desventajas tiene esto?*

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

### Ventajas de la programación funcional

+ Las funciones están completamente encapsuladas, por definición.
+ Asegura determinismo.
+ Simplifica programas paralelos.

## Cálculo lambda

...

## Programación genérica

* Las funciones programadas pueden funcionar para **más de un data type**.

* En Haskell podemos definir una misma función que opere con cualquiér **data type**, o cualquier **data type** que cumplan ciertas condiciones (tenga cierto comportamiento definido).

* Además, **las funciones son tipos de datos también** y son tratados como cualquier otro.

## Plan B (Haskell)

* Haskell se basa en modelo computacional del **cálculo lambda**, que inicialmente es restrictivo (*ausencia de efectos*), de hecho "*inútil*".

* Para que sea útil, los **side effects**, cuando son necesarios, se manejan usando el *typesystem*.

* El *typesystem* se encarga de que el código *puro* no se "contamine" con el *impuro* (**efectos controlados**).

* La **programación funcional** facilita la **modularidad** y la **escalabilidad**.

* La **reusabilidad** se logra mediante la **programación genérica**.

# Programar con Haskell

## Programación funcional

Para poder programar funcionalmente tenemos que tener en cuenta:

* Nunca vamos a *modificar* algo, si necesitamos cambiar una sola celda en una matriz, usaremos una función que a partir de la matriz, nos entregará otra con el valor modificado.

* Debemos pensar en las funciones como **tubos**, entra algo y sale algo diferente, no cambia el estado del tubo ni nada fuera de él.

* El único efecto que puede tener nuestras funciones está en el valor de **retorno**, y se puede escribir con una sola expresión (aunque podemos usar *alias*).

* Generalmente reemplazaremos **iteración** con **recursión**.

<!-- ## Programación funcional

Analógamente a la **POO**, podemos usar funciones matemáticas para:

* Actualizar un objeto (más bien, obtener el como estará el objeto tras la actualización).

* Obtener un *"atributo"* de un objeto.

* -->

## Experiencia personal

+ El programador debe pensar más para resolver los problemas, pero las soluciones resultan **robustas**.
+ Usualmente el código es compacto y puede resultar poco legible, pero es completamente **modular**.
+ El **tipificado fuerte** y **estático** de Haskell, junto con la programación funcional, atrapa la mayor parte de los errores en tiempo de compilación.
+ La ausencia de **side effects** permite un código más seguro.
+ El **sistema de tipos** de Haskell permite realizar **programación genérica**, siendo una herramienta muy poderosa.
+ Generalmente es posible optimizar Haskell a una velocidad comparable a la de C (depende mucho de la programación).
+ El lenguaje es rápido y cuenta con **garbage collection** automática.

## Experiencia personal

- La **laziness** complica conocer la complejidad real de los programas y la comunicación entre threads y es fuente de *sorpresas desagradables*, relacionadas con el uso de memoria. Esto se puede evitar con **evaluación estricta**.
- El concepto de ``**monad**'', usado por muchas soluciones de Haskell, es difícil de entender, pero se puede evitar o usar superficialmente.
- Aunque la comunidad es amigable, está más relacionada con la academia, las librerías son limitadas.

## Videos extra

* [**Haskell is useless**](https://www.youtube.com/watch?v=iSmkqocn0oQ)

* [Lambda Calculus - Computerphile](https://www.youtube.com/watch?v=eis11j_iGMs)
