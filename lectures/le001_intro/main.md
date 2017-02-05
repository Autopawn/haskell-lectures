---
title: "Introducción a Haskell y la programación funcional"
author: Francisco J. A. Casas B.
date: 14 de marzo del 2017
output: beamer_presentation
theme: Warsaw
highlight: tango
colortheme: seagull
---

## Haskell

Haskell es un lenguaje de programación **puramente funcional**.

La **programación funcional** es un tipo de **programación declarativa**.

## Programación declarativa

* Suele corresponderse con la **lógica matemática**.
* Uno NO le dice al computador **qué hacer** para obtener el resultado.
* Uno indica **qué es** el resultado, tal como se hace con una definición matemática.
* Las instrucciones se deducen a partir de las expresiones.
* Carece de **side effects** (cuenta con **transparencia referencial**).

## Programación funcional

* Una vez se define algo, jamás cambia de estado (**data inmutable**).
* Trata la computación como la evaluación de **funciones matemáticas**.
* Tiene sus orígenes en el **cálculo lambda**.

## Cálculo Lambda

Es un modelo de computación



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
