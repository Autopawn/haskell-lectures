---
title: "Comenzando con Haskell"
author: Francisco J. A. Casas B.
date: 14 de marzo del 2017
output: beamer_presentation
theme: Warsaw
highlight: tango
colortheme: seagull
---

# Programación funcional

Para poder programar funcionalmente tenemos que tener en cuenta:

* Nunca vamos a *modificar* algo, si necesitamos cambiar una sola celda en una matriz, usaremos una función que a partir de la matriz, nos entregará otra con el valor modificado.

* Debemos pensar en las funciones como **circuítos**, entra algo y sale algo diferente, no cambia el estado del componente ni nada fuera de él.

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
