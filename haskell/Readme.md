# Entrega proyecto haskinator

### Integrantes

* Luis Marcoz Diaz Naranjo 15-10420
* Cesar Alfonso Rosario Escobar 15-11295

## Importante

El texto reconocible por haskinator en sus archivos de entrada tiene el siguiente formato: 

* preguntas:

>    ¿text?

en donde text es cualquier conjunto de caracteres distinto de ?, pues en tal caso, se interpretaria lo que sigue como una pregunta

* opciones:

>     -texto: 

en donde texto es cualquiero conjunto de caracteres distinto de ':'. 

* predicciones:

>    texto

cualquier conjunto de caracteres distinto de '\n'.  
  
**Toda línea debe acabar en un salto de línea.**  
  
Por ejemplo: 

```
¿El actor interpretó un villano?
   -sí: ¿Cuál es el nombre del villano?
      -Joker: ¿De qué película es el Joker?
         - Suicide Squad: Jared Leto
         - The Dark Knight: Heath Ledger
      -Bane: Tom Hardy
      -Scarecrow: Cilian Murphy
   -no: Christian Bale
   
```
  
