module temp where 

{- Casos de prueba -} 

-- Test basico para leer
sampleText = "¿Eres venezolano?\n"++
             "   -si: ¿eres mayor de edad?\n"++
             "      -si: mal por ti\n"++
             "      -no: aun hay esperanza, huye\n"++
             "   -no: ¿que se siente comer bien?\n"++
             "      -es lo mejor del mundo: ¿por que lo dices?\n"++
             "         -porque si: ok\n"++
             "         -no quiero responder: ok\n"++
             "      -no es la gran cosa: no sabes lo que estas diciendo\n"

-- Test basico para imprimir (NO BORRAR)
sampleOraculo = Pregunta { pregunta = "Eres hombre" ,
                   opciones = M.fromList [ ("Si", Pregunta { pregunta="Te gusta el pan",
                     opciones=M.fromList [ ("Si", Prediccion {prediccion="gordo"}),
                       ("No", Prediccion "sano") ] } ),
                   ("No", Pregunta { pregunta = "Eres veneco",
                       opciones=M.fromList [ ("Si", Prediccion "tu futuro es incierto"),  
                     ("No", Prediccion "Bendecida y afortunada") ] }) ]
                  }
