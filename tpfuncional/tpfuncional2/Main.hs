module Main where
import Arboles
import Test.HUnit
import Data.List



--arboles para pruebas:
maderaSolo = Brote Madera
arbol1 = Brote Hoja
arbol2 = Rama Flor (Brote Fruto) (Brote Hoja)
arbol3 = Rama Hoja (Brote Fruto) (Brote Flor)
arbol3bis = Rama Flor (Brote Fruto) (Brote Flor)
arbol4 = Rama Madera arbol2 arbol3
arbol5 = Rama Madera arbol3 arbol2
arbol6 = Rama Madera arbol4 arbol1
arbol7 = Rama Madera arbol1 arbol5
arbol8 = Rama Madera arbol1 arbol6
arbol9 = Rama Fruto
		(Rama Fruto (Rama Fruto (Brote Fruto) (Brote Flor)) (Rama Hoja (Brote Madera) (Brote Hoja)))
		(Rama Fruto (Rama Madera (Brote Hoja) (Brote Madera)) (Rama Fruto (Brote Flor) (Brote Fruto)))

soloUnaHoja = Brote Hoja
puroMadera = Rama Madera (Brote Madera) (Brote Madera)
tresFlores = Rama Flor (Brote Flor) (Brote Flor)

maderaYFlor = Rama Madera (Brote Flor) (Brote Madera)
otraFlor = Rama Madera (Brote Madera) (Brote Flor)

frutoSinFlor = Rama Fruto (Brote Madera) (Brote Madera)

protegido = Rama Fruto (Brote Madera) (Brote Flor)
protegidoNivel1Derecha = Rama Madera soloUnaHoja protegido

superProtegido = Rama Hoja protegido protegido

desbalanceado = Rama Fruto (Brote Madera) protegido


--Ejecución de los tests
main :: IO Counts
main = do runTestTT allTests

allTests = test [
  "ejercicio2" ~: testsEj2,
  "ejercicio3" ~: testsEj3,
  "ejercicio4" ~: testsEj4,
  "ejercicio5" ~: testsEj5,
  "ejercicio6" ~: testsEj6,
  "ejercicio7" ~: testsEj7,
  "ejercicio8" ~: testsEj8
  ]

testsEj2 = test [
  0 ~=? peso soloUnaHoja,
  3 ~=? peso puroMadera
  ]

testsEj3 = test [
  arbol4 ~=? masPesado [arbol4, arbol1] --Cambiar esto por tests verdaderos.
  ]

testsEj4 = test [
  arbol3bis ~=? ultimaPrimavera arbol2 --Cambiar esto por tests verdaderos.
  ]

testsEj5 = test [
  0 ~=? 0 --Cambiar esto por tests verdaderos.
  ]

testsEj6 = test [
  0 ~=? 0 --Cambiar esto por tests verdaderos.
  ]

testsEj7 = test [
	0 ~=? 0 --Cambiar esto por tests verdaderos.
  ]

testsEj8 = test [
	0 ~=? 0 --Cambiar esto por tests verdaderos.
  ]
