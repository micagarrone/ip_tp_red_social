module Tests where

import Test.HUnit
import Solucion

main = runTestTT todosLosTest
todosLosTest = test [testsuite1, testsuite2, testsuite3, testsuite4, testsuite5, testsuite6, testsuite7, testsuite8, testsuite9, testsuite10]

testsuite1 = test [
    " nombresDeUsuarios sin nombres repetidos" ~: esPermutacion (nombresDeUsuarios redA) ["Juan","JOJO","PEPSIMAN","Mirtha Legrand", "Spiderman"],
    " nombresDeUsuarios con nombres repetidos" ~: esPermutacion (nombresDeUsuarios redB) ["Juan","PEPSIMAN","JOJO"],
    " nombresDeUsuarios red sin usuarios " ~: (nombresDeUsuarios redVacia) ~?= [] 
    ]   

testsuite2 = test [
    " amigosDe alguien social" ~: esPermutacion (amigosDe redA usuario2) [usuario1, usuario4, usuario3, usuario7],
    " amigosDe alguien solitario"  ~: (amigosDe redA usuario999) ~?= [] 
    ]

testsuite3 = test [
    " cantidadDeAmigos alguien social" ~: (cantidadDeAmigos redA usuario1) ~?= 3,
    " cantidadDeAmigos alguien solitario" ~: (cantidadDeAmigos redR usuario999) ~?= 0,
    " cantidadDeAmigos alguien MUY social " ~: (cantidadDeAmigos redR usuarioR) ~?= 11
    ]

testsuite4 = test [
    " usuarioConMasAmigos no hay empate" ~: (usuarioConMasAmigos redR) ~?= usuarioR,
    " usuarioConMasAmigos hay empate" ~: expectAny (usuarioConMasAmigos redA) [usuario2, usuario4],
    " usuarioConMasAmigos nadie tiene amigos " ~: expectAny (usuarioConMasAmigos redLobosSolitarios) usuariosR
    ]

testsuite5 = test [
    " estaRobertoCarlos pocos amigos" ~: (estaRobertoCarlos redA) ~?= False,
    " estaRobertoCarlos 11amigos"  ~: (estaRobertoCarlos redR) ~?= True
    ]

testsuite6 = test [
    " publicacionesDe 1" ~: esPermutacion (publicacionesDe redA usuario2) [publicacion2_1, publicacion2_2]
    ]

testsuite7 = test [
    " publicacionesQueLeGustanA 1" ~: esPermutacion (publicacionesQueLeGustanA redA usuario1) [publicacion2_2, publicacion4_1]
    ]

testsuite8 = test [
    " lesGustanLasMismasPublicaciones 2" ~: (lesGustanLasMismasPublicaciones redB usuario1 usuario3) ~?= True
    ]

testsuite9 = test [
    " tieneUnSeguidorFiel 1" ~: (tieneUnSeguidorFiel redA usuario1) ~?= True
    ]

testsuite10 = test [
    " existeSecuenciaDeAmigos 1" ~: (existeSecuenciaDeAmigos redA usuario1 usuario3) ~?= True
    ]

expectAny actual expected = elem actual expected ~? ("expected any of: " ++ show expected ++ "\n but got: " ++ show actual)

--Al usarse con listas que no tienen repetidos, la especificación de "mismosElementos" funciona igual que chequear si una lista es permutación de otra
esPermutacion actual expected = mismosElementos actual expected ~? ("expected any of: " ++ show expected ++ "\n but got: " ++ show actual) 

-- Ejemplos

usuario1 = (1, "Juan")
usuario2 = (2, "JOJO")
usuario3 = (3, "PEPSIMAN")
usuario4 = (4, "Mirtha Legrand")
usuario5 = (5, "JOJO")
usuario6 = (6, "Batman")
usuario7 = (7, "Spiderman")
usuario8 = (8, "Goku")
usuario9 = (9, "Pikachu")
usuario10 = (10, "Adolfo")
usuario11 = (11, "Juan")
usuario12 = (12, "Maximo Cozzeti")
usuario999 = (999, "Bobby Vinton")
usuarioR = (10000, "ROBERTO")

relacion1_2 = (usuario1, usuario2)
relacion1_3 = (usuario1, usuario3)
relacion2_4 = (usuario2, usuario4)
relacion1_4 = (usuario4, usuario1) -- Notar que el orden en el que aparecen los usuarios es indistinto
relacion2_3 = (usuario3, usuario2)
relacion3_4 = (usuario4, usuario3)
relacion3_3 = (usuario3, usuario3)
relacion2_5 = (usuario2, usuario5)
relacion3_6 = (usuario3, usuario6)
relacion4_7 = (usuario4, usuario7)
relacion5_7 = (usuario7, usuario5)
relacion8_9 = (usuario9, usuario8)
relacion7_9 = (usuario7, usuario9)
relacion10_9 = (usuario10, usuario9)
relacion2_7 = (usuario2, usuario7)
relacion1_6 = (usuario1, usuario6)

relacionR_1 = (usuarioR, usuario1)
relacionR_2 = (usuarioR, usuario2)
relacionR_3 = (usuarioR, usuario3)
relacionR_4 = (usuarioR, usuario4)
relacionR_5 = (usuarioR, usuario5)
relacionR_6 = (usuarioR, usuario6)
relacion7_R = (usuario7, usuarioR)
relacion8_R = (usuario8, usuarioR)
relacion9_R = (usuario9, usuarioR)
relacion10_R = (usuario10, usuarioR)
relacion11_R = (usuario11, usuarioR)

publicacion1_1 = (usuario1, "S", [usuario2, usuario4])
publicacion1_2 = (usuario1, "P", [usuario4])
publicacion1_3 = (usuario1, "A", [usuario2, usuario4])
publicacion1_4 = (usuario1, "M", [])
publicacion1_5 = (usuario1, "X", [usuario4])
publicacion1_6 = (usuario1, "D", [usuario1])

publicacion2_1 = (usuario2, "The cake is a lie", [usuario4])
publicacion2_2 = (usuario2, "Still alive", [usuario1, usuario4])
publicacion2_3 = (usuario2, "Want you gone", [usuario10, usuario11, usuario1])

publicacion3_1 = (usuario3, "saitama le gana", [])
publicacion3_2 = (usuario3, "HEY LISTEN", [usuario2])
publicacion3_3 = (usuario3, "put your grasses on", [usuario2, usuario5])

publicacion4_1 = (usuario4, "maximo cozzeti es un robot ruso", [usuario1, usuario2])
publicacion4_2 = (usuario4, "kjjjjjjjjjjjjj", [])
publicacion4_3 = (usuario4, "un año en la selva hablandole a una camara apagada?", [usuario1, usuario3])


usuariosA = [usuario1, usuario2, usuario3, usuario4, usuario7]
relacionesA = [relacion1_2, relacion1_4, relacion2_3, relacion2_4, relacion3_4, relacion1_6, relacion2_7]
publicacionesA = [publicacion1_1, publicacion1_2, publicacion1_3, publicacion1_5, publicacion2_1, publicacion2_2, publicacion3_1, publicacion3_2, publicacion4_1, publicacion4_2]
redA = (usuariosA, relacionesA, publicacionesA)

usuariosB = [usuario1, usuario2, usuario3, usuario5, usuario11]
relacionesB = [relacion1_2, relacion2_3]
publicacionesB = [publicacion1_3, publicacion1_4, publicacion1_5, publicacion3_1, publicacion3_2, publicacion3_3]
redB = (usuariosB, relacionesB, publicacionesB)



usuariosVacios = []
publicacionesVacias = []
relacionesVacias = []

redVacia = (usuariosVacios, relacionesVacias, publicacionesVacias)

usuariosR = [usuarioR, usuario1, usuario2, usuario3, usuario4, usuario5, usuario6, usuario7, usuario8, usuario9, usuario10, usuario11, usuario999]
relacionesR = [relacionR_1, relacionR_2, relacionR_3, relacionR_4, relacionR_5, relacionR_6, relacion7_R, relacion8_R, relacion9_R, relacion10_R, relacion11_R, relacion1_2,relacion1_3,relacion1_4]

redR = (usuariosR, relacionesR, publicacionesVacias)

redLobosSolitarios = (usuariosR, relacionesVacias, publicacionesVacias)