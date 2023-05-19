module Tests where

import Test.HUnit
import Solucion

main = runTestTT todosLosTest
todosLosTest = test [testsuite1, testsuite2, testsuite3, testsuite4, testsuite5, testsuite6, testsuite7, testsuite8, testsuite9, testsuite10]

testsuite1 = test [
    " nombresDeUsuarios 1" ~: esPermutacion (nombresDeUsuarios redA) ["Juan","Natalia","Pedro","Mariela"],
    " nombresDeUsuarios 2" ~: (nombresDeUsuarios redB) ~?= ["Juan","Pedro","Natalia"]  
    ]   

testsuite2 = test [
    " amigosDe 1" ~: (amigosDe redA usuario1) ~?= [usuario2, usuario4]
    ]

testsuite3 = test [
    " cantidadDeAmigos 1" ~: (cantidadDeAmigos redA usuario1) ~?= 2
    ]

testsuite4 = test [
    " usuarioConMasAmigos 1" ~: expectAny (usuarioConMasAmigos redA) [usuario2, usuario4]
    ]

testsuite5 = test [
    " estaRobertoCarlos 1" ~: (estaRobertoCarlos redA) ~?= False
    ]

testsuite6 = test [
    " publicacionesDe 1" ~: (publicacionesDe redA usuario2) ~?= [publicacion2_1, publicacion2_2]
    ]

testsuite7 = test [
    " publicacionesQueLeGustanA 1" ~: (publicacionesQueLeGustanA redA usuario1) ~?= [publicacion2_2, publicacion4_1]
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
esPermutacion actual expected = mismosElementos actual expected ~? ("expected any of: " ++ show expected ++ "\n but got: " ++ show actual)

-- Ejemplos

usuario1 = (1, "Juan")
usuario2 = (2, "Natalia")
usuario3 = (3, "Pedro")
usuario4 = (4, "Mariela")
usuario5 = (5, "Natalia")
usuario6 = (6, "Batman")
usuario7 = (7, "Spiderman")
usuario8 = (8, "Goku")
usuario9 = (9, "Pikachu")
usuario10 = (10, "Adolfo")

relacion1_2 = (usuario1, usuario2)
relacion1_3 = (usuario1, usuario3)
relacion2_4 = (usuario2, usuario4)
relacion1_4 = (usuario4, usuario1) -- Notar que el orden en el que aparecen los usuarios es indistinto
relacion2_3 = (usuario3, usuario2)
relacion3_4 = (usuario4, usuario3)
relacion1_1 = (usuario1, usuario1)
relacion3_3 = (usuario3, usuario3)
relacion2_5 = (usuario2, usuario5)
relacion3_6 = (usuario3, usuario6)
relacion4_7 = (usuario4, usuario7)
relacion5_7 = (usuario7, usuario5)
relacion8_9 = (usuario9, usuario8)
relacion7_9 = (usuario7, usuario9)
relacion10_9 = (usuario10, usuario9)

publicacion1_1 = (usuario1, "Este es mi primer post", [usuario2, usuario4])
publicacion1_2 = (usuario1, "Este es mi segundo post", [usuario4])
publicacion1_3 = (usuario1, "Este es mi tercer post", [usuario2, usuario5])
publicacion1_4 = (usuario1, "Este es mi cuarto post", [])
publicacion1_5 = (usuario1, "Este es como mi quinto post", [usuario5])

publicacion2_1 = (usuario2, "Hello World", [usuario4])
publicacion2_2 = (usuario2, "Good Bye World", [usuario1, usuario4])

publicacion3_1 = (usuario3, "Lorem Ipsum", [])
publicacion3_2 = (usuario3, "dolor sit amet", [usuario2])
publicacion3_3 = (usuario3, "consectetur adipiscing elit", [usuario2, usuario5])

publicacion4_1 = (usuario4, "I am Alice. Not", [usuario1, usuario2])
publicacion4_2 = (usuario4, "I am Bob", [])
publicacion4_3 = (usuario4, "Just kidding, i am Mariela", [usuario1, usuario3])


usuariosA = [usuario1, usuario2, usuario3, usuario4]
relacionesA = [relacion1_2, relacion1_4, relacion2_3, relacion2_4, relacion3_4]
publicacionesA = [publicacion1_1, publicacion1_2, publicacion2_1, publicacion2_2, publicacion3_1, publicacion3_2, publicacion4_1, publicacion4_2]
redA = (usuariosA, relacionesA, publicacionesA)

usuariosB = [usuario1, usuario2, usuario3, usuario5]
relacionesB = [relacion1_2, relacion2_3]
publicacionesB = [publicacion1_3, publicacion1_4, publicacion1_5, publicacion3_1, publicacion3_2, publicacion3_3]
redB = (usuariosB, relacionesB, publicacionesB)
