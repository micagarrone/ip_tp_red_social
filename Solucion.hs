module Solucion where

-- Nombre de Grupo: Empanada4Quesos
-- Integrante 1: Micaela Valeria Garrone, micaelagarrone@gmail.com, 860/23
-- Integrante 2: Valeria Andreina Simoza Sanchez, vsimoza.vs@gmail.com, 1027/22
-- Integrante 3: Lucia Berterreix, berterreixlucia@gmail.com, 1204/22
-- Integrante 4: Juan José García Vizioli, juanjogvizioli@gmail.com, 353/20

type Usuario = (Integer, String) -- (id, nombre)
type Relacion = (Usuario, Usuario) -- usuarios que se relacionan
type Publicacion = (Usuario, String, [Usuario]) -- (usuario que publica, texto publicación, likes)
type RedSocial = ([Usuario], [Relacion], [Publicacion])

-- Funciones básicas

usuarios :: RedSocial -> [Usuario]
usuarios (us, _, _) = us

relaciones :: RedSocial -> [Relacion]
relaciones (_, rs, _) = rs

publicaciones :: RedSocial -> [Publicacion]
publicaciones (_, _, ps) = ps

idDeUsuario :: Usuario -> Integer
idDeUsuario (id, _) = id 

nombreDeUsuario :: Usuario -> String
nombreDeUsuario (_, nombre) = nombre 

usuarioDePublicacion :: Publicacion -> Usuario
usuarioDePublicacion (u, _, _) = u

likesDePublicacion :: Publicacion -> [Usuario]
likesDePublicacion (_, _, us) = us

-- Ejercicios

--Ejercicio 1:
--Dada una red social válida, devuelve una lista con los nombres de cada usuario, sin repetidos
nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios red = proyectarNombres (usuarios red)

--Dada una lista de usuarios, devuelve una lista con el nombre de cada uno, sin repetidos
proyectarNombres :: [Usuario] -> [String]
proyectarNombres [] = []
proyectarNombres (u:us) | not (pertenece (nombreDeUsuario u) (proyectarNombres us)) = (nombreDeUsuario u) : (proyectarNombres us)
                        | otherwise = proyectarNombres us

pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece x (y:ys)  | (x == y) || (pertenece x ys) = True
                    | otherwise = False

--Ejercicio 2: 
--Dada una red social válida y un usuario válido perteneciente a esta, devuelve una lista de todos los usuarios  que se relacionan con él dentro de la red.
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe red u = amigosDeEnListaRelaciones (relaciones red) u

amigosDeEnListaRelaciones :: [Relacion] -> Usuario -> [Usuario]
amigosDeEnListaRelaciones [] _ = []
amigosDeEnListaRelaciones (r:rs) u  | fst r == u = snd r : (amigosDeEnListaRelaciones rs u) --No hace falta chequear repetidos, porque en una red social válida no hay relaciones repetidas ni usuarios auto-relacionados.
                                    | snd r == u = fst r : (amigosDeEnListaRelaciones rs u)
                                    | otherwise = amigosDeEnListaRelaciones rs u

--Ejercicio 3:
--Dada una red social válida y un usuario válido perteneciente a esta, devuelve la cantidad de usuarios válidos dentro de la red que se relacionan con él.
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos red u = longitud (amigosDe red u)

longitud :: [t] -> Int
longitud [] = 0
longitud (_:xs) = 1 + longitud xs

--Ejercicio 4:
--Dada una red social válida, devuelve el usuario dentro de esta que tenga la mayor cantidad de amigos. En caso de empate entre varios usuarios, devuelve alguno de ellos.
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos red = usuarioConMasAmigosEnListaUsuarios red (usuarios red)

usuarioConMasAmigosEnListaUsuarios :: RedSocial -> [Usuario] -> Usuario
usuarioConMasAmigosEnListaUsuarios red [u] = u
usuarioConMasAmigosEnListaUsuarios red (u:us)   | (cantidadDeAmigos red u) > (cantidadDeAmigos red (usuarioConMasAmigosEnListaUsuarios red us)) = u
                                                | otherwise = usuarioConMasAmigosEnListaUsuarios red us

--Ejercicio 5:
--Dada una red social válida, devuelve True si y solo si dentro de ella hay un usuario con más de diez amigos.
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos red = estaRobertoCarlosEnListaUsuarios red (usuarios red)

estaRobertoCarlosEnListaUsuarios :: RedSocial -> [Usuario] -> Bool
estaRobertoCarlosEnListaUsuarios red [] = False
estaRobertoCarlosEnListaUsuarios red (u:us) = (cantidadDeAmigos red u > 10) || (estaRobertoCarlosEnListaUsuarios red us)

--Ejercicio 6:
--Dada una red social válida y un usuario válido perteneciente a esta, devuelve una lista con todas las publicaciones válidas que haya hecho dentro de la red
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe red u = publicacionesDeUsuarioEnListaPublicaciones (publicaciones red) u

publicacionesDeUsuarioEnListaPublicaciones :: [Publicacion] -> Usuario -> [Publicacion]
publicacionesDeUsuarioEnListaPublicaciones [] _ = []
publicacionesDeUsuarioEnListaPublicaciones (p:ps) u | usuarioDePublicacion p == u = p : (publicacionesDeUsuarioEnListaPublicaciones ps u)
                                                    | otherwise = publicacionesDeUsuarioEnListaPublicaciones ps u

--Ejercicio 7:
--Dada una red social válida y un usuario válido perteneciente a esta, devuelve una lista con todas las publicaciones dentro de la red a las cuales les haya dado like
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA red u = publicacionesQueLeGustanAUsuarioEnListaPublicaciones (publicaciones red) u

publicacionesQueLeGustanAUsuarioEnListaPublicaciones :: [Publicacion] -> Usuario -> [Publicacion]
publicacionesQueLeGustanAUsuarioEnListaPublicaciones [] _ = []
publicacionesQueLeGustanAUsuarioEnListaPublicaciones (p:ps) u   | pertenece u (likesDePublicacion p) = p : (publicacionesQueLeGustanAUsuarioEnListaPublicaciones ps u) 
                                                                | otherwise = publicacionesQueLeGustanAUsuarioEnListaPublicaciones ps u

--Ejercicio 8:
--Dada una red social válida y dos usuarios válidos pertenecientes a esta, devuelve True si y solo si a ambos usuarios les gustan exactamente las mismas publicaciones de la red
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones r u1 u2 = mismosElementos (publicacionesQueLeGustanA r u1 ) (publicacionesQueLeGustanA r u2)

mismosElementos :: (Eq t) => [t] -> [t] -> Bool
mismosElementos t1 t2 = (longitud t1 == longitud t2) && (elementosContenidosEn t1 t2) && (elementosContenidosEn t2 t1)

elementosContenidosEn :: (Eq t) => [t] -> [t] -> Bool
elementosContenidosEn [] t2 = True
elementosContenidosEn (t1:t1s) t2 = (pertenece t1 t2) && (elementosContenidosEn t1s t2)

--Ejercicio 9:
--Dada una red social válida y un usuario válido perteneciente a esta, devuelve True si y solo si existe algún otro usuario válido dentro de la red que haya dado like a todas las publicaciones de la red del usuario original
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel red u = (longitud (publicacionesDe red u) > 0) && (tieneUnSeguidorFielEnListaCandidatos red u candidatosSeguidorFiel)
                            where candidatosSeguidorFiel = removerElemento (usuarios red) u

removerElemento :: (Eq t) => [t] -> t -> [t]
removerElemento [] _ = []
removerElemento (l:ls) e    | l == e = removerElemento ls e
                            | otherwise = l : (removerElemento ls e)

tieneUnSeguidorFielEnListaCandidatos :: RedSocial -> Usuario -> [Usuario] -> Bool
tieneUnSeguidorFielEnListaCandidatos red u [u2] = elementosContenidosEn (publicacionesDe red u) (publicacionesQueLeGustanA red u2)
tieneUnSeguidorFielEnListaCandidatos red u (u2:u2s) = tieneUnSeguidorFielEnListaCandidatos red u [u2] || tieneUnSeguidorFielEnListaCandidatos red u u2s

--Ejercicio 10:
--Dada una red social válida y dos usuarios A y Z válidos pertenecientes a esta, devuelve True si y solo si existe una cadena de amigos que los una. Es decir, si y solo si A es amigo de B, y B de C, y... y Y de Z.
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos red u1 u2   | u1 == u2 && (cantidadDeAmigos red u1 > 0) = True --Si tiene al menos 1 amigo u3, existe la secuencia [u1, u3, u1]
                                    | otherwise = estanRelacionadosIndirectamente (relaciones red) u1 u2

estanRelacionadosIndirectamente :: [Relacion] -> Usuario -> Usuario -> Bool
estanRelacionadosIndirectamente r u1 u2 | primerRelacionadoDeUsuario r u1 == u1 = False
                                        | primerRelacionadoDeUsuario r u1 == u2 = True
                                        | otherwise = estanRelacionadosIndirectamente relacionSimplificada u1 u2
                                        where relacionSimplificada = simplificarRelacionesDeUsuario r u1

--"El amigo de mi amigo es mi amigo"
simplificarRelacionesDeUsuario :: [Relacion] -> Usuario -> [Relacion]
simplificarRelacionesDeUsuario r u =  reemplazarConUsuarioAUsuario r u (primerRelacionadoDeUsuario r u)

reemplazarConUsuarioAUsuario :: [Relacion] -> Usuario -> Usuario -> [Relacion]
reemplazarConUsuarioAUsuario [r] u1 u2  | fst r == u2 = [(u1, snd r)]
                                        | snd r == u2 = [(fst r, u1)]
                                        | otherwise = [r]
reemplazarConUsuarioAUsuario (r:rs) u1 u2   = (head (reemplazarConUsuarioAUsuario [r] u1 u2)) : (reemplazarConUsuarioAUsuario rs u1 u2 )

primerRelacionadoDeUsuario :: [Relacion] -> Usuario -> Usuario
primerRelacionadoDeUsuario [] u = u
primerRelacionadoDeUsuario (r:rs) u | fst r == u && snd r /= u = snd r
                                    | snd r == u && fst r /= u = fst r
                                    | otherwise = primerRelacionadoDeUsuario rs u
