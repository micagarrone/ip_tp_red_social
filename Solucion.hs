module Solucion where

-- Nombre de Grupo: Empanada4Quesos
-- Integrante 1: Micaela Valeria Garrone, micaelagarrone@gmail.com, 860/23
-- Integrante 2: Valeria Andreina Simoza Sanchez, vsimoza.vs@gmail.com, 1027/22
-- Integrante 3: Lucia Berterreix, berterreixlucia@gmail.com, 1204/22
-- Integrante 4: Juan José García Vizioli, juanjogvizioli@gmail.com, 353/20

type Usuario = (Integer, String) -- (id, nombre)
type Relacion = (Usuario, Usuario) -- usuarios que se relacionan
type Publicacion = (Usuario, String, [Usuario]) -- (usuario que publica, texto publicacion, likes)
type RedSocial = ([Usuario], [Relacion], [Publicacion])

-- Funciones basicas

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
--Dada una red social válida, devuelve una lista con los nombres de cada usuario, sin repetidos.
nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios red = proyectarNombres (usuarios red)

--Dada una lista de usuarios, devuelve una lista con el nombre de cada uno
proyectarNombres :: [Usuario] -> [String]
proyectarNombres [] = []
proyectarNombres (u:us) | pertenece (nombreDeUsuario u) (proyectarNombres us) = proyectarNombres us
                        | otherwise = (nombreDeUsuario u) : proyectarNombres us

-- describir qué hace la función: .....
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe red u = amigosDeEnListaRelaciones (relaciones red) u

amigosDeEnListaRelaciones :: [Relacion] -> Usuario -> [Usuario]
amigosDeEnListaRelaciones [] _ = []
amigosDeEnListaRelaciones (r:rs) u  | fst r == u && snd r /= u = snd r : (amigosDeEnListaRelaciones rs u)
                                    | snd r == u && fst r /= u = fst r : (amigosDeEnListaRelaciones rs u)
                                    | otherwise = amigosDeEnListaRelaciones rs u

-- describir qué hace la función: .....
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos red u = longitud (amigosDe red u)

longitud :: [t] -> Int
longitud [] = 0
longitud (_:xs) = 1 + longitud xs

-- describir qué hace la función: .....
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos red = usuarioConMasAmigosEnListaUsuarios red (usuarios red)

usuarioConMasAmigosEnListaUsuarios :: RedSocial -> [Usuario] -> Usuario
usuarioConMasAmigosEnListaUsuarios red [u] = u
usuarioConMasAmigosEnListaUsuarios red (u:us)   | (cantidadDeAmigos red u) > (cantidadDeAmigos red (usuarioConMasAmigosEnListaUsuarios red us)) = u
                    | otherwise = usuarioConMasAmigosEnListaUsuarios red us

-- describir qué hace la función: .....
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos red = estaRobertoCarlosEnListaUsuarios red (usuarios red)

estaRobertoCarlosEnListaUsuarios :: RedSocial -> [Usuario] -> Bool
estaRobertoCarlosEnListaUsuarios red [] = False
estaRobertoCarlosEnListaUsuarios red (u:us) = (cantidadDeAmigos red u > 10) || (estaRobertoCarlosEnListaUsuarios red us)

-- describir qué hace la función: .....
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe red u = publicacionesDeUsuarioEnListaPublicaciones (publicaciones red) u

publicacionesDeUsuarioEnListaPublicaciones :: [Publicacion] -> Usuario -> [Publicacion]
publicacionesDeUsuarioEnListaPublicaciones [] _ = []
publicacionesDeUsuarioEnListaPublicaciones (p:ps) u | usuarioDePublicacion p == u = p : (publicacionesDeUsuarioEnListaPublicaciones ps u)
                                                    | otherwise = publicacionesDeUsuarioEnListaPublicaciones ps u

-- describir qué hace la función: .....
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA red u = publicacionesQueLeGustanAUsuarioEnListaPublicaciones (publicaciones red) u

publicacionesQueLeGustanAUsuarioEnListaPublicaciones :: [Publicacion] -> Usuario -> [Publicacion]
publicacionesQueLeGustanAUsuarioEnListaPublicaciones [] _ = []
publicacionesQueLeGustanAUsuarioEnListaPublicaciones (p:ps) u   | pertenece u (likesDePublicacion p) = p : (publicacionesQueLeGustanAUsuarioEnListaPublicaciones ps u) 
                                                                | otherwise = publicacionesQueLeGustanAUsuarioEnListaPublicaciones ps u

pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece x (y:ys)  | (x == y) || (pertenece x ys) = True
                    | otherwise = False

-- describir qué hace la función: .....
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones r u1 u2 = mismosElementos (publicacionesQueLeGustanA r u1 ) (publicacionesQueLeGustanA r u2)

mismosElementos :: (Eq t) => [t] -> [t] -> Bool
mismosElementos t1 t2 = (longitud t1 == longitud t2) && (elementosContenidosEn t1 t2) && (elementosContenidosEn t2 t1)

elementosContenidosEn :: (Eq t) => [t] -> [t] -> Bool
elementosContenidosEn [] t2 = True
elementosContenidosEn (t1:t1s) t2 = (pertenece t1 t2) && (elementosContenidosEn t1s t2)

-- describir qué hace la función: .....
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel red u = (longitud (publicacionesDe red u) > 0) && (tieneUnSeguidorFielEnListaCandidatos red u candidatosSeguidorFiel)
                            where
                                candidatosSeguidorFiel = removerElemento (usuarios red) u

removerElemento :: (Eq t) => [t] -> t -> [t]
removerElemento [] _ = []
removerElemento (l:ls) e    | l == e = removerElemento ls e
                            | otherwise = l : (removerElemento ls e)

tieneUnSeguidorFielEnListaCandidatos :: RedSocial -> Usuario -> [Usuario] -> Bool
tieneUnSeguidorFielEnListaCandidatos red u [u2] = elementosContenidosEn (publicacionesDe red u) (publicacionesQueLeGustanA red u2)
tieneUnSeguidorFielEnListaCandidatos red u (u2:u2s) = tieneUnSeguidorFielEnListaCandidatos red u [u2] || tieneUnSeguidorFielEnListaCandidatos red u u2s

-- describir qué hace la función: .....
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos red u1 u2 = estanRelacionadosIndirectamente (relaciones red) u1 u2

estanRelacionadosIndirectamente :: [Relacion] -> Usuario -> Usuario -> Bool
estanRelacionadosIndirectamente r u1 u2 | (not (sigueHabiendoUsuarioOrigen r u1)) = False
                                        | seEncontroRelacion r u1 u2 = True
                                        | otherwise = estanRelacionadosIndirectamente relacionSimplificada u1 u2
                                        where
                                            relacionSimplificada = simplificarRelacionesDeUsuario r u1

simplificarRelacionesDeUsuario :: [Relacion] -> Usuario -> [Relacion]
simplificarRelacionesDeUsuario r u = eliminarRelacionesMismoUsuario (reemplazarConUsuarioAUsuario (relacionNormalizada) u (primerRelacionadoDeUsuario relacionNormalizada u))
                                     where relacionNormalizada = ponerSiemprePrimeroAUsuario (eliminarRelacionesMismoUsuario r) u

sigueHabiendoUsuarioOrigen :: [Relacion] -> Usuario -> Bool
sigueHabiendoUsuarioOrigen [r] u = (fst r == u) || (snd r == u)
sigueHabiendoUsuarioOrigen (r:rs) u = sigueHabiendoUsuarioOrigen [r] u || sigueHabiendoUsuarioOrigen rs u

seEncontroRelacion :: [Relacion] -> Usuario -> Usuario -> Bool
seEncontroRelacion [r] u1 u2 = ((fst r == u1) && (snd r == u2)) || ((fst r == u2) && (snd r == u1))
seEncontroRelacion (r:rs) u1 u2 = seEncontroRelacion [r] u1 u2 || seEncontroRelacion rs u1 u2

reemplazarConUsuarioAUsuario :: [Relacion] -> Usuario -> Usuario -> [Relacion]
reemplazarConUsuarioAUsuario [r] u1 u2  | fst r == u2 = [(u1, snd r)]
                                        | snd r == u2 = [(fst r, u1)]
                                        | otherwise = [r]
reemplazarConUsuarioAUsuario (r:rs) u1 u2   = (head (reemplazarConUsuarioAUsuario [r] u1 u2)) : (reemplazarConUsuarioAUsuario rs u1 u2 )

ponerSiemprePrimeroAUsuario :: [Relacion] -> Usuario -> [Relacion]
ponerSiemprePrimeroAUsuario [r] u   | snd r == u = [(snd r, fst r)]
                                    | otherwise = [r]
ponerSiemprePrimeroAUsuario (r:rs) u = (head (ponerSiemprePrimeroAUsuario [r] u)) : (ponerSiemprePrimeroAUsuario rs u)

primerRelacionadoDeUsuario :: [Relacion] -> Usuario -> Usuario
primerRelacionadoDeUsuario [r] u = snd r
primerRelacionadoDeUsuario (r:rs) u | fst r == u = snd r
                                    | otherwise = primerRelacionadoDeUsuario rs u

eliminarRelacionesMismoUsuario :: [Relacion] -> [Relacion]
eliminarRelacionesMismoUsuario [] = []
eliminarRelacionesMismoUsuario (r:rs)   | fst r == snd r = eliminarRelacionesMismoUsuario rs
                                        | otherwise = r : eliminarRelacionesMismoUsuario rs
