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

nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios = undefined

-- describir qué hace la función: .....
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe = undefined

-- describir qué hace la función: .....
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos = undefined

-- describir qué hace la función: .....
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos = undefined

-- describir qué hace la función: .....
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos = undefined

-- describir qué hace la función: .....
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe = undefined

-- describir qué hace la función: .....
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA = undefined

-- describir qué hace la función: .....
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones = undefined

-- describir qué hace la función: .....
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel = undefined

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

pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece x (y:ys)  | (x == y) || (pertenece x ys) = True
                    | otherwise = False

usuariosValidos :: [Usuario] -> Bool
usuariosValidos [u] = usuarioValido u
usuariosValidos (u:us) = usuarioValido u && noHayIdsRepetidos (u:us) && usuariosValidos us

usuarioValido :: Usuario -> Bool
usuarioValido u = (idDeUsuario u) > 0 && (longitud (nombreDeUsuario u)) > 0

longitud :: [t] -> Int
longitud [] = 0
longitud (_:xs) = 1 + longitud xs

noHayIdsRepetidos :: [Usuario] -> Bool
noHayIdsRepetidos us = sinRepetidos (idsDeUsuarios us)

idsDeUsuarios :: [Usuario] -> [Integer]
idsDeUsuarios [u] = [idDeUsuario u]
idsDeUsuarios (u:us) = (idDeUsuario u) : idsDeUsuarios us

usuariosDeRelacionValidos :: [Usuario] -> [Relacion] -> Bool
usuariosDeRelacionValidos u [r] = (fst r /= snd r) && (pertenece (fst r) u) && (pertenece (snd r) u)
usuariosDeRelacionValidos u (r:rs) = usuariosDeRelacionValidos u [r] && usuariosDeRelacionValidos u rs

relacionesAsimetricas :: [Relacion] -> Bool
relacionesAsimetricas [r] = True
relacionesAsimetricas (r:rs)  = not (pertenece (snd r, fst r) rs) && relacionesAsimetricas rs

cadenaDeAmigos :: [Usuario] -> RedSocial -> Bool
cadenaDeAmigos [] r = True
cadenaDeAmigos [x] r = True
cadenaDeAmigos (x:y:ys) r   | relacionadosDirecto x y r && cadenaDeAmigos (y:ys) r = True
                            | otherwise = False

relacionadosDirecto :: Usuario -> Usuario -> RedSocial -> Bool
relacionadosDirecto u1 u2 r = (pertenece (u1,u2) (relaciones r)) || (pertenece (u2,u1) (relaciones r)) 

sonDeLaRed :: RedSocial ->  [Usuario] -> Bool
sonDeLaRed r [] = True
sonDeLaRed r (u:us) | pertenece u (usuarios r) && sonDeLaRed r us = True
                    | otherwise = False

empiezaCon :: (Eq t) => t -> [t] -> Bool
empiezaCon x [] = False
empiezaCon x y  | x == head y = True
                | otherwise = False

terminaCon :: (Eq t) => t -> [t] -> Bool
terminaCon x [] = False
terminaCon x [y]    | x == y = True
                    | otherwise = False
terminaCon x (y:ys)  = terminaCon x ys

sinRepetidos :: (Eq t) => [t] -> Bool
sinRepetidos [x] = True
sinRepetidos (x:xs) | pertenece x xs = False
                    | otherwise = sinRepetidos xs
