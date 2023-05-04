-- Completar con los datos del grupo
--
-- Nombre de Grupo: Maquinas
-- Integrante 1: Gastón Julián Hamu, gastonhamu@gmail.com, 1149/22
-- Integrante 2: Tatiana Grinspan, tatugrin@gmail.com, 350/22
-- Integrante 3: Santiago Riso, santiagoriso@gmail.com , 792/22
-- Integrante 4: Dante Waisman, waismandante@gmail.com, 1163/22

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
nombresDeUsuarios RS = nombresDeUsuariosAux (usuarios RS)

nombresDeUsuariosAux:: [Usuario] -> [String]
nombresDeUsuariosAux usus |  usus == [] = []
                          |  otherwise = nombreDeUsuario [head usus] ++ nombresDeUsuariosAux (tail usus)


-- describir qué hace la función: .....
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe RS usu = amigosDeAux (relaciones RS) usu

amigosDeAux:: [Relacion] -> Usuario -> [Usuario]
amigosDeAux rel usu | rel == [] = []
                    | fst n == usu = snd n ++ amigosDeAux (tail rel) usu
                    | snd n == usu = fst n ++ amigosDeAux (tail rel) usu
                    | otherwise = amigosDeAux (tail rel) usu
                    where n = head rel
                    

-- describir qué hace la función: .....
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos RS usu = longitud (amigosDe RS usu) 

longitud:: (Eq t)  => [t] -> Int
longitud [] = 0
longitud (_:xs) = 1 + longitud xs 

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
existeSecuenciaDeAmigos = undefined
