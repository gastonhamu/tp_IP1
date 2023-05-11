module Solucion where

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
nombresDeUsuarios rs = nombresDeUsuariosAux (usuarios rs)

nombresDeUsuariosAux:: [Usuario] -> [String]
nombresDeUsuariosAux usus |  usus == [] = []
                          |  otherwise = [nombreDeUsuario (head usus) nombresDeUsuariosAux (tail usus)

-- describir qué hace la función: .....
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe rs usu = amigosDeAux (relaciones rs) usu

amigosDeAux:: [Relacion] -> Usuario -> [Usuario]
amigosDeAux rel usu | rel == [] = []
                    | fst n == usu = snd n : amigosDeAux (tail rel) usu
                    | snd n == usu = fst n : amigosDeAux (tail rel) usu
                    | otherwise = amigosDeAux (tail rel) usu
                    where n = head rel
                    
-- describir qué hace la función: .....
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos rs usu = longitud (amigosDe rs usu) 

longitud:: (Eq t)  => [t] -> Int
longitud [] = 0
longitud (_:xs) = 1 + longitud xs 

-- describir qué hace la función: ..... 
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos rs = usuarioConMasAmigosAux rs (head (usuarios rs)) (tail (usuarios rs)) 

usuarioConMasAmigosAux:: RedSocial -> Usuario -> [Usuario] -> Usuario
usuarioConMasAmigosAux rs usu usuList | usuList == [] = usu
                                      | cantidadDeAmigos rs usu >= cantidadDeAmigos rs siguiente = usuarioConMasAmigosAux rs usu (tail usuList)
                                      | otherwise = usuarioConMasAmigosAux rs siguiente (tail usuList)
                                       where siguiente = head usuList

-- describir qué hace la función: .....
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos rs = cantidadDeAmigos rs (usuarioConMasAmigos rs) > 1000000

-- describir qué hace la función: .....
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe rs usu = filtrarPubsUsu (publicaciones rs) usu

filtrarPubsUsu :: [Publicacion] -> Usuario -> [Publicacion]
filtrarPubsUsu pubs usu | pubs == [] = []
                        | primerElemento pub1 == usu = pub1 : filtrarPubsUsu (tail pubs) usu
                        | otherwise = filtrarPubsUsu (tail pubs) usu
                         where pub1 = head pubs


primerElemento:: (a,b,c) -> a
primerElemento (a,_,_) = a

-- describir qué hace la función: .....
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA rs usu = pqlga_Aux usu (publicaciones rs)

pqlga_Aux :: Usuario -> [Publicacion] -> [Publicacion]
pqlga_Aux usu pubs | pubs == [] = []
                   | existeEn mgs usu = pub1 : pqlga_Aux usu (tail pubs)
                   | otherwise = pqlga_Aux usu (tail pubs)
                   where mgs = tercero pub1
                         pub1 = head pubs

existeEn :: (Eq t) => [t] -> t -> Bool
existeEn list obj | list == [] = False
                  | head list == obj = True
                  | otherwise = existeEn (tail list) obj

tercero:: (a,b,c) -> c
tercero (_,_,c) = c

-- describir qué hace la función: .....
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones rs usu1 usu2 = lglmp_Aux (publicaciones rs) usu1 usu2

lglmp_Aux :: [Publicacion] -> Usuario -> Usuario -> Bool
lglmp_Aux pubs usu1 usu2 | pubs == [] = True
                         | existeEn mgs usu1 == existeEn mgs usu2 = lglmp_Aux (tail pubs) usu1 usu2
                         | otherwise = False
                         where mgs = tercero pub1
                               pub1 = head pubs

-- describir qué hace la función: .....
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel rs usu = seguidorFielAux rs (tercero (head (publicacionesDe rs usu))) usu

seguidorFielAux :: RedSocial -> [Usuario] -> Usuario -> Bool
seguidorFielAux rs mgs usu | mgs == [] = False
                           | primero == usu = seguidorFielAux rs (tail mgs) usu
                           | longitud (filtrarPubsUsu (publicacionesQueLeGustanA rs primero) usu) == longitud (publicacionesDe rs usu) = True
                           | otherwise = seguidorFielAux rs (tail mgs) usu
                           where primero = head mgs

-- describir qué hace la función: .....
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos rs usu1 usu2  = hayVinculo rs [usu1] usu2 0

hayVinculo :: RedSocial -> [Usuario] -> Usuario -> Int -> Bool
hayVinculo rs usuList usu2 cantVieja | longitud usuList == cantVieja = False
                                     | existeEn usuList usu2 = True
                                     | otherwise = hayVinculo rs (sacarRepes (agrAmigos rs usuList)) usu2 (longitud usuList)

agrAmigos :: RedSocial -> [Usuario] -> [Usuario]
agrAmigos rs usuList | usuList == [] = []
                     | otherwise = usu1: amigosDe rs usu1 ++ agrAmigos rs (tail usuList)
                     where usu1 = head usuList

sacarRepes :: (Eq t) => [t] -> [t]
sacarRepes list | list == [] = []
                | existeEn (tail list) (head list) = sacarRepes (tail list)
                | otherwise = [head list] ++ sacarRepes (tail list)
