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
usuarioConMasAmigos RS = usuarioConMasAmigosAux (head (usuarios RS)) (tail (usuarios RS)) 

usuarioConMasAmigosAux:: Usuario -> [Usuario] -> Usuario
usuarioConMasAmigosAux usu usuList | usuList == [] = usu
                                   | cantidadDeAmigos usu >= cantidadDeAmigos siguiente = usuarioConMasAmigosAux usu (tail usuList)
                                   | otherwise = usuarioConMasAmigosAux siguiente (tail usuList)
                                   where siguiente = head usuList

-- describir qué hace la función: .....
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos RS = cantidadDeAmigos (usuarioConMasAmigos RS) > 1000000

-- describir qué hace la función: .....
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe RS usu = filtrarPubsUsu (Publicacion RS) usu

filtrarPubsUsu :: [Publicacion] -> Usuario -> [Publicacion]
filtrarPubsUsu pubs usu | pubs == [] = []
                                     | fst (head pubs) == usu = head pubs ++ filtrarPubsUsu (tail pubs) usu
                                     | otherwise = filtrarPubsUsu (tail pubs) usu

-- describir qué hace la función: .....
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA RS usu = pqlga_Aux usu (publicaciones RS)

pqlga_Aux :: Usuario -> [Publicacion] -> [Publicacion]
pqlga_Aux usu pubs | pubs == [] = []
                   | existeEn mgs usu = pub1 ++ pqlga_Aux usu (tail pubs)
                   | otherwise = pqlga_Aux usu (tail pubs)
                   where mgs = tercero pub1, pub1 = head pubs

existeEn :: (Eq t) => [t] -> t -> Bool
existeEn list obj | list == [] = False
                  | head list == obj = True
                  | otherwise = existeEn (tail list) obj

tercero:: (a,b,c) -> c
tercero (_,_,c) = c

-- describir qué hace la función: .....
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones RS usu1 usu2 = lglmp_Aux (publicaciones RS) usu1 usu2

lglmp_Aux :: [Publicacion] -> Usuario -> Usuario -> Bool
lglmp_Aux pubs usu1 usu2 | pubs == [] = True
                         | existeEn mgs usu1 == existeEn mgs usu2 = lglmp_Aux (tail pubs) usu1 usu2
                         | otherwise = False
                         where mgs = tercero pub1, pub1 = head pubs

-- describir qué hace la función: .....
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel RS usu = SeguidorFielAux RS (tercero (head (publicacionesDe RS usu))) usu

SeguidorFielAux :: RedSocial -> [Usuario] -> Usuario -> Bool
SeguidorFielAux RS mgs usu | mgs == [] = []
                            | primero == usu = SeguidorFielAux RS (tail mgs) usu2
                            | longitud (filtrarPubsUsu (publicacionesQueLeGustanA RS primero) usu) == longitud (publicacionesDe RS usu) = True
                            | otherwise = SeguidorFielAux RS (tail mgs) usu
                            where primero = head mgs


{-SeguidorFielAux :: RedSocial -> [Usuario] -> Usuario -> Bool
SeguidorFielAux RS mgs usu | mgs == [] = False
                                  | head mgs == usu = SeguidorFielAux RS (tail mgs) usu
                                  | leGustanTodas (head mgs) (tail(publicacionesDe RS usu)) = True
                                  | otherwise = SeguidorFielAux RS (tail mgs) usu
                                   
leGustanTodas :: Usuario -> [Publicacion] -> Bool
leGustanTodas usu pubs | pubs == [] = True
                       | existeEn mgs usu = leGustanTodas usu (tail pubs)
                       | otherwise = False
                       where mgs = tercero pub1, pub1 = head pubs
-}
-- describir qué hace la función: .....
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos RS usu1 usu2 | sonAmigos RS usu1 usu2 = True
                                     | otherwise = esda_Aux RS usu2 (amigosDe usu1) [usu1]

hayVinculo :: RedSocial -> [Usuario] -> Usuario -> Integer -> Bool
hayVinculo RS usuList usu2 cantVieja | longitud usuList == cantVieja = False
                                     | existeEn usuList usu2 = True
                                     | otherwise = hayVinculo RS (sacarRepes (agrAmigos RS usuList)) usu2 (longitud usuList)

agrAmigos :: RedSocial -> [Usuario] -> [Usuario]
agrAmigos RS usuList | usuList == [] = []
                     | otherwise = amigosDe (head usuList) ++ (head usuList) ++ agrAmigos RS (tail usuList)

sacarRepes :: (Eq t) => [t] -> [t]
sacarRepes List | existeEn (tail List) (head List) = sacarRepes (tail List)
                | otherwise = (head List) ++ sacarRepes (tail List)

{-
esda_Aux :: RedSocial -> Usuario -> [Usuario] -> [Usuario] -> Bool
esda_Aux RS usu2 usuList usuYaTest | usuList == [] = False
                                   | existeEn usuYaTest usu1 = esda_Aux RS usu2 (tail usuList) usuYaTest
                                   | sonAmigos RS usu1 usu2 = True
                                   | otherwise = susAmigosTest RS usu2 (amigosDe usu1) (usu1 ++ usuYaTest) || esda_Aux RS usu2 (tail usuList) (usu1 ++ usuYaTest)
                                   where usu1 = head usuList

susAmigosTest :: RedSocial -> Usuario -> [Usuario] -> [Usuario] -> Bool
susAmigosTest RS usu2 usuList usuYaTest | usuList == [] = False
                                        | existeEn usuYaTest usu1 = esda_Aux RS usu2 (tail usuList) usuYaTest
                                        | sonAmigos RS usu1 usu2 = True
                                        | otherwise = susAmigosTest RS usu2 (amigosDe usu1) (usu1 ++ usuYaTest) || susAmigosTest RS usu2 (tail usuList) (usu1 ++ usuYaTest)
                                        where usu1 = head usuList
                                   

sonAmigos :: RedSocial -> Usuario -> Usuario -> Bool
sonAmigos RS usu1 usu2 = existeEn (relaciones RS) (usu1, usu2) || existeEn (relaciones RS) (usu2, usu1) -}