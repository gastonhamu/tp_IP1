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

{- Para este ejercicio, creamos una función auxiliar que recibe la lista de todos los usuarios de la red, esta va recorriendo toda la lista
y va sumando a la lista que devuelve solo los nombres usuarios usando la recursividad, teniendo solo una lista vacía al final y en las
vueltas anteriores agregando los nombres. -}

-- describir qué hace la función: .....
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe RS usu = amigosDeAux (relaciones RS) usu

amigosDeAux:: [Relacion] -> Usuario -> [Usuario]
amigosDeAux rel usu | rel == [] = []
                    | fst n == usu = snd n ++ amigosDeAux (tail rel) usu
                    | snd n == usu = fst n ++ amigosDeAux (tail rel) usu
                    | otherwise = amigosDeAux (tail rel) usu
                    where n = head rel
                    
{- Para este ejercicio, creamos una función auxiliar que se fija si el usuario aparece en la primera o en la segunda posición de la relación,
si este usuario forma parte de la relación, agrega al otro usuario a la lista que va creando de manera recursiva. -}

-- describir qué hace la función: .....
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos RS usu = longitud (amigosDe RS usu) 

longitud:: (Eq t)  => [t] -> Int
longitud [] = 0
longitud (_:xs) = 1 + longitud xs 

{- Para este ejercicio, creamos la función longitud. cantidadDeAmigos esta definida como la longitud de la lista amigosDe el usuario en
cuestión. La función longitud la creamos de manera genérica para poder utilizarla más adelante. -}

-- describir qué hace la función: ..... 
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos RS = usuarioConMasAmigosAux (head (usuarios RS)) (tail (usuarios RS)) 

usuarioConMasAmigosAux:: Usuario -> [Usuario] -> Usuario
usuarioConMasAmigosAux usu usuList | usuList == [] = usu
                                   | cantidadDeAmigos usu >= cantidadDeAmigos siguiente = usuarioConMasAmigosAux usu (tail usuList)
                                   | otherwise = usuarioConMasAmigosAux siguiente (tail usuList)
                                   where siguiente = head usuList

{- Para este ejercicio, creamos una función auxiliar en donde evalua al primer elemento de la lista con el siguiente, y compara cual tiene
más amigos. Se queda con el que más tiene de los dos y avanza para evaluarlo con el siguiente, de manera recursiva va revisando cual es el
que más tiene de a dos, hasta que se termine la lista y devuelva el último que va a ser el que más tenga. -}

-- describir qué hace la función: .....
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos RS = cantidadDeAmigos (usuarioConMasAmigos RS) > 1000000

{- Para este ejercicio, definimos la función estaRobertoCarlos que nos pide ver si existe algún usuario con más de un millón de amigos, 
pero deberíamos fijarnos uno por uno si pasa el millón de amigos. De esta manera, solo nos fijamos si existe alguno, no importa cuantos, 
cuantos amigos tenga, o quién sea, solo nos importa si existe alguno que tenga más de un millón. Para esto, nos fijamos si el usuario que más
amigos tiene, pasa el millón de amigos y devolvemos eso. -}

-- describir qué hace la función: .....
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe RS usu = filtrarPubsUsu (Publicacion RS) usu

filtrarPubsUsu :: [Publicacion] -> Usuario -> [Publicacion]
filtrarPubsUsu pubs usu | pubs == [] = []
                                     | fst (head pubs) == usu = head pubs ++ filtrarPubsUsu (tail pubs) usu
                                     | otherwise = filtrarPubsUsu (tail pubs) usu

{- Para este ejercicio, creamos una función auxiliar que nos permite filtrar todas las publicaciones de una lista de publicaciones (en este
ejercicio puntualmente, lo estamos haciendo con todas las publicaciones de la Red social) y deja solamente las que hayan sido publicadas por
el usuario en cuestión. -}

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


{- Para este ejercicio, creamos dos funciones de manera genericas que vamos a utilizar más adelante, estas siendo Tercero y existeEn.
Tercero sirve para agarrar el tercer elemento de un tripla (Principalmente para usarlo como fst y snd pero para el tercer elemento y para poder
agarrar los me gusta de una publicación). existeEn sirve para poder revisar si un elemento esta en una lista de elementos.
También necesitamos de una función auxiliar que al pasarle un usuario y una lista de publicaciones (todas las de la Red Social), filtraría
todas las publicaciones revisando si tienen el me gusta de ese usuario, sino tienen me gusta no será agregada a la lista que se devulve,
formada de manera recursiva, porque luego de revisar todas se devuelve una lista vacía cuando en los pasos anteriores se agregan las 
publicaciones que tienen el me gusta. -}

-- describir qué hace la función: .....
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones RS usu1 usu2 = lglmp_Aux (publicaciones RS) usu1 usu2

lglmp_Aux :: [Publicacion] -> Usuario -> Usuario -> Bool
lglmp_Aux pubs usu1 usu2 | pubs == [] = True
                         | existeEn mgs usu1 == existeEn mgs usu2 = lglmp_Aux (tail pubs) usu1 usu2
                         | otherwise = False
                         where mgs = tercero pub1, pub1 = head pubs

{- Para este ejercicio, solo necesitamos de una función auxiliar que revisa todas las publicaciones de la Red Social y revisa que el me gusta
del usu1 y del usu2 este en el mismo estado, que la existencia del me gusta de los dos usuarios tenga el mismo estado, si esto es así, se 
seguirá evaluando con las siguientes publicaciones, hasta que alguna fallé, entonces será falso. Si no falla y se revisaron todas las 
publicaciones, será verdadero. -}

-- describir qué hace la función: .....
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel RS usu = SeguidorFielAux RS (tercero (head (publicacionesDe RS usu))) usu

SeguidorFielAux :: RedSocial -> [Usuario] -> Usuario -> Bool
SeguidorFielAux RS mgs usu | mgs == [] = False
                           | primero == usu = SeguidorFielAux RS (tail mgs) usu
                           | longitud (filtrarPubsUsu (publicacionesQueLeGustanA RS primero) usu) == longitud (publicacionesDe RS usu) = True
                           | otherwise = SeguidorFielAux RS (tail mgs) usu
                           where primero = head mgs

{- Para este ejercicio, solo necesitamos de una funcion auxiliar que recibe la Red Social, los me gusta de la primer publicación del usuario
en cuestión y el usuario. Nosotros tenemos que buscar si existe algún usuario fiel, no tenemos opciones, entonces comenzamos por agarrar los
me gusta de la primer publicación, esas son nuestras opciones. 

La función auxiliar primero evalúa si la lista esta vacía, eso implica que ya se revisaron todos y que ninguno cumplió, eso devuelve falso.
Luego nos fijamos que no sea él mismo (en el enunciado pedía que NO sea él mismo), eso pasa al siguiente. Ya sabiendo que hay otra persona 
para evaluar que no es él, utilizamos la función auxiliar de publicacionesDe, que agarra una lista de publicaciones y las filtra para que solo
queden las que son de un usuario específico. Entonces la utilizamos dandole de parámetro las publicaciones que le gustan al posible seguidor 
fiel y las filtre para saber cuantas tiene que son de este usuario. Si la cantidad restantes después de la filtración es igual a la cantidad
de publicaciones del usuario, ese posible será un seguidor fiel. Sino, se evaluará con el siguiente hasta encontrar un seguidor fiel o 
quedarse sin opciones. -}


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
existeSecuenciaDeAmigos RS usu1 usu2  = hayVinculo RS [usu1] usu2 0

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

{- Para este ejercicio, creamos algunas funciones auxiliares con funciones específicas y puntuales. La primer funcion auxiliar recibe la red 
social, una lista de usuarios, el usuario a llegar, y la cantidad vieja de elementos en la lista de usuarios. La lógica de esta función 
auxiliar es fijarse que luego de agregar los amigos de las personas en la lista, aparezca el usuario al que queremos llegar, y para poder parar
y saber que no existe, es cuando agregamos a los amigos de los que ya tenemos en la lista y la cantidad de amigos no modifica, para eso esta
la cantidad vieja. Esto funciona porque no nos pide trazar la secuencia de amigos para llegar del primer usuario al segundo.

Entonces, la función primero evalúa si la cantidad de elementos de la lista nueva es igual a la vieja, si es cierto implica que no se agregaron
amigos nuevos y que el usuario a llegar no esta, así que el ejercicio devolvería falso.
En la siguiente guarda pregunta si esta el usuario a llegar en la lista, si esta existe una conexión entre el usuario original y al que hay
que llegar, entonces devuelve True.
El otro caso posible es que hay que agregar a los amigos de las personas que ya tenemos, para eso nos encargamos de agregar a los amigos
y de sacar a los repetidos, sumado a que hay que encargarnos de cambiar la cantidad vieja de elementos en la lista por la actual.

Para agregar amigos, recorremos la lista para agregar amigos, sin dejar afuera a nadie, vamos recursivamente por la lista, agregando a los 
amigos de todos en la lista, incluyendose a sí mismo.

Para eliminar a los repetidos, va elemento por elemento fijandose si existe en la tail de la lista, si existe no se agrega, sino, se agrega.
-}

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