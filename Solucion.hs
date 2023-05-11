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
                          |  otherwise = nombreDeUsuario (head usus) : nombresDeUsuariosAux (tail usus)

{- Para este ejercicio, creamos una función auxiliar que recibe la lista de todos los usuarios de la red, esta va recorriendo toda la lista
y va sumando a la lista que devuelve solo los nombres usuarios usando la recursividad, teniendo solo una lista vacía al final y en las
vueltas anteriores agregando los nombres. -}

-- describir qué hace la función: .....
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe rs usu = amigosDeAux (relaciones rs) usu

amigosDeAux:: [Relacion] -> Usuario -> [Usuario]
amigosDeAux rel usu | rel == [] = []
                    | fst n == usu = snd n : amigosDeAux (tail rel) usu
                    | snd n == usu = fst n : amigosDeAux (tail rel) usu
                    | otherwise = amigosDeAux (tail rel) usu
                    where n = head rel
                    
{- Para este ejercicio, creamos una función auxiliar que se fija si el usuario aparece en la primera o en la segunda posición de la relación,
si este usuario forma parte de la relación, agrega al otro usuario a la lista que va creando de manera recursiva. -}

-- describir qué hace la función: .....
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos rs usu = longitud (amigosDe rs usu) 

longitud:: (Eq t)  => [t] -> Int
longitud [] = 0
longitud (_:xs) = 1 + longitud xs 

{- Para este ejercicio, creamos la función longitud. cantidadDeAmigos esta definida como la longitud de la lista amigosDe el usuario en
cuestión. La función longitud la creamos de manera genérica para poder utilizarla más adelante. -}

-- describir qué hace la función: ..... 
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos rs = usuarioConMasAmigosAux rs (head (usuarios rs)) (tail (usuarios rs)) 

usuarioConMasAmigosAux:: RedSocial -> Usuario -> [Usuario] -> Usuario
usuarioConMasAmigosAux rs usu usuList | usuList == [] = usu
                                      | cantidadDeAmigos rs usu >= cantidadDeAmigos rs siguiente = usuarioConMasAmigosAux rs usu (tail usuList)
                                      | otherwise = usuarioConMasAmigosAux rs siguiente (tail usuList)
                                       where siguiente = head usuList

{- Para este ejercicio, creamos una función auxiliar en donde evalua al primer elemento de la lista con el siguiente, y compara cual tiene
más amigos. Se queda con el que más tiene de los dos y avanza para evaluarlo con el siguiente, de manera recursiva va revisando cual es el
que más tiene de a dos, hasta que se termine la lista y devuelva el último que va a ser el que más tenga. -}

-- describir qué hace la función: .....
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos rs = cantidadDeAmigos rs (usuarioConMasAmigos rs) > 1000000

{- Para este ejercicio, definimos la función estaRobertoCarlos que nos pide ver si existe algún usuario con más de un millón de amigos, 
pero deberíamos fijarnos uno por uno si pasa el millón de amigos. De esta manera, solo nos fijamos si existe alguno, no importa cuantos, 
cuantos amigos tenga, o quién sea, solo nos importa si existe alguno que tenga más de un millón. Para esto, nos fijamos si el usuario que más
amigos tiene, pasa el millón de amigos y devolvemos eso. -}

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

{- Para este ejercicio, creamos una función auxiliar que nos permite filtrar todas las publicaciones de una lista de publicaciones (en este
ejercicio puntualmente, lo estamos haciendo con todas las publicaciones de la Red social) y deja solamente las que hayan sido publicadas por
el usuario en cuestión. -}

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


{- Para este ejercicio, creamos dos funciones de manera genericas que vamos a utilizar más adelante, estas siendo Tercero y existeEn.
Tercero sirve para agarrar el tercer elemento de un tripla (Principalmente para usarlo como fst y snd pero para el tercer elemento y para poder
agarrar los me gusta de una publicación). existeEn sirve para poder revisar si un elemento esta en una lista de elementos.
También necesitamos de una función auxiliar que al pasarle un usuario y una lista de publicaciones (todas las de la Red Social), filtraría
todas las publicaciones revisando si tienen el me gusta de ese usuario, sino tienen me gusta no será agregada a la lista que se devulve,
formada de manera recursiva, porque luego de revisar todas se devuelve una lista vacía cuando en los pasos anteriores se agregan las 
publicaciones que tienen el me gusta. -}

-- describir qué hace la función: .....
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones rs usu1 usu2 = lglmp_Aux (publicaciones rs) usu1 usu2

lglmp_Aux :: [Publicacion] -> Usuario -> Usuario -> Bool
lglmp_Aux pubs usu1 usu2 | pubs == [] = True
                         | existeEn mgs usu1 == existeEn mgs usu2 = lglmp_Aux (tail pubs) usu1 usu2
                         | otherwise = False
                         where mgs = tercero pub1
                               pub1 = head pubs

{- Para este ejercicio, solo necesitamos de una función auxiliar que revisa todas las publicaciones de la Red Social y revisa que el me gusta
del usu1 y del usu2 este en el mismo estado, que la existencia del me gusta de los dos usuarios tenga el mismo estado, si esto es así, se 
seguirá evaluando con las siguientes publicaciones, hasta que alguna fallé, entonces será falso. Si no falla y se revisaron todas las 
publicaciones, será verdadero. -}

-- describir qué hace la función: .....
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel rs usu = seguidorFielAux rs (tercero (head (publicacionesDe rs usu))) usu

seguidorFielAux :: RedSocial -> [Usuario] -> Usuario -> Bool
seguidorFielAux rs mgs usu | mgs == [] = False
                           | primero == usu = seguidorFielAux rs (tail mgs) usu
                           | longitud (filtrarPubsUsu (publicacionesQueLeGustanA rs primero) usu) == longitud (publicacionesDe rs usu) = True
                           | otherwise = seguidorFielAux rs (tail mgs) usu
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

Para agrAmigos, recorremos la lista de amigos que ya tenemos, sin dejar afuera a nadie, vamos recursivamente por la lista, agregando a los 
amigos de todos en la lista, incluyendose a sí mismo. 

Para eliminar a los repetidos, utilizamos la función sacarRepes en donde vemos como recibe una lista de tipo genérico, en la que agarramos
el primer elemento, revisamos si esta en el resto de la fila (importante que sea el tail porque sino él seguro aparecería en la posición 
original y si no lo agregamos por eso, puede que lo perdamos) y si aparece en el resto, no lo agregamos a la lista. Si el elemento no esta en
el resto de la lista, no se repite de vuelta, significa que debemos agregarlo, entonces lo agrega a la lista que vamos a devolver, formada de 
manera recursiva. -}
