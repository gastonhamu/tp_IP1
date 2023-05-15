import Test.HUnit
import Solucion

main = runTestTT tests

tests = test [
    " nombresDeUsuarios 1" ~: (nombresDeUsuarios redA) ~?= ["Juan","Natalia","Pedro","Mariela"],

    " amigosDe 1" ~: (amigosDe redA usuario1) ~?= [usuario2, usuario4],

    " cantidadDeAmigos 1" ~: (cantidadDeAmigos redA usuario1) ~?= 2,

    " usuarioConMasAmigos 1" ~: expectAny (usuarioConMasAmigos redA) [usuario2, usuario4],

    " estaRobertoCarlos 1" ~: (estaRobertoCarlos redA) ~?= False,

    " publicacionesDe 1" ~: (publicacionesDe redA usuario2) ~?= [publicacion2_1, publicacion2_2],

    " publicacionesQueLeGustanA 1" ~: (publicacionesQueLeGustanA redA usuario1) ~?= [publicacion2_2, publicacion4_1],

    " lesGustanLasMismasPublicaciones 1" ~: (lesGustanLasMismasPublicaciones redB usuario1 usuario3) ~?= True,

    " lesGustanLasMismasPublicaciones 2" ~: (lesGustanLasMismasPublicaciones redA usuario1 usuario4) ~?= False,

    " tieneUnSeguidorFiel 1" ~: (tieneUnSeguidorFiel redA usuario1) ~?= True,

    " tieneUnSeguidorFiel 2" ~: (tieneUnSeguidorFiel redA usuario2) ~?= True,

    " tieneUnSeguidorFiel 3" ~: (tieneUnSeguidorFiel redA usuario3) ~?= False,

    " existeSecuenciaDeAmigos 1" ~: (existeSecuenciaDeAmigos redA usuario1 usuario3) ~?= True,

    " existeSecuenciaDeAmigos 2" ~: (existeSecuenciaDeAmigos redA usuario1 usuario6) ~?= False,

    " existeSecuenciaDeAmigos 3" ~: (existeSecuenciaDeAmigos redA usuario1 usuario5) ~?= False,

    " nombresDeUsuarios C_1" ~: (nombresDeUsuarios redC) ~?= ["Federico", "Gaston", "Santiago", "Dante", "Tatiana", "Martin", "Felipe", "Juan", "Lucas"],

    " amigosDe C_1" ~: (amigosDe redC usuarioC_1) ~?= [usuarioC_9],

    " amigosDe C_2" ~: (amigosDe redC usuarioC_2) ~?= [usuarioC_9, usuarioC_3, usuarioC_4, usuarioC_5],

    " amigosDe C_4" ~: (amigosDe redC usuarioC_4) ~?= [usuarioC_2, usuarioC_3, usuarioC_5],

    " amigosDe C_7" ~: (amigosDe redC usuarioC_7) ~?= [usuarioC_6, usuarioC_8],

    " cantidadDeAmigos C_1" ~: (cantidadDeAmigos redC usuarioC_1) ~?= 1,

    " cantidadDeAmigos C_2" ~: (cantidadDeAmigos redC usuarioC_2) ~?= 4,

    " cantidadDeAmigos C_4" ~: (cantidadDeAmigos redC usuarioC_4) ~?= 3,

    " cantidadDeAmigos C_7" ~: (cantidadDeAmigos redC usuarioC_7) ~?= 2,

    " usuarioConMasAmigos C_1" ~: (usuarioConMasAmigos redC) ~?= usuarioC_2,

    " estaRobertoCarlos C_1" ~: (estaRobertoCarlos redC) ~?= False,

    " publicacionesDe C_1" ~: (publicacionesDe redC usuarioC_1) ~?= [publicacionC_1, publicacionC_2, publicacionC_3, publicacionC_4], 

    " publicacionesDe C_2" ~: (publicacionesDe redC usuarioC_2) ~?= [publicacionC_5], 

    " publicacionesDe C_3" ~: (publicacionesDe redC usuarioC_3) ~?= [],

    " publicacionesQueLeGustanA C_1" ~: (publicacionesQueLeGustanA redC usuarioC_1) ~?= [],
 
    " publicacionesQueLeGustanA C_2" ~: (publicacionesQueLeGustanA redC usuarioC_2) ~?= [publicacionC_1, publicacionC_2, publicacionC_3, publicacionC_4, publicacionC_5],

    " publicacionesQueLeGustanA C_4" ~: (publicacionesQueLeGustanA redC usuarioC_4) ~?= [publicacionC_2, publicacionC_4],

    " lesGustanLasMismasPublicaciones C_48" ~: (lesGustanLasMismasPublicaciones redC usuarioC_4 usuarioC_8) ~?= False,

    " lesGustanLasMismasPublicaciones C_49" ~: (lesGustanLasMismasPublicaciones redC usuarioC_4 usuarioC_9) ~?= True,

    " tieneUnSeguidorFiel C_1" ~: (tieneUnSeguidorFiel redC usuarioC_1) ~?= True,

    " tieneUnSeguidorFiel C_2" ~: (tieneUnSeguidorFiel redC usuarioC_2) ~?= False,

    " existeSecuenciaDeAmigos C_15" ~: (existeSecuenciaDeAmigos redC usuarioC_1 usuarioC_5) ~?= True,

    " existeSecuenciaDeAmigos C_25" ~: (existeSecuenciaDeAmigos redC usuarioC_2 usuarioC_5) ~?= True,

    " existeSecuenciaDeAmigos C_65" ~: (existeSecuenciaDeAmigos redC usuarioC_6 usuarioC_5) ~?= False
 ]

expectAny actual expected = elem actual expected ~? ("expected any of: " ++ show expected ++ "\n but got: " ++ show actual)

-- Ejemplos

usuario1 = (1, "Juan")
usuario2 = (2, "Natalia")
usuario3 = (3, "Pedro")
usuario4 = (4, "Mariela")
usuario5 = (5, "Natalia")
usuario6 = (6, "Felipe")

relacion1_2 = (usuario1, usuario2)
relacion1_3 = (usuario1, usuario3)
relacion1_4 = (usuario4, usuario1) -- Notar que el orden en el que aparecen los usuarios es indistinto

relacion2_3 = (usuario3, usuario2)
relacion2_4 = (usuario2, usuario4)

relacion3_4 = (usuario4, usuario3)

relacion5_6 = (usuario6, usuario5)

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
relacionesA = [relacion1_2, relacion1_4, relacion2_3, relacion2_4, relacion3_4, relacion5_6]
publicacionesA = [publicacion1_1, publicacion1_2, publicacion2_1, publicacion2_2, publicacion3_1, publicacion3_2, publicacion4_1, publicacion4_2]
redA = (usuariosA, relacionesA, publicacionesA)

usuariosB = [usuario1, usuario2, usuario3, usuario5]
relacionesB = [relacion1_2, relacion2_3]
publicacionesB = [publicacion1_3, publicacion1_4, publicacion1_5, publicacion3_1, publicacion3_2, publicacion3_3]
redB = (usuariosB, relacionesB, publicacionesB)


usuarioC_1=(1, "Federico")
usuarioC_2=(2, "Gaston")
usuarioC_3=(3, "Santiago")
usuarioC_4=(4, "Dante")
usuarioC_5=(5, "Tatiana")
usuarioC_6=(6, "Martin")
usuarioC_7=(7, "Felipe")
usuarioC_8=(8, "Juan")
usuarioC_9=(9, "Lucas")
usuariosC = [usuarioC_1, usuarioC_2, usuarioC_3, usuarioC_4, usuarioC_5, usuarioC_6, usuarioC_7, usuarioC_8, usuarioC_9]


relacionC_19 =(usuarioC_1, usuarioC_9)
relacionC_29 =(usuarioC_2, usuarioC_9)
relacionC_23 =(usuarioC_2, usuarioC_3)
relacionC_24 =(usuarioC_4, usuarioC_2)
relacionC_25 =(usuarioC_5, usuarioC_2)
relacionC_34 =(usuarioC_4, usuarioC_3)
relacionC_35 =(usuarioC_3, usuarioC_5)
relacionC_45 =(usuarioC_4, usuarioC_5)
relacionC_67 =(usuarioC_7, usuarioC_6)
relacionC_68 =(usuarioC_6, usuarioC_8)
relacionC_78 =(usuarioC_8, usuarioC_7)

relacionesC = [relacionC_19, relacionC_29, relacionC_23, relacionC_24, relacionC_25, relacionC_34, relacionC_35, relacionC_45, relacionC_67, relacionC_68, relacionC_78]

publicacionC_1=(usuarioC_1, "Tomando Cafe", [usuarioC_2])
publicacionC_2=(usuarioC_1, "Se me cayò el cafe :(", [usuarioC_2, usuarioC_3, usuarioC_4, usuarioC_5, usuarioC_6, usuarioC_7, usuarioC_8, usuarioC_9])
publicacionC_3=(usuarioC_1, "El perro tomo el cafe, tengo miedo", [usuarioC_2, usuarioC_8])
publicacionC_4=(usuarioC_1, "El veterinario me dijo que no pasa nada :)", [usuarioC_2, usuarioC_4, usuarioC_9])


publicacionC_5=(usuarioC_2, "Acabo de ver lo del perro, F", [usuarioC_2])

publicacionesC = [publicacionC_1, publicacionC_2, publicacionC_3, publicacionC_4, publicacionC_5]

redC = (usuariosC, relacionesC, publicacionesC)
