type Set a = [a]
type Usuario = (Integer, String) -- (id, nombre)
type Relacion = (Usuario, Usuario) -- usuarios que se relacionan
type Publicacion = (Usuario, String, Set Usuario) -- (usuario que publica, texto publicacion, likes)
type RedSocial = (Set Usuario, Set Relacion, Set Publicacion)

-- Funciones basicas

usuarios :: RedSocial -> Set Usuario
usuarios (us, _, _) = us

relaciones :: RedSocial -> Set Relacion
relaciones (_, rs, _) = rs

publicaciones :: RedSocial -> Set Publicacion
publicaciones (_, _, ps) = ps

idDeUsuario :: Usuario -> Integer
idDeUsuario (id, _) = id 

nombreDeUsuario :: Usuario -> String
nombreDeUsuario (_, nombre) = nombre 

usuarioDePublicacion :: Publicacion -> Usuario
usuarioDePublicacion (u, _, _) = u

likesDePublicacion :: Publicacion -> Set Usuario
likesDePublicacion (_, _, us) = us

-- Ejercicios

-- Dada una red social retorna un conjunto con los nombres de todos los usuarios.

nombresDeUsuarios :: RedSocial -> Set String
nombresDeUsuarios redS = redSocial2SetNombres (usuarios redS)

-- Agarra listas de usuarios "[(id,nombre)]" y devuleve una lista de strings "[nombre]"
redSocial2SetNombres :: Set Usuario -> Set String
redSocial2SetNombres [x] = [nombreDeUsuario x]
redSocial2SetNombres (x:xs) = nombreDeUsuario x : redSocial2SetNombres xs






-- Dada una red social y un usuario retorna el conjunto de amigos del mismo
amigosDe :: RedSocial -> Usuario -> Set Usuario
amigosDe (_,rs,_) us = aislarAmigosDeUsuario (aislarTuplasDeAmigosDeUsuario rs us) us

-- Agarra una tupla de usuarios "T" y un usuario "Us" y se fija si Us pertenece a T 
usuarioPerteneceATupla :: (Usuario,Usuario) -> Usuario -> Bool
usuarioPerteneceATupla (x1,x2) n = n == x1 || n == x2

-- Agarra una lista de tuplas de usuarios "L" y un usuario "Us" y devuelve otra lista de tuplas de usuario unicamente con las tuplas a las cuales Us pertenezca
aislarTuplasDeAmigosDeUsuario :: Set (Usuario,Usuario) -> Usuario -> Set (Usuario,Usuario)
aislarTuplasDeAmigosDeUsuario [] _ = []
aislarTuplasDeAmigosDeUsuario (x:xs) n | usuarioPerteneceATupla x n = x : aislarTuplasDeAmigosDeUsuario xs n
                                       | otherwise = aislarTuplasDeAmigosDeUsuario xs n
-- Agarra Una lista de tuplas de usuarios "L" y un usuario "Us" y devuelve una lista de usuarios unicamente con los usuarios distintos a Us
aislarAmigosDeUsuario :: Set (Usuario, Usuario) -> Usuario -> Set Usuario
aislarAmigosDeUsuario [] _ = []
aislarAmigosDeUsuario (x:xs) us | fst x == us = snd x : aislarAmigosDeUsuario xs us
                                | otherwise = fst x : aislarAmigosDeUsuario xs us






-- Dada una red social y un usuario retorna la cantidad de amigos de dicho usuario
-- Alcanza con ver el tamaño de la lista devuelta por la funcion "amigosDe"
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos redS us = length (amigosDe redS us)





-- Dada una red social retorna el usuario con mas amigos. De existir más de uno devuelve cualquiera de ellos.
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos redS = fst (maximoUsuarios (cantidadDeAmigosTodosLosUsuarios redS))

--cantidadDeAmigosDeUsuario :: RedSocial -> Usuario -> (Usuario,Int)
--cantidadDeAmigosDeUsuario redS us = (us ,cantidadDeAmigos redS us)

cantidadDeAmigosTodosLosUsuarios :: RedSocial -> Set (Usuario,Int)
cantidadDeAmigosTodosLosUsuarios ([],_,_) = []
cantidadDeAmigosTodosLosUsuarios (us,rs,ps) = (head us, cantidadDeAmigos (us,rs,ps) (head us)) : cantidadDeAmigosTodosLosUsuarios (tail us,rs,ps) 

maxUsuarios :: (Usuario,Int) -> (Usuario, Int) -> (Usuario,Int)
maxUsuarios (us1,n1) (us2,n2) | n1 >= n2 = (us1,n1)
                              | otherwise = (us2,n2)


maximoUsuarios :: Set (Usuario,Int) -> (Usuario,Int)
maximoUsuarios [x] = x
maximoUsuarios (x:xs) = maxUsuarios x (maximoUsuarios xs) 





-- Dada una red social retorna True si algún usuario tiene más de un millón de amigos
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos redS = masDeUnMillon (cantidadDeAmigosTodosLosUsuarios redS)

masDeUnMillon :: [(Usuario,Int)] -> Bool
masDeUnMillon [x] = snd x > 1000000
masDeUnMillon (x:xs) = snd x > 1000000 || masDeUnMillon xs





-- Dada una red social y un usuario retorna el conjunto de publicaciones del mismo.
publicacionesDe :: RedSocial -> Usuario -> Set Publicacion
publicacionesDe redS us = aislarPublicacionesDe (publicaciones redS) us

aislarPublicacionesDe :: Set Publicacion -> Usuario -> Set Publicacion
aislarPublicacionesDe [x] us | usuarioDePublicacion x == us = [x]
                             | otherwise = []
aislarPublicacionesDe (x:xs) us | usuarioDePublicacion x == us = x : aislarPublicacionesDe xs us
                                | otherwise = aislarPublicacionesDe xs us 




-- Dada una red social y un usuario retorna el conjunto de publicaciones a las que el usuario les dió like.
publicacionesQueLeGustanA :: RedSocial -> Usuario -> Set Publicacion
publicacionesQueLeGustanA redS = aislarPublicacionesLikeadasDe (publicaciones redS)

aislarPublicacionesLikeadasDe :: Set Publicacion -> Usuario -> Set Publicacion
aislarPublicacionesLikeadasDe [x] us | usuarioPerteneceAConjunto (likesDePublicacion x) us = [x]
                                     | otherwise = []
aislarPublicacionesLikeadasDe (x:xs) us | usuarioPerteneceAConjunto (likesDePublicacion x) us  = x : aislarPublicacionesLikeadasDe xs us
                                        | otherwise = aislarPublicacionesLikeadasDe xs us
 
usuarioPerteneceAConjunto :: Set Usuario -> Usuario -> Bool
usuarioPerteneceAConjunto [x] us = x == us 
usuarioPerteneceAConjunto (x:xs) us = x == us || usuarioPerteneceAConjunto xs us





-- Dada una red social y dos usuarios indica si les gustan las mismas publicaciones
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones redS us1 us2 = igualesConjuntosDePublicaciones (publicacionesQueLeGustanA redS us1) (publicacionesQueLeGustanA redS us2)

perteneceConjuntosDePublicaciones :: Set Publicacion -> Set Publicacion -> Bool
perteneceConjuntosDePublicaciones [] _ = True
perteneceConjuntosDePublicaciones (x:xs) (y:ys) = elem x (y:ys) && perteneceConjuntosDePublicaciones xs (y:ys)

igualesConjuntosDePublicaciones :: Set Publicacion -> Set Publicacion -> Bool
igualesConjuntosDePublicaciones ps1 ps2 = perteneceConjuntosDePublicaciones ps1 ps2 && perteneceConjuntosDePublicaciones ps2 ps1






-- Dada una red social y un usuario u, indica si existe un usuario que le puso like a todas las publicaciones de u.
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel ([],_,_) _ = False
tieneUnSeguidorFiel (us,rs,ps) us1 = perteneceConjuntosDePublicaciones (publicacionesDe (us,rs,ps) us1) (publicacionesQueLeGustanA (us,rs,ps) (head us)) || tieneUnSeguidorFiel ((tail us),rs,ps) us1 





-- Dada una red social y dos usuarios, indica si existe una secuencia de usuarios relacionados para llegar del primero al segundo.
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos = undefined

--armarCadena :: RedSocial -> Usuario -> Usuario -> [Integer] -> Bool
--armarCadena redS us1 us2 (x:xs) | esAmigoDe redS us1 us2 = True
--                                |   = 

esAmigoDe :: RedSocial -> Usuario -> Usuario -> Bool
esAmigoDe redS us1 us2 = usuarioPerteneceAConjunto (amigosDe redS us1) us2


fst3 (x, _, _) = x
snd3 (_, y, _) = y
trd3 (_, _, z) = z

user1 = (1,"Juan")
user2 = (2,"Laura")
user3 = (3,"Fran")
user4 = (4,"Vicky")
user5 = (5,"Flor")
user6 = (6,"Ludmi")
user7 = (7,"Pedro")
user8 = (8,"Tomas")
user9 = (9,"Ale")
userSet = [user1,user2,user3,user4,user5,user6,user7,user8,user9]

relacion1 = (user1,user4)
relacion2 = (user4,user5)
relacion3 = (user4,user6)
relacion4 = (user6,user8)
relacion5 = (user2,user3)
relacion6 = (user2, user7)
relacion7 = (user2, user9)
relacion8 = (user7, user9)

relacionSet = [relacion1,relacion2,relacion3,relacion4,relacion5,relacion6,relacion7,relacion8]

post1 = (user1,"MUY BUENAS CHAVALES",[user2,user3,user1])
post2 = (user2,"TODO BIEN? TODO CORRECTO?",[user1,user3,user4])
post3 = (user3,"Y YO QUE ME ALEGRO",[user2])
post4 = (user4,"ENTONCES TU Y YO SOMOS GEMELAS",[user1,user2,user3])
post5 = (user1,"SEGUNDA PUBLICACION",[user3])
post6 = (user1,"TERCERA PUBLICACION",[user2])
post7 = (user1,"CUARTA PUBLICACION",[user2,user4])

postSet = [post1,post2,post3,post4,post5,post6,post7]
postSet1 = [post1,post2,post4]
postSet2 = [post1,post2,post3,post4]


redSocialSet = (userSet,relacionSet,postSet)
