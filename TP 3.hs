module Library where
import Data.Char 
import PdePreludat

---------------LA BIBLIOTECA DE BABEL---------------

data Libro = UnLibro{
    titulo :: String,
    cantPaginas :: Number,
    texto :: String,
    generos :: [String]
} deriving (Show, Eq)

data Biblioteca = UnaBiblioteca{
    nombre :: String,
    libros :: [Libro],
    logica :: (Libro -> Bool)
} deriving Show

--Requerimientos funcionales--

--1)
promedioPaginas :: Biblioteca -> Number
promedioPaginas biblioteca = (sum (map cantPaginas (libros biblioteca))) / (length (libros biblioteca))

cantLibrosEnComun :: Biblioteca -> Biblioteca -> Number
cantLibrosEnComun biblioteca1 biblioteca2 = length (filter (==True) (map (libroEstaEnLaBiblioteca biblioteca2) (libros biblioteca1)))

libroEstaEnLaBiblioteca :: Biblioteca -> Libro -> Bool
libroEstaEnLaBiblioteca biblioteca libro = any (==libro) (libros biblioteca)

--2)
bibliotecaDeBabel :: Libro -> Bool
bibliotecaDeBabel libro = ((cantPaginas libro) == 410) && (all (==True) (map simboloValido (texto libro)))

simboloValido :: Char -> Bool
simboloValido simbolo 
    | isAlpha simbolo = True --tuve que agregar la libreria Data.Char
    | simbolo == ' ' = True
    | simbolo == ',' = True
    | simbolo == '.' = True
    | otherwise = False

depurarBiblioteca :: Biblioteca -> Biblioteca
depurarBiblioteca biblioteca = biblioteca {libros = filter (logica biblioteca) (libros biblioteca)}

--3)
cantLibrosGenero :: String -> Biblioteca -> Number
cantLibrosGenero genero biblioteca = length (filter (==(map toLower genero)) (generosMinuscula (foldl (++) [] (map generos (libros biblioteca)))))

todosLosGeneros :: Biblioteca -> [String]
todosLosGeneros biblioteca = borrarDuplicado (generosMinuscula (foldl (++) [] (map generos (libros biblioteca))))

borrarDuplicado :: (Eq a) => [a] -> [a]
borrarDuplicado [] = []
borrarDuplicado (x:xs) = x : borrarDuplicado (filter (/= x) xs)

generoMasComun :: Biblioteca -> String
generoMasComun biblioteca = last (ordenarGeneros (todosLosGeneros biblioteca) biblioteca)

--la funcion ordenarGeneros esta abajo de todo porque el "where" me estaba bugeando la visual de las variables

--Ejemplo de consultas--

principito = UnLibro "principito" 5 "erase una vez" ["aventura"]
harryPotter = UnLibro "harry potter" 410 "espectro patronus" ["fantasia","AveNtuRa"]
libro1 = UnLibro "valido" 410 ", ." ["nada"]
gameOfThrones = UnLibro "game of thrones" 410 "valar morghulis" ["fantasia"]
donQuijote = UnLibro "don quijote" 24 "luchamos contra gigantes" ["aventura"]
sherlockHolmes = UnLibro "sherlock holmes" 200 "elemental mi querido watson" ["policial","drama"]
laTragedia = UnLibro "la tragedia" 410 "te amo" ["romantica","drama"]
libroBasico = UnLibro "libro" 410 "Hola, como estas. " ["nada"]

bibliotecaBabel = UnaBiblioteca "bibliotecaDeBabel" [principito,harryPotter,libro1] bibliotecaDeBabel
bibliotecaBerlin = UnaBiblioteca "bibliotecaDeBerlin" [sherlockHolmes,harryPotter] bibliotecaDeBerlin
bibliotecaAlejandria = UnaBiblioteca "bibliotecaDeAlejandria" [gameOfThrones,donQuijote] bibliotecaDeAlejandria
bibliotecaParis = UnaBiblioteca "bibliotecaDeParis" [laTragedia,principito] bibliotecaDeParis
bibliotecaArgentina = UnaBiblioteca "bibliotecaDeArgentina" [laTragedia,sherlockHolmes,gameOfThrones] bibliotecaDeArgentina
bibliotecaBasica = UnaBiblioteca "bibliotecaInfinita" [] bibliotecaDeBabel

listaDeBibliotecas = [bibliotecaBabel,bibliotecaBerlin,bibliotecaAlejandria,bibliotecaParis,bibliotecaArgentina]

--Problemas de registración--

generosMinuscula :: [String] -> [String]
generosMinuscula [] = []
generosMinuscula (x:xs) = (map toLower x) : generosMinuscula xs
--esta funcion se utiliza en las funciones del punto 3)

--Bibliotecas selectivas--

bibliotecaDeBerlin :: Libro -> Bool
bibliotecaDeBerlin libro = any (=="policial") (generosMinuscula (generos libro))

bibliotecaDeAlejandria :: Libro -> Bool
bibliotecaDeAlejandria libro = (cantPaginas libro) == (length (texto libro))

bibliotecaDeParis :: Libro -> Bool
bibliotecaDeParis libro = any (=="romantica") (generosMinuscula (generos libro))

bibliotecaDeArgentina :: Libro -> Bool
bibliotecaDeArgentina libro = length (generos libro) > 1 --cada libro tiene que tener mas de un genero

podriaEstar :: Libro -> [Biblioteca] -> [String]
podriaEstar libro [] = []
podriaEstar libro (x:xs)
    | (logica x) libro = (nombre x) : (podriaEstar libro xs)
    | otherwise = podriaEstar libro xs

--Biblioteca… ¿infinita?--

bibliotecaInfinita :: Biblioteca -> Libro -> (Libro -> [Libro]) -> Biblioteca
bibliotecaInfinita biblioteca libro f = biblioteca {libros = f libro}

librosInfinitosLiteral :: Libro -> [Libro]
librosInfinitosLiteral libro = libro {texto = foldl (++) [] (take 3200 (repeat (texto libro)))} : librosInfinitosLiteral libro

librosInfinitosSimplificada :: Libro -> [Libro]
librosInfinitosSimplificada libro = libro {texto = foldl (++) [] (take 410 (repeat (texto libro)))} : librosInfinitosSimplificada libro

librosInfinitosPersonalizada :: Libro -> [Libro]
librosInfinitosPersonalizada libro = libro {texto = foldl (++) [] (take 2 (repeat (texto libro)))} : librosInfinitosPersonalizada libro

estaFragmentoTexto :: Biblioteca -> String -> Libro
estaFragmentoTexto biblioteca fragmento = fragmentoTexto (libros biblioteca) fragmento

fragmentoTexto :: [Libro] -> String -> Libro
fragmentoTexto [] fragmento = UnLibro "" 0 "" [""]
fragmentoTexto (x:xs) fragmento
    | fragmento == take (length fragmento) (texto x) = x
    | otherwise = fragmentoTexto xs fragmento

cuantosTienenFragmento :: Biblioteca -> String -> Number
cuantosTienenFragmento biblioteca fragmento = length (filter (==True) (map (tieneFragmento fragmento) (libros biblioteca)))

tieneFragmento :: String -> Libro -> Bool
tieneFragmento fragmento libro = fragmento == take (length fragmento) (texto libro)

{-
La evaluacion diferida es la manera que el lenguaje procesa los datos, va procesando los datos a medida que llegan.
Esto sirve para poder trabajar con listas infinitas, por ejemplo al hacer:
take 2 (repeat 'a') = aa
Si la evaluacion no seria diferida el programa primero deberia de procesar la lista infinita y luego realizar la funcion take.
Esta evaluacion sirvio principalmente a la ultima parte del TP
-}

ordenarGeneros :: [String] -> Biblioteca -> [String]
ordenarGeneros [] biblioteca = []
ordenarGeneros (x:xs) biblioteca = ordenarGeneros (menores) biblioteca ++ x : ordenarGeneros(mayores) biblioteca
    where
    menores = [y|y <- xs, (cantLibrosGenero y biblioteca) < (cantLibrosGenero x biblioteca)]
    mayores = [z|z <- xs, (cantLibrosGenero z biblioteca) >= (cantLibrosGenero x biblioteca)]
