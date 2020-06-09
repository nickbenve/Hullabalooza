module Lib where
import Text.Show.Functions

data Festival=UnFestival{
    lugar::String,
    cantidadDePublico::Float,
    estadoDeAnimo::String,
    bandas::[Banda]
}deriving(Show)

hullabalooza= UnFestival "argentina" 20000 "indiferente" [metallica]

data Banda=UnaBanda{
    decibeles::Int,
    descripciones::[String],
    genero::[Genero]
}deriving(Show)

type Genero=Festival->Festival


rockNacional::Genero
rockNacional =modificaPublico 100


modificaPublico::Float->Festival->Festival
modificaPublico incremento festival=festival{cantidadDePublico=cantidadDePublico festival + incremento}

modificaEstado::String->Festival->Festival
modificaEstado nuevoEstado festival=festival{estadoDeAnimo=nuevoEstado}
pop::Festival->Festival
pop festival
    |estadoDeAnimo festival =="indiferente"=modificaEstado "euforico" ( modificaPublico (cantidadDePublico festival) festival )
    |otherwise=festival

heavyMetal::Genero
heavyMetal   festival =modificaEstado "pesado" ( modificaPublico ((cantidadDePublico festival) * 0.1) festival )

trashMetal::Genero
trashMetal festival  =modificaEstado "basura" ( modificaPublico (cantidadDePublico festival*0.1) festival )

laVerdad = True

losRedondos= UnaBanda 45 ["legendaria","pegosa"] [rockNacional] 
soda=UnaBanda 40 ["irrependible"] [rockNacional] 
miranda=UnaBanda 60 ["insipida","incolora","inodora"] [pop] 
metallica=UnaBanda 60 ["legendaria","vendida"] [heavyMetal] 
ejemplo=UnaBanda 60 ["horrible","fea"] [trashMetal] 



aplicarGenero::Festival->Genero->Festival
aplicarGenero festival genero=genero festival

tocar::Festival->Banda->Festival
tocar   festival banda = foldl aplicarGenero festival (genero banda) 

--3
theStrokes=UnaBanda 45 ["suicidio asistido","emocional","linda"] [pop,heavyMetal]


suceder::Festival->Festival
suceder festival= foldl tocar festival (bandas festival) 



type Critica=Banda->Bool

critica1::[Critica]
critica1=[vendida,legendaria,acustica]


clasificarBanda::Banda->[Critica]->[Critica]
clasificarBanda banda criterios=filter (aplicarClasificacion banda) criterios


aplicarClasificacion::Banda->(Banda->Bool)->Bool
aplicarClasificacion banda critica =critica banda



vendida::Critica -----------------
vendida banda=buenaCantidadDescripciones banda ||tieneDescripcion "vendida" banda


buenaCantidadDescripciones::Banda->Bool
buenaCantidadDescripciones=(>=3).length.descripciones 

tieneDescripcion::String->Banda->Bool
tieneDescripcion criticaClave = any (==criticaClave) . descripciones


legendaria:: Critica-------------
legendaria banda = tieneDescripcion "legendaria" banda && superaDecibeles (>40) banda


acustica:: Critica ---------------------------------
acustica =superaDecibeles (>55) 


superaDecibeles::(Int->Bool)->Banda->Bool
superaDecibeles condicion =condicion.decibeles


popular::[Critica]->Banda->Int
popular criticas banda = 100*length (clasificarBanda  banda criticas)
    


--popularidad::Banda->[String]->Int
--popularidad banda clasificaciones=100 * length . filter 


buenFest::Festival->[Critica]->Bool
buenFest festival criticas=sumaPopularidad (bandas festival) >1000 && cronologicamenteCorrecto (bandas festival) criticas

sumaPopularidad::[Banda]->Int
sumaPopularidad =sum . map (popular critica1) 


cronologicamenteCorrecto::[Banda]->[Critica]->Bool
cronologicamenteCorrecto [] _ =True
cronologicamenteCorrecto (x:y:xs) criticas=popular criticas x < popular criticas y && cronologicamenteCorrecto (y:xs) criticas

