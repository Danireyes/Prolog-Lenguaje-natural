pasiva --> sujeto(Numero,Persona,Sujeto),predicado(Verbo,Persona,Numero,Generocd,Numerocd,Cd,Cc),pasiva2(Sujeto,Verbo,Numerocd,Generocd,Cd,Cc).

%Gramática DCG
sujeto(singular,3,Sujeto)--> nombre_propio(_,Sujeto).
sujeto(Numero,3,Sujeto) --> gn(Numero,_,3,Sujeto).
sujeto(Numero,Persona,Sujeto)--> pronombre(Persona,_,Numero,Sujeto).

pronombre(Numero,Genero,Persona,Pasiva)-->[P],
	{es_pronombre(P,Numero,Genero,Persona,Pasiva)}.

nombre_propio(Genero,P)-->[P],
	{es_nombre_propio(P,Genero)}.



gn(Numero,Genero,3,Sujeto)-->
	articulo(Genero,Numero,Articulo),nombre(Genero,Numero,Nombre),{obtenerCadena(Articulo,Nombre,Sujeto)}.

nombre(Genero,Numero,P)-->[P],
	{es_nombre(P,Genero,Numero)}.

articulo(Genero,Numero,P)-->[P],
	{es_articulo(P,Genero,Numero)}.



predicado(Verbo,Persona,Numero,Generocd,Numerocd,Cd,Complemento)-->gv(Verbo,Persona,Numero,Prep),cd(Numerocd,Generocd,Cd),cc(Prep,Complemento).
predicado(Verbo,Persona,Numero,Generocd,Numerocd,Cd,_)--> gv(Verbo,Persona,Numero,_),cd(Numerocd,Generocd,Cd).

gv(Verbo,Persona,Numero,Preposicion) --> verbo(Verbo,Persona,Numero,Preposicion).

verbo(Raiz,Persona,Numero,Prep)--> [V],
    {
     atom_concat(Raiz,Terminacion,V),
     es_verbo(Raiz,_,Prep),
     es_terminacion(Terminacion,Persona,Numero)
    }.

%da igual la persona porque no va a haber otro verbo.
cd(Numero,Generocd,Cd) --> gn(Numero,Generocd,_,Cd).

%En el complemento circunstancial pueden haber fallos semánticos.
cc([Preposicion|_],Complemento)-->[Preposicion],sujeto(_,_,Cd),
   {
     obtenerCadena(Preposicion,Cd,Complemento)
   }.



pasiva2(Sujeto,Verbo,Numerocd,Generocd,Cd,Cc)-->{write(Cd),printVerbo(Verbo,Numerocd,Generocd), write(Sujeto),printCC(Cc)}.


%Diccionario
es_nombre(niño,masculino,singular).
es_nombre(niños,masculino,plural).
es_nombre(niña,femenino,singular).
es_nombre(niñas,femenino,plural).
es_nombre(flor,femenino,singular).
es_nombre(libreta,femenino,singular).
es_nombre(lápiz,masculino,singular).
es_nombre(guagua,femenino,singular).

es_nombre_propio(mario,masculino).
es_nombre_propio(maria,femenino).
es_nombre_propio(españa,femenino).

es_verbo(dibuj,ar,[con]).
es_verbo(par,ar,[en]).
es_verbo(pis,ar,[]).
es_verbo(tir,ar,[]).

es_terminacion(é,1,singular).
es_terminacion(aste,2,singular).
es_terminacion(ó,3,singular).
es_terminacion(amos,1,plural).
es_terminacion(aron,2,plural).%está con ustedes
es_terminacion(aron,3,plural).

es_pronombre(yo,1,_,singular,mí).
es_pronombre(tú,2,_,singular,tí).
es_pronombre(él,3,masculino,singular,él).
es_pronombre(ella,3,femenino,singular,ella).
es_pronombre(nosotros,1,masculino,plural,nosotros).
es_pronombre(nosotras,1,femenino,plural,nosotras).
es_pronombre(ustedes,2,_,plural,ustedes).
es_pronombre(ellos,3,masculino,plural,ellos).
es_pronombre(ellas,3,femenino,plural,ellas).

es_articulo(el,masculino,singular).
es_articulo(la,femenino,singular).
es_articulo(los,masculino,plural).
es_articulo(las,femenino,plural).
es_articulo(un,masculino,singular).
es_articulo(una,femenino,singular).
es_articulo(unos,masculino,plural).
es_articulo(unas,femenino,plural).







obtenerCadena(A,B,Cadena) :- concat(A," ",D), concat(D,B,Cadena).

printVerbo(Verbo,singular,masculino):-write(" fue "),write(Verbo), write("ado "),write("por ").
printVerbo(Verbo,plural,masculino):-write(" fueron "),write(Verbo), write("ados "),write("por ").
printVerbo(Verbo,singular,femenino):-write(" fue "),write(Verbo), write("ada "),write("por ").
printVerbo(Verbo,plural,femenino):-write(" fueron "),write(Verbo), write("adas "),write("por ").

%Tenemos que asegurarnos de que A no es una variable libre.
%printCC(Adverbio):- not(nonvar(Advrbio)).
printCC(Adverbio):- nonvar(Adverbio),write(" "),write(Adverbio).
printCC(_).
