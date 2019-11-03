:-consult('Mapa.pl').
f5:-consult('Acciones.pl').
%rotar_determina_el_costo_de_girar_desde_una_direccion_a_otra
rotar(norte,oeste,1).
rotar(norte,este,1).
rotar(norte,sur,2).
rotar(oeste,norte,1).
rotar(oeste,sur,1).
rotar(oeste,este,2).
rotar(sur,oeste,1).
rotar(sur,este,1).
rotar(sur,norte,2).
rotar(este,norte,1).
rotar(este,sur,1).
rotar(este,oeste,2).

/*El costo esta determinado por el tipo de suelo de la pos destino*/
suelo(firme,2).
suelo(resbaladizo,3).

%estado_sucesor(+Estado,-EstadoV,-Accion,-Costo).
%caminar_______________________________________________________________________________
%norte
estado_sucesor(Estado,EstadoV,caminar,Costo):-Estado=[[F,C],norte,ListaPoseciones,FlagCCP],
                                          I is F-1,
                                          celda([I,C],S),
                                          suelo(S,Costo),
                                          not(estaEn([p,_,_],[I,C])),
                                          not(estaEn([v,_,_],[I,C])),
                                          %writeln("caminar norte "),
                                          EstadoV=[[I,C],norte,ListaPoseciones,FlagCCP].

%sur
estado_sucesor(Estado,EstadoV,caminar,Costo):-Estado=[[F,C],sur,ListaPoseciones,FlagCCP],
                                          I is F+1,
                                          celda([I,C],S),
                                          suelo(S,Costo),
                                          not(estaEn([p,_,_],[I,C])),
                                          not(estaEn([v,_,_],[I,C])),
                                          %writeln("caminar sur "),
                                          EstadoV=[[I,C],sur,ListaPoseciones,FlagCCP].
%este
estado_sucesor(Estado,EstadoV,caminar,Costo):-Estado=[[F,C],este,ListaPoseciones,FlagCCP],
                                          J is C+1,
                                          
                                          celda([F,J],S),
                                          suelo(S,Costo),
                                          not(estaEn([p,_,_],[F,J])),

                                          not(estaEn([v,_,_],[F,J])),
                                                                                                                              

                                          %writeln("caminar este "),
                                          EstadoV=[[F,J],este,ListaPoseciones,FlagCCP].
%oeste
estado_sucesor(Estado,EstadoV,caminar,Costo):-Estado=[[F,C],oeste,ListaPoseciones,FlagCCP],
                                          J is C-1,
                                          celda([F,J],S),
                                          suelo(S,Costo),
                                          not(estaEn([p,_,_],[F,J])),
                                          not(estaEn([v,_,_],[F,J])),
                                          %writeln("caminar oeste"),
                                          EstadoV=[[F,J],oeste,ListaPoseciones,FlagCCP].

%rotar__________________________________________________________________________________________
%norte
estado_sucesor(Estado,EstadoV,rotar(norte),Costo):-Estado=[[F,C],Dir,ListaPoseciones,FlagCCP],
                                                EstadoV=[[F,C],norte,ListaPoseciones,FlagCCP],
                                                %writeln("rotar norte"),
                                                rotar(norte,Dir,Costo).
%sur
estado_sucesor(Estado,EstadoV,rotar(sur),Costo):-Estado=[[F,C],Dir,ListaPoseciones,FlagCCP],
                                                EstadoV=[[F,C],sur,ListaPoseciones,FlagCCP],
                                                %writeln("rotar sur"),
                                                rotar(sur,Dir,Costo).                                              
%este
estado_sucesor(Estado,EstadoV,rotar(este),Costo):-Estado=[[F,C],Dir,ListaPoseciones,FlagCCP],
                                                EstadoV=[[F,C],este,ListaPoseciones,FlagCCP],
                                                %writeln("rotar este"),
                                                rotar(este,Dir,Costo). 
%oeste
estado_sucesor(Estado,EstadoV,rotar(oeste),Costo):-Estado=[[F,C],Dir,ListaPoseciones,FlagCCP],
                                                EstadoV=[[F,C],oeste,ListaPoseciones,FlagCCP],
                                               % writeln("rotar oeste"),
                                                rotar(oeste,Dir,Costo).
%saltar_________________________________________________________________________________________
%norte
estado_sucesor(Estado,EstadoV,saltar_valla(Valla),Costo):-Estado=[[F,C],norte,ListaPoseciones,FlagCCP],
                                                Valla=[v,NombreV,AlturaV],
                                                %Informacion_para_la_valla_que_tiene_que_saltar.
                                                X is F-1,
                                                estaEn([v,NombreV,AlturaV],[X,C]),
                                                AlturaV<4,
                                                I is X-1,
                                                celda([I,C],S),
                                                suelo(S,Cos),
                                                Costo is Cos+1,
                                                not(estaEn([p,_,_],[I,C])),
                                                not(estaEn([v,_,_],[I,C])),
                                                not(estaEn([r,_],[I,C])),
                                                EstadoV=[[I,C],norte,ListaPoseciones,FlagCCP].
%sur
estado_sucesor(Estado,EstadoV,saltar_valla(Valla),Costo):-Estado=[[F,C],sur,ListaPoseciones,FlagCCP],
                                                Valla=[v,NombreV,AlturaV],
                                                %Informacion_para_la_valla_que_tiene_que_saltar.
                                                X is F+1,
                                                estaEn([v,NombreV,AlturaV],[X,C]),
                                                AlturaV<4,
                                                I is X+1,
                                                celda([I,C],S),
                                                suelo(S,Cos),
                                                Costo is Cos+1,
                                                not(estaEn([p,_,_],[I,C])),
                                                not(estaEn([v,_,_],[I,C])),
                                                not(estaEn([r,_],[I,C])),
                                                EstadoV=[[I,C],sur,ListaPoseciones,FlagCCP].
%este
estado_sucesor(Estado,EstadoV,saltar_valla(Valla),Costo):-Estado=[[F,C],este,ListaPoseciones,FlagCCP],
                                                Valla=[v,NombreV,AlturaV],
                                                %Informacion_para_la_valla_que_tiene_que_saltar.
                                                Y is C+1,
                                                estaEn([v,NombreV,AlturaV],[F,Y]),
                                                AlturaV<4,
                                                J is Y+1,
                                                celda([F,J],S),
                                                suelo(S,Cos),
                                                Costo is Cos+1,
                                                not(estaEn([p,_,_],[F,J])),
                                                not(estaEn([v,_,_],[F,J])),
                                                not(estaEn([r,_],[F,J])),
                                                EstadoV=[[F,J],este,ListaPoseciones,FlagCCP].
%oeste
estado_sucesor(Estado,EstadoV,saltar_valla(Valla),Costo):-Estado=[[F,C],oeste,ListaPoseciones,FlagCCP],
                                                Valla=[v,NombreV,AlturaV],
                                                %Informacion_para_la_valla_que_tiene_que_saltar.
                                                Y is C-1,
                                                estaEn([v,NombreV,AlturaV],[F,Y]),
                                                AlturaV<4,
                                                J is Y-1,
                                                celda([F,J],S),
                                                suelo(S,Cos),
                                                Costo is Cos+1,
                                                not(estaEn([p,_,_],[F,J])),
                                                not(estaEn([v,_,_],[F,J])),
                                                not(estaEn([r,_],[F,J])),
                                                EstadoV=[[F,J],oeste,ListaPoseciones,FlagCCP].

%juntar_llave__________________________________________________________________________________________
estado_sucesor(Estado,EstadoV,juntar_llave(Llave),1):-Estado=[[F,C],Dir,ListaPoseciones,FlagCCP],
                                            estaEn([l,NombreL],[F,C]),
                                            Llave=[l,NombreL],
                                            not(member(Llave,ListaPoseciones)),
                                            EstadoV=[[F,C],Dir,[Llave|ListaPoseciones],FlagCCP].
%juntar_carga____________________________________________________________________________________________
estado_sucesor(Estado,EstadoV,juntar_carga(Carga),3):-Estado=[[F,C],Dir,ListaPoseciones,FlagCCP],
                                            estaEn([c,NombreC],[F,C]),
                                            Carga=[c,NombreC],
                                            not(member(Carga,ListaPoseciones)),
                                            EstadoV=[[F,C],Dir,[Carga|ListaPoseciones],si].
%juntar_detonador________________________________________________________________________________________
estado_sucesor(Estado,EstadoV,juntar_detonador(Detonador),2):-Estado=[[F,C],Dir,ListaPoseciones,FlagCCP],
                                            estaEn([d,NombreD,ActivadoD],[F,C]),
                                            Detonador=[d,NombreD,ActivadoD],
                                            not(member(Detonador,ListaPoseciones)),
                                            EstadoV=[[F,C],Dir,[Detonador|ListaPoseciones],FlagCCP].

%dejar_carga_____________________________________________________________________________________________
estado_sucesor(Estado,EstadoV,dejar_carga(Carga),1):-Estado=[[F,C],Dir,ListaPoseciones,FlagCCP],
                                            ubicacionCarga([F,C]),
                                            buscarC(ListaPoseciones,Carga),
                                            Carga=[c,NombreC],
                                            elim(Carga,ListaPoseciones,Lista),
                                            EstadoV=[[F,C],Dir,Lista,no].
%detonar__________________________________________________________________________________________________
estado_sucesor(Estado,EstadoV,detonar(Detonador),1):-Estado=[[F,C],Dir,ListaPoseciones,no],
                                            sitioDetonacion([F,C]),
                                            buscarD(ListaPoseciones,Det),
                                            Det=[d,NombreD,ActivadoD],
                                            elim(Det,ListaPoseciones,Lista),
                                            Detonador=[d,NombreD,si],
                                            EstadoV=[[F,C],Dir,[Detonador|ListaPoseciones],no].

buscarC([],[]).
buscarC([H|T],H):-H=[c,_],!.
buscarC([H|T],R):-buscarC(T,R).

buscarD([],[]).
buscarD([H|T],H):-H=[d,_,_],!.
buscarD([H|T],R):-buscarD(T,R).

elim(E,[],[]).
elim(E,[E|T],R):-!,elim(E,T,R).
elim(E,[H|T],[H|R]):-elim(E,T,R).