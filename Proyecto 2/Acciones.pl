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
/*estado_sucesor(Estado,EstadoV,saltar_valla(Valla),Costo):-Estado=[[F,C],Dir,ListaPoseciones,FlagCCP],
                                                EstadoV=[[F,C],Dir,ListaPoseciones,FlagCCP].

   */                                      



