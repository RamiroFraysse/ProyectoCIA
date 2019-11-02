:-dynamic frontera/1, visitados/1.
:- consult('Acciones.pl').
:- consult('Mapa.pl').


%test_de_meta
%es_meta(+ubicacionCargaExplosiva,+UbicacionDetonador).
es_meta([F,C],[X,Y]):-ubicacionCarga([F,C]),sitioDetonacion([X,Y]).



%buscar_plan(+Einicial,-Plan,-Destino,-Costo).
buscar_plan(EstadoInicial,Plan,Destino,Costo):- retractall(frontera(_)),
                                                retractall(visitados(_)),
                                                EstadoInicial=[[X,Y],_Dir,_ListaPoseciones,_FlagCCP],
                                                estaEn([c,c1],[Cx,Cy]),
                                                (dif_abs(X,Cx,Rx),dif_abs(Y,Cy,Ry),Fx is Rx+Ry),
                                                /*nodo(Estado,Camino,CostoCamino,Fx)*/
                                                Nodo=nodo(EstadoInicial,[],0,Fx),
                                                assertz(frontera(Nodo)),
                                                buscarA*(Solucion),
                                                reverse(Solucion, SolucionAux),
                                                writeln(""),write("El camino es "),writeln(SolucionAux).

/*Heuristica se encarga de calcular el valor de la heuristica
de un estado a la meta.*/

%Caso_Base
buscarA*(Camino):-
                    seleccionar(Nodo),
                    Nodo = nodo(Estado,Camino,_CostoCamino,_Fx),
                    Estado = [[F,C],_Dir,_ListaPoseciones,_FlagCCP],
                    estaEn([c,c1],[F,C]),!,
                    write("Selecciona "),write(Nodo),write(" De la frontera ").
%Caso_Recursivo.
buscarA*(Solucion):-                                   
                    seleccionar(Nodo),
                    write("Selecciona "),writeln(Nodo),
                    assertz(visitados(Nodo)),
                    retract(frontera(Nodo)),
                    generarVecinos(Nodo, Vecinos),
                    write("Vecinos "),
                    agregarVecinos(Vecinos),
                    writeln(""),
                    buscarA*(Solucion).

seleccionar(Nodo):-
                frontera(Nodo),
                Nodo = nodo(_, _, _, MenorCosto),
                %Cond_Action_Tiene_Exito_si_para_todas_las_sol_alternativas_del_antecedente_se_verifica_el_consecuente.
                forall(frontera(nodo(_, _, _, Costo)), Costo >= MenorCosto), !.

generarVecinos(Nodo,Vecinos):-Nodo=nodo(Estado,Camino,CostoCamino,F),
                                    estaEn([c,c1],[Cf,Cc]),
                                    findall(nodo(EstadoV,[Accion|Camino],CostoCaminoV,Fv),
                                    (estado_sucesor(Estado,EstadoV,Accion,C),heuristica(EstadoV,[Cf,Cc],H),CostoCaminoV is CostoCamino+C,Fv is H + CostoCaminoV),
                                    Vecinos).


agregarVecinos([]).
agregarVecinos([NodoVecino|T]):-superaControlDeVisitados(NodoVecino),
                                superaControlDeFrontera(NodoVecino),
                                write(NodoVecino),write(" "),
                                assertz(frontera(NodoVecino)),!,
                                agregarVecinos(T).
agregarVecinos([NodoVecino|T]):-agregarVecinos(T).
                                
superaControlDeVisitados(nodo(Estado, _Camino, _CostoG, _CostoF)) :- not(visitados(nodo(Estado,_,_,_))),!.
superaControlDeVisitados(nodo(Estado, _Camino, _CostoG, CostoF)) :-
    visitados(nodo(Estado, _, _, CostoFV)), CostoFV > CostoF,
    retract(visitados(nodo(Estado, _, _, CostoFV))). 

superaControlDeFrontera(nodo(Estado, _Camino, _CostoG, _CostoF)) :- not(frontera(nodo(Estado,_,_,_))),!.
superaControlDeFrontera(nodo(Estado, _Camino, _CostoG, CostoF)) :-
    %En_la_frontera_hay_uno_con_el_mismo_estado_con_mayor_costo.
    frontera(nodo(Estado, _, _, CostoFV)), CostoFV > CostoF,
    retract(frontera(nodo(Estado, _, _, CostoFV))). 

heuristica(EstadoV,[Cf,Cc],H):-EstadoV=[[F,C],_Dir,_ListaPoseciones,_FlagCCP],(dif_abs(F,Cf,Rx),dif_abs(C,Cc,Ry),H is Rx+Ry).

                      
dif_abs(X,Y,Z):-X>=Y,!,Z is	X-Y.	
dif_abs(X,Y,Z):-Z is Y-X.