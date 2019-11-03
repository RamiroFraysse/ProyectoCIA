:-dynamic frontera/1, visitados/1.
:- consult('Acciones.pl').
:- consult('Mapa.pl').
:-set_prolog_flag(answer_write_options,[quoted(true),portray(true),spacing(next_argument)]).





%test_de_meta
%es_meta(+ubicacionCargaExplosiva,+UbicacionDetonador).
es_meta(Nodo):-Nodo=nodo(Estado,_Camino,_CostoCamino,_Fx),
            Estado=[[F,C],_Dir,ListaPoseciones,FlagCCP],
            %Posicion_coincide_con_el_sitio_de_detonacion.
            sitioDetonacion([F,C]),
            %La_Carga_Dejada_en_su_ubicacion
            FlagCCP=no,
            %Detonador_esta_activo
            member([d,_Nombre,si],ListaPoseciones).

            



%buscar_plan(+Einicial,-Plan,-Destino,-Costo).
buscar_plan(EstadoInicial,Plan,Destino,Costo):- statistics(runtime,[TiempoInicio|_]),
                                                retractall(frontera(_)),
                                                retractall(visitados(_)),
                                                EstadoInicial=[[X,Y],_Dir,_ListaPoseciones,_FlagCCP],
                                                estaEn([c,c1],[Cx,Cy]),
                                                (dif_abs(X,Cx,Rx),dif_abs(Y,Cy,Ry),Fx is Rx+Ry),
                                                /*nodo(Estado,Camino,CostoCamino,Fx)*/
                                                Nodo=nodo(EstadoInicial,[],0,Fx),
                                                assertz(frontera(Nodo)),
                                                buscarA*(Solucion),
                                                Solucion=nodo(Estado,Camino,Costo,_),
                                                reverse(Camino, Plan),
                                                Estado=[Destino,_,_,_],
                                                statistics(runtime,[TiempoFinal|_]),
TiempoTotal is ((TiempoFinal-TiempoInicio)/1000)/60,
write("Tiempo total transcurrido: "),write(TiempoTotal),write(" minutos.").

/*Heuristica se encarga de calcular el valor de la heuristica
de un estado a la meta.*/

%Caso_Base
buscarA*(Nodo):-
                    seleccionar(Nodo),
                    Nodo = nodo(Estado,_Camino,_CostoCamino,_Fx),
                    Estado = [[_F,_C],_Dir,_ListaPoseciones,_FlagCCP],
                    es_meta(Nodo),!.
%Caso_Recursivo.
buscarA*(Solucion):-                                   
                    seleccionar(Nodo),
                    assertz(visitados(Nodo)),
                    retract(frontera(Nodo)),
                    generarVecinos(Nodo, Vecinos),
                    agregarVecinos(Vecinos),
                    buscarA*(Solucion).

seleccionar(Nodo):-
                frontera(Nodo),
                Nodo = nodo(_, _, _, MenorCosto),
                %Cond_Action_Tiene_Exito_si_para_todas_las_sol_alternativas_del_antecedente_se_verifica_el_consecuente.
                forall(frontera(nodo(_, _, _, Costo)), Costo >= MenorCosto), !.

generarVecinos(Nodo,Vecinos):-Nodo=nodo(Estado,Camino,CostoCamino,_F),
                                   
                                    findall(nodo(EstadoV,[Accion|Camino],CostoCaminoV,Fv),
                                    (estado_sucesor(Estado,EstadoV,Accion,C),heuristica(EstadoV,H),CostoCaminoV is CostoCamino+C,Fv is H + CostoCaminoV),
                                    Vecinos).


agregarVecinos([]).
agregarVecinos([NodoVecino|T]):-superaControlDeVisitados(NodoVecino),
                                superaControlDeFrontera(NodoVecino),
                                assertz(frontera(NodoVecino)),!,
                                agregarVecinos(T).
agregarVecinos([_NodoVecino|T]):-agregarVecinos(T).
                                
superaControlDeVisitados(nodo(Estado, _Camino, _CostoG, _CostoF)) :- not(visitados(nodo(Estado,_,_,_))),!.
superaControlDeVisitados(nodo(Estado, _Camino, _CostoG, CostoF)) :-
    visitados(nodo(Estado, _, _, CostoFV)), CostoFV > CostoF,
    retract(visitados(nodo(Estado, _, _, CostoFV))). 

superaControlDeFrontera(nodo(Estado, _Camino, _CostoG, _CostoF)) :- not(frontera(nodo(Estado,_,_,_))),!.
superaControlDeFrontera(nodo(Estado, _Camino, _CostoG, CostoF)) :-
    %En_la_frontera_hay_uno_con_el_mismo_estado_con_mayor_costo.
    frontera(nodo(Estado, _, _, CostoFV)), CostoFV > CostoF,
    retract(frontera(nodo(Estado, _, _, CostoFV))). 

%caso1_No_tengo_nada_noCarga_noDeto_flagSi.
heuristica(EstadoV,H):-EstadoV=[Pos,_Dir,ListaPoseciones,FlagCCP],
                    not(member([c,_Nombre],ListaPoseciones)),
                    not(member([d,_,_],ListaPoseciones)),
                    FlagCCP=si,!,
                    sec_recorrido1(Pos,HRecorrido1),
                    sec_recorrido2(Pos,HRecorrido2),
                    sec_recorrido3(Pos,HRecorrido3),
                    menor(HRecorrido1,HRecorrido2,HRecorrido3,R),H is R.

%caso2_No_tengo_nada_noCarga_noDeto_flagNo
heuristica(EstadoV,H):-EstadoV=[Pos,_Dir,ListaPoseciones,FlagCCP],
                    not(member([c,_nombre],ListaPoseciones)),
                    not(member([d,_nombre,_],ListaPoseciones)),
                    FlagCCP=no,!,
                    %pos_detonador
                    estaEn([d,d1,_],PosD),
                    distancia(Pos,PosD,H1),
                    %pos_detonador_sitioDetonacionMasCorto
                    sitioDetonacion(Po),
                    distancia(PosD,Po,MasCorto),
                    %forall(frontera(nodo(_, _, _, Costo)), Costo >= MenorCosto),
                    forall(sitioDetonacion(P),(distancia(PosD,P,Costo), Costo>= MasCorto)),H is H1+MasCorto.

%caso3_TengoCarga_NotengoDetonador_flagSi.
heuristica(EstadoV,H):-EstadoV=[Pos,_Dir,ListaPoseciones,FlagCCP],
                    (member([c,_N],ListaPoseciones)),
                    (not(member([d,_,_],ListaPoseciones))),
                    FlagCCP=si,!,
                    sec_recorrido1_caso3(Pos,HRecorrido1),
                    sec_recorrido2_caso3(Pos,HRecorrido2),
                    menor(HRecorrido1,HRecorrido2,H).

%caso4_NoTengoCarga_tengoDetonador_flagSi.
heuristica(EstadoV,H):-EstadoV=[Pos,_Dir,ListaPoseciones,FlagCCP],
                    not(member([c,_],ListaPoseciones)),
                    (member([d,_,_],ListaPoseciones)),
                    FlagCCP=si,!,
                    %pos_carga
                    estaEn([c,c1],PosC),
                    distancia(Pos,PosC,H1),
                    %pos_carga_sitioDejarCarga
                    ubicacionCarga(PosUbCarga),
                    distancia(PosC,PosUbCarga,H2),
                    %pos_carga_sitioDejarCarga_sitioDetonacionMasCorto
                    sitioDetonacion(Po),
                    distancia(PosUbCarga,Po,MasCorto),
                    forall(sitioDetonacion(P),(distancia(PosUbCarga,P,Costo), Costo>= MasCorto)),H is H1+H2+MasCorto.

%caso5_NoTengoCarga_tengoDetonador_flagNo.
heuristica(EstadoV,H):-EstadoV=[Pos,_Dir,ListaPoseciones,FlagCCP],
                    not(member([c,_],ListaPoseciones)),
                    (member([d,_,_],ListaPoseciones)),
                    FlagCCP=no,!,
                    %pos_detonador_sitioDetonacionMasCorto
                    sitioDetonacion(Po),
                    distancia(Pos,Po,MasCorto),
                    forall(sitioDetonacion(P),(distancia(Pos,P,Costo), Costo>= MasCorto)),H is MasCorto.

%caso6_TengoCarga_TengoDet_FlagSi.
heuristica(EstadoV,H):-EstadoV=[Pos,_Dir,ListaPoseciones,FlagCCP],
                    (member([c,_],ListaPoseciones)),
                    (member([d,_Nombre,_],ListaPoseciones)),
                    FlagCCP=si,!,
                    %pos_carga_sitioDejarCarga
                    ubicacionCarga(PosUbCarga),
                    distancia(Pos,PosUbCarga,H1),
                    %pos_detonador_sitioDetonacionMasCorto
                    sitioDetonacion(Po),
                    distancia(PosUbCarga,Po,MasCorto),
                    forall(sitioDetonacion(P),(distancia(PosUbCarga,P,Costo), Costo>= MasCorto)),H is H1 + MasCorto.
                    
                   
%pos_carga_detonador_sitioDejarCarga_SitioDetonacionMasCorto
sec_recorrido1(Pos,HRecorrido1):-%pos_carga
                    estaEn([c,c1],PosC),
                    distancia(Pos,PosC,H1), 
                    %pos_carga_detonador
                    estaEn([d,d1,_],PosD),
                    distancia(PosC,PosD,H2),
                    %pos_carga_detonador_sitioDejarCarga
                    ubicacionCarga(PosUbCarga),
                    distancia(PosD,PosUbCarga,H3),
                    %pos_carga_detonador_sitioDejarCarga_SitioDetonacionMasCorto
                    sitioDetonacion(Po),
                    distancia(PosUbCarga,Po,MasCorto),
                    %forall(frontera(nodo(_, _, _, Costo)), Costo >= MenorCosto),
                    forall(sitioDetonacion(P),(distancia(PosUbCarga,P,Costo), Costo>= MasCorto)),HRecorrido1 is H1+H2+H3+MasCorto.

%pos_detonador_carga_sitioDejarCarga_SitioDetonacionMasCorto
sec_recorrido2(Pos,HRecorrido2):-%pos_detonador

                    estaEn([d,d1,_],PosD),
                    distancia(Pos,PosD,H1),
                    %pos_detonador_carga
                    estaEn([c,c1],PosC),
                    distancia(PosD,PosC,H2),
                    %pos_detonador_carga_sitioDejarCarga
                    ubicacionCarga(PosUbCarga),
                    distancia(PosC,PosUbCarga,H3),
                    %pos_detonador_carga_sitioDejarCarga_SitioDetonacionMasCorto
                    sitioDetonacion(Po),
                    distancia(PosUbCarga,Po,MasCorto),
                    forall(sitioDetonacion(P),(distancia(PosUbCarga,P,Costo), Costo>= MasCorto)),HRecorrido2 is H1+H2+H3+MasCorto.

%pos_carga_sitioDejarCarga_detonador_SitioDetonacionMasCorto
sec_recorrido3(Pos,HRecorrido3):-%pos_carga


                    estaEn([c,c1],PosC),
                    distancia(Pos,PosC,H1),
                    %pos_carga_sitioDejarCarga
                    ubicacionCarga(PosUbCarga),
                    distancia(PosC,PosUbCarga,H2),
                    %pos_carga_sitioDejarCarga_detonador_
                    estaEn([d,d1,_],PosD),
                    distancia(PosUbCarga,PosD,H3),
                    %pos_carga_sitioDejarCarga_detonador_SitioDetonacionMasCorto
                    sitioDetonacion(Po),
                    distancia(PosD,Po,MasCorto),
                    forall(sitioDetonacion(P),(distancia(PosD,P,Costo), Costo>= MasCorto)),HRecorrido3 is H1+H2+H3+MasCorto.

sec_recorrido1_caso3(Pos,HRecorrido1):-%pos_detonador
                    estaEn([d,d1,_],PosD),
                    distancia(Pos,PosD,H1),
                    %pos_detonador_sitioDejarCarga    
                    ubicacionCarga(PosUbCarga),
                    distancia(PosD,PosUbCarga,H2),
                    %pos_detonador_sitioDejarCarga_SitioDetonacionMasCorto
                    sitioDetonacion(Po),
                    distancia(PosUbCarga,Po,MasCorto),
                    forall(sitioDetonacion(P),(distancia(PosUbCarga,P,Costo), Costo>= MasCorto)),HRecorrido1 is H1+H2+MasCorto.

sec_recorrido2_caso3(Pos,HRecorrido2):-%pos_sitioDejarCarga
                    ubicacionCarga(PosUbCarga),
                    distancia(Pos,PosUbCarga,H1),
                    %pos_sitioDejarCarga_Detonador
                    estaEn([d,d1,_],PosD),
                    distancia(PosUbCarga,PosD,H2),                    
                    %pos_detonador_sitioDejarCarga_SitioDetonacionMasCorto
                    sitioDetonacion(Po),
                    distancia(PosD,Po,MasCorto),
                    forall(sitioDetonacion(P),(distancia(PosD,P,Costo), Costo>= MasCorto)),HRecorrido2 is H1+H2+MasCorto.



menor(N1,N2,N3,N1):-(N1=<N2),(N1=<N3).
menor(N1,N2,N3,N2):-(N2=<N1),(N2=<N3).
menor(N1,N2,N3,N3):-(N3=<N1),(N3=<N2).

menor(N1,N2,N1):-(N1=<N2).
menor(N1,N2,N2):-(N2=<N1).


distancia([F1,C1],[F2,C2],R):-(dif_abs(F1,F2,Rf),dif_abs(C1,C2,Rc),R is Rf+Rc). 


dif_abs(X,Y,Z):-X>=Y,!,Z is	X-Y.	
dif_abs(X,Y,Z):-Z is Y-X.