(* ::Package:: *)

(* ::Chapter:: *)
(*GeneralOracleBack: Descrizione*)


(* ::Text:: *)
(*Questo pacchetto contiene funzioni per la gestione generica di un database per il calcolo di gradi di separazione, e potrebbe essere usato anche nel caso in cui si dovesse creare un progetto su un differente Database (ad esempio giocatori di club sportivi, ricercatori che hanno pubblicato articoli insieme, amici sui social, ecc...).*)


(* ::Subsection:: *)
(*GenerateGraph*)


(* ::Text:: *)
(*Funzione che ritorna all'utente un grafo a partire da un dataset i cui nodi sono gli elementi della colonna entityName e gli archi costituiti dagli elementi della colonna groupingAttribute (Ad esempio, nel caso del database degli attori i nodi sono gli attori mentre gli archi i film).*)


(* ::Subsection:: *)
(*CalcShortestPath*)


(* ::Text:: *)
(*Ritorna una lista di associazioni con le chiavi entityPath e groupsPath, contenenti, rispettivamente, i nodi e gli archi del cammino minimo tra firstEntity e secondEntity (Ad esempio il cammino minimo tra due attori inseriti dall'utente).*)


(* ::Subsection:: *)
(*RandomExtract*)


(* ::Text:: *)
(*Ritorna una lista di due entit\[AGrave] estratte casualmente  da un grafo usando un seed fornito in input.*)


(* ::Subsection:: *)
(*displaySolution*)


(* ::Text:: *)
(*Mostra graficamente la lista ritornata dalla funzione CalcShortestPath. *)


(* ::Section:: *)
(*Implementazione*)


BeginPackage["GeneralOracleBack`"]

GenerateGraph::usage = "GenerateGraph[dataset, entityName, groupingAttribute] ritorna un grafo, a partire dal dataset, i cui nodi sono gli elementi 
				della colonna entityName e gli archi sono costituiti dagli elementi della colonna groupingAttribute";

CalcShortestPath::usage = "CalcShortestPath[graph, firstEntity, secondEntity] ritorna una lista di associazioni con le chiavi entityPath e groupsPath,
			 contenenti, rispettivamente, i nodi e gli archi del percorso pi\[UGrave] breve tra firstEntity e secondEntity";

RandomExtract::usage = "RandomExtract[graph, seed] ritorna una lista di due entit\[AGrave] estratte casualmente da un grafo usando un seed.";

displaySolution::usage = "displaySolution[output] mostra graficamente la lista ritornata dalla funzione CalcShortestPath";

Begin["`Private`"]
GenerateGraph[dataset_, entityName_, groupingAttribute_] :=
    Module[
        {entities, groups, edges, buildEdge}
        ,
        (* lista, senza duplicati, di tutte le entit\[AGrave] della colonna entityName nel dataset *)
        entities =
            dataset[All, entityName] //
            Normal //
            DeleteDuplicates;
        (* lista di associazioni contenente tutte le entit\[AGrave] correlate a un determinato gruppo, 
        nella forma <|group -> {entity_1, ..., entity_n}|> *)
        groups = Flatten /@ Normal @ dataset[GroupBy[groupingAttribute
            ], List, entityName];
        (* funzione che crea un arco tra ogni entit\[AGrave] di un gruppo, assegnando il nome del gruppo come tag dell'arco *)
        buildEdge[groupName_, group_] :=
            Module[{subsets},
                subsets = Normal @ Subsets[group, {2}];
                UndirectedEdge[#[[1]], #[[2]], groupName]& /@ subsets
            ];
        (* mappando la funzione precedente sull'intera lista di gruppi  *)
        edges =
            (* viene usata KeyValueMap al posto di Map dato che i gruppi sono una lista di associazioni *)
            KeyValueMap[buildEdge, groups] //
            Flatten //
            DeleteDuplicates;
        (* creazione del grafo *)
        Graph[edges, VertexLabels -> "Name", EdgeLabels -> "EdgeTag"]
    ];

CalcShortestPath[graph_, firstEntity_, secondEntity_] :=
    Module[
        {entityPath, groupsPath}
        ,
        (* FindShortestPath ritorna i nodi del percorso pi\[UGrave] breve tra firstEntity e secondEntity *)
        entityPath = FindShortestPath[graph, firstEntity, secondEntity
            ];
        (* entityPath viene prima partizionato in liste di 2 elementi con offset 1, poi viene estratto il tag per ogni coppia *)
        groupsPath = EdgeTags[graph, #]& /@ Partition[entityPath, 2, 1];
        <|"entityPath" -> entityPath, "groupsPath" -> groupsPath|>
    ];
    


RandomExtract[graph_, seed_] :=
    Module[
        {entities, randomPicks}
        ,
        (* lista di nodi del grafo *)
        entities = VertexList[graph];
        (* imposta il seed per tutti i metodi Random *)
        SeedRandom[seed];
        (* estrae casualmente due enitit\[AGrave] *)
        randomPicks = RandomChoice[entities, 2]
    ];


displaySolution[output_]:=
Module[
	{list, graphPlot, imageSizeX, imageSizeY}
	,
	(* Variabili predefinite che definiscono le dimensioni dei vari componenti di uscita *)
	imageSizeX = 550;
	imageSizeY = 1100;
	(*unisce due liste intervallandone gli elementi 
		Riffle[{Subscript[e, 1],Subscript[e, 2],\[Ellipsis]},{Subscript[x, 1],Subscript[x, 2],\[Ellipsis]}] = {e1,x1,e2,x2,\[Ellipsis]}*)
	list = Riffle[output[["entityPath"]], output[["groupsPath"]]] //Flatten;
	graphPlot = {};
	(* costruisco una lista contenente le regole degli archi per il grafo che andr\[OGrave] a generare *)
	For[i = 1, i < Length[list], i++, {
		(* OddQ ritorna true se i \[EGrave] dispari, false se \[EGrave] pari *)
		If[OddQ[i],label = "Was in", label = "With"];
   	 graphPlot = Append[graphPlot, {list[[i]]-> list[[i+1]], Style[label, Black]}];
    }];
	(*LayeredGraphPlot utilizza le regole definite in graphPlot per generare graficamente il grafo relativo.
		VertexShapeFunction, EdgeLabels, EdgeShapeFunction permettono di definire lo stile del grafo.
	*)
	LayeredGraphPlot[graphPlot,
         VertexShapeFunction -> ({If[MemberQ[output[["entityPath"]], #2], 
            Text[Framed[Style[#2, 8, Black], Background -> LightBlue], #1],
            Text[Framed[Style[#2, 8, Black], Background -> LightGreen], #1]
         ]} &),
         EdgeLabels -> ({If[#3 =!= None, {Line[#], Inset[#3, Mean[#1], Automatic, Automatic, #[[1]] - #[[2]], Background -> White]}, Line[#]]} &),
         EdgeShapeFunction -> None, (*rimuove le frecce degli archi*)
         ImageSize -> {imageSizeX, imageSizeY}] 

];


End[]

EndPackage[]
