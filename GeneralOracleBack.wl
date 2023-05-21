(* ::Package:: *)

(* ::Chapter:: *)
(*GeneralOracleBack: Descrizione*)


(* ::Text:: *)
(*Questo pacchetto contiene funzioni per la gestione generica di un database per il calcolo di gradi di separazione, e potrebbe essere usato anche nel caso in cui si dovesse creare un progetto su un differente dataset (ad esempio giocatori di club sportivi, ricercatori che hanno pubblicato articoli insieme, amici sui social, ...).*)


(* ::Subsection:: *)
(*GenerateGraph*)


(* ::Text:: *)
(*Funzione che ritorna un grafo a partire da un dataset i cui nodi sono gli elementi della colonna entityName e gli archi costituiti dagli elementi della colonna groupingAttribute (e.g. in questo caso, i nodi sono gli attori, mentre gli archi i film).*)


(* ::Subsection:: *)
(*CalcShortestPath*)


(* ::Text:: *)
(*Ritorna una lista di associazioni con le chiavi entityPath e groupsPath contenenti, rispettivamente, i nodi e gli archi del cammino minimo tra firstEntity e secondEntity (e.g., il cammino minimo tra due attori inseriti dall'utente).*)


(* ::Subsection:: *)
(*RandomExtract*)


(* ::Text:: *)
(*Ritorna una coppia di entit\[AGrave] estratte casualmente  da un grafo, utilizzando un seed fornito in input.*)


(* ::Subsection:: *)
(*displaySolution*)


(* ::Text:: *)
(*Mostra graficamente la lista ritornata dalla funzione CalcShortestPath. *)


(* ::Section:: *)
(*Implementazione*)


BeginPackage["GeneralOracleBack`"]

GenerateGraph::usage = "Ritorna un grafo, a partire dal dataset, i cui nodi sono gli elementi della colonna entityName e gli archi sono costituiti dagli 
	elementi della colonna groupingAttribute.";

CalcShortestPath::usage = "Ritorna una lista di associazioni con le chiavi entityPath e groupsPath contenenti, rispettivamente, i nodi e gli 
	archi del percorso pi\[UGrave] breve tra firstEntity e secondEntity.";

RandomExtract::usage = "Ritorna una lista di due entit\[AGrave] estratte casualmente da un grafo usando un seed.";

displaySolution::usage = "Mostra graficamente la lista ritornata dalla funzione CalcShortestPath.";

Begin["`Private`"]
GenerateGraph[dataset_, entityName_, groupingAttribute_] :=
    Module[
        {entities, groups, edges, buildEdge}
        ,
        (* Lista, senza duplicati, di tutte le entit\[AGrave] della colonna entityName nel dataset *)
        entities =
            dataset[All, entityName] //
            Normal //
            DeleteDuplicates;
        (* Lista di associazioni contenente tutte le entit\[AGrave] correlate a un determinato gruppo, 
            nella forma <|group -> {entity_1, ..., entity_n}|> *)
        groups = Flatten /@ Normal @ dataset[GroupBy[groupingAttribute
            ], List, entityName];
        (* Funzione che crea un arco tra ogni entit\[AGrave] di un gruppo, assegnando il nome del gruppo 
            come tag dell'arco *)
        buildEdge[groupName_, group_] :=
            Module[{subsets},
                subsets = Normal @ Subsets[group, {2}];
                UndirectedEdge[#[[1]], #[[2]], groupName]& /@ subsets
            ];
        (* Mappando la funzione precedente sull'intera lista di gruppi  *)
        edges =
            (* Viene usata KeyValueMap al posto di Map dato che i gruppi sono una lista di 
                 associazioni *)
            KeyValueMap[buildEdge, groups] //
            Flatten //
            DeleteDuplicates;
        (* Creazione del grafo *)
        Graph[edges, VertexLabels -> "Name", EdgeLabels -> "EdgeTag"]
    ];

CalcShortestPath[graph_, firstEntity_, secondEntity_] :=
    Module[
        {entityPath, groupsPath}
        ,
        (* FindShortestPath ritorna i nodi del percorso pi\[UGrave] breve tra firstEntity e secondEntity *)
        entityPath = FindShortestPath[graph, firstEntity, secondEntity
            ];
        (* entityPath viene prima partizionato in liste di 2 elementi con offset 1, poi viene estratto il 
            tag per ogni coppia *)
        groupsPath = EdgeTags[graph, #]& /@ Partition[entityPath, 2, 1];
        <|"entityPath" -> entityPath, "groupsPath" -> groupsPath|>
    ];
    


RandomExtract[graph_, seed_] :=
    Module[
        {entities, randomPicks}
        ,
        (* Lista di nodi del grafo *)
        entities = VertexList[graph];
        (* Imposta il seed per tutti i metodi Random *)
        SeedRandom[seed];
        (* Estrae casualmente due enitit\[AGrave] *)
        randomPicks = RandomChoice[entities, 2]
    ];


displaySolution[output_]:=
Module[
	{list, graphPlot, imageSizeX, imageSizeY}
	,
	(* Variabili predefinite che definiscono le dimensioni dei vari componenti di uscita *)
	imageSizeX = 550;
	imageSizeY = 1100;
	(* Unisce due liste intervallandone gli elementi 
		Riffle[{Subscript[e, 1],Subscript[e, 2],\[Ellipsis]},{Subscript[x, 1],Subscript[x, 2],\[Ellipsis]}] = {e1,x1,e2,x2,\[Ellipsis]} *)
	list = Riffle[output[["entityPath"]], Map[First, output[["groupsPath"]]]] //Flatten; (*Utilizzo map di first per avere un solo film dall'elenco*)
	graphPlot = {};
	(* Costruisco una lista contenente le regole degli archi per il grafo che andr\[OGrave] a generare *)
	For[i = 1, i < Length[list], i++, {
		(* OddQ ritorna true se i \[EGrave] dispari, false se \[EGrave] pari *)
		If[OddQ[i],label = "Ha recitato in", label = "Con"];
   	 graphPlot = Append[graphPlot, {list[[i]]-> list[[i+1]], Style[label, Black]}];
    }];
	(* LayeredGraphPlot utilizza le regole definite in graphPlot per generare graficamente il grafo relativo.
		VertexShapeFunction, EdgeLabels, EdgeShapeFunction permettono di definire lo stile del grafo. *)
	LayeredGraphPlot[graphPlot,
         VertexShapeFunction -> ({If[MemberQ[output[["entityPath"]], #2], 
            Text[Framed[Style[#2, 8, Black], Background -> LightBlue], #1],
            Text[Framed[Style[#2, 8, Black], Background -> LightGreen], #1]
         ]} &),
         EdgeLabels -> ({If[#3 =!= None, {Line[#], Inset[#3, Mean[#1], Automatic, Automatic, #[[1]] - #[[2]], Background -> White]}, Line[#]]} &),
         (* EdgeShapeFunction -> None, Rimuove le frecce degli archi *)
         DirectedEdges->false,
         ImageSize -> {imageSizeX, imageSizeY}] 

];


End[]

EndPackage[]
