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


(* :Title: GeneralOracleBack *)
(* :Context: GeneralOracleBack` *)
(* :Author: Michele Bianco, Chiara Mengoli, Akira Petrolini, Christian Preti, Riccardo Scotti *)
(* :Summary: pacchetto che permette di generare un grafo per calcolare i gradi di separazione tra entit\[AGrave] secondo un attributo di raggruppamento *)
(* :Copyright: MB/CM/AP/CP/RS 2023 *)
(* :Package Version: 1 *)
(* :Mathematica Version: 13 *)
(* :History: last modified 24/05/2023 *)

BeginPackage["GeneralOracleBack`"]

GenerateGraph::usage = "Ritorna un grafo, a partire dal dataset, i cui nodi sono gli elementi della colonna entityName e gli archi sono costituiti dagli 
	elementi della colonna groupingAttribute.";

CalcShortestPath::usage = "Ritorna una lista di associazioni con le chiavi entityPath e groupsPath contenenti, rispettivamente, i nodi e gli 
	archi del percorso pi\[UGrave] breve tra firstEntity e secondEntity.";

RandomExtract::usage = "Ritorna una lista di due entit\[AGrave] estratte casualmente da un grafo usando un seed.";

displaySolution::usage = "Mostra graficamente la lista ritornata dalla funzione CalcShortestPath.";

Begin["`Private`"]
(* Il parametro dataset deve essere un tipo di dato Dataset (built-in di Mathematica) *)
(* entityName \[EGrave] una stringa che indica la colonna del dataset in cui sono memorizzate le entit\[AGrave] *)
(* groupingAttribute \[EGrave] una stringa che indica la colonna del dataset in cui sono memorizzati gli elementi che connettono pi\[UGrave] entit\[AGrave] *)
(* Esempio: per i gradi di separazione tra attori, le entit\[AGrave] sono gli attori e gli elementi che li connettono sono i film, per gli 
sportivi sono le gare/partite, per i ricercatori sono le pubblicazioni ecc. *)
GenerateGraph[dataset_, entityName_, groupingAttribute_] :=
    Module[
        {entities, groups, edges, buildEdge}
        ,
        (* Lista, senza duplicati, di tutte le entit\[AGrave] della colonna entityName nel dataset *)
        entities =
            dataset[All, entityName] //  (* Seleziona tutti gli elementi della colonna con titolo uguale al valore di entityName *)
            Normal // (* La funzione Normal converte vari tipi di dato (in questo caso Dataset) in liste associative *)
            DeleteDuplicates; (* Si rimuovono i duplicati per avere una lista di entit\[AGrave] uniche *)
            
        (* Lista di associazioni contenente tutte le entit\[AGrave] correlate a un determinato gruppo, 
            nella forma <|group -> {entity_1, ..., entity_n}|> *)
        groups = Flatten /@ Normal @ dataset[GroupBy[groupingAttribute
            ], List, entityName]; (* La funzione GroupBy permette di raggruppare le tuple di un dataset secondo una delle colonne 
            (in questo caso entityName); il dataset risultante viene poi convertito in una lista di liste associative da Normal e successivamente la 
            funzione Flatten lo trasforma in una lista associativa *)
        
        (* Si mappa la funzione BuildEdge sull'intera lista di gruppi  *)
        edges =
            (* Viene usata KeyValueMap al posto di Map dato che i gruppi sono una lista di 
                 associazioni; KeyValueMap mappa una funzione F di due parametri su una lista di associazioni,
                 usando la chiave come primo parametro di F e il relativo valore come secondo parametro di F.
                 Esempio: se l = <|a -> b, c -> d|> e F una funzione che accetta due parametri,
                 KeyValueMap[F, l] equivale alla lista {F[a, b], F[c, d]}       *)
            (* La funzione BuildEdge costruisce un arco tra tutte le entit\[AGrave] dello stesso gruppo, impostando il nome
            del gruppo come tag (un tag \[EGrave] un attributo che si pu\[OGrave] assegnare all'arco di un grafo) *)
            KeyValueMap[BuildEdge, groups] // 
            Flatten //
            DeleteDuplicates;
        (* Creazione del grafo *)
        (* Il parametro opzionale EdgeLabels permette di impostare che le etichette degli archi siano i tag, cos\[IGrave] come costruito nella funzione
        BuildEdge *)
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
	{list, graphPlot, imageSizeX, imageSizeY, label}
	,
	(* Variabili predefinite che definiscono le dimensioni dei vari componenti di uscita *)
	imageSizeX = 550;
	imageSizeY = 100;
	
	(* Unisce due liste intervallandone gli elementi 
		Riffle[{Subscript[e, 1],Subscript[e, 2],\[Ellipsis]},{Subscript[x, 1],Subscript[x, 2],\[Ellipsis]}] = {e1,x1,e2,x2,\[Ellipsis]} *)
		
	(* Utilizzo map di first per avere un solo film dall'elenco *)
	list = Riffle[output[["entityPath"]], Map[First, output[["groupsPath"]]]] //Flatten;
	graphPlot = {};
	
	(* 
	Costruisco una lista contenente le regole degli archi per il grafo che andr\[OGrave] a generare. 
	Ogni elemento della lista ha la seguente struttura {nodo1-> nodo2, label dell'arco}.
	*)
	For[i = 1, i < Length[list], i++, {
		(* OddQ ritorna true se i \[EGrave] dispari, false se \[EGrave] pari*)
		If[OddQ[i],label = "Ha recitato in", label = "Con"];
   	 graphPlot = Append[graphPlot, {list[[i]]-> list[[i+1]], Style[label, Black]}];
    }];
	(* 
	LayeredGraphPlot utilizza le regole definite in graphPlot per generare graficamente il grafo relativo.
	Tramite le seguenti propriet\[AGrave] andiamo a definire lo stile del grafo:
		VertexShapeFunction ->  definisce lo stile dei nodi; se il nodo corrisponde ad un attore il background del frame sar\[AGrave] blu, 
								mentre se il nodo corrisponde ad un film il background sar\[AGrave] verde.
		EdgeLabels ->  definisce lo stile delle labels degli archi. Inset rappresenta un oggetto inserito in un grafico, 
					in questo caso la label, definendone la posizione e la dimensione.
		DirectedEdges -> se true il grafo sar\[AGrave] un grafo diretto, quindi gli archi saranno delle frecce, se false sar\[AGrave] un grafo 
						indiretto e gli archi saranno privi di frecce.
	    ImageSize -> definisce le dimensioni del grafico. L'altezza viene calcolata in base al numero di nodi.
	*)
	LayeredGraphPlot[graphPlot,
         VertexShapeFunction -> ({If[MemberQ[output[["entityPath"]], #2], 
            Text[Framed[Style[#2, 8, Black], Background -> LightBlue], #1],
            Text[Framed[Style[#2, 8, Black], Background -> LightGreen], #1]
         ]} &),
         EdgeLabels -> ({If[#3 =!= None, {Line[#], Inset[#3, Mean[#1], Automatic, Automatic, #[[1]] - #[[2]], Background -> White]}, Line[#]]} &),
         DirectedEdges->False,
         ImageSize -> {imageSizeX, imageSizeY + (50 * Length[graphPlot])}] 

];


(* Funzione che crea un arco tra ogni entit\[AGrave] di un gruppo, assegnando il nome del gruppo come tag dell'arco *)
(* Questa funzione non \[EGrave] esportata in quanto \[EGrave] solamente di supporto alla funzione GenerateGraph *)
BuildEdge[groupName_, group_] := Module[{subsets},
	(* Vengono estratti tutti i possibili sottoinsiemi di due elementi delle entit\[AGrave] contenute in un gruppo *)
	subsets = Normal @ Subsets[group, {2}]; (* La funzione Subsets permette di costruire i sottoinsiemi di un insieme, 
	specificandone eventualmente il numero di elementi; Normal converte il risultato in lista *)
	UndirectedEdge[#[[1]], #[[2]], groupName]& /@ subsets (* Si costruisce una funzione anonima che crea, attraverso UndirectedEdge, un arco tra i 
	due elementi di un sottoinsieme precedentemente creato, assegnando il nome del gruppo come tag; la funzione anonima viene poi mappata
	sull'intero insieme di sottoinsiemi precedentemente creati *)
];


End[]

EndPackage[]
