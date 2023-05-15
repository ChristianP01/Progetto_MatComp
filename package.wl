(* ::Package:: *)

(* ::Text:: *)
(*Costruzione del Dataset*)
(*Per la costruzione del dataset, utilizzeremo due dataset con cui costruirne uno finale su cui costruire un grafo necessario all'analisi per il calcolo del grado*)
(*di separazione tra attori. *)
(*Il primo dataset (actorFilms.csv) \[EGrave] stato reperito al seguente link (https://www.kaggle.com/datasets/darinhawley/imdb-films-by-actor-for-10k-actors).*)
(*Contiene 10000 attori e indica in ogni sua riga un film (e alcune sue caratteristiche) in cui l'attore ha recitato.*)
(*Gli attributi del dataset necessari per il progetto sono:*)
(*	Actor: Il nome dell'attore.*)
(*	ActorID: Codice univoco collegato a ogni attore, in particolare riferito all'ID presente su IMDB.*)
(*	Film: Nome del film in cui esso ha recitato.*)
(*	Year: Anno di uscita del film*)
(*	FilmID: Codice univoco collegato a ogni film, in particolare riferito all'ID presente su IMDB.*)
(*Il secondo dataset (actorFilms2.csv) \[EGrave] stato reperito al seguente link (https://www.kaggle.com/datasets/rounakbanik/the-movies-dataset?select=movies_metadata.csv).*)
(*Esso contiene metadati interessanti su ogni film, utilizzando lo stesso FilmID di IMDB, in modo da consentirci di avere informazioni ulteriori da aggiungere*)
(*al dataset precedente, in particolare noi sfrutteremo l'attributo che ne dice la lingua originale, per effettuare in seguito un filtro sugli attori presenti in film*)
(*in lingua originale italiana.*)
(*Gli attributi del dataset necessari per il progetto nel secondo dataset sono:*)
(*	original_language: La lingua originale del film.*)
(*	original_title: Il titolo originale (dunque non tradotto in inglese) del film.*)


(* Import dei dataset *)
path = NotebookDirectory[];

(* Import dei file CSV, essi risiedono nella cartella data *)
datasetActors = Import["data/actorfilms.csv", "Dataset", "HeaderLines" -> 1, Path -> path];
datasetFilms = Import["data/actorfilms2.csv", "Dataset", "HeaderLines" -> 1, Path -> path];


(* Unione dei dataset sull'identificativo del film *)
joinDataset = JoinAcross[datasetActors, datasetFilms, "FilmID" -> "imdb_id"];

(* Rimozione delle colonne non utili al progetto *)
joinDataset = KeyDrop[joinDataset, List["adult", "belongs_to_collection", "Votes", "Rating", 
"budget", "genres", "homepage", "id", "title", "overview", "popularity", "poster_path", 
"production_companies", "production_countries", "release_date", "revenue", "imdb_id", "spoken_languages",
"runtime", "status", "tagline", "video", "vote_average", "vote_count"]];

(* Rinomina di colonne per leggibilit\[AGrave] *)
joinDataset = joinDataset[All,KeyMap[Replace["original_language" -> "OriginalLanguage"]]];
joinDataset = joinDataset[All,KeyMap[Replace["original_title" -> "OriginalTitle"]]];

(* Filtro del dataset sui film con lingua originale in italiano *)
italianFilms = joinDataset[Select[#OriginalLanguage == "it"&]];


titles = Normal[italianFilms[All, "OriginalTitle"]] //DeleteDuplicates;
actors = Union @@ Flatten @@@ Values @ Normal @ italianFilms[GroupBy["OriginalTitle"], List, "Actor"];


GenerateGraph[dataset_, entityName_, groupingAttribute_] :=
		    Module[{entities, groups, edges, buildEdge},
		        (* list of unique entities in the dataset, which are in the column entityName *)
		        entities =
		            dataset[All, entityName] //
		            Normal //
		            DeleteDuplicates;
		         
		        (* association list containing all entities related to a certain group, in the form <|group -> {entity_1, ..., entity_n}|> *)
		        groups = Flatten /@ Normal @ dataset[GroupBy[groupingAttribute], List, entityName];
		        
		        (* function that creates an edge between each entity in a group, assigning the group name as an edge tag *) 
		        buildEdge[groupName_, group_] :=
		            Module[{subsets},
		                subsets = Normal @ Subsets[group, {2}];
		                UndirectedEdge[#[[1]], #[[2]], groupName]& /@ subsets
		                    
		            ];
		            
		        (* mapping the function above onto the entire list of groups *)
		        edges =
		            (* KeyValueMap used instead of Map since groups is an association list *)
		            KeyValueMap[buildEdge, groups] //
		            Flatten //
		            DeleteDuplicates;
		            
		        (* creation of the graph *) 
		        Graph[edges, VertexLabels -> "Name", EdgeLabels -> "EdgeTag"]]


(* OLD
calcShortestPath[act1_, act2_] :=
	(
		shPath = FindShortestPath[gr, act1, act2];
		(* Recuperiamo tutti i film in comune per ogni coppia di attori facente
			 parte del path *)
		dist = Length[shPath] - 1;
		actorCouple = {};
		filmCouple = {};
		
		(*Inizializzazione lista output*)
		resultAssociation = <||>; 
		
		For[i = 1, i <= Length[shPath] - 1, i++,
			{
				firstActor = shPath[[i]];
				secondActor = shPath[[i + 1]];
				actor1Movies = italianFilms[GroupBy["Actor"]][[firstActor]][[All,
					 "Film"]];
				actor2Movies = italianFilms[GroupBy["Actor"]][[secondActor]][[All,
					 "Film"]];
				joinedFilmList = Intersection[actor1Movies, actor2Movies] // Normal
					;
				resultAssociation[{firstActor, secondActor}] = joinedFilmList;
			}
		];
		
		resultAssociation["Distanza"] = dist;
		resultAssociation
	)
*)


CalcShortestPath[graph_, firstEntity_, secondEntity_] := Module[{entityPath, groupsPath},
			(* getting the vertices in the shortest path *)
			entityPath = FindShortestPath[graph, firstEntity, secondEntity];
			splittedEntityPath = Partition[entityPath, 2];
			
			(* entityPath is first partitioned in lists of 2 elements with offset 1, then tag is extracted for each couple *) 
			groupsPath = EdgeTags[graph, #]& /@ Partition[entityPath, 2, 1];
			Print[groupsPath];
			<| "entityPath" -> splittedEntityPath, "groupsPath" -> groupsPath, "Distance" -> Length[splittedEntityPath] |>
		]


actorGraph = GenerateGraph[italianFilms, "Actor", "OriginalTitle"];
CalcShortestPath[actorGraph, "Roberto Benigni", "Pierfrancesco Favino"];


checkForm[inputActor1_, inputActor2_] := (
		Which[
	         (* Controllo se gli InputField non contengono SOLO caratteri alfabetici. *)
	         Not[StringFreeQ[inputActor1, DigitCharacter]] == True || Not[StringFreeQ[inputActor2, DigitCharacter]] == True,
	           (CreateDialog[{TextCell["Errore, uno o pi\[UGrave] nomi inseriti non sono validi."], DefaultButton[]}, WindowSize -> {300, 70}];),
	         
	         (* Controllo se gli InputField non sono vuote. *)
	         (StringLength[inputActor1] == 0 || StringLength[inputActor2] == 0),
	           (CreateDialog[{TextCell["Errore, uno o pi\[UGrave] box di testo risultano vuoti."], DefaultButton[]}, WindowSize -> {300, 70}];),

			(* Se uno o pi\[UGrave] attori non sono presenti nel dataset. *)
			(MemberQ[actors, inputActor1] == False || MemberQ[actors, inputActor2] == False),
				(CreateDialog[{TextCell["Errore, uno o pi\[UGrave] attori non sono stati trovati."], DefaultButton[]}, WindowSize -> {300, 70}];),

	         (* Campo default, in caso l'input sia corretto. *)
	         True, (
	           output = CalcShortestPath[actorGraph, inputActor1, inputActor2];
	           Print[output];
	         )
	       ]
	);


(* ::Input:: *)
(**)


Begin["Frontend`"]
	Panel[
	 Grid[{
	   {
	     InputField[Dynamic[inputActor1], String, FieldHint -> "Inserisci il primo attore..."],
	     InputField[Dynamic[inputActor2], String, FieldHint -> "Inserisci il secondo attore..."],
	     InputField[Dynamic[answer], Number, FieldHint -> "Indovina la Distance..."]
	   },
	   
	   {
	     Button[Style["Calcola", Medium], (
	       checkForm[inputActor1, inputActor2];  
	     ), ImageSize -> {100, 50}],
	     
	     Button[Style["Reset", Medium], (
	       (* Resetto i valori eventualmente contenuti all'interno dei tre box di testo. *)
	       inputActor1 = inputActor2 = Null;
	       answer = Null;
	     ), ImageSize -> {100, 50}],
	     
	     Button[Style["Indovina", Medium],(
	       Which[
	         (* Controllo che l'utente abbia inserito un valore. *)
	         Not[NumberQ[answer]],
	           (CreateDialog[{TextCell["\[CapitalEGrave] necessario inserire un numero per provare ad indovinare."], DefaultButton[]}, WindowSize -> {300, 70}];),

	         (* Campo default, in caso l'input sia corretto. *)
	         True, (
	           checkForm[inputActor1, inputActor2];
	           If[answer == output[["Distance"]],
	            (CreateDialog[{TextCell["Complimenti, hai indovinato!"], DefaultButton[]}, WindowSize -> {300, 70}];),
	            (CreateDialog[{TextCell["Peccato, risposta errata!"], DefaultButton[]}, WindowSize -> {300, 70}];)
	          ]
	         )
	       ];
	     ), ImageSize->{100, 50}],
	     
	     (* Estrae due nomi casuali dal dataset e calcola la Distance tra loro *)
         Button[Style["Casuale", Medium], (
           inputActor1 = RandomChoice[actors];
           inputActor2 = RandomChoice[actors];
           checkForm[inputActor1, inputActor2];
         ), ImageSize -> {100, 50}]
	   }
	  },
	  Spacings -> {4, 1}
	  ],
	 ImageSize -> {800, 200}
	 ]
End[]

(* Sopprimiamo la comparsa della finestra Messages di Mathematica *)
(*SetOptions[$FrontEnd, MessageOptions -> {"ShowMessagesInConsole" -> False}]*)
















(* ::Print:: *)
(*CalcShortestPath["Roberto Benigni","Pierfrancesco Favino"]*)