(* ::Package:: *)

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
joinDataset = joinDataset[All,KeyMap[Replace["original_title" -> "OriginalTitle"]]]


(* Filtro del dataset sui film con lingua originale in italiano *)
italianFilms = joinDataset[Select[#OriginalLanguage == "it"&]];

GroupBy[italianFilms, #Actor&];

titles = Normal[italianFilms[All, "OriginalTitle"]] //DeleteDuplicates;
actorsNames = Flatten @@@ Values @ Normal @ italianFilms[GroupBy["OriginalTitle"], List, "Actor"];
actors = Union @@ actorsNames;
edges = UndirectedEdge @@@Subsets[#, {2}]&/@actorsNames //Flatten//DeleteDuplicates;
gr = Graph[actors, edges, VertexLabels->"Name"];


Begin["ShortestPathEnv`"]
	calcShortestPath[act1_, act2_] := (
	
		shPath = FindShortestPath[gr, act1, act2];
		
		(* Recuperiamo tutti i film in comune per ogni coppia di attori facente parte del path *)
		finale = {};
		count = 1;
		actorFilm = {shPath[[1]]};
		dist=Length[shPath]-1;
		
		actorCouple = {};
		filmCouple = {};
		
		For[i = 1, i <= Length[shPath]-1, i++, {
		    firstActor = shPath[[i]];
		    secondActor = shPath[[i+1]];
		    
		    actor1Movies = italianFilms[GroupBy["Actor"]][[firstActor]][[All, "Film"]];
		    actor2Movies = italianFilms[GroupBy["Actor"]][[secondActor]][[All, "Film"]];
		    joinedFilmList = Intersection[actor1Movies, actor2Movies] // Normal;
		    
		    resultAssociation[{firstActor, secondActor}] = joinedFilmList;
		    

		    actorFilm = Append[actorFilm,joinedFilmList];
		    actorFilm = Append[actorFilm,secondActor];
		}];
	resultAssociation["Distanza"] = dist;
	resultAssociation
	)
End[]


shPath = FindShortestPath[gr, "Roberto Benigni", "Maria Grazia Cucinotta"];
		dist=Length[shPath]-1;
		
		actorCouple = {};
		filmCouple = {};
		resultAssociation = <||>;




(* ::Input:: *)
(**)


Begin["Frontend`"]

	checkForm[] := (
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
	           output = ShortestPathEnv`calcShortestPath[inputActor1, inputActor2];
	           Print[output];
	         )
	       ]
	);
	
	Panel[
	 Grid[{
	   {
	     InputField[Dynamic[inputActor1], String, FieldHint -> "Inserisci il primo attore..."],
	     InputField[Dynamic[inputActor2], String, FieldHint -> "Inserisci il secondo attore..."],
	     InputField[Dynamic[answer], Number, FieldHint -> "Indovina la distanza..."]
	   },
	   
	   {
	     Button[Style["Calcola", Medium], (
	       checkForm[];  
	     ), ImageSize -> {100, 50}],
	     
	     Button[Style["Reset", Medium], (
	       (* Resetto i valori eventualmente contenuti all'interno dei tre box di testo. *)
	       inputActor1 = inputActor2 = "";
	       answer = "";
	     ), ImageSize -> {100, 50}],
	     
	     Button[Style["Indovina", Medium],(
	       Which[
	         (* Controllo che l'utente abbia inserito un valore. *)
	         Not[NumberQ[answer]],
	           (CreateDialog[{TextCell["\[CapitalEGrave] necessario inserire un numero per provare ad indovinare."], DefaultButton[]}, WindowSize -> {300, 70}];),

	         (* Campo default, in caso l'input sia corretto. *)
	         True, (
	           checkForm[];
	           If[answer == output[["Distanza"]],
	            (CreateDialog[{TextCell["Complimenti, hai indovinato!"], DefaultButton[]}, WindowSize -> {300, 70}];),
	            (CreateDialog[{TextCell["Peccato, risposta errata!"], DefaultButton[]}, WindowSize -> {300, 70}];)
	          ]
	         )
	       ];
	       
	       
	     ), ImageSize->{100, 50}]
	   }
	  },
	  Spacings -> {4, 1}
	  ],
	 ImageSize -> {600, 200}
	 ]
End[]

(* Sopprimiamo la comparsa della finestra Messages di Mathematica *)
SetOptions[$FrontEnd, MessageOptions -> {"ShowMessagesInConsole" -> False}]




