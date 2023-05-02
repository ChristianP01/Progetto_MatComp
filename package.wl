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
italianFilms = joinDataset[Select[#OriginalLanguage == "it"&]]

GroupBy[italianFilms, #Actor&]
(* italianFilms[Select[#Actor == "Marcello Mastroianni"&]] *)





Begin["Funzione`"]
	calcShortestPath[act1_, act2_] := (
	titles = Normal[italianFilms[All, "OriginalTitle"]] //DeleteDuplicates;
	actorsNames = Flatten @@@ Values @ Normal @ italianFilms[GroupBy["OriginalTitle"], List, "Actor"];
	actors = Union @@ actorsNames;
	edges = UndirectedEdge @@@Subsets[#, {2}]&/@actorsNames //Flatten//DeleteDuplicates;
	
	gr = Graph[actors, edges, VertexLabels->"Name"];
	
	shPath = FindShortestPath[gr, act1, act2];
	
	(* Recuperiamo tutti i film in comune per ogni coppia di attori facente parte del path *)
	finale = {};
	count = 1;
	
	For[i = 1, i <= Length[shPath]-1, i++, 
	  {
	    firstActor = shPath[[i]];
	    secondActor = shPath[[i+1]];
	    
	    actor1Movies = italianFilms[GroupBy["Actor"]][[firstActor]][[All, "Film"]];
	    actor2Movies = italianFilms[GroupBy["Actor"]][[secondActor]][[All, "Film"]];
	    joinedFilmList = Intersection[actor1Movies, actor2Movies] // Normal ;
	    finale = Append[finale, joinedFilmList];
	  }
	];
	<|"actorPath"->shPath,"filmPath"->Flatten@finale|>
)

End[]


calcShortestPath["Roberto Benigni", "Pierfrancesco Favino"][["filmPath"]]





Begin["Frontend`"]
	Panel[
	 Grid[{
	   {
	     InputField[Dynamic[inputActor1], String], InputField[Dynamic[inputActor2], String]
	   },
	   
	   {
	     Button["Calcola", {
	       Print[calcShortestPath[inputActor1, inputActor2][["actorPath"]]];
	       Print[calcShortestPath[inputActor1, inputActor2][["filmPath"]]];
	     },
	     ImageSize -> {150, 50}],
	     Button["Reset", (* inserire qui l'azione del pulsante *), ImageSize -> {150, 50}]
	   }
	  },
	  Spacings -> {2, 1}
	  ],
	 ImageSize -> {600, 200}
	 ]
End[]



