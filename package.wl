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


titles = Normal[italianFilms[All, "OriginalTitle"]] //DeleteDuplicates;
actorsNames = Flatten @@@ Values @ Normal @ italianFilms[GroupBy["OriginalTitle"], List, "Actor"];

actors = Union @@ actorsNames;
edges = UndirectedEdge @@@Subsets[#, {2}]&/@actorsNames //Flatten//DeleteDuplicates;

gr = Graph[actors, edges, VertexLabels->"Name"];
shPath = FindShortestPath[gr, "John Turturro", "Rade Serbedzija"]
Print["La distanza tra gli attori \[EGrave] di ", Length[shPath]-1]




