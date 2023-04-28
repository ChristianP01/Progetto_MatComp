(* ::Package:: *)

{
 {path = FileNameJoin[{NotebookDirectory[], "data"}];},
 {\[Placeholder]}
}
dataset = Import["actorfilms.csv", "Dataset", "HeaderLines" -> 1, Path -> path];
dataset2 = Import["actorfilms2.csv", "Dataset", "HeaderLines" -> 1, Path -> path];


joinds = JoinAcross[dataset, dataset2, "FilmID" -> "imdb_id"];

joinds = KeyDrop[joinds, List["adult", "belongs_to_collection", "Votes", "Rating", 
"budget", "genres", "homepage", "id", "title", "overview", "popularity", "poster_path", 
"production_companies", "production_countries", "release_date", "revenue", "imdb_id", "spoken_languages",
"runtime", "status", "tagline", "video", "vote_average", "vote_count"]];

joinds = joinds[All,KeyMap[Replace["original_language" -> "OriginalLanguage"]]];
joinds = joinds[All,KeyMap[Replace["original_title" -> "OriginalTitle"]]]


italianFilms = joinds[Select[#OriginalLanguage == "it"&]]
GroupBy[italianFilms, #Actor&]
(* italianFilms[Select[#Actor == "Marcello Mastroianni"&]] *)


titles = Normal[italianFilms[[All,"OriginalTitle"]]] // DeleteDuplicates;
actorsNames = Flatten @@@ Values @ Normal @ italianFilms[GroupBy["OriginalTitle"],List,"Actor"];

actors = Union @@ actorsNames;
edges =UndirectedEdge @@@Subsets[#,{2}]&/@actorsNames //Flatten//DeleteDuplicates;

gr = Graph[actors, edges, VertexLabels->"Name"];
shPath = FindShortestPath[gr, "John Turturro", "Rade Serbedzija"]
Print["La distanza tra gli attori \[EGrave] di ", Length[shPath]-1]



