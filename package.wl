(* ::Package:: *)

path = NotebookDirectory[];

dataset = Import["actorfilms.csv", "Dataset", "HeaderLines" -> 1, Path -> path];
dataset2 = Import["actorfilms2.csv", "Dataset", "HeaderLines" -> 1, Path -> path];


(* itFilms = Select[dataset2, #[[8]] == "it" &]; *)


(*SortBy[Select[dataset, #[[1]] == "Fred Astaire" &], #[[4]] &];*)


joinds = JoinAcross[dataset, dataset2, "FilmID" -> "imdb_id"];

joinds = KeyDrop[joinds, List["adult", "belongs_to_collection", "Votes", "Rating", 
"budget", "genres", "homepage", "id", "title", "overview", "popularity", "poster_path", 
"production_companies", "production_countries", "release_date", "revenue", "imdb_id", "spoken_languages",
"runtime", "status", "tagline", "video", "vote_average", "vote_count"]];

joinds = joinds[All,KeyMap[Replace["original_language" -> "Original Language"]]];
joinds = joinds[All,KeyMap[Replace["original_title" -> "Original Title"]]]
