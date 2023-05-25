(* ::Package:: *)

(* ::Input:: *)
(**)


(* ::Section:: *)
(*ItalianActorPreprocessing*)


(* ::Text:: *)
(*Questo pacchetto \[EGrave] specifico e relativo al Database usato nel progetto, e in particolare sono i comandi necessari a caricare ed elaborare il dataset.*)


(* ::Subsection:: *)
(*Costruzione del Dataset*)


(* ::Text:: *)
(*Sono stati utilizzati due dataset, la cui unione ha permesso di costruire un grafo necessario all'analisi per il calcolo del livello di separazione tra attori.*)
(*Il primo dataset (actorFilms.csv) \[EGrave] stato reperito al seguente link https://www.kaggle.com/datasets/darinhawley/imdb-films-by-actor-for-10k-actors.*)
(*Esso contiene circa 10000 attori e indica, per ogni entry, un film in cui l'attore stesso ha recitato.*)
(*Gli attributi principali del dataset sono:*)
(*	Actor: Nome e Cognome dell'attore;*)
(*	ActorID: Codice univoco collegato a ogni attore, in particolare riferito all'ID presente su IMDB;*)
(*	Film: Nome del film in cui esso ha recitato;*)
(*	Year: Anno di uscita del film;*)
(*	FilmID: Codice univoco collegato a ogni film, in particolare riferito all'ID presente su IMDB.*)
(*	*)
(*Il secondo dataset (actorFilms2.csv) \[EGrave] stato reperito al seguente link https://www.kaggle.com/datasets/rounakbanik/the-movies-dataset?select=movies_metadata.csv.*)
(*Esso \[EGrave] stato unito al primo dataset in modo da fornire informazioni aggiuntivi su ogni film. Per effettuare il match tra i film \[EGrave] stato utilizzato il parametro FilmID.*)
(*Successivamente, sono stati selezionati i soli film aventi il parametro original_language pari a IT, filtrando quindi gli attori presenti in film in lingua originale italiana.*)
(*Gli attributi principali del secondo dataset sono:*)
(*	original_language: La lingua originale del film.*)
(*	original_title: Il titolo originale (dunque non tradotto in inglese) del film.*)
(**)


(* ::Subsection:: *)
(*Filtro applicato al dataset*)


(* ::Text:: *)
(*Come accennato nella sottosezione precedente, sfruttiamo l'attributo original_language per estrarre dal dataset ottenuto in precedenza solo le righe contenenti film in lingua originale italiana. *)
(*In seguito, viene utilizzata la funzione GroupBy per aggregare ogni attore su una sola riga.*)


(* ::Section:: *)
(*Implementazione*)


(*Copyright: GS2023*)
(*Authors: 
    Michele Bianco (Curriculum A)
	Chiara Mengoli (Curriculum A)
	Akira Petrolini (Curriculum B)
	Christian Preti (Curriculum A)
	Riccardo Scotti (Curriculum A)
	*)
(*Mathematica Version: 13*)
BeginPackage["ItalianActorPreprocessing`"]

GetDataset::usage = "Ritorna un dataset di attori e film con lingua originale in italiano; 
	ogni entry del dataset \[EGrave] costituita dalla coppia attore-film, gli identificativi dell'attore e del 
	film, l'anno di produzione del film, il titolo del film e la lingua in cui \[EGrave] stato prodotto.";

Begin["`Private`"]

GetDataset[] :=
    Module[
        {path, datasetActors, datasetFilms, joinDataset}
        ,
        (* Import dei dataset *)
        path = NotebookDirectory[];
        (* Import dei file CSV, essi risiedono nella cartella data *)            
        datasetActors = Import["data/actorfilms.csv", "Dataset", "HeaderLines"
             -> 1, Path -> path];
        datasetFilms = Import["data/actorfilms2.csv", "Dataset", "HeaderLines"
             -> 1, Path -> path];
        (* Unione dei dataset sull'identificativo del film *)
        joinDataset = JoinAcross[datasetActors, datasetFilms, "FilmID"
             -> "imdb_id"];
        (* Rimozione delle colonne non utili al progetto *)
        joinDataset = KeyDrop[joinDataset, List["adult", "belongs_to_collection",
             "Votes", "Rating", "budget", "genres", "homepage", "id", "title", "overview",
             "popularity", "poster_path", "production_companies", "production_countries",
             "release_date", "revenue", "imdb_id", "spoken_languages", "runtime",
             "status", "tagline", "video", "vote_average", "vote_count"]];
        (* Rinomina di colonne per leggibilit\[AGrave] *)
        joinDataset = joinDataset[All, KeyMap[Replace["original_language"
             -> "OriginalLanguage"]]];
        joinDataset = joinDataset[All, KeyMap[Replace["original_title"
             -> "OriginalTitle"]]];
        (* Filtro del dataset sui film con lingua originale in italiano *)
        joinDataset[Select[#OriginalLanguage == "it"&]
            ]
    ]

End[]

EndPackage[]
