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
(*Utilizzeremo due dataset con cui ne otterremo uno finale su cui costruire un grafo necessario all'analisi per il calcolo del grado di separazione tra attori.*)
(*Il primo dataset (actorFilms.csv) \[EGrave] stato reperito al seguente link (https://www.kaggle.com/datasets/darinhawley/imdb-films-by-actor-for-10k-actors).*)
(*Contiene 10000 attori e indica in ogni sua riga un film (e alcune sue caratteristiche) in cui l'attore ha recitato.*)
(*Gli attributi del dataset necessari per il progetto sono:*)
(*	Actor: Nome e Cognome dell'attore.*)
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
(**)


(* ::Subsection:: *)
(*Filtro applicato al dataset*)


(* ::Text:: *)
(*Come accennato nella sottosezione precedente, sfruttiamo l'attributo OriginalLanguage per ottenere dal dataset ottenuto in precedenza solo le righe contenenti film in lingua originale italiana. *)
(*In seguito, viene utilizzata la funzione GroupBy per aggregare ogni attore in una sola riga.*)


(* ::Section:: *)
(*Implementazione*)


BeginPackage["ItalianActorPreprocessing`"]

GetDataset::usage = "GetDataset[] ritorna un dataset di attori e film con lingua originale in italiano; 
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
