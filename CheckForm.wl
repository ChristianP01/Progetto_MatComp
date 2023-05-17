(* ::Package:: *)

(* ::Section:: *)
(*CheckForm*)


(* ::Text:: *)
(*Questo pacchetto contiene tutte le funzioni utili al controllo dell'input inserito da utente.*)


(* ::Subsection:: *)
(*InputCorrection*)


(* ::Text:: *)
(*A partire da una stringa di input, ne restituisce una di output nella forma "Nome Cognome", eliminando eventuali spazi di troppo e correggendo un uso errato di maiuscole e minuscole. (Ad esempio la stringa "  noMe CoGnOme     " viene restituita come "Nome Cognome").*)


(* ::Subsection:: *)
(*CheckForm*)


(* ::Text:: *)
(*Ritorna una stringa di errore nel caso in cui la stringa sia fornita con caratteri non validi o se l'attore non \[EGrave] presente all'interno del dataset, True altrimenti.*)


(* ::Section:: *)
(*Implementazione*)


BeginPackage["CheckForm`"]

InputCorrection::usage = "InputCorrection[string] ritorna una stringa formattata rimuovendo gli spazi 
	in eccesso e capitalizzando le iniziali mantenendo gli altri caratteri in minuscolo.";

CheckForm::usage = "CheckForm[graph, entity1, entity2] ritorna una stringa di errore in se l'input 
	ricevuto non \[EGrave] adeguato, altrimenti ritorna True.";

Begin["`Private`"]

InputCorrection[string_]:=
	StringRiffle[#, {"", " ", ""}]& @ StringSplit @ StringTrim @ StringReplace[
		#, WordBoundary ~~ x_ :> ToUpperCase[x]]& @ ToLowerCase @ string

CheckForm[graph_, entity1_, entity2_] :=
	Module[
		{},
		(Which[
			 (* Controlla se le entit\[AGrave] sono stringhe vuote *)
	         StringLength[entity1] == 0 || StringLength[entity2] == 0,
	            "Errore, uno o pi\[UGrave] box di testo risultano vuoti.",
	         
	         (* Controlla se le entit\[AGrave] contengono SOLO caratteri alfabetici *)
	         StringMatchQ[entity1, RegularExpression["^[a-zA-Z ]+$"]] == False 
	            || StringMatchQ[entity2, RegularExpression["^[a-zA-Z ]+$"]] == False,
	            "Errore, uno o pi\[UGrave] nomi contengono caratteri non validi; Sono ammessi 
					solo caratteri alfabetici.",

			 (* Controlla se le entit\[AGrave] appartengono al grafo *)
			 MemberQ[VertexList[graph], entity1] == False 
			    || MemberQ[VertexList[graph], entity2] == False,
				"Errore, il valore immesso non \[EGrave] presente nel dataset",

	         (* Campo default, in caso l'input sia corretto *)
	         True,
	            True
	        ]
		)
	]

End[]

EndPackage[]
