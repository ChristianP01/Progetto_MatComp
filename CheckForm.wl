(* ::Package:: *)

(* ::Section:: *)
(*CheckForm*)


(* ::Text:: *)
(*Questo pacchetto contiene le funzioni utilizzate per il controllo degli input inseriti dall'utente nel men\[UGrave] frontend.*)


(* ::Subsection:: *)
(*InputCorrection*)


(* ::Text:: *)
(*Partendo da una stringa fornita in input, viene restituita in output una stringa nella forma "Nome Cognome", eliminando tutti gli spazi superflui (eccezion fatta per quelli tra nome e cognome).*)
(*Inoltre, viene corretto un eventuale uso improprio di maiuscole e minuscole. (e.g. La stringa "  noMe CoGnOme     " viene modificata in "Nome Cognome").*)


(* ::Subsection:: *)
(*CheckForm*)


(* ::Text:: *)
(*Ritorna una stringa di errore nel caso in cui la stringa sia fornita con caratteri non validi o se l'attore non \[EGrave] presente all'interno del dataset, True altrimenti.*)


(* ::Section:: *)
(*Implementazione*)


BeginPackage["CheckForm`"]

InputCorrection::usage = "Ritorna una stringa formattata rimuovendo gli spazi in eccesso e capitalizzando le iniziali mantenendo gli altri caratteri in minuscolo.";

CheckForm::usage = "Ritorna una stringa di errore se l'input ricevuto non \[EGrave] adeguato, True altrimenti.";

Begin["`Private`"]

(*La seguente funzione (InputCorrection) prende in input una stringa e: 
 - Converte tutta la stringa in minuscolo (ToLowerCase);
 - Converte la prima lettera di ogni parola in maiuscolo (StringReplace);
 - Rimuove gli spazi iniziali e finali (StringTrim);
 - Divide la stringa in una lista di parole (StringSplit);
 - Ricombina rimuovendo spazi iniziali e finali, inserendo uno spazio tra ogni parola (StringRiffle);
*)

InputCorrection[string_]:=
	StringRiffle[#, {"", " ", ""}]& @ StringSplit @ StringTrim @ StringReplace[
		#, WordBoundary ~~ x_ :> ToUpperCase[x]]& @ ToLowerCase @ string


(*La seguente funzione (CheckForm) prende in input un grafo e due entit\[AGrave]: 
 - Il grafo viene ottenuto tramite il metodo GenerateGraph, dal modulo GeneralOracleBack.
 - entity1 ed entity2 sono stringhe contenenti "Nome Cognome" di una coppia di attori del dataset.
*)

CheckForm[graph_, entity1_, entity2_] :=
	Module[
		{},
		(
		(* Utilizziamo un which, avendo diverse condizioni da controllare *)
		Which[
		(* Controlla se le entit\[AGrave] sono stringhe vuote, altrimenti ritorna un messaggio di errore *)
	         StringLength[entity1] == 0 || StringLength[entity2] == 0,
	            "Errore, uno o pi\[UGrave] box di testo risultano vuoti.",
	         
	         (* Controlla che i valori inseriti dall'utente corrispondano a caratteri dell'alfabeto latino esteso, comprendendo inoltre '.', '-' e ' *)
			  StringMatchQ[entity1, RegularExpression["[[:alpha:].' -]+"]] == False (*Controllo che i due entity corrispondano a espressioni regolari che contengano i valori indicati nel commento precedente*)
                    || StringMatchQ[entity2, RegularExpression["[[:alpha:].' -]+"]] == False,
              "Errore, uno o pi\[UGrave] nomi contengono caratteri non validi. Sono ammessi solo caratteri alfabetici e '-'",

			 (* Controlla se le entit\[AGrave] appartengono al grafo *)
			 MemberQ[VertexList[graph], entity1] == False, (*MemberQ verifica che entity1 sia contenuto nell'elenco di nodi del grafo*)
				"Errore, il valore immesso (" <> entity1 <> ") non \[EGrave] presente nel dataset",
				
			 MemberQ[VertexList[graph], entity2] == False,
			    "Errore, il valore immesso (" <> entity2 <> ") non \[EGrave] presente nel dataset",
	         (* Campo default, in caso l'input sia corretto *)
	         True,
	            True
	        ]
		)
	]

End[]

EndPackage[]
