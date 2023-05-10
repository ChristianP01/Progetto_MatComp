(* ::Package:: *)

BeginPackage["CheckForm`"]

InputCorrection::usage = "InputCorrection[string] ToDo!!!!!!!!!!";

CheckForm::usage = "CheckForm[graph, entity1, entity2] ritorna una stringa di errore
						in se l'input ricevuto non \[EGrave] adeguato, altrimenti ritorna \"OK\"";

Begin["`Private`"]

InputCorrection[string_]:=
	StringRiffle[#, {"", " ", ""}]& @ StringSplit @ StringTrim @ StringReplace[
		#, WordBoundary ~~ x :> ToUpperCase[x]]& @ ToLowerCase @ string

CheckForm[graph_, entity1_, entity2_] :=
	Module[
		{},
		(Which[
			 (* Controlla se le entit\[AGrave] sono stringhe vuote. *)
	         StringLength[entity1] == 0 || StringLength[entity2] == 0,
	            "Errore, uno o pi\[UGrave] box di testo risultano vuoti.",
	         
	         (* Controlla se le entit\[AGrave] contengono SOLO caratteri alfabetici. *)
	         StringMatchQ[entity1, RegularExpression["^[a-zA-Z]+$"]] == True 
	            || StringMatchQ[entity2, RegularExpression["^[a-zA-Z]+$"]] == True,
	            "Errore, uno o pi\[UGrave] nomi contengono caratteri non validi; Sono ammessi 
					solo caratteri alfabetici.",

			 (* Controlla se le entit\[AGrave] appartengono al grafo. *)
			 MemberQ[VertexList[graph], entity1] == False 
			    || MemberQ[VertexList[graph], entity2] == False,
				"Errore, il valore immesso non \[EGrave] presente nel dataset",

	         (* Campo default, in caso l'input sia corretto. *)
	         True,
	            "OK"
	        ]
		)
	]

End[]

EndPackage[]
