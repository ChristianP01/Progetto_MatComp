(* ::Package:: *)

BeginPackage["CheckForm`"]

CheckForm::usage = "";

Begin["`Private`"]

CheckForm[entity1_, entity2_] :=
	Module[
		{},
		(Which[
	         (* Controllo se gli InputField non contengono SOLO caratteri alfabetici. *)
	         Not[StringFreeQ[entity1, DigitCharacter]] == True || Not[StringFreeQ[entity2, DigitCharacter]] == True,
	           "Errore, uno o pi\[UGrave] nomi inseriti non sono validi."(*(CreateDialog[{TextCell["Errore, uno o pi\[UGrave] nomi inseriti non sono validi."], DefaultButton[]}, WindowSize -> {300, 70}];)*),
	         
	         (* Controllo se gli InputField non sono vuote. *)
	         (StringLength[entity1] == 0 || StringLength[entity2] == 0),
	           "Errore, uno o pi\[UGrave] box di testo risultano vuoti."(*(CreateDialog[{TextCell["Errore, uno o pi\[UGrave] box di testo risultano vuoti."], DefaultButton[]}, WindowSize -> {300, 70}];)*),
(*
			(* Se uno o pi\[UGrave] attori non sono presenti nel dataset. *)
			(MemberQ[actors, inputActor1] == False || MemberQ[actors, inputActor2] == False),
				False(*(CreateDialog[{TextCell["Errore, uno o pi\[UGrave] attori non sono stati trovati."], DefaultButton[]}, WindowSize -> {300, 70}];)*),
*)
	         (* Campo default, in caso l'input sia corretto. *)
	         True, (
	           "OK"
	         )
	       ]
	)
	]

End[]

EndPackage[]
