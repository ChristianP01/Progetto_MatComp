(* ::Package:: *)

loadData::usage = "Function that retrieves the dataset and generates the relative graph. ";
showFrontend::usage = "Function used to show frontend of the project";


loadData[] := (
	italianFilms = GetDataset[];
	gr = GenerateGraph[italianFilms, "Actor", "OriginalTitle"]
);


showFrontend[] := 
	Module[
	{}, 
		
		gr = loadData[];
		
		Panel[
	 Grid[{
	   {
	     InputField[Dynamic[inputActor1], String, FieldHint -> "Inserisci il primo attore..."],
	     InputField[Dynamic[inputActor2], String, FieldHint -> "Inserisci il secondo attore..."],
	     InputField[Dynamic[answer], Number, FieldHint -> "Indovina la distanza..."],
	     InputField[Dynamic[seed], Number, FieldHint -> "Scegli un seed..."]
	   },
	   
	   {
	   (*Bottone che calcola la distanza tra due attori forniti in input*)
	     Button[Style["Calcola", Medium], (
	     inputActor1 = InputCorrection[inputActor1]; (*Correzione nomi attori*)
	     inputActor2 = InputCorrection[inputActor2];
	     cfOutput = CheckForm[gr, inputActor1, inputActor2];
		 If[StringQ[cfOutput], 
	        (CreateDialog[{TextCell[cfOutput], DefaultButton[]}, WindowSize -> {300, 70}]), 
	        (*Print[CalcShortestPath[gr, inputActor1, inputActor2]];*)
	        sp = CalcShortestPath[gr, inputActor1, inputActor2];
	        Print[displaySolution[sp]];
	        (*mostra il grafico in una nuova finestra
	        CreateDocument[Dynamic[displaySoution[sp]], WindowSize -> {500, 500}]*)
	        (*mostra il grafico in una cella*)
	        (*AttachCell[EvaluationCell[],Panel[Grid[{{Row[{"La distanza \[EGrave] ", Length[sp[["entityPath"]]]-1}]},{displaySolution[sp]}}]]];*)
	     ]), ImageSize -> {100, 50}],
	     
	     Button[Style["Reset", Medium], (
	       (* Resetto i valori eventualmente contenuti all'interno dei tre box di cfOutput. *)
	       inputActor1 = inputActor2 = Null;
	       answer = Null;
	       seed = Null;
	     ), ImageSize -> {100, 50}],
	     
	     Button[Style["Indovina", Medium],(
	       Which[
	         (* Controllo che l'utente abbia inserito un valore. *)
	         Not[NumberQ[answer]],
	           (CreateDialog[{TextCell["\[CapitalEGrave] necessario inserire un numero per provare ad indovinare."], DefaultButton[]}, WindowSize -> {300, 70}];),

	         (* Campo default, in caso l'input sia corretto. *)
	         True, (
	           inputActor1 = InputCorrection[inputActor1]; (*Correzione nomi attori*)
	           inputActor2 = InputCorrection[inputActor2];
	           cfOutput = CheckForm[gr, inputActor1, inputActor2];
		       If[StringQ[cfOutput], 
	              (CreateDialog[{TextCell[cfOutput], DefaultButton[]}, WindowSize -> {300, 70}]), 
	              sp = CalcShortestPath[gr, inputActor1, inputActor2];
	           ];
	           If[answer == Length[sp[["entityPath"]]]-1,
	            (CreateDialog[{TextCell["Complimenti, hai indovinato!"], DefaultButton[]}, WindowSize -> {300, 70}];),
	            (CreateDialog[{TextCell["Peccato, risposta errata!"], DefaultButton[]}, WindowSize -> {300, 70}];)
	          ]
	         )
	       ]; 
	     ), ImageSize->{100, 50}], 
	     
	     (* Estrae due nomi casuali dal dataset e calcola la distanza tra loro *)
         Button[Style["Casuale", Medium], (
             Which[
             Not[NumberQ[seed]],
                 (CreateDialog[{TextCell["\[CapitalEGrave] necessario inserire un seed per generare risultati casuali."], DefaultButton[]}, WindowSize -> {300, 70}];),
             True,
                 (re = RandomExtract[gr, seed]; inputActor1 = re[[1]]; inputActor2 = re[[2]];)
             ]
         ), ImageSize -> {100, 50}] 
	   }
	  },
	  Spacings -> {4, 1}
	  ],
	 ImageSize -> {900, 200}
	]
]
