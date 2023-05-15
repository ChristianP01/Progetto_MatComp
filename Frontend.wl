(* ::Package:: *)

loadData::usage = "Function that retrieves the dataset and generates the relative graph. ";
showFrontend::usage = "Function used to show frontend of the project";


italianFilms = GetDataset[];
gr = GenerateGraph[italianFilms, "Actor", "OriginalTitle"];


(* Hardcoded variables defining the size of various frontend components *)
buttonX = 100;
buttonY = 50;

labelX = 300;
labelY = 70;

panelX = 900;
panelY = 200;


showFrontend[] := 
	Module[
	{}, 
	
	
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
	        (CreateDialog[{TextCell[cfOutput], DefaultButton[]}, WindowSize -> {labelX, labelY}]), 
	        (*Print[CalcShortestPath[gr, inputActor1, inputActor2]];*)
	        sp = CalcShortestPath[gr, inputActor1, inputActor2];
	        Print[displaySolution[sp]];
	        (*mostra il grafico in una nuova finestra
	        CreateDocument[Dynamic[displaySoution[sp]], WindowSize -> {500, 500}]*)
	        (*mostra il grafico in una cella*)
	        (*AttachCell[EvaluationCell[],Panel[Grid[{{Row[{"La distanza \[EGrave] ", Length[sp[["entityPath"]]]-1}]},{displaySolution[sp]}}]]];*)
	     ]), ImageSize -> {buttonX, buttonY}],
	     
	     Button[Style["Reset", Medium], (
	       (* Resetto i valori eventualmente contenuti all'interno dei tre box di cfOutput. *)
	       inputActor1 = inputActor2 = Null;
	       answer = Null;
	       seed = Null;
	     ), ImageSize -> {buttonX, buttonY}],
	     
	     Button[Style["Indovina", Medium],(
	       Which[
	         (* Controllo che l'utente abbia inserito un valore. *)
	         Not[NumberQ[answer]],
	           (CreateDialog[{TextCell["\[CapitalEGrave] necessario inserire un numero per provare ad indovinare."], DefaultButton[]}, WindowSize -> {labelX, labelY}];),

	         (* Campo default, in caso l'input sia corretto. *)
	         True, (
	           inputActor1 = InputCorrection[inputActor1]; (*Correzione nomi attori*)
	           inputActor2 = InputCorrection[inputActor2];
	           cfOutput = CheckForm[gr, inputActor1, inputActor2];
		       If[StringQ[cfOutput], 
	              (CreateDialog[{TextCell[cfOutput], DefaultButton[]}, WindowSize -> {labelX, labelY}]), 
	              sp = CalcShortestPath[gr, inputActor1, inputActor2];
	           ];
	           If[answer == Length[sp[["entityPath"]]]-1,
	            (CreateDialog[{TextCell["Complimenti, hai indovinato!"], DefaultButton[]}, WindowSize -> {labelX, labelY}];),
	            (CreateDialog[{TextCell["Peccato, risposta errata!"], DefaultButton[]}, WindowSize -> {labelX, labelY}];)
	          ]
	         )
	       ]; 
	     ), ImageSize->{buttonX, buttonY}], 
	     
	     (* Estrae due nomi casuali dal dataset e calcola la distanza tra loro *)
         Button[Style["Casuale", Medium], (
             Which[
             Not[NumberQ[seed]],
                 (CreateDialog[{TextCell["\[CapitalEGrave] necessario inserire un seed per generare risultati casuali."], DefaultButton[]}, WindowSize -> {labelX, labelY}];),
             True,
                 (re = RandomExtract[gr, seed]; inputActor1 = re[[1]]; inputActor2 = re[[2]];)
             ]
         ), ImageSize -> {buttonX, buttonY}] 
	   }
	  },
	  Spacings -> {4, 1}
	  ],
	 ImageSize -> {panelX, panelY}
	]
]

