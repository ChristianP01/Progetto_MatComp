(* ::Package:: *)

(* ::Section:: *)
(*Frontend*)


(* ::Text:: *)
(*Pacchetto dedicato alla costruzione del men\[UGrave] utilizzabile nel notebook, con annessa implementazione delle funzionalit\[AGrave] dei bottoni.*)


(* ::Subsection:: *)
(*showFrontend*)


(* ::Text:: *)
(*Unica funzione del pacchetto invocabile, inizializza il dataset degli attori e stampa un men\[UGrave] con 4 celle di input e 4 bottoni collegati alle 4 funzionalit\[AGrave] descritte nel file Frontend.nb.*)


(* ::Section:: *)
(*Implementazione*)


(* ::Subsection:: *)
(*Definizione dello Usage della funzione showFrontend*)


showFrontend::usage = "showFrontend[] ritorna una struttura grafica utilizzata per interagire con il 
	programma.";


(* ::Subsection:: *)
(*Inizializzazione del Dataset*)


(* ::Text:: *)
(*Nella seguente cella di codice, vengono invocate due funzioni:*)
(*GetDaset : Presente nel pacchetto ItalianActorPreprocessing.wl, inizializza e restituisce un dataset che presenta al suo interno un elenco di attori e di film in lingua originale italiana in cui hanno recitato.*)
(*GenerateGraph: A partire dal dataset restituito dalla funzione precedente, genera un grafo su cui \[EGrave] possibile calcolare la distanza minima tra attori. \[CapitalEGrave] implementata nel pacchetto GeneralOracleBack.wl, in quanto applicabile genericamente a ogni dataset.*)


italianFilms = GetDataset[];
gr = GenerateGraph[italianFilms, "Actor", "OriginalTitle"];


(* ::Subsection:: *)
(*Parametrizzazione delle variabili*)


(* ::Text:: *)
(*Variabili parametrizzate e predefinite per rendere pi\[UGrave] leggibile e mantenibile il codice, ottenute in modo "sperimentale" fino a raggiungere un risultato desiderato.*)


(* Variabili predefinite contenenti la dimensione di varie componenti del men\[UGrave] utente*)
(*Dimensioni dei bottoni delle funzionalit\[AGrave]*)
buttonX = 100;
buttonY = 50;
(*Dimensione celle di dialogo*)
labelX = 300;
labelY = 90;
(*Dimensione del pannello*)
panelX = 900;
panelY = 200;
(*Dimensioni della finestra di output, e inizializzazione a Null di essa*)
owX = 550;
owY = 1100;
outputWindow = Null;


(* ::Subsection:: *)
(*Implementazione funzione showFrontend*)


showFrontend[] := 
	DynamicModule[ (*Viene utilizzato un DynamicModule che fa in modo di avere un men\[UGrave] interattivo dinamicamente*)
	{inputActor1, inputActor2, answer, seed, cfOutput, sp, re}, 
		Panel[
	 Grid[{
	   {
	     InputField[Dynamic[inputActor1], String, FieldHint -> "Inserisci il primo attore..."], (*Cella di input del primo attore*)
	     InputField[Dynamic[inputActor2], String, FieldHint -> "Inserisci il secondo attore..."], (*Cella di input del secondo attore*)
	     InputField[Dynamic[answer], Number, FieldHint -> "Indovina la distanza..."], (*Cella di input della risposta della distanza*)
	     InputField[Dynamic[seed], Number, FieldHint -> "Scegli un seed..."] (*Cella di input del seed randomico*)
	   },
	   
	   {
	   (* Bottone che calcola la distanza tra due attori forniti in input, e il grafico di collegamento tra i due*)
	     Button[Style["Calcola", Medium], (
	     inputActor1 = InputCorrection[inputActor1]; (* Correzione nomi attori *)
	     inputActor2 = InputCorrection[inputActor2];
	     cfOutput = CheckForm[gr, inputActor1, inputActor2]; (*Controllo corretteza dei nomi inseriti*)
		 (*Se presente un errore lo segnala in una finestra. In caso contrario, calcola lo shortest path tra i due attori forniti in input*)
		 If[StringQ[cfOutput], 
	        (CreateDialog[{TextCell[cfOutput], DefaultButton[]}, WindowSize -> {labelX, labelY}]), 
	        sp = CalcShortestPath[gr, inputActor1, inputActor2];

            
            (* Chiude la finestra di output precedente, se presente *)
            NotebookClose[outputWindow];

            (* Crea una nuova box di dialogo e stampa distanza e grafo *)
	        outputWindow = CreateDocument[
            {
                TextCell["Distance between actors is " <> ToString[Length[sp[["entityPath"]]] - 1], Magnification->2], (*Stampa della distanza*)
                displaySolution[sp] (*Stampa del grafico*)
            },
            WindowSize -> {owX, owY}
                ]
	     ]), ImageSize -> {buttonX, buttonY}],
	     
	     (*Bottone che elimina eventuali valori presenti nei box di input*)
	     Button[Style["Reset", Medium], ( 
	       (* Resetto i valori eventualmente contenuti all'interno dei tre box di cfOutput *)
	       inputActor1 = inputActor2 = Null;
	       answer = Null;
	       seed = Null;
	     ), ImageSize -> {buttonX, buttonY}],
	     
	     (*Bottone che verifica che la distanza inserita dall'utente, se presente, sia la stessa tra i due attori*)
	     Button[Style["Indovina", Medium],(
	       Which[
	       
	          (* Controllo che l'utente abbia inserito un valore *)
	          Not[NumberQ[answer]],
	           (CreateDialog[{TextCell["\[CapitalEGrave] necessario inserire un numero per provare ad indovinare."], 
	               DefaultButton[]}, WindowSize -> {labelX, labelY}];),
	       
	         (* Controllo che l'utente non abbia inserito valori < 0, eccetto -1 *)
	         answer < 0 && answer != -1,
	           (CreateDialog[{TextCell["Non \[EGrave] possibile inserire valori negativi, eccezion fatta per -1."], 
	               DefaultButton[]}, WindowSize -> {labelX, labelY}];), 

	         (* Campo default, in caso l'input sia corretto *)
	         True, (
	           inputActor1 = InputCorrection[inputActor1]; (* Correzione nomi attori *)
	           inputActor2 = InputCorrection[inputActor2];
	           cfOutput = CheckForm[gr, inputActor1, inputActor2]; (*Controllo analogo al bottone Calcola*)
		       If[StringQ[cfOutput], 
	              (CreateDialog[{TextCell[cfOutput], DefaultButton[]}, WindowSize -> {labelX, labelY}]), 
	              sp = CalcShortestPath[gr, inputActor1, inputActor2];
	           ];
	           (*Comunica all'utente in una finestra di dialogo se l'utente ha indovinato*)
	           If[answer == Length[sp[["entityPath"]]]-1,
	                (CreateDialog[{TextCell["Complimenti, hai indovinato!"], DefaultButton[]}, 
	                    WindowSize -> {labelX, labelY}];),
	                (CreateDialog[{TextCell["Peccato, risposta errata!"], DefaultButton[]}, 
	                    WindowSize -> {labelX, labelY}];)
	          ]
	         )
	       ]; 
	     ), ImageSize->{buttonX, buttonY}], 
	     
	     (* Estrae due nomi casuali dal dataset *)
         Button[Style["Casuale", Medium], (
             Which[
             Not[NumberQ[seed]],
                 (CreateDialog[{TextCell["\[CapitalEGrave] necessario inserire un seed per generare risultati casuali."], 
                     DefaultButton[]}, WindowSize -> {labelX, labelY}];),
             True,
                 (re = RandomExtract[gr, seed]; inputActor1 = re[[1]]; inputActor2 = re[[2]];) (*Assegna ai box di input i valori dei due attori estratti*)
             ]
         ), ImageSize -> {buttonX, buttonY}] 
	   }
	  },
	  Spacings -> {4, 1}
	  ],
	 ImageSize -> {panelX, panelY}
	]
	
]

