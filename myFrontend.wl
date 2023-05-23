(* ::Package:: *)

(* ::Section:: *)
(*myFrontend*)


(* ::Text:: *)
(*Pacchetto dedicato alla costruzione del men\[UGrave] utilizzabile nel notebook, con annessa implementazione delle funzionalit\[AGrave] dei pulsanti. *)


(* ::Section:: *)
(*Implementazione*)


BeginPackage["myFrontend`"]
showFrontend::usage = "Ritorna una struttura grafica utilizzata per interagire con il software.";


Begin["Private`"]
Needs["GeneralOracleBack`"]; (*Inserisco le dipendenze per richiamare funzioni definite in altri pacchetti come GetDataset[], GenerateGraph[], ...*)
Needs["ItalianActorPreprocessing`"];
Needs["CheckForm`"];


(*
	ShowFrontend si occupa di disegnare il Frontend del software.
	Al suo interno \[EGrave] presente un pannello (Panel), contenente in "modalit\[AGrave] griglia" (Grid) una sequenza di componenti.
	Alla prima riga troviamo quattro InputField, ovvero:
		- Attore1;
		- Attore2;
		- Tentativo per "Indovina", dovr\[AGrave] necessariamente contenere un valore numerico;
		- Seed per la generazione casuale, dovr\[AGrave] necessariamente contenere un valore numerico.
	
		
	Alla seconda riga troviamo invece i pulsanti per chiamare i relativi metodi, tra cui:
		- Calcola, per generare lo ShortestPath tra la coppia di attori;
		- Reset, per ripulire gli eventuali input inseriti dall'utente;
		- Indovina, per provare ad indovinare la distanza tra la coppia di attori;
		- Casuale, per estrarre in modo semi-casuale, partendo da un seed, una coppia di attori.
*)

showFrontend[] := 
	DynamicModule[
	{inputActor1, inputActor2, answer, seed, cfOutput, sp, re, outputMessage, italianFilms,
	gr, buttonX, buttonY, labelX, labelY, panelX, panelY, owX, owY, outputWindow}, 
	italianFilms = GetDataset[]; (* Utilizziamo il metodo GetDataset, definito in GeneralOracleBack, per estrarre e salvare in una variabile il dataset ottenuto a partire dall'unione dei due file .csv *)
    gr = GenerateGraph[italianFilms, "Actor", "OriginalTitle"];
    (* Variabili hardcoded utilizzate per definire la dimensione dei vari componenti del frontend. *)
    (* 
	Utilizziamo il metodo GenerateGraph, definito in GeneralOracleBack, per creare il grafo a partire dal dataset appena calcolato.
	"Actor" rappresenta la colonna del dataset su cui verranno definiti i nodi del grafo.
	"OriginalTitle" rappresenta la colonna del dataset attraverso cui verranno congiunti i nodi, ovvero gli archi del grafo.
     *)
    gr = GenerateGraph[italianFilms, "Actor", "OriginalTitle"];
    (* Dimensione dei pulsanti *)
    buttonX = 100;
    buttonY = 50;

    (* Dimensioni delle label *)
    labelX = 300;
    labelY = 90;

    (* Dimensioni dei pannelli *)
    panelX = 900;
    panelY = 200;

    (*Dimensioni finestra di output per "Calcola"*)
    owX = 550;
    owY = 1100;

    (*Variabile che verifica o meno la presenza della finestra di output*)
    outputWindow = Null;
    
    
		Panel[
	 Grid[{
	   {
	     (* Prima riga del Grid, contenente i quattro InputField in cui l'utente pu\[OGrave] inserire i relativi valori. *)
	     InputField[Dynamic[inputActor1], String, FieldHint -> "Inserisci il primo attore..."], (* Primo attore *)
	     InputField[Dynamic[inputActor2], String, FieldHint -> "Inserisci il secondo attore..."], (* Secondo attore *)
	     InputField[Dynamic[answer], Number, FieldHint -> "Indovina la distanza..."], (* Tentativo di "Indovina" *)
	     InputField[Dynamic[seed], Number, FieldHint -> "Scegli un seed..."] (* Seed per la generazione casuale *)
	   },
	   
	   (* Seconda riga del Grid *)
	   {
	   (* Pulsante che calcola la distanza tra due attori forniti in input. *)
	     Button[Style["Calcola", Medium], (
	     
	     (* Controlla che l'utente abbia inserito valori in entrambe le InputField. *)
	     If[StringQ[inputActor1] == False || StringQ[inputActor2] == False,
	       CreateDialog[{TextCell["\[CapitalEGrave] necessario inserire entrambi gli attori."]}],
	       (
	         (* Correzione nomi attori attraverso il metodo InputCorrection, definito in GeneralOracleBack. *)
	         inputActor1 = InputCorrection[inputActor1];
	         inputActor2 = InputCorrection[inputActor2];
	     
	         (* Vari controlli sui valori immessi dall'utente, attraverso il metodo CheckForm definito in GeneralOracleBack. *)
	         cfOutput = CheckForm[gr, inputActor1, inputActor2];
	     
			 (*
			    Viene controllato il valore contenuto in "cfOutput". 
			    Se esso \[EGrave] una stringa, sar\[AGrave] uno dei vari messaggi di errore, da inserire quindi in un Dialog.
			    Altrimenti, il valore contenuto sar\[AGrave] "True", ottenuto dalla condizione di default, e verr\[AGrave] quindi calcolato lo shortest path.
			 *)	     
		     If[StringQ[cfOutput], 
	           (CreateDialog[{TextCell[cfOutput], DefaultButton[]}, WindowSize -> {labelX, labelY}]),
	           sp = CalcShortestPath[gr, inputActor1, inputActor2]; (* Utilizzo del metodo CalcShortestPath, contenuto in GeneralOracleBack. *)

            
	           (* Chiude la finestra di output precedente, se presente *)
	           NotebookClose[outputWindow];


			   (*
			       Controlla se la coppia di attori presenta un collegamento, altrimenti stampa un messaggio all'utente.
			       N.B. -1 indica che gli attori non presentano un collegamento tra loro. 
			   *)
			   If[Length[sp[["entityPath"]]] - 1 == -1,
				outputMessage = "Gli attori non hanno nessun collegamento.",
				outputMessage = "Il grado di separazione tra gli attori \[EGrave] " <> ToString[Length[sp[["entityPath"]]] - 1]
			   ];
			
			
               (*
                   Crea una nuova finestra, essa pu\[OGrave] contenere:
                       - Distanza e relativo grafo, se nell'If precedente il percorso \[EGrave] stato trovato.
                       - Messaggio di errore (inserito sopra), altrimenti.
               *)
	           outputWindow = CreateDocument[
               {
                 TextCell[outputMessage, Magnification->2],
                 displaySolution[sp]
               }, WindowSize -> {owX, owY}]])
	     ]), ImageSize -> {buttonX, buttonY}],
	     
	     
	     (* Pulsante di reset, per ripulire gli InputField dagli eventuali valori inseriti. *)
	     Button[Style["Reset", Medium], (
	     
	       (* Resetto i valori eventualmente contenuti all'interno dei quattro InputField. *)
	       inputActor1 = inputActor2 = Null;
	       answer = Null;
	       seed = Null;
	     ), ImageSize -> {buttonX, buttonY}],
	     
	     
	     (* Pulsante per fornire all'utente la possibilit\[AGrave] di indovinare il risultato della distanza. *)
	     Button[Style["Indovina", Medium],(
	       
	       (* Vengono effettuati vari controlli sull'input inserito dall'utente. *)
	       Which[
	       
	          (* Controllo che l'utente abbia inserito un valore. *)
	          Not[NumberQ[answer]],
	           (CreateDialog[{TextCell["\[CapitalEGrave] necessario inserire un numero per provare ad indovinare."], 
	            DefaultButton[]}, WindowSize -> {labelX, labelY}];),

	       	       
	         (* Controllo che l'utente non abbia inserito valori negativi, eccezion fatta per -1, ad indicare un percorso non trovato. *)
	         answer < 0 && answer != -1,
	           (CreateDialog[{TextCell["Non \[EGrave] possibile inserire valori negativi, eccezion fatta per -1."], 
	            DefaultButton[]}, WindowSize -> {labelX, labelY}];), 


	         (* Campo default, in caso l'input sia corretto *)
	         True, (
	           (* Correzione nomi attori *)
	           inputActor1 = InputCorrection[inputActor1]; 
	           inputActor2 = InputCorrection[inputActor2];
	         
	           cfOutput = CheckForm[gr, inputActor1, inputActor2];
		       
		       If[StringQ[cfOutput], 
	              (CreateDialog[{TextCell[cfOutput], DefaultButton[]}, WindowSize -> {labelX, labelY}]), 
	              sp = CalcShortestPath[gr, inputActor1, inputActor2];
	           ];
	           
	           
	           (*
	               answer \[EGrave] il Dynamic collegato all'InputField di "Indovina".
	               Se il tentativo inserito dall'utente \[EGrave] uguale alla risposta corretta, viene stampato un messaggio di "successo".
	               Altrimenti, verr\[AGrave] invitato l'utente a fare un altro tentativo.
	           *)
	           If[answer == Length[sp[["entityPath"]]]-1,
	                (CreateDialog[{TextCell["Complimenti, hai indovinato!"], DefaultButton[]}, 
	                WindowSize -> {labelX, labelY}];),
	                
	                (CreateDialog[{TextCell["Peccato, risposta errata!"], DefaultButton[]}, 
	                WindowSize -> {labelX, labelY}];)
	          ]
	         )
	       ]; 
	     ), ImageSize->{buttonX, buttonY}], 
	     
	     (* Attraverso il seguente pulsante, \[EGrave] possibile estrarre due attori casuali dal dataset, i cui "Nome Cognome" verranno inseriti nei relativi InputField. *)
         Button[Style["Casuale", Medium], (
             
             (*
                 Viene controllato che l'InputField non sia vuoto.
                 Se vuoto, viene stampato un messaggio di errore.
                 Altrimenti, viene utilizzato il metodo RandomExtract, definito in GeneralOracleBack, per estrarre una coppia di attori dal dataset.
             *)
             If[NumberQ[seed] == False,
                 (CreateDialog[{TextCell["\[CapitalEGrave] necessario inserire un seed per generare risultati casuali."], 
                  DefaultButton[]}, WindowSize -> {labelX, labelY}];),
                 
                 (*
                     RandomExtract prende come parametri due argomenti:
                         - gr: il grafo da cui estrarre la coppia di attori.
                         - seed: Il seed a partire da cui vengono scelti gli attori.
                 *)
                 (re = RandomExtract[gr, seed]; inputActor1 = re[[1]]; inputActor2 = re[[2]];)
             ]
             
         ), ImageSize -> {buttonX, buttonY}] 
	   }
	  },Spacings -> {4, 1}
	
	],ImageSize -> {panelX, panelY}
  ]
]

End[]
EndPackage[]
