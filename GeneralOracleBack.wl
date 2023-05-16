(* ::Package:: *)

BeginPackage["GeneralOracleBack`"]

GenerateGraph::usage = "GenerateGraph[dataset, entityName, groupingAttribute] returns a graph from dataset whose 
		vertices are the elements of the entityName column and the edges are built according to groupingAttribute
	";

CalcShortestPath::usage = "CalcShortestPath[graph, firstEntity, secondEntity] returns an association list with keys entityPath and groupsPath,
			containing, respectively, the vertices and the edges of the shortest path between firstEntity and secondEntity
	";

RandomExtract::usage = "RandomExtract[graph, seed] ritorna una lista di due entit\[AGrave] estratte casualmente usando un seed.
    ";
displaySolution::usage = "displaySolution[output] graphically shows the list returned by CalcShortestPath";

Begin["`Private`"]
GenerateGraph[dataset_, entityName_, groupingAttribute_] :=
    Module[
        {entities, groups, edges, buildEdge}
        ,
        (* list of unique entities in the dataset, which are in the column
             entityName *)
        entities =
            dataset[All, entityName] //
            Normal //
            DeleteDuplicates;
        (* association list containing all entities related to a certain
             group, in the form <|group -> {entity_1, ..., entity_n}|> *)
        groups = Flatten /@ Normal @ dataset[GroupBy[groupingAttribute
            ], List, entityName];
        (* function that creates an edge between each entity in a group
            , assigning the group name as an edge tag *)
        buildEdge[groupName_, group_] :=
            Module[{subsets},
                subsets = Normal @ Subsets[group, {2}];
                UndirectedEdge[#[[1]], #[[2]], groupName]& /@ subsets
            ];
        (* mapping the function above onto the entire list of groups 
            *)
        edges =
            (* KeyValueMap used instead of Map since groups is an association
                 list *)
            KeyValueMap[buildEdge, groups] //
            Flatten //
            DeleteDuplicates;
        (* creation of the graph *)
        Graph[edges, VertexLabels -> "Name", EdgeLabels -> "EdgeTag"]
    ];

CalcShortestPath[graph_, firstEntity_, secondEntity_] :=
    Module[
        {entityPath, groupsPath}
        ,
        (* getting the vertices in the shortest path *)
        entityPath = FindShortestPath[graph, firstEntity, secondEntity
            ];
        (* entityPath is first partitioned in lists of 2 elements with
             offset 1, then tag is extracted for each couple *)
        groupsPath = EdgeTags[graph, #]& /@ Partition[entityPath, 2, 
            1];
        <|"entityPath" -> entityPath, "groupsPath" -> groupsPath|>
    ];
    


RandomExtract[graph_, seed_] :=
    Module[
        {entities, randomPicks}
        ,
        (* list of entities in the graph *)
        entities = VertexList[graph];
        (* set the seed for all the Random methods *)
        SeedRandom[seed];
        (* extract two entities *)
        randomPicks = RandomChoice[entities, 2]
    ];


displaySolution[output_]:=
Module[
	{dist, list, graphPlot, imageSizeX, imageSizeY},
	(*esempio 
	output = \[LeftAssociation]"entityPath"\[Rule]{"Roberto Benigni","Jim Belushi","Quentin Tarantino"},"groupsPath"\[Rule]{{"Pinocchio"},{"Destiny Turns on the Radio"}}\[RightAssociation]
	list = {"Roberto Benigni",{"Pinocchio"},"Jim Belushi",{"Destiny Turns on the Radio"},"Quentin Tarantino"}
	*)
	(* Hardcoded variables defining the size of various output components *)
	imageSizeX = 150;
	imageSizeY = 1100;
	dist = Length[output[["entityPath"]]]-1;
	list = Riffle[output[["entityPath"]], output[["groupsPath"]]] //Flatten;
	graphPlot = {};
	For[i = 1, i < Length[list], i++, {
		If[OddQ[i],label = "Was in", label = "With"];
   	 graphPlot = Append[graphPlot, {list[[i]]-> list[[i+1]], Style[label, Black]}];
    }];
	
	(*LayeredGraphPlot[ graphPlot,VertexShapeFunction -> (Text[Framed[Style[#2, 8, Black], Background -> White], #1] &),
	        EdgeLabels->({If[#3 =!= None, {Line[#], Inset[#3, Mean[#1], Automatic, Automatic, #[[1]] - #[[2]], Background -> White]}, Line[#]]} &), ImageSize->{150,1100}] *)
	
	LayeredGraphPlot[graphPlot,
         VertexShapeFunction -> (Text[Framed[Style[#2, 8, Black], Background -> White], #1] &),
         EdgeLabels -> ({If[#3 =!= None, {Line[#], Inset[#3, Mean[#1], Automatic, Automatic, #[[1]] - #[[2]], Background -> White]}, Line[#]]} &),
         EdgeShapeFunction -> None, (* Rimozione Frecce per evitare il baco delle frecce giganti*)
         ImageSize -> {imageSizeX, imageSizeY}] 

];


End[]

EndPackage[]
