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


End[]

EndPackage[]
