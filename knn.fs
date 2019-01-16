//Tommy Zumstein
//F# Assignment 2
//

module knn =
    ///Returns a query function given a result list length, distance function, and dataset
    let knn k func data = 
        ///Inserts the closest value to th query with the given "distance" function 
        let insertTopK queryVal insertVal resultList =
            let listLen = List.length resultList
            ///Compares an insert value against the query value using the "distance" function and inserts it
            ///into the result list if it is closer than any other value currently in the result list
            let rec insertHelper length cTuple inVal curList =
                let (inTuple,_) = inVal
                match curList, length with
                | head::tail, _ -> 
                    let (hTuple,_) = head
                    if func cTuple inTuple <= func cTuple hTuple then
                        inVal::insertHelper (length-1) cTuple head tail
                    else 
                        head::insertHelper (length-1) cTuple inVal tail
                | _, 0 -> []
            insertHelper listLen queryVal insertVal resultList

        ///Constructs the result list using insertTopK
        let rec KNNHelper resultList dataSet qVal i= 
            match dataSet with 
            | head::tail -> 
                if i < k then
                    KNNHelper (head::resultList) tail qVal (i+1)
                else 
                    KNNHelper (insertTopK qVal head resultList) tail qVal i
            | [] -> resultList

        ///Return function that takes a tuple query and returns a result list
        let returnFunc query = 
            KNNHelper [] data query 0
        returnFunc
    
    ///Returns the average cost from a result list
    let avgInstances instances =
        let instancesLength = List.length instances
        ///Returns the sum of all the cost values in a data tuple list
        let rec avgInstancesHelper list =
            match list with
            | ((features),cost)::tail -> 
                cost + avgInstancesHelper tail
            | [] -> 0.0
        (avgInstancesHelper instances) / float(instancesLength)