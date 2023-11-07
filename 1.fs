(*  1. Implement the following as a recursive F# function nf : int -> int.
  nf (x) =
        |nf (2 − x) if x < 0
        |1 if x = 0
        |x ∗ nf (x − 1) + x if x > 0    *)

      let rec nf x =
          if x < 0 then nf(2-x)
          elif x = 0 then 1
          else x * nf(x-1) + x;;

    
(*  2.Consider the recursive functions:
        threes : int -> int
        notThrees : int -> int
Implement a function threesAndNotThrees : int -> int * int that does the same as {fun x -> (threes x, notThrees x)}
but using only one recursion instead of two. You should do this by direct recursion, without calling threes or notThrees    *)

      // threes : int -> int
      let rec threes x =
          if x = 0 then 0
          elif x % 3 = 0 then 1 + threes (x / 3)
          else threes (x - 1)
      
      // notThrees : int -> int
      let rec notThrees x =
          if x = 0 then 0
          elif x % 3 = 0 then notThrees (x / 3)
          else 1 + notThrees (x - 1)
  
      // My attempt for threesAndNotThrees : int -> int * int. It does not work. See implementation for a proper solution below
        let rec threesAndNotThrees x = 
            (threes x , notThrees x)
    
      // Proper solution provided by Tarmo Uustalu (Reykjavík University)
      let rec threesAndNotThrees x =
          if x = 0 then (0, 0)
          elif x % 3 = 0 then
              let (t, nt) = threesAndNotThrees (x / 3)
              (1 + t, nt)
          else
              let (t, nt) = threesAndNotThrees (x - 1)
              (t, 1 + nt)

              
(*  3. Implement a function sumTo10 : int list -> int * int list that sums the first elements of
the list until the sum reaches at least 10, and then returns this sum together with unused part of
the list. If no proper prefix has a sum of at least 10, then your function should return the sum of
the entire list, together with the empty list. You may wish to do this with a helper function using
an accumulator for the sum of the elements seen so far.    *)

      // sumTo10 : int list -> int * int list
      
      let rec sumTo10_helper xs lis =
          // return first parameter plus the list
          if xs >= 10 || lis = [] then (xs, lis)
          // acc. Tail is every element in the list that is not the head (= [0:], slicing)
          else sumTo10_helper (xs + lis.Head) lis.Tail
      
      let sumTo10 lis =
          // return the tuple
          if lis = [] then (0, lis)
          else sumTo10_helper lis.Head lis.Tail

(*  4. The given functions below {collectUntilFirstNegative : int list -> int list} and {collectUntilFirstEmpty : ’a list list -> ’a list list} 
return a given list, but truncated at (i.e., just before) the first negative number or empty list (if the given list contains a negative number or empty list).
(4.1) Without using functions from the List module, implement a similar function {collectUntil :(’a -> bool) -> ’a list -> ’a list} 
that truncates at the first element that satisfies the given predicate. It should be possible to implement collectUntilFirstNegative and
collectUntilFirstEmpty using collectUntil by writing:
      let collectUntilFirstNegative xs = collectUntil (fun x -> x < 0) xs
      let collectUntilFirstEmpty xss = collectUntil List.isEmpty xss.   *)

      // collectUntilFirstNegative : int list -> int list
      let rec collectUntilFirstNegative xs =
          match xs with
          | [] -> []
          | x::xs -> if x < 0 then [] else x :: collectUntilFirstNegative xs
      
      // collectUntilFirstEmpty : 'a list list -> 'a list list (list of lists)
      let rec collectUntilFirstEmpty xss = 
          match xss with
          | [] -> []
          | xs::xss -> if List.isEmpty(xs) then [] else xs :: collectUntilFirstEmpty xss


      
      let rec collectUntil p xs =
          match xs with
          | [] -> []
          | x::xr -> if p x then [] else x :: collectUntil p xr
      
      let collectUntilFirstNegative2 xs = collectUntil (fun x -> x < 0) xs

          
(*  (4.2) Using collectUntil, implement a function {collectUntilNot : (’a -> bool) -> ’a list -> ’a list} which truncates at the 
first element that does not satisfy the predicate.     *)

      // collectUntilNot : ('a -> bool) -> 'a list -> 'a list
      let collectUntilNot p xs =
          match xs with
          | [] -> []
          | x::xr -> collectUntil (fun x -> not (p x)) xs

          

(*  5. Consider the function {multiplyPairs : int list -> int list -> int list}. (5.1) Implement a function multiplyPairs2 : int list -> int list -> int list that does
exactly the same as multiplyPairs, but using appropriate functions from the List module. (Visit  https://fsharp.github.io/fsharp-core-docs/reference/fsharp-collections-listmodule.html)
Your function should not (directly) use recursion    *)

      // multiplyPairs : int list -> int list -> int list
      let rec multiplyPairs xs ys =
          match xs, ys with
          | [], [] -> []
          | [], _ | _, [] -> failwith "The lists do not have the same length"
          | x::xs, y::ys -> (x * y) :: multiplyPairs xs ys

          
      
      // multiplyPairs2 : int list -> int list -> int list
      let multiplyPairs2 (xs:int list) (ys:int list) =
          if List.length xs <> List.length ys then raise (System.ArgumentException"The lists do not have the same length") 
          else List.mapi2 (fun i x y -> x * y) xs ys


(* (5.2) If the lengths of xs and ys are different, then multiplyPairs xs ys fails. Implement {multiplyPairsPad : int list -> int list -> int list}, 
which should behave like multiplyPairs if the shorter list were made as long as the longer one with additional zeroes at the end. The length of 
multiplyPairsPad xs ys should be the common length of the two lists so obtained. You should implement this function directly by recursion, without using
multiplyPairs.  *)

      // multiplyPairsPad : int list -> int list -> int list
      let rec multiplyPairsPad (xs:int list) (ys:int list) =
          match xs , ys with
          | [], [] -> []
          | [], _  ->  0 :: multiplyPairsPad xs ys.Tail
          | _, [] -> 0 :: multiplyPairsPad xs.Tail ys
          | x::xs, y::ys -> (x * y) :: multiplyPairsPad xs ys


(* 6. The provided function {oddJoin : ’a list list -> ’a list} uses functions from the List module (Visit  https://fsharp.github.io/fsharp-core-docs/reference/fsharp-collections-listmodule.html)
(6.1) Can either or both the folds in the implementation be replaced with foldBacks? Explain your answer.   *)

      // concatenating lists of odd length
      let oddJoin xss = 
        let oddLength xs = List.fold (fun b _ -> not b) false xs
        // return a result of all the elements which returned true by the prev function, concatenated
        xss |> List.filter oddLength |> List.fold (@) []

    // Proper solution provided by Tarmo Uustalu (Reykjavík University) below, since mine was not correct
    (* Both of the folds can be replaced. Consider:

            List.fold (<*>) a [x1; x2; ...; xn]
              = (((a <*> x1) <*> x2) <*> ...) <*> xn
        
            List.foldBack (<+>) [x1; x2; ...; xn] b
              = x1 <+> (x2 <+> (... <+> (xn <+> b)))

    so when these are equal, replacing one with the other does not change the result. (List.fold (@) [] xs) is the same as (List.foldBack (@) xs [])
    because ((([] @ x1) @ x2) @ ...) @ xn = x1 @ (x2 @ (... @ (xn @ [])))

    The other fold is a bit more difficult, because we have to change the function. In this case, (List.fold (fun b _ -> not b) false xs) is the
    same as (List.foldBack (fun _ b -> not b) xs false). So oddJoin is the same as oddJoinBack below.  *)
    

(* (6.2) Implement {oddJoin2 : ’a list list -> ’a list} so that it does the same as oddJoin, but directly using recursion, without using any List functions. 
You may wish to implement a recursive helper function for one of the folds     *)

    
      // Recursive oddLength checker function
      let rec oddLength xs b =  // b value is going to be initialised as false (we need to indicate so) when we call this function. [] -> false
          match xs with
          | [] -> b
          | x :: xs -> oddLength xs (not b)
      
      // oddJoin2 : 'a list list -> 'a list
      let rec oddJoin2 (xss:'a list list) =
          match xss with
          | [] -> []
          | xs :: xss -> match (oddLength xs false) with
                         | true -> xs @ (oddJoin2 xss)
                         | false -> oddJoin2 xss


(* 7. Each element of the type ’a listOfLists represents a lists of lists of ’as:
      -InnerNil represents [[]];
      -SepInner(t) separates two inner lists;
      -ConsToInner(x, t) adds x to the beginning of the first inner list (where x : ’a) *)
      type 'a listOfLists =
        | InnerNil
        | SepInner of 'a listOfLists
        | ConsToInner of 'a * 'a listOfLists
        
(*Every element of ’a listOfLists contains at least one inner list, but inner lists may be empty. If there are two or more adjacent ConsToInners, 
then all of their elements appear in the same inner list. Consider the given functions: *)
        //-convert : ’a list list -> ’a listOfLists takes a standard F# list of lists, and converts it to an element of this type (unless there has no inner lists, in which case convert raises an exception);  
        let rec convert xss =
              match xss with
              | [] -> failwith "Only non-empty lists are allowed"
              | [[]] -> InnerNil
              | []::xss -> SepInner (convert xss)
              | (x::xs)::xss -> ConsToInner (x, convert (xs :: xss))
      //-firstInnerList : ’a listOfLists -> ’a list returns the first inner list;
          let rec firstInnerList t =
              match t with
              | InnerNil -> []
              | SepInner _ -> []
              | ConsToInner (x, t) -> x::firstInnerList t
      //-hasSep : ’a listOfLists -> bool checks whether there are at least two inner lists (in other words, whether there is at least one SepInner);
            let rec hasSep t =
              match t with
              | InnerNil -> false
              | SepInner (t) -> true
              | ConsToInner (x, t) -> hasSep t
      //-afterFirstSep : ’a listOfLists -> ’a listOfLists removes the first inner list (unless there is only one, in which case it raises an exception)
            let rec afterFirstSep t =
              match t with
              | InnerNil -> failwith "No separators"
              | SepInner (t) -> t
              | ConsToInner (_, t) -> afterFirstSep t
          
(* (7.1) Implement {totalLength : ’a listOfLists -> int}, which finds the total number of ’a elements. 
(Duplicates should be counted as separate elements, like List.length does.) Do this directly by recursion and pattern-matching, without using any of 
the four functions in the list above, and without using the any of the functions from the List module.  *)

      // totalLength : 'a listOfLists -> int
      let rec totalLength t =
          match t with
          | InnerNil -> 0
          | SepInner (t) -> totalLength t
          | ConsToInner(x, t) -> 1 + totalLength t 

(* (7.2) Implement {mapListOfLists : (’a -> ’b) -> ’a listOfLists -> ’b listOfLists}, which should apply the given function to each element of the list of lists
(without changing the total length or the locations of the separators). Do this directly by recursion and pattern-matching, without using any of the four functions
in the list above, and without using any of the functions from the List module.    *)

      // mapListOfLists : ('a -> 'b) -> 'a listOfLists -> 'b listOfLists
      let rec mapListOfLists f t =
          match t with
          | InnerNil -> InnerNil
          | SepInner (t) -> SepInner t
          | ConsToInner(x, t) -> ConsToInner((f x), (mapListOfLists f t))


(* (7.3) Implement unconvert : ’a listOfLists -> ’a list list. This should do the opposite of convert, so that convert (unconvert t) is equal to t. 
You can do this without (directly) pattern-matching, by using firstInnerList, hasSep, and afterFirstSep.   *)

    // unconvert : 'a listOfLists -> 'a list list
    let rec unconvert t =
        if hasSep t then firstInnerList t :: unconvert (afterFirstSep t) else firstInnerList t :: []


