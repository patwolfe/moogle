open Util ;;    
open CrawlerServices ;;
open Order ;;
open Pagerank ;;
open Myset ;;

module MoogleRanker
  = InDegreeRanker (PageGraph) (PageScore)
  (*
     = RandomWalkRanker (PageGraph) (PageScore) (struct 
       let do_random_jumps = Some 0.20
       let num_steps = 1000
     end)
  *)

(* Dictionaries mapping words (strings) to sets of crawler links *)
module WordDict = Dict.Make(
  struct 
    type key = string
    type value = LinkSet.set
    let compare = string_compare
    let string_of_key = (fun s -> s)
    let string_of_value = LinkSet.string_of_set

    (* These functions are for testing purposes *)
    let gen_key () = ""
    let gen_key_gt x () = gen_key ()
    let gen_key_lt x () = gen_key ()
    let gen_key_random () = gen_key ()
    let gen_key_between x y () = None
    let gen_value () = LinkSet.empty
    let gen_pair () = (gen_key(),gen_value())
  end)

(* A query module that uses LinkSet and WordDict *)
module Q = Query.Query(
  struct
    module S = LinkSet
    module D = WordDict
  end)

let print s = 
  let _ = Printf.printf "%s\n" s in
  flush_all();;

let get o = match o with 
              Some v -> v
            | _ -> raise Not_found

(***********************************************************************)
(*    PART 1: CRAWLER                                                  *)
(***********************************************************************)

(* TODO: Build an index as follows:
 * 
 * Remove a link from the frontier (the set of links that have yet to
 * be visited), visit this link, add its outgoing links to the
 * frontier, and update the index so that all words on this page are
 * mapped to linksets containing this url.
 *
 * Keep crawling until we've
 * reached the maximum number of links (n) or the frontier is empty. *)
let rec crawl (n : int) (frontier : LinkSet.set)
    (visited : LinkSet.set) (d : WordDict.dict) : WordDict.dict =
      let _ = print "Inside crawl" in
      match n with 
        0 -> d
      | _ -> if (LinkSet.is_empty frontier) 
             then d
             else
                 let _ = print ("Current frontier: " ^ (LinkSet.string_of_set frontier)) in 
                 let (link, frontier') = get (LinkSet.choose frontier)  in
                 let { words; _ } = get (get_page link) in
                 let lookup_in_lset = (fun d word -> 
                                match WordDict.lookup d word with
                                  None -> LinkSet.empty
                                | Some lset -> lset)    
                 in
                 let add_link = (fun word d -> 
                           WordDict.insert d word (LinkSet.insert link (lookup_in_lset d word)))    
                 in
                 let d' = List.fold_right add_link words d in
                 let is_new_link = (fun link -> 
                                      not (LinkSet.member frontier' link) &&
                                      not (LinkSet.member visited link)) in
                 let {links = page_links; _ } = get (get_page link) in
                 let new_links = List.filter is_new_link page_links in
                 let new_links_set = List.fold_right LinkSet.insert new_links LinkSet.empty in 
                 crawl (n - 1) (LinkSet.union new_links_set frontier')
                    (LinkSet.insert link visited) d'
;;

let crawler () = 
  crawl num_pages_to_search (LinkSet.singleton initial_link) LinkSet.empty
    WordDict.empty
;;

(* Debugging note: if you set debug=true in moogle.ml, it will print out your
 * index after crawling. *)
