  
let maze_3_4 = ['#';'#';'#';'#';
	        '#';'A';'B';'#';
	        '#';'#';'#';'#']
  
let rec get_kth_element_from_list list = (fun k -> match k, list with
  | 1, h :: t -> h
  | _, h :: t-> get_kth_element_from_list t (k-1))

let k_3_4 = get_kth_element_from_list maze_3_4
					  
let current_3_by_4_view_letters = ref ['A';'#';'#';'#';'B'] (* positions [6; 2; 10; 5; 7] in 3_by_4 maze *)

let current_3_by_4_view_positions = ref [6;2;10;5;7]
					  									  
let update_3_by_4_view_positions c move = match move with
  | 'U' -> c := [((get_kth_element_from_list !current_3_by_4_view_positions 1)-4);
					     ((get_kth_element_from_list !current_3_by_4_view_positions 2)-4);
					     ((get_kth_element_from_list !current_3_by_4_view_positions 3)-4);
					     ((get_kth_element_from_list !current_3_by_4_view_positions 4)-4);
					     ((get_kth_element_from_list !current_3_by_4_view_positions 5)-4)]
					      
  | 'D' -> c := [((get_kth_element_from_list !current_3_by_4_view_positions 1)+4);
					     ((get_kth_element_from_list !current_3_by_4_view_positions 2)+4);
					     ((get_kth_element_from_list !current_3_by_4_view_positions 3)+4);
					     ((get_kth_element_from_list !current_3_by_4_view_positions 4)+4);
					     ((get_kth_element_from_list !current_3_by_4_view_positions 5)+4)]
  
  | 'L' -> c := [((get_kth_element_from_list !current_3_by_4_view_positions 1)-1);
					     ((get_kth_element_from_list !current_3_by_4_view_positions 2)-1);
					     ((get_kth_element_from_list !current_3_by_4_view_positions 3)-1);
					     ((get_kth_element_from_list !current_3_by_4_view_positions 4)-1);
					     ((get_kth_element_from_list !current_3_by_4_view_positions 5)-1)]
 
  | 'R' -> c := [((get_kth_element_from_list !current_3_by_4_view_positions 1)+1);
					     ((get_kth_element_from_list !current_3_by_4_view_positions 2)+1);
					     ((get_kth_element_from_list !current_3_by_4_view_positions 3)+1);
					     ((get_kth_element_from_list !current_3_by_4_view_positions 4)+1);
					     ((get_kth_element_from_list !current_3_by_4_view_positions 5)+1)]

let update_3_by_4_view_letters  = (fun c ->
    c := [k_3_4 (get_kth_element_from_list !current_3_by_4_view_positions 1);
				           			    k_3_4 (get_kth_element_from_list !current_3_by_4_view_positions 2);
					         		    k_3_4 (get_kth_element_from_list !current_3_by_4_view_positions 3);
						        	    k_3_4 (get_kth_element_from_list !current_3_by_4_view_positions 4);
							            k_3_4 (get_kth_element_from_list !current_3_by_4_view_positions 5);
							           ])
let rec list_of_chars_to_string clist =
  match clist with
  | [] -> ""
  | h :: t -> (String.make 1 (get_kth_element_from_list clist 1)) ^ (list_of_chars_to_string t);;
								      
let update_view = (fun m -> (update_3_by_4_view_positions current_3_by_4_view_positions m);
			     update_3_by_4_view_letters current_3_by_4_view_letters; list_of_chars_to_string !current_3_by_4_view_letters)

let starting_view = "A###B"
let current_view  = list_of_chars_to_string !current_3_by_4_view_letters;;

let rec sort lst =
  match lst with
    [] -> []
  | head :: tail -> insert head (sort tail)
and insert elt lst =
  match lst with
    [] -> [elt]
  | head :: tail -> if elt <= head then elt :: lst else head :: insert elt tail

let end_game () = "K" ^ (list_of_chars_to_string (sort(!letters_caught)))


			  
(* to reset *)		   
(current_pos := (0,0); pos_visited := [(0,0)];
current_3_by_4_view_positions := [6;2;10;5;7];
current_3_by_4_view_letters := ['A';'#';'#';'#';'B'];
letters_caught := []);;


let rec lets_play current_view current_pos pos_visited =
  check_if_sitting_on_letter current_view;
  let move = (make_new_move (view_to_tuples current_view) current_pos pos_visited) in
    if move = 'K' then Printf.printf "%s\n%!" (end_game ()) else
         (Printf.printf "%s\n%!" current_view;
         Printf.printf "%c\n%!" move;
	 let cp = (update_current_pos current_pos move) in
	 let pv = (update_pos_visited pos_visited !cp) in
         lets_play (update_view move) cp pv)

let start_game () = lets_play starting_view current_pos pos_visited
			      
  (* to do
        1. implement general solution algorithm
        2. create function to sort and return character trophies
   *)

(* testing slightly harder maze  *)
  

let maze_6_13 = ['#';'#';'#';'#';'#';'#';'#';'#';'#';'#';'#';'#';'#';
		 '#';' ';' ';' ';'A';' ';' ';' ';'#';' ';' ';' ';'#';
		 '#';' ';'#';'#';' ';'#';'#';' ';'#';' ';'#';' ';'#';
		 '#';' ';'Q';'#';' ';'#';' ';' ';' ';'B';' ';' ';'#';
		 '#';' ';' ';'#';'A';'#';' ';'#';' ';'#';'#';'#';'#';
		 '#';'#';'#';'#';'#';'#';'#';'#';'#';'#';'#';'#';'#';]


let k_6_13 = get_kth_element_from_list maze_6_13
					  
let current_6_by_13_view_letters = ref ['A';'#';' ';' ';' '] (* positions [18; 5; 31; 17; 19;] in 6_by_13 maze *)

let current_6_by_13_view_positions = ref [18;5;31;17;19]
					  									  
let update_6_by_13_view_positions c move = match move with
  | 'U' -> c := [((get_kth_element_from_list !current_6_by_13_view_positions 1)-13);
					     ((get_kth_element_from_list !current_6_by_13_view_positions 2)-13);
					     ((get_kth_element_from_list !current_6_by_13_view_positions 3)-13);
					     ((get_kth_element_from_list !current_6_by_13_view_positions 4)-13);
					     ((get_kth_element_from_list !current_6_by_13_view_positions 5)-13)]
					      
  | 'D' -> c := [((get_kth_element_from_list !current_6_by_13_view_positions 1)+13);
					     ((get_kth_element_from_list !current_6_by_13_view_positions 2)+13);
					     ((get_kth_element_from_list !current_6_by_13_view_positions 3)+13);
					     ((get_kth_element_from_list !current_6_by_13_view_positions 4)+13);
					     ((get_kth_element_from_list !current_6_by_13_view_positions 5)+13)]
  
  | 'L' -> c := [((get_kth_element_from_list !current_6_by_13_view_positions 1)-1);
					     ((get_kth_element_from_list !current_6_by_13_view_positions 2)-1);
					     ((get_kth_element_from_list !current_6_by_13_view_positions 3)-1);
					     ((get_kth_element_from_list !current_6_by_13_view_positions 4)-1);
					     ((get_kth_element_from_list !current_6_by_13_view_positions 5)-1)]
 
  | 'R' -> c := [((get_kth_element_from_list !current_6_by_13_view_positions 1)+1);
					     ((get_kth_element_from_list !current_6_by_13_view_positions 2)+1);
					     ((get_kth_element_from_list !current_6_by_13_view_positions 3)+1);
					     ((get_kth_element_from_list !current_6_by_13_view_positions 4)+1);
					     ((get_kth_element_from_list !current_6_by_13_view_positions 5)+1)]

let update_6_by_13_view_letters  = (fun c ->
    c := [k_6_13 (get_kth_element_from_list !current_6_by_13_view_positions 1);
	  k_6_13 (get_kth_element_from_list !current_6_by_13_view_positions 2);
	  k_6_13 (get_kth_element_from_list !current_6_by_13_view_positions 3);
	  k_6_13 (get_kth_element_from_list !current_6_by_13_view_positions 4);
	  k_6_13 (get_kth_element_from_list !current_6_by_13_view_positions 5);
	 ])

let starting_view_h = "A#   "
let current_view_h  = list_of_chars_to_string !current_6_by_13_view_letters;;

  (* to reset *)
(current_pos := (0,0); pos_visited := [(0,0)];
current_6_by_13_view_positions := [18;5;31;17;19];
current_6_by_13_view_letters := ['A';'#';' ';' ';' '];
letters_caught := []);;

let update_view1 = (fun m -> (update_6_by_13_view_positions current_6_by_13_view_positions m);
			     update_6_by_13_view_letters current_6_by_13_view_letters; list_of_chars_to_string !current_6_by_13_view_letters)

  
let rec lets_play_harder current_view current_pos pos_visited =
  check_if_sitting_on_letter current_view;
  let move = (make_new_move (view_to_tuples current_view) current_pos pos_visited) in
    if move = 'K' then Printf.printf "%s\n%!" (end_game ()) else
         (Printf.printf "%s\n%!" current_view;
         Printf.printf "%c\n%!" move;
	 let cp = (update_current_pos current_pos move) in
	 let pv = (update_pos_visited pos_visited !cp) in
         lets_play_harder (update_view1 move) cp pv)

let start_game1 () = lets_play_harder starting_view_h current_pos pos_visited
