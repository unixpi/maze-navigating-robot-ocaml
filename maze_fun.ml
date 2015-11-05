(*  
read_line  (* reads from standard input *)
print_string  (* writes to standard output *)
print_newline (* writes to standard output *)
 *)

(* naive implementation -- always grabs first letter available (may grab same letter multiple times *)
let rec maze_robot = ()

let letters_caught = ref [] 

let remove_first_letter = (fun s -> String.sub s 1 ((String.length s) - 1))
			    
let rec string_to_list_of_chars = (fun s ->
    match s with
    | "" -> []
    | _ -> (String.get s 0) :: (string_to_list_of_chars (remove_first_letter s)))

let rec there_is_a_letter_we_can_move_to  = (fun s -> match (string_to_list_of_chars (remove_first_letter s)) with
  | [] -> false
  | h :: t -> if (h = '#' || h = ' ') then (there_is_a_letter_we_can_move_to (remove_first_letter s))
	      else
		true)

			       
let check_if_sitting_on_letter = (fun s -> match (String.get s 0) with
					   | '#' -> (letters_caught := !letters_caught)
					   | ' ' -> (letters_caught := !letters_caught)
					   | _ -> (letters_caught := ((String.get s 0) :: !letters_caught)))

			
let move_to_first_available_letter = (fun s ->
  let rec aux s moves_left =  match (string_to_list_of_chars s), moves_left with
    | h1 :: t1 , h2 :: t2 -> if (h1 = '#' || h1 = ' ') then (aux (remove_first_letter s) t2)
			     else 
			        h2 
  in aux (remove_first_letter s) ['U';'D';'L';'R'] )

let move_to_first_available_space = (fun s ->
  let rec aux s moves_left =  match (string_to_list_of_chars s), moves_left with
    | h1 :: t1 , h2 :: t2 -> if (h1 = '#') then (aux (remove_first_letter s) t2)
			     else 
			       h2
   in aux (remove_first_letter s) ['U';'D';'L';'R'] ) 
				   
let make_naive_move = (fun s -> (check_if_sitting_on_letter s); if (there_is_a_letter_we_can_move_to s) then (move_to_first_available_letter s)
				else
				  (move_to_first_available_space s))
    

let sort_char_list_alphabetically = ()	
let concatenate_list = ()
	
