open Stdlib                                                                                                                   
                                                                                                                                   
(**function that for each word in the text increments its counter*)                                                                
                                                      
open Str  (* Import the Str module for regular expressions *)
                                                      
(* Function to split a string into words *)           
let split_into_words text =                           
  let lowercase_text = String.lowercase_ascii text in 
  let word_list = Str.split (Str.regexp "[ \t\n.,;:!?]+") lowercase_text in
  List.filter (fun word -> word <> "") word_list  (* Remove empty strings *)
;;
                                     
(* Function to count word frequencies *)              
let count_word_frequencies filename =                 
  let channel = open_in filename in                   
  let rec process_lines word_counts =
    try                              
      let line = input_line channel in
      let words = split_into_words line in
      let updated_counts =           
        List.fold_left               
          (fun counts word ->        
            let count =              
              try                    
                List.assoc word counts + 1
              with Not_found -> 1    
            in                       
            (word, count) :: counts) 
          word_counts                
          words                      
      in                             
      process_lines updated_counts                                 
    with End_of_file ->                                            
      close_in channel;                                            
      word_counts                                                  
  in                                                               
  let word_frequencies = process_lines [] in                       
  word_frequencies                                                 
;;                                                                  
(* Example usage *)                                                
let word_frequencies = count_word_frequencies "your_text_file.txt" 
in                                                                 
List.iter (fun (word, count) -> Printf.printf "%s: %d\n" word count) word_frequencies
                                                                   