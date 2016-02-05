let t1a = result (Rock, Paper) = SndWin
let t1b = result (Rock, Rock) = Tie
let t1c = result (Rock, Scissors) = FstWin
let t1d = result (Paper, Paper) = Tie
let t1e = result (Paper, Scissors) = SndWin
let t1f = result (Paper, Rock) = FstWin
let t1g = result (Scissors, Paper) = FstWin
let t1h = result (Scissors, Scissors) = Tie
let t1i = result (Scissors, Rock) = SndWin

let t2a = is_tie (Rock, Paper) = false
let t2b = is_tie (Rock, Rock) = true
let t2c = is_tie (Paper, Paper) = true
let t2d = is_tie (Scissors, Scissors) = true
let t2e = is_tie (Scissors, Paper) = false

let t3a = game_from_plays ([Rock; Paper; Rock], [Scissors; Rock; Rock]) =
               [(Rock, Scissors); (Paper, Rock); (Rock, Rock)]
let t3b = game_from_plays ([Rock; Paper], [Scissors; Rock; Rock]) =
               [(Rock, Scissors); (Paper, Rock)]
let t3c = game_from_plays ([Rock; Paper; Rock], [Scissors; Rock]) =
               [(Rock, Scissors); (Paper, Rock)]  
let t3d = game_from_plays ([], []) = []                            

let t4a = valid_game [(Rock, Scissors)] = true
let t4b = valid_game [] = true
let t4c = valid_game [(Rock, Rock);(Rock, Paper)] = true
let t4d = valid_game [(Rock, Rock); (Rock, Paper); (Paper, Scissors)] = false
let t4e = valid_game [(Rock, Rock); (Paper, Paper)] = false

let t5a = play_game [(Rock, Rock); (Scissors, Rock)] = SndWin

let t6a = to_f (F 2.3) = 2.3
let t6b = to_f (F 0.0) = 0.0
let t6c = to_f (C 0.0) = 32.0
let t6d = to_f (C 100.0) = 212.0

let t7a = temp_compare (F 2.3, F 4.5) = -1
let t7a = temp_compare (F 2.3, F 2.3) = 0
let t7a = temp_compare (F 7.3, F 4.5) = 1
let t7a = temp_compare (C 2.3, C 2.3) = 0
let t7a = temp_compare (C 100.0, F 100.0) = 1
let t7a = temp_compare (C 0.0, F 450.5) = -1

let t8a = string_of_temp (C 2.3) = "2.3C"
let t8b = string_of_temp (F 2.3) = "2.3F"
let t8c = string_of_temp (C 0.0) = "0.C"

let t9a = max_temp [F 2.1; C 2.1] = C 2.1

let t10a = max_temp2 [F 2.1; C 2.1] = C 2.1
