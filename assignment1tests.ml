(* Add your own tests. Make sure to pay attention to edge cases. *)
let t1a = fixLastTwo (5, 1, 2) = (5, 1, 2)
let t1b = fixLastTwo (5, 2, 1) = (5, 1, 2)
let t1c = fixLastTwo (6,3,3) = (6,3,3)

let t2a = order (2, 5, 3) = (2, 3, 5)
let t2b = order (5, 3, 2) = (2, 3, 5)
let t2c = order (1,2,2) = (1,2,2)
let t2d = order (4,3,2) = (2,3,4)

let t3a = distance (6, 3) = 3
let t3b = distance (3, 6) = 3
let t3c = distance (-3, 6) = 9

let t4a = greeting (23, "Pete") = "Greetings Pete, you are 23 years old!"
let t4b = greeting (23, "Peter Jackson") = "Greetings Peter Jackson, you are 23 years old!"

let t5a = greeting2 (0, "Jackson") = "Greetings Jackson, you are not born yet!"
let t5b = greeting2 (1, "Jackson") = "Greetings Jackson, you are a youngster!"
let t5c = greeting2 (20, "Jackson") = "Greetings Jackson, you are a youngster!"
let t5d = greeting2 (21, "Jackson") = "Greetings Jackson, you are young at heart!"

let t6a = tooShort (4, "tree") = false
let t6b = tooShort (5, "tree") = true
let t6c = tooShort (0, "") = false

let t7a = totalLength ("you", "me") = 5
let t7b = totalLength ("", "me") = 2
let t7c = totalLength ("", "") = 0
let t7d = totalLength ("you are", "me") = 9

let t8a = orderedByLength ("long", "one", "at") = false
let t8b = orderedByLength ("one", "one", "at") = false
let t8c = orderedByLength ("at", "one", "at") = false
let t8d = orderedByLength ("at", "one", "long") = true

let t9a = prodInRange (3, 5) == true
let t9b = prodInRange (4, 5) == false
let t9c = prodInRange (2, 5) == false
let t9d = prodInRange (5, 10) == false
let t9e = prodInRange (3, 6) == true
