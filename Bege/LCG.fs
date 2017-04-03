module LCG

type [<Struct>] LCG<'a> = { value : 'a; a : 'a; c : 'a }
let inline next lcg = { lcg with value = lcg.value * lcg.a + lcg.c }
let inline bits n (lcg : LCG<'a>) = lcg.value >>> (sizeof<'a> - n)

let createKnuth seed = { value = seed; a = 6364136223846793005UL; c = 1442695040888963407UL }