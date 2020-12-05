module FParsec.Test.CETests


open FParsec
open FParsec.Error
open FParsec.Primitives
open FParsec.CharParsers

open FParsec.Test.Test

let monadicParser (): Parser<string * string , unit> =
    parse {
        let letters = many1Chars (satisfy isLetter)
        do! spaces
        let! token1 = letters
        do! spaces
        let! token2 = letters
        return token1, token2
    }

let applicativeParser () =
    parse {
        let letters = many1Chars (satisfy isLetter)
        let! _ = spaces
        and! token1 = letters
        and! _ = spaces
        and! token2 = letters
        return token1 token2
    }

let testMonadicCE () =
    ROk "   hi hello" 0 ("hi", "hello") (monadicParser())

let testApplicativeCE () =
    ROk "   hi hello" 0 "hi" (applicativeParser())


let run() = ()

