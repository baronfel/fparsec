module FParsec.Test.CETests


open FParsec
open FParsec.Error
open FParsec.Primitives
open FParsec.CharParsers

open FParsec.Test.Test

let monadicParser: Parser<_, unit> =
    parse {
        let letters = many1Chars (satisfy isLetter)
        do! spaces
        let! token1 = letters
        do! spaces
        let! token2 = letters
        return token1, token2
    }

let applicativeParser: Parser<_, unit> =
    parse {
        let letters = many1Chars (satisfy isLetter)
        let! _ = spaces
        and! token1 = letters
        and! _ = spaces
        and! token2 = letters
        return (token1, token2)
    }

let testMonadicCE () =
    match runParserOnString monadicParser () "blah" "hi hello" with
    | ParserResult.Failure (m,_,_) -> failwith m
    | ParserResult.Success _ -> ()

let testApplicativeCE () =
    match runParserOnString applicativeParser () "blah" "hi hello" with
    | ParserResult.Failure (m,_,_) -> failwith m
    | ParserResult.Success _ -> ()

let run() =
        testMonadicCE ()
        testApplicativeCE ()

module Perf =
    open FParsec.Primitives
    open FSharp.Quotations

    let parse = QuotedParserCombinator()

    let monadicParser: Expr<Parser<_, unit>> =
        parse {
            let letters = many1Chars (satisfy isLetter)
            do! spaces
            let! token1 = letters
            do! spaces
            let! token2 = letters
            return token1, token2
        }

    let applicativeParser: Expr<Parser<_, unit>> =
        parse {
            let letters = many1Chars (satisfy isLetter)
            let! _ = spaces
            and! token1 = letters
            and! _ = spaces
            and! token2 = letters
            return (token1, token2)
        }


    let testPerf () =
        let m = monadicParser
        let a = applicativeParser
        printfn "monadic:\n%A" m
        printfn "applicative:\n%A" a
        ()



