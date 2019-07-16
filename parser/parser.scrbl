#lang scribble/manual
@(require (for-label racket syntax/parse))

@title[#:tag "macro-ll1"]{Macro Tutorial: LL(1) Parser Generator}
@author[@author+email["Ryan Culpepper" "ryanc@racket-lang.org"]]

@; ============================================================
@section{Version 1: An LL(1) parser-interpreter}

@;{
1. interpreter
- use lists of tokens for simplicity; Token = (list Symbol Any)

1b. grammar AST

1c. LL(1) algorithm
recursive descent parser (use token list for easy peeking?)
need to precompute First, Follow sets; v0 in parser
}

@;{
FIXME:
*. Points of dissatisfaction:
- grammar wf checked at runtime
- action routine interface is clunky
- First+Follow computation ...
}

@; ============================================================
@section{Version 2: Improving the front end}

@; ----------------------------------------
@subsection{An S-expression front end}

@;{
add S-expression front end that is "parsed" (ouch) to AST
- When is a grammar syntactically well-formed?
- Maybe pre-process a little?
  - wf checks
  - disambiguate ntelem vs telem
  - def list to hash, prod struct
}

@; ----------------------------------------
@subsection{A basic macro front end}

@;{
2a. syntax classes, just data language ("eliminate quotes")
- OLD: parse to data structure, NEW: parse to expr to make data structure
}

@; ----------------------------------------
@subsection{Compile-time context-sensitive checks}

@;{
2b. now check wf at compile time
- two approaches: multi-pass parsing vs HO attributes
}

@; ----------------------------------------
@subsection{Make action routines nicer}

@;{
2c. revise syntax, make action routines nicer
}

@; ============================================================
@section{Version 3: Moving computation to compile time}

@;{
3. want to compute First/Follow at compile time => compile-time ASTs!
- NOW we need to analyze grammar structure at compile time

3a. compile-time ASTs
- separate module, require at both phase 1 and phase 0
- new approch: attrs are/return compile-time ASTs, translate to exprs later!
  - tricks with prefab AST structs:
    - quasiquote and unquote
    - datum-to-expression

3b. do First/Follow
}

@; ============================================================
@; ============================================================

@;{
Beyond?
- add token information?
- add multiple backends (eg, LR(0)?)
  - split parser-specific ct data from general ct data
}
