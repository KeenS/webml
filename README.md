# WebML -- A Standard ML compiler for the Web
[![Build Status](https://travis-ci.org/KeenS/webml.svg?branch=master)](https://travis-ci.org/KeenS/webml)

WebML is to be a Standard ML (SML '97) Compiler that works on web browsers and generates [WebAssembly](http://WebAssembly.org) binaries. WebAssembly binaries can be run on web browsers.
This means you can run SMl REPL on web browsers.

## Status
Under very early stage of initial development.
Compiles only minimal subset of SML codes. The garbage collector is not complete.

## Implemented features
### Core

* Declaration
  + [ ] `val`
    - [x] basic (`val ident = expr`)
    - [x] pattern (`val pat = expr`)
    - [ ] tyvar `val 'a pat = expr`
    - [ ] typed (`val pat : ty = expr`)
    - [ ] `and` (`val pat = expr and pat = expr`)
  + [ ] `fun`
    - [x] basic (`fun ident ident ... = expr`)
    - [x] pattern (`fun ident pat ... = expr`)
    - [x] multi-clause (`fun ident pat ... = expr | ident pat ... = expr`)
    - [x] `op` (`fun op ident pat ... = expr`)
    - [ ] tyvar (`fun 'a ident pat ... = expr`)
    - [ ] typed (`fun ident pat ... : ty = expr`)
    - [ ] `and` (`fun ident pat ... = expr and ident pat ... = expr`)
  + [ ] `type` (`type ident = ty`)
  + [ ] `datatype`
    - [ ] `datatype ident = Con of ty | Con ...`
      - [x] basic (`datatype ident = Con of ty | Con ...`)
      - [ ] tyvar (`datatype 'a ident = Con of ty | Con ...`)
      - [ ] `and` (`datatype ident = Con | ... and ident = Con | ...`)
      - [ ] `withtype` (`datatype ident = Con ... withtype ..`)
    - [ ] `datatype ident = datatype ident`
  + [ ] `abstype`
  + [ ] `exception`
  + [ ] `local ... in ... end`
  + [ ] `open ..`
  + [ ] `decl ; decl`
    - [x] `decl decl`
    - [ ] `decl ; decl`
  + [x] `infix`
  + [ ] `infixr`
  + [ ] `nofix`
* Expressions
  + [ ] special constant
    - [x] integer
    - [ ] real
      - [x] `123.456`
      - [ ] `123e456`
      - [ ] `123E456`
      - [ ] `123e~456`
    - [ ] word
    - [x] char
    - [ ] string
  + [x] value identifier
  + [ ] `op`
  + [ ] record
    - [ ] basic (`{ label = expr , ...}`)
    - [x] tuple
    - [x] 0-tuple
    - [ ] `#label`
  + [ ] list (`[expr, ..., expr]`)
  + [ ] `(expr; ...; expr)`
  + [x] paren (`(expr)`)
  + [ ] `let .. in .. end`
    - [x] basic (`let decl ... in expr end`)
    - [ ] derived (`let decl ... in expr; ...; expr end`)
  + [x] function application
  + [ ] infix operator
    - [x] L
    - [ ] R
  + [ ] typed (`exp : ty`)
  + [ ] exception
    - [ ] `handle`
    - [ ] `raise`
  + [ ] `fn`
    - [x] basic (`fn ident => expr`)
    - [ ] pattern (`fn pat => expr`)
    - [ ] multi-clause `fn pat => expr | pat => expr ...`
  + [ ] `andalso`
  + [ ] `orelse`
  + [x] `if .. then .. else`
  + [ ] `while .. do ..`
  + [x] `case .. of ..`
* Pattern
  + [x] wildcard
  + [ ] special constant
    - [x] integer
    - [ ] word
    - [x] char
    - [ ] string
  + [x]  value identifier
  + [ ] `op`
  + [ ] record
    - [ ] basic (`{ label = pat , ...}`)
    - [ ] wildcard (`...`)
    - [ ] label as variable (`{ var (as pat), ...}`)
    - [x] tuple
    - [x] 0-tuple
  + [ ] list
  + [x] paren
  + [x] Constructor
  + [ ] infix
  + [ ] typed (`pat : ty`)
  + [ ] layerd (`ident as pat`)
* Type
  + [ ] type variable
  + [ ] record
  + [ ] type construction
    - [x] without param (`ident`)
    - [ ] with param (`ty ident`)
  + [x] tuple
  + [x] function
  + [x] paren
* Initial Basis
  + [x] `unit`
  + [x] `bool`
    - [x] `true`
    - [x] `false`
  + [x] `int`
  + [ ] `word`
  + [ ] `string`
  + [ ] `char`
  + [ ] `list`
    - [ ] `nil`
    - [ ] `::`
  + [ ] `ref`
    - [ ] `ref`
    - [ ] `:=`
  + [ ] `exn`
  + [x] `=`
  + [ ] `Match`
  + [ ] `Bind`
* Overloaded
  + [x] `+`
  + [x] `-`
  + [x] `*`
  + [x] `div`
  + [x] `mod`
  + [x] `/`
  + [x] `<`
  + [x] `>`
  + [x] `<=`
  + [x] `>=`
  + [ ] `abs`
  + [ ] `~`

### Module

not yet implemented

### Program

* Program
  + [x] decl (`decl decl ...`)
  + [ ] expr (`expr decl ...`)
    - Note: toplevel expression `expr` should be treated as `val it = expr`
