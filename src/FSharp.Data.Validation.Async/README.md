# FSharp.Data.Validation.Async

## Description

This library provides a small set of functions that extend the `FSharp.Data.Validation` library to work with asynchronous workflows.

## Functions

- `bindToAsync: ('A -> Async<VCtx<'F, 'B>>) -> VCtx<'F, 'A> -> Async<VCtx<'F, 'B>>`
- `bindAsync: ('A -> Async<VCtx<'F, 'B>>) -> Async<VCtx<'F, 'A>> -> Async<VCtx<'F, 'B>>`
- `bindFromAsync: ('A -> Async<'B>) -> VCtx<'F, 'A> -> Async<VCtx<'F, 'B>>`
- `combineAsync: Async<VCtx<'F, 'A>> -> Async<VCtx<'F, 'B>> -> Async<VCtx<'F, 'A * 'B>>`
- `bindAndCombineAsync: ('A -> Async<VCtx<'F, 'B>>) -> Async<VCtx<'F, 'A>> -> Async<VCtx<'F, 'B>>`
- `mapAsync: ('A -> Async<'B>) -> VCtx<'F, 'A> -> Async<VCtx<'F, 'B>>`
