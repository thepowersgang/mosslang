# Overview

# Syntax

## Items (top-level syntax constructs)

## Types

## Expressions and Statements (executed code)
```bnf
block: '{'   '}'
statement: block_expr ';' | expt ';' | 'let' pattern [':' type] ['=' expt]
```