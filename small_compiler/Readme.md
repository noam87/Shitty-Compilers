# A Simple Recursive-Descent Expression Compiler

Follows the grammar below and compiles to a (very) small subset of C:

    statements   ->  EOF
                  |  expression; statements
    expression   ->  term expression'
    expression'  ->  + term expression'
                  |  EMPTY
    term         ->  factor term'
    term'        ->  * factor term'
                  |  EMPTY
    factory      ->  NUMBER
                  |  (expression)

## C Version

Based on the introductory compiler described in chapter 1 of *Compiler Design
In C*.

The C version will follow the description in the book closely, with very few
modifications.

## Guile Scheme Version

I'm gonna write this in a more functional style, and also as a first exercise
in writing larger scheme programs.
