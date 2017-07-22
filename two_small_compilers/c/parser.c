#include <stdio.h>
#include "lex.h"

void statements(void);
void factor(void);
void term(void);
void expression(void);

int legal_lookahead(int first_arg, ...);

void parse(void) {
    statements();
}

/**
 * statements -> expression SEMI
 *             | expression SEMI statements
 */
void statements(void) {
    while (!match(EOI)) {
        expression();
        if (match(SEMI)) advance();
        else fprintf(stderr, "%d: Inserting missing ';'\n", yylineno);
    }
}

/**
 * expression  -> term expression'
 * expression' -> PLUS term expression'
 *              | NOTHING
 */
void expression(void) {
    if (!legal_lookahead(NUM_OR_ID, L_PAREN, 0)) return;

    term();

    while (match(PLUS)) {
        advance();
        term();
    }
}

/**
 * term  -> factor term'
 * term' -> TIMES factor term'
 *        | NOTHING
 */
void term(void) {
    if (legal_lookahead(NUM_OR_ID, L_PAREN, 0)) return;

    factor();

    while (match(TIMES)) {
        advance();
        factor();
    }
}

/**
 * factor -> NUM_OR_ID
 *         | L_PAREN expression R_PAREN
 */
void factor(void)  {
    if (!legal_lookahead(NUM_OR_ID, L_PAREN, 0)) return;

    if (match(NUM_OR_ID)) {
        advance();
    } else if (match(L_PAREN)) {
        advance();
        expression();
        if (match(R_PAREN)) advance();
        else fprintf(stderr, "%d: Mismatched ')'\n", yylineno);
    } else {
        fprintf(stderr, "%d: Number or identifier expected\n", yylineno);
    }

}

/**
 * Error Recovery
 */

#include <stdarg.h>

#define MAXFIRST 16

int legal_lookahead(int first_arg, ...) {
    va_list args;
    int tok;
    int lookaheads[MAXFIRST];
    int *p = lookaheads;
    int *current;
    int error_printed = 0;
    int rval = 0;

    va_start(args, first_arg);

    if (!first_arg) {
        if (match(EOI)) rval = 1;
    } else {
        *p++ = first_arg;

        while ((tok = va_arg(args, int)) && p < &lookaheads[MAXFIRST]) {
            *++p = tok;
        }

        while (!match(SEMI)) {
            for (current = lookaheads; current < p; ++current) {
                if (match(*current)) {
                    rval = 1;
                    goto exit;
                }
            }

            if (!error_printed) {
                fprintf(stderr, "%d: Syntax Error\n", yylineno);
                error_printed = 1;
            }

            advance();
        }
    }

    exit:
        va_end(args);
        return rval;
}
