#include "lex.h"
#include <stdio.h>
#include <ctype.h>

#define BUFFER_LENGTH 128

char* yytext = ""; // lexeme (not '\0' terminated)
int yyleng = 0;
int yylineno = 0;

static int lookaheadToken = -1;

int lex() {
    static char input_buffer[BUFFER_LENGTH];
    char* current;

    current = yytext + yyleng;

    while(1) {
        while(!*current) {
            current = input_buffer;

            if (!fgets(input_buffer, BUFFER_LENGTH, stdin)) {
                *current = '\0';
                return EOI;
            }

            ++yylineno;

            while (isspace(*current)) ++current;
        }

        for ( ; *current; ++current) {
            yytext = current;
            yyleng = 1;

            switch (*current) {
                case EOF: return EOI;
                case '$': return EOI; // cheat for sdtin
                case ';': return SEMI;
                case '+': return PLUS;
                case '*': return TIMES;
                case '(': return L_PAREN;
                case ')': return R_PAREN;

                case '\n':
                case '\t':
                case ' ': break;

                default:
                    if (!isalnum(*current))
                        fprintf(stderr,
                                "Ignoring illegal input <%c>\n",
                                *current);
                    else {
                        while (isalnum(*current)) ++current;
                        yyleng = current - yytext;
                        return NUM_OR_ID;
                    }

                    break;
            }
        }
    }
}

int match(int token) {
    if (lookaheadToken == -1) lookaheadToken = lex();
    return token == lookaheadToken;
}

void advance() {
    lookaheadToken = lex();
}
