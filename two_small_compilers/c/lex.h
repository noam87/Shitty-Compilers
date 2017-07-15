#define EOI        0  // End of input
#define SEMI       1  // ";"
#define PLUS       2  // "+"
#define TIMES      3  // "*"
#define L_PAREN    4  // "("
#define R_PAREN    5  // ")"
#define NUM_OR_ID  6  // A number or identifier

extern char* yytext;   // Current string, pointing at current offset.
extern int   yyleng;   // Length of current lexeme.
extern int   yylineno; // Current line number.

/**
 * Returns int 0-6 matching a lexeme (EOI, SEMI, etc...)
 */
int match(int token);

/**
 * Moves on to next token, to be matched by calling match().
 */
void advance();
