#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

typedef enum {
	//single character tokens
	L_PAREN; R_PAREN; MINUS; PLUS;
	SLASH; STAR;

	//one or two character tokens (comparisons)
	EQUAL; EQUAL_EQUAL;
	GREATER; GREATER_EQUAL;
	LESS; LESS_EQUAL;

	//literals
	ID; STRING; NUMBER;

	//keywords
	AND; ELSE; FALSE; TRUE; FOR; IF; OR; PRINT; RETURN; VAR; WHILE;

} TokenType;

typedef struct {
	TokenType type;
	String lexeme;
	Object literal;
	int line;
	
	Token(
}
	
