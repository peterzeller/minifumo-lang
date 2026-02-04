grammar Minifumo;

// NOTE: This grammar assumes your lexer injects the tokens NL / BEGIN / END.
// You can implement that with a custom lexer that buffers tokens and emits
// BEGIN/END based on indentation changes after NL.
// Here we declare them as normal lexer tokens so the parser can reference them.

// -------------------- Parser --------------------

program
  : importStatement* (NL | topLevel)* EOF
  ;

// Import statements
// 1. project local wrt. to package root
//    For example, import symbol foo from a file "blub/bla.minifumo":
//    import foo from "blub/bla"
// 2. from a Git repo, specifying a tag or commit hash (version locked in local lock file and in global proxy)
//    For example, import symbol foo from a file "blub/bla.minifumo" in a Github repo with a specific tag:
//    import foo from "blub/bla" in "github.com/example/blub@v1.3"
importStatement: 'import' ID  ('from' from=STRING ('in' in=STRING)?)? NL;

topLevel
  : dataDecl
  | funDecl
  ;

// -------- ADTs --------

dataDecl
  : 'export'? 'data' typeName implicitParams? '=' ctorDecl? ('|' ctorDecl)*
  ;

typeName
  : ID
  ;

implicitParams
  : '[' funParam (',' funParam)* ']'
  ;


ctorDecl
  : ID ctorFields?
  ;

ctorFields
  : '(' ctorField (',' ctorField)* ')'
  ;

ctorField
  : ID ':' expr
  ;

// -------- Functions --------

funDecl
  : 'export'? funSig suite
  ;

funSig
  : 'fun' ID implicitParams? '(' funParams? ')' ':' returnType=expr
  ;

funParams
  : funParam (',' funParam)*
  ;

funParam
  : ID ':' expr
  ;

suite
  : NL BEGIN block END
  ;

block
  : (NL)* expr (NL+ expr)* (NL)*
  ;


// -------- Expressions (ANTLR4 precedence via left recursion) --------
// Highest precedence first, lowest last.

expr
  : literal                                   #Lit
  | ID                                        #Var
  | '(' expr ')'                              #Paren
  | '(' ')' # Unit
  | lambdaParams '=>' expr                    #Lambda

  // postfix
  | expr typeArgs                             #TypeApply
  | expr typeArgs? '(' argList? ')'                                  #Call
  | expr '.' ID ('(' argList? ')')?           #Dot

  // unary
  | '-' expr                                  #Neg

  // multiplicative / additive
  | expr op=('*'|'/'|'%') expr                #MulDiv
  | expr op=('+'|'-') expr                    #AddSub

  // comparisons / equality
  | expr op=('<'|'<='|'>'|'>=') expr          #Compare
  | expr op=('=='|'!=') expr                  #EqNeq

  // boolean
  | expr 'and' expr                            #And
  | expr 'or' expr                            #Or

  // Function type
  | base=expr '->' result=expr # FunctionType

  // Dependent function type
  | 'forall' ID ':' base=expr '.' result=expr # DependentFunctionType

  // ---- "statement-like" expressions (lowest precedence) ----

  // let/var with "in"
  | 'let' ID (':' varType=expr)? '=' value=expr 'in' body=expr         #LetIn

  // let binding statements
  | 'let' ID (':' varType=expr)? '=' value=expr                   #LetStmt

  // if
  | 'if' expr 'then' expr 'else' expr  #IfThenElse
  | 'if' expr suite 'else' suite       #IfSuite

  // match
  | 'match' expr NL BEGIN matchCase+ END      #Match
  ;

lambdaParams
  : lambdaParam                                  #LambdaSingle
  | '(' lambdaParam (',' lambdaParam)* ')'    #LambdaMulti
  ;

lambdaParam
  : ID (':' expr)?
  ;

argList
  : expr (',' expr)*
  ;

typeArgs
  : '[' expr (',' expr)* ']'
  ;

matchCase
  : 'case' pattern suite
  ;

// -------- Literals & patterns --------

literal
  : INT
  | BOOL
  | STRING
  ;

pattern
  : '_'                                   #PatWildcard
  | literal                               #PatLit
  | ID                                    #PatBinderOrCtor0
  | ID '(' patternArgs? ')'               #PatCtor
  | '(' pattern ')'                       #PatParen
  ;

patternArgs
  : pattern (',' pattern)*
  ;

// -------------------- Lexer --------------------
// Keep keyword tokens above ID so they win ties.

NL      : ('\r'? '\n')+ ;

// Keywords (optional to keep explicit; literals in parser also work, but
// defining them here gives you nicer tokenization + avoids surprises).
DATA    : 'data';
FUN     : 'fun';
FUNC    : 'func';
EXPORT  : 'export';
MATCH   : 'match';
CASE    : 'case';
IF      : 'if';
THEN    : 'then';
ELSE    : 'else';
FOR     : 'for';
IN      : 'in';
WHILE   : 'while';
LET     : 'let';
VAR     : 'var';
IF_UP   : 'IF';

// Operators & punctuation
// (You can omit most of these because the parser uses literals, but keeping
// multi-char operators here helps ensure correct tokenization.)
ASSIGN_COLON_EQ : ':=' ;
EQEQ    : '==';
LE      : '<=';
GE      : '>=';
PAREN_LEFT: '(';
PAREN_RIGHT: ')';
BRACKET_LEFT: '[';
BRACKET_RIGHT: ']';
BRACE_LEFT: '{';
BRACE_RIGHT: '}';
DOT: '.';
NOT: 'not';

COMMA: ',';
PLUS: '+';
MULT: '*';
MINUS: '-';
DIV: 'div';
MOD: 'mod';
AND: 'and';
OR: 'or';
COLON: ':';
COLONCOLON: '::';
EQ: '=';
NOTEQ: '!=';
BAR: '|';
IMPLIES: '==>';
IFF: '<==>';


// Literals
INT     : [0-9]+ ;
BOOL    : 'true' | 'false' ;
STRING  : '"' ( '\\' . | ~["\\] )* '"' ;

// Identifier
ID      : [a-zA-Z_][a-zA-Z0-9_]* ;

// Whitespace/comments
SPACETAB:' ' ' '+;
SPACES: ' ' -> skip;
LINE_COMMENT : '//' ~[\r\n]* -> skip ;
BLOCK_COMMENT: '/*' .*? '*/' -> skip ;

// Dummy tokens (generated by extended lexer)
BEGIN:[()];
END:[()];
INVALID:[()];
