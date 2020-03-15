%{ 
  #include<string.h>
  #include<stdio.h> 
  #include<stdlib.h>
  #include "header.h"
  extern FILE *yyin;
  extern int yylineno;
  extern char* yytext;
  extern int line_number;
  int yylex();
  int yyerror(char*);
  void insert_entry(char *sym_id);
  int search_entry();
  extern int scope;
  void print_table();
  extern char type[200];
  void print_undefined();	
  void print_multi_decl();
  char id[10];
  int get_value(char* id); 
  void insert_value(char* id, int value);
  Node* newNode(Node* leftp, char* value, Node* rightp);
  Node* newIfNode(Node* leftp, char* value, Node* rightp, Node* thirdp);
  void print_ast(Node* root);
  char relop[10];
  int assign=0;
%} 

%union
{
	struct 
	{ 
		Node* ptr; 
		int value; 
	} mytype;
}
%type <mytype> start
%type <mytype> S
%type <mytype> Assign
%type <mytype> K
%type <mytype> LIST_IDS
%type <mytype> ConditionalExpression
%type <mytype> C
%type <mytype> RelationalExpression
%type <mytype> relop
%type <mytype> Expression
%type <mytype> T
%type <mytype> F
%type <mytype> G
%type <mytype> G_NEW
%type <mytype> G_NEW2
%type <mytype> Type
%type <mytype> Declaration
%type <mytype> L
%type <mytype> X
%type <mytype> P
%type <mytype> Q
%type <mytype> AssignmentExpression
%type <mytype> ifStatement
%type <mytype> E
%type <mytype> ES
%type <mytype> forStatement
%type <mytype> Init

%token T_NUM T_ID T_INT T_FLOAT T_CHAR T_DOUBLE T_VOID T_IF T_ELSE_IF T_ELSE T_FOR T_RETURN T_BREAK T_GE T_LE T_EQUALS T_NE T_OR T_AND T_INCR T_DECR T_PRINT T_STRING_CONST T_CHAR_CONST T_FLOAT_NUM
%%

start : S 
{
	printf("\n---------------------AST------------------------ \n\n"); 
	print_ast($$.ptr); 
}

;
S :   Declaration ';'  S  { $$.ptr = newNode($1.ptr,"S",$3.ptr); }
|  Assign ';' S { $$.ptr = newNode($1.ptr,"S",$3.ptr); }
| ifStatement S { $$.ptr = newNode($1.ptr,"S",$2.ptr); }
| forStatement S { $$.ptr = newNode($1.ptr,"S",$2.ptr); }
| T_PRINT '(' T_STRING_CONST LIST_IDS ')' ';' {  }
| %empty { $$.ptr = newNode(NULL,"",NULL); }
;

Assign : K AssignmentExpression 
{ 
	$$.ptr = newNode($1.ptr,"=",$2.ptr); 
}
;

K : T_ID 
	{ 
		int present = search_entry(yytext);
		if(!present)
			print_undefined();
		else
			{
				$$.ptr = newNode(NULL,yytext,NULL); 
				strcpy(id, yytext);
			}
	}
;

LIST_IDS : ',' T_ID LIST_IDS {} | %empty {}
;

ConditionalExpression : ConditionalExpression T_OR C 
{ 
	$$.value = $1.value || $3.value; 
	$$.ptr = newNode($1.ptr,"||",$3.ptr); 
}
 
| C				 
;

C : C T_AND RelationalExpression 
{ 
	$$.value = $1.value && $3.value; 
	$$.ptr = newNode($1.ptr,"&&",$3.ptr);
}
 
| RelationalExpression
;

/*D : '!' D { $$=!$2; } | RelationalExpression 
;*/

RelationalExpression : RelationalExpression relop  Expression 
{ 
	$$.ptr = newNode($1.ptr,relop,$3.ptr);

	switch($2.value)
	{
		case 1 :  $$.value = $1.value == $3.value;
			break;

		case 2 : $$.value = $1.value > $3.value;
			break;
		
		case 3 : $$.value = $1.value < $3.value;
			break;

		case 4 : $$.value = $1.value >= $3.value;
			break;

		case 5 : $$.value = $1.value <= $3.value;
			break;

		case 6 : $$.value = $1.value != $3.value;
			break;
	}
}

| Expression 
;

relop : T_EQUALS 
{ 
	strcpy(relop,"=="); 
	$$.value = 1;
} 

| '>' 
{ 
	strcpy(relop,">"); 
	$$.value = 2;
} 

| '<' 
{ 
	strcpy(relop,"<"); 
	$$.value = 3;
} 

| T_GE 
{ 
	strcpy(relop,">="); 
	$$.value = 4;
} 

| T_LE 
{ 
	strcpy(relop,"<="); 
	$$.value = 5;
} 

| T_NE 
{ 
	strcpy(relop,"!="); 
	$$.value = 6;
}
;

Expression : Expression '+' T 
{
	$$.ptr = newNode($1.ptr,"+",$3.ptr);
	$$.value = $1.value + $3.value; 
} 

| Expression '-' T 
{ 
	$$.ptr = newNode($1.ptr,"-",$3.ptr); 
	$$.value = $1.value - $3.value; 
} 

| T  
;

T : T '*' F 
{ 
	$$.ptr = newNode($1.ptr,"*",$3.ptr); 
	$$.value = $1.value * $3.value; 
} 
| 

F 
;

F : F '/' G_NEW 
{ 
	$$.ptr = newNode($1.ptr, "/" ,$3.ptr); 
	$$.value = $1.value / $3.value;
} 

| G_NEW 
;

G_NEW: G_NEW T_INCR 
| G_NEW2 
;

G_NEW2: G_NEW2 T_DECR 
| G  
;

G : T_NUM  
{ 
	$$.value = atoi(yytext);  
	$$.ptr = newNode(NULL,yytext,NULL); 
}
 
| T_ID 
{ 
	int present = search_entry(yytext);
	if(!present)
		print_undefined();
	else 
		{
			$$.value = get_value(yytext); 
			$$.ptr = newNode(NULL,yytext,NULL); 
		}
}
 
|  '(' ConditionalExpression ')'  
{ 
	$$.ptr = $2.ptr; 
	$$.value = $2.value;
}
;

// 
// |  '-' T_NUM { $$.ptr= newNode(NULL,atoi(yytext*-1),NULL); } | T_CHAR_CONST | T_STRING_CONST | T_FLOAT_NUM { $$ = newNode(NULL,atof(yytext),NULL);}

Type : T_INT 
{ 
	$$.ptr = newNode(NULL,"int",NULL); 
} 

| T_FLOAT 
{ 
	$$.ptr = newNode(NULL,"float",NULL); 
}  

| T_VOID 
{ 
	$$.ptr = newNode(NULL,"void",NULL); 
} 

| T_CHAR 
{ 
	$$.ptr = newNode(NULL,"char",NULL); 
} 

| T_DOUBLE  
{ 
	$$.ptr = newNode(NULL,"double",NULL); 
}
;

Declaration : Type L 
{ 
	$$.ptr = newNode($1.ptr,"Declaration",$2.ptr); 
}
;

L : L ',' X 
{ 
	$$.ptr = newNode($1.ptr,"L",$3.ptr); 
} 

| X 
;

X : P Q 
{   
	if(assign == 1) 
		$$.ptr = newNode($1.ptr,"=",$2.ptr); 
	else 
		$$.ptr = $1.ptr; 
	assign=0; 
}
;

P : T_ID 
{ 
	int present = search_entry(yytext);
	if(present) 
		print_multi_decl();
	else 
		{ 
			insert_entry(yytext);
			strcpy(id,yytext);
			$$.ptr = newNode(NULL,yytext,NULL);  
		} 
}
;

Q : %empty {} | AssignmentExpression | T_INCR {} | T_DECR {} | '[' T_NUM ']'  {}
;
 
AssignmentExpression :  '=' Expression  
{ 
	assign=1; 
	insert_value(id,$2.value); 
	$$.ptr = $2.ptr;
}
;

ifStatement : T_IF '(' ConditionalExpression ')' '{' S '}' E 
{  
	$$.ptr = newIfNode($3.ptr,"if",$6.ptr, $8.ptr); 
}
;

E : T_ELSE_IF '(' ConditionalExpression ')' '{' S '}' E  
{ 
	$$.ptr = newIfNode($3.ptr,"else if",$6.ptr, $8.ptr); 
}
 
| ES
;

ES : T_ELSE '{' S '}' 
{ 
	$$.ptr = newNode(NULL,"else",$3.ptr); 
} 

| %empty 
{ 
	$$.ptr = NULL; 
}
;

forStatement : T_FOR '(' Init ';' ConditionalExpression ';' Assign ')' '{' S '}'  
{  
	Node* Init_Cond = newNode($3.ptr,"Init_Cond",$5.ptr); 
	$$.ptr = newIfNode(Init_Cond,"for",$7.ptr,$10.ptr); 
}
;

Init : X | Declaration | %empty { $$.ptr = NULL; }
;

%% 
  
int yyerror(char *msg) 
{ 
	printf("%s\n",msg);
	//printf("error, %s: '%s' in line %d\n", msg, yytext, line_number);
	//yyparse();
} 



int main() 
{ 	
	printf("----------------Tokens generated------------\n\n");
	yyin = fopen("input.txt", "r");
	yyparse(); 
	fclose(yyin);
	print_table();
	printf("\n");  
	return 0;
} 
