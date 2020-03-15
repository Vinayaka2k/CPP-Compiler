%{
	#include<stdio.h>
	#include<stdlib.h>
	#include<ctype.h>
	#include "header.h"
	#include "y.tab.h"
	#include<string.h>
	int scope = 0;
	int line_number = 1; 
	int comments_multi = 0;
	int comments_single = 0;
	char type[200];
	void print_ast(Node* root);

	typedef struct sym
	{
		char sym_id[10];
		int sym_scope;
		int sym_line;
		char sym_type[5];
		int sym_valid;
		int sym_value; 
		struct sym *next;
	}sym;
	sym *head =NULL;
	
	void insert_entry(char *sym_id)
	{		
		sym *new = (sym*)malloc(sizeof(sym));
		new -> next = head;	
		strcpy(new -> sym_id,sym_id);
		new -> sym_scope = scope;
		new -> sym_line = line_number;
		new->sym_valid=1;
		new->sym_value=-1;
		strcpy(new -> sym_type, type);
		head = new;
	}

	int search_entry()
		{
			sym * curr= head;
			while(curr!=NULL)
			{
				int m = strcmp(curr->sym_id,yytext);
				if(m==0 && scope>=curr->sym_scope && curr->sym_valid) 
				{
					return 1;
				}
				curr = curr->next;
			}
		return 0;	
		}

	void print_table()	
	{
		sym * curr= head;
		printf("\n\n------------------- SYMBOL TABLE -----------------------\n\n");
		printf("id | scope | line | type | valid | value\n\n");
		while(curr!=NULL)
		{	
			sym *new =curr;
			printf("%s,%d,%d,%s,%d,%d\n",new -> sym_id, new -> sym_scope, new -> sym_line,new -> sym_type, new->sym_valid, new->sym_value);
			curr = curr->next;
		}
	}

	void mark_invalid()
	{
		sym * curr= head;
		while(curr!=NULL)
		{	
			sym *new =curr;
			if(new->sym_scope == scope+1)
				new->sym_valid = 0;			
			curr = curr->next;
		}	
	}
	
	void print_undefined()
	{
		printf("undefined variable %s in line %d\n",yytext,line_number);
	}

	void print_multi_decl()
	{
		printf("multiple declaration of %s in line %d\n",yytext,line_number);
	}

	void insert_value(char* id, int value)
	{
		sym * curr= head;
		while(curr!=NULL)
		{	
			if(!strcmp(curr->sym_id, id))
				curr->sym_value = value;			
			curr = curr->next;
		}	
	}

	int get_value(char* id)
	{	
	sym * curr= head;
	int value;
	while(curr!=NULL)
		{	
			if(!strcmp(curr->sym_id, id))
				value = curr->sym_value;			
			curr = curr->next;
		}	
	return value;	
	}

	Node* newNode(Node* leftp, char* value, Node* rightp)
	{
		Node* temp = (Node*)malloc(sizeof(Node));
		temp->left = leftp;
		temp->right = rightp;
		temp->third = NULL;
		strcpy(temp->value, value);
		return temp;
	}

	Node* newIfNode(Node* leftp, char* value, Node* rightp, Node* thirdp)
	{
		Node* temp = (Node*)malloc(sizeof(Node));
		temp->left = leftp;
		temp->right = rightp;
		temp->third = thirdp;
		strcpy(temp->value, value);
		return temp;
	}


	/*
	void print_ast(Node* root)
	{
	if(root != NULL)
		{
		printf("%s ",root->value);
		print_ast(root->left);
		print_ast(root->right);
		}
	}
*/

int height(Node* node)  
{  
    if (node == NULL)  
        return 0;  
    else
    {  
        /* compute the depth of each subtree */
        int lDepth = height(node->left);  
        int rDepth = height(node->right);  
      
        /* use the larger one */
        if (lDepth > rDepth)  
            return(lDepth + 1);  
        else return(rDepth + 1);  
    }  
}  



void printGivenLevel(Node* root, int level) 
{ 
    if (root == NULL) 
        return; 
    if (level == 1) 
        printf("%s ", root->value); 
    if(strcmp(root->value,"if") == 0)
	print_ast(root->third);
    else if (level > 1) 
    { 
        printGivenLevel(root->left, level-1); 
        printGivenLevel(root->right, level-1); 
    } 
} 

/*
void print_ast(Node* root) 
{ 
    int h = height(root); 
    int i; 
    for (i=1; i<=h; i++) 
    { 
        printGivenLevel(root, i); 
        printf("\n"); 
    } 
} 
  */



void print_ast(Node* root) 
{
    if (root == NULL) 
   {
        return;
    }
    printf("%s ",root->value);
    print_ast(root->left);
    print_ast(root->right);
    print_ast(root->third);
}
%}

%%


"{" { scope++; printf("%s\n",yytext); return '{'; }

"}" { scope--;  printf("%s\n",yytext); mark_invalid(); return '}';  }

[\n] {
	line_number++;
}

\/\/(.*) ;					

\/\*(.*\n)*.*\*\/  ;

int { printf("%s\n",yytext); strcpy(type,yytext); return T_INT; }
float { printf("%s\n",yytext);strcpy(type,yytext); return T_FLOAT; }
char { printf("%s\n",yytext);strcpy(type,yytext); return T_CHAR; }
double { printf("%s\n",yytext);strcpy(type,yytext); return T_DOUBLE; }
void { printf("%s\n",yytext);strcpy(type,yytext); return T_VOID; }

"else if" {printf("%s\n",yytext); return T_ELSE_IF;}
"if" { printf("%s\n",yytext); return T_IF; }
"else" { printf("%s\n",yytext); return T_ELSE; }
"for" { printf("%s\n",yytext); return T_FOR; }
"return" { printf("%s\n",yytext); return T_RETURN; }
"break" { printf("%s\n",yytext); return T_BREAK; }
"printf" { printf("%s\n",yytext); return T_PRINT; }
(\".*\") { printf("%s\n",yytext); return T_STRING_CONST;  }
(\'.\') { printf("%s\n",yytext); return T_CHAR_CONST; }
[a-zA-Z_][a-zA-Z0-9_]* {

	printf("%s\n",yytext); 
	return T_ID;
	}

[0-9]+\.[0-9]+ { printf("float:%s\n",yytext); return T_FLOAT_NUM; }
[0-9]+ {  printf("%s\n",yytext); return T_NUM; }
">=" { printf("%s\n",yytext); return T_GE; }
"<=" { printf("%s\n",yytext); return T_LE; }
"==" { printf("%s\n",yytext); return T_EQUALS; }
"!=" { printf("%s\n",yytext); return T_NE; }
"||" { printf("%s\n",yytext); return T_OR; }
"&&" { printf("%s\n",yytext); return T_AND; }

"++" { printf("%s\n",yytext); return T_INCR; }
"--" { printf("%s\n",yytext); return T_DECR; }
"+" { printf("%s\n",yytext); return '+'; } 
"-" { printf("%s\n",yytext); return '-'; }
"*" { printf("%s\n",yytext); return '*'; }
"/" { printf("%s\n",yytext); return '/'; }
"%" { printf("%s\n",yytext); return '%'; }
">" { printf("%s\n",yytext); return '>'; }
"<" { printf("%s\n",yytext); return '<'; }
"=" { printf("%s\n",yytext); return '='; }
"!" { printf("%s\n",yytext); return '!'; }

"(" { printf("%s\n",yytext); return '('; }
")" { printf("%s\n",yytext); return ')'; }
"[" { printf("%s\n",yytext); return '['; }
"]" { printf("%s\n",yytext); return ']'; }

"," { printf("%s\n",yytext); return ','; }
; { printf(";\n"); return ';';}
[ \t\n]		{ }
.			{ }

%%



int yywrap()
{
	return 1;
	
}

