%{
#include <stdio.h>
#include <unistd.h>
#include <math.h>
#include <string.h>
#include <stdlib.h>
static char rcsid[] = "$Id: olmenu.y,v 1.1.1.1 1999/09/26 12:34:39 mib Exp $";
#define YYERROR_VERBOSE 1
extern FILE *yyin;
extern int errno;
extern char *optarg;
extern int optind, opterr, optopt;
int yydebug;

#include "olmenu.h"

struct menu_item *new_item(char *label, int def, int type, void *arg);
struct menu *
new_menu();
void add_item(struct menu *menu, struct menu_item *new);

struct menu *menuptr;
struct menu_item *itemptr;

struct menu *top_menu;
struct menu *menu_list;
struct menu *menu_list_end;


%}
%token_table /* Decl+Summary */
%token       BACK_SELN
%token       COLUMNS
%token       DEFAULT
%token       DIRMENU
%token       END
%token       EXIT
%token       EXIT_NO_CONFIRM
%token       FLIPDRAG
%token       FLIPFOCUS
%token       FULL_RESTORE_SIZE_SELN
%token       INCLUDE
%token       MENU
%token       MOVE_DESKTOP
%token       NOP
%token       OPEN_CLOSE_SELN
%token       QUIT_SELN
%token       PIN
%token       PROPERTIES
%token       REFRESH
%token       REREAD_MENU_FILE
%token       RESTART
%token       SAVE_WORKSPACE
%token       SEPARATOR
%token       START_DSDM
%token       STICK_UNSTICK_SELN
%token       STOP_DSDM
%token       TITLE
%token       WINMENU
%token       WMEXIT
%token       SUBMENU	/* not a real token */
%token <str> LABEL
%token <str> EXEC
%token <num> INT

%type  <str> filename 
%type  <str> label
%type  <itm> menu_command olvwm_cmd_arg menu_item
%type  <itm> option menu_file submenu
%type  <num> olvwm_cmd default pin include end
%type  <mnu> menu_items

%union {
	char *str;
	int  num;
	struct menu *mnu;
	struct menu_item *itm;
}

%start menufile

%%
menufile	:	menu_items
				{ top_menu = $1; }
		;
menu_items	:	/* empty */
				{ $$=new_menu(); }
		|	menu_items menu_item
				{ add_item($1,$2);
				  $$=$1;
				}
		;
menu_item	:	menu_command
		|	option       '\n'
		|	SEPARATOR    '\n'
				{ $$=new_item(NULL,0,SEPARATOR,0); }
		|	menu_file    '\n'
		|	submenu
		|	             '\n'
				{ $$=0; }
		|	error        '\n'
				{ printf("Invalid menu item at line %d\n",@1.first_line);
				  $<itm>$=new_item("error",0,0,0);
				}
		;
menu_file	:	label default include filename
				{ $$=new_item($1,$2,$3,$4); }
		;
filename	:	EXEC
		;
include		:	MENU
				{ $$=MENU; }
		|	INCLUDE
				{ $$=INCLUDE; }
		;
submenu		:	label default MENU '\n' menu_items end '\n'
				{ itemptr=new_item($1,$2,SUBMENU,$5);
				   $5->pinnable |= $6;
				   if($5->title==NULL)
					$5->title=$1;
				   $5->name=$1;
				   $$=itemptr;
				}
		;
default		:	/* empty */
				{ $$=0; }
		|	DEFAULT
				{ $$=1; }
		;
end		:	label END pin
				{ $$=$3; }
		;
pin		:	/* emtpy */
				{ $$=0; }
		|	PIN
				{ $$=1; }
		;
menu_command	:	label  default  EXEC '\n'
				{ $$=new_item($1,$2,EXEC,$3); }
		|	label  default  olvwm_cmd '\n'
				{ $$=new_item($1,$2,$3,0); }
		|	label  default  olvwm_cmd_arg '\n'
				{ $3->def_item=$2; $$=$3; }
		|	label  error '\n'
				{ printf("Invalid menu item (%s) at line %d is invalid\n",
					  $1,@1.first_line);
				  $$=new_item("error",0,0,0);
				}
		;
option		:	label  COLUMNS  INT
				{ $$=new_item($1,0,COLUMNS,(void*)$3); }
		|	label  TITLE  pin
				{ $$=new_item($1,0,TITLE,(void*)$3); }
		;
label		:	LABEL
		|	'<'  LABEL  '>'
				{ $$=$2; $2[0] |= 0x80; /* what a hack */ }
	        |	'<'  error  '>'
				{ printf("Bad icon-label at line %d\n",
					  @1.first_line);
				  $$="Bad Label";
				}
		;
olvwm_cmd	:	BACK_SELN
				{ $$=BACK_SELN; }
		|	EXIT
				{ $$=EXIT; }
		|	EXIT_NO_CONFIRM
				{ $$=EXIT_NO_CONFIRM; }
		|	FLIPDRAG
				{ $$=FLIPDRAG; }
		|	FLIPFOCUS
				{ $$=FLIPFOCUS; }
		|	FULL_RESTORE_SIZE_SELN
				{ $$=FULL_RESTORE_SIZE_SELN; }
		|	NOP
				{ $$=NOP; }
		|	OPEN_CLOSE_SELN
				{ $$=OPEN_CLOSE_SELN; }
		|	QUIT_SELN
				{ $$=QUIT_SELN; }
		|	PROPERTIES
				{ $$=PROPERTIES; }
		|	REFRESH
				{ $$=REFRESH; }
		|	REREAD_MENU_FILE
				{ $$=REREAD_MENU_FILE; }
		|	RESTART
				{ $$=RESTART; }
		|	SAVE_WORKSPACE
				{ $$=SAVE_WORKSPACE; }
		|	SEPARATOR
				{ $$=SEPARATOR; }
		|	START_DSDM
				{ $$=START_DSDM; }
		|	STICK_UNSTICK_SELN
				{ $$=STICK_UNSTICK_SELN; }
		|	STOP_DSDM
				{ $$=STOP_DSDM; }
		|	WINMENU
				{ $$=WINMENU; }
		|	WMEXIT
				{ $$=WMEXIT; }
		;
olvwm_cmd_arg	:	DIRMENU EXEC
				{ $$=new_item(NULL,0,DIRMENU,$2); }
		|	MOVE_DESKTOP INT
				{ $$=new_item(NULL,0,MOVE_DESKTOP,(void*)$2); }
		|	WINMENU	INT
				{ $$=new_item(NULL,0,WINMENU,(void*)$2); }
		;
%%
/*
   The openwin-menu syntax is somewhat loose, in that
   the options TITLE, COLUMNS etc may appear anywhere in a
   submenu, even between other menu items. This is convinient from
   the point of view of our parser,
   Stylistically, it would be better to constrain options like TITLE
   to appearing before any menu commands, but we want to be "compatible".
*/

int main(int argc,char *argv[]) {
	char c;
	int result;
	while( (c=getopt(argc, argv,"v")) != EOF ) {
		switch(c) {
			case 'v':
				yydebug=1;
				break;
		}
	}
        if ( argc > optind ) {
                if ( (yyin = fopen(argv[optind],"r")) == 0 ) {
                        perror(argv[optind]);
                        exit(1);
                }
        }
	result = yyparse();
	read_menu_files();
#ifdef DEBUG
	datadump();
#endif
	dtwmrc(0);
	return(result);
}

int yywrap() {
	return(1);
}

int yyerror(char *err) {
	fprintf(stderr, "%s at line %d\n",err,yylloc.first_line);
}


struct menu_item *
new_item(char *label, int def, int type, void *arg) {
	struct menu_item *new;
	new = (struct menu_item *) malloc( sizeof(struct menu_item));
	new->label = label;
	new->type = type;
	new->def_item = def;
	new->arg = arg;
	return new;
}

struct menu *
new_menu() {
	struct menu *new;
	new = (struct menu *) malloc( sizeof(struct menu));
	new->name = NULL;
	new->title = NULL;
	new->parent_item = 0;
	new->optional = 0;
	new->pinnable = 0;
	new->columns = 0;
	new->first_item=0;
	new->next = 0;
	if( menu_list==0 ) {
		menu_list=new;
	} else {
		menu_list_end->next=new;
	}
	menu_list_end=new;

	return new;
}

void
add_item(struct menu *menu, struct menu_item *item) {
	if( item ) {
		switch( item->type ) {
		    case COLUMNS:
			menu->columns = item->arg.columns;
			free(item);
			break;
		    case TITLE:
			menu->title = item->label;
			menu->pinnable |= item->arg.pinnable;
			free(item);
			break;
		    default:
			if( menu->first_item==0 ) {
				menu->first_item=item;
			} else {
				menu->last_item->next = item;
			}
			menu->last_item=item;
		}
	}
}

datadump() {
	struct menu *menu_ptr;
	for(menu_ptr=menu_list; menu_ptr; menu_ptr=menu_ptr->next) {
		printf("Menu: %s %s %d cols\n",
			menu_ptr->title?menu_ptr->title:"(no title)",
			menu_ptr->pinnable? "PIN":"",
			menu_ptr->columns);
		for(itemptr=menu_ptr->first_item; itemptr; itemptr=itemptr->next) {
			printf("  Item: %-10s %s%s\n",
				itemptr->type>255? yytname[itemptr->type-255]:"???",
				itemptr->label?itemptr->label:"-",
				itemptr->def_item? " (*)": "" );
			switch( itemptr->type ) {
			case SUBMENU:
			case WINMENU:
			case MOVE_DESKTOP:
			case 0:
				break;
			case SEPARATOR:
				printf("   SEPARATOR\n");
				break;
			default:
				if( itemptr->arg.exec != 0)
					printf("   arg: %s\n",itemptr->arg.exec);
			}

		}
	}
}

char *env_subs(char *in);
int
read_menu_files() {
	struct menu *menu_ptr;
	char *file_name;
	int result;
	for(menu_ptr=menu_list; menu_ptr; menu_ptr=menu_ptr->next) {
		for(itemptr=menu_ptr->first_item; itemptr; itemptr=itemptr->next) {
			switch( itemptr->type ) {
			case MENU:
			case INCLUDE:
           		     file_name = expand_env(itemptr->arg.filename);
           		     if ( (yyin = fopen(file_name,"r")) == 0 ) {
               	                 perror(file_name);
				 result=1;
                             } else {
           		         result = yyparse();
			     }
               	             if ( result==0 ) {
               	                 itemptr->type = SUBMENU;
                                 itemptr->arg.submenu = top_menu;
                                 top_menu->name = itemptr->label;
                                 if (top_menu->title == NULL)
                                     top_menu->title = itemptr->label;
			     }
			     break;
			default:
			}
		}
	}
}

