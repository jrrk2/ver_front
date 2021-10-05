//*****************************************************************************
//
//    <vscr - Verilog converter to abc format.>
//    Copyright (C) <2011,2012>  <Jonathan Richard Robert Kimmitt>
//    Derived from verilator/src/verilog.y bison grammar code by Wilson Snyder
//
//    This program is free software: you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation, either version 3 of the License, or
//    (at your option) any later version.
//
//    This program is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with this program.  If not, see <http://www.gnu.org/licenses/>.
//
//*****************************************************************************

// Generic void
%token EMPTY
%token ENDOFFILE
%token <string> IDSTR
%token <string> P_TIMESCALE
%token <string> PREPROC

%start start

%type <token list*token> start

%%

//**********************************************************************
//

start:		idlst ENDOFFILE				{ $1,ENDOFFILE }

idlst:          IDSTR                                   { [IDSTR $1] }
        |       idlst IDSTR                             { let s = $2 in (* output_string stdout s; *) IDSTR s :: $1 }
