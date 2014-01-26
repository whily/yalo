#summary Pseudo Instructions of Assembly.
#labels Phase-Implementation
#sidebar SBX64Assembly

<wiki:toc max_depth="1" />

Directives (e.g. bits, origin) are categorized as pseudo instructions
here too.

= bits: Bit Mode =

|| Instruction || Description           ||
|| bits 16     || 16-bit mode           ||
|| bits 64     || 64-bit mode (default) ||

--------

= db: Declaring initialized byte value(s) =

|| Instruction                         || Description || 
|| db [_var_] [_number_ | _string_]    || Declare byte values via atoms ||
|| db [_var_] ([_number_ | _string_]*) || Declare byte values via lists ||

== Note ==

 * _number_ should be in range -128..+255. Expression within the same
range can be used as well.

 * _string_ is converted byte by bytes.

= dw: Declaring initialized word value(s) =

|| Instruction            || Description                    ||
|| dw [_var_] _number_    || Declare word value via numbers ||
|| dw [_var_] (_number_)* || Declare word value via lists   ||

== Note ==

 * _number_ should be in range -32,768..+65,535. Expression within the same
range can be used as well.

= dd: Declaring initialized doubleword value(s) =

|| Instruction            || Description                          ||
|| dd [_var_] _number_    || Declare doubleword value via numbers ||
|| dd [_var_] (_number_)* || Declare doubleword value via lists   ||

== Note ==

 * _number_ should be in range -2,147,483,648..+4,294,967,295.
Expression within the same range can be used as well.

= dq: Declaring initialized quadword value(s) =

|| Instruction            || Description                          ||
|| dq [_var_] _number_    || Declare quadword value via numbers ||
|| dq [_var_] (_number_)* || Declare quadword value via lists   ||

== Note ==

 * _number_ should be in range 
-9,223,372,036,854,775,808..+18,446,744,073,709,551,615.
Expression within the same range can be used as well.

--------

= equ: Defint Constant =

|| Instruction        || Description                               ||
|| equ _const_ _expr_ || Define constant _const_ with value _expr_ ||

== Note ==

_expr_ should be evaulated when encoutering the instruction,
i.e. forward reference is not allowed.

--------

= org: Define Origin =

|| Instruction || Description             ||
|| org val     || Define origin to be val || 

== Note ==

*org* should precede any (pseudo) instructions that actually generating code.

--------

= times: Repeat instruction =

|| Instruction                 || Description                             ||
|| times _count_ _instruction_ || Assemble _count_ times of _instruction_ ||

== Note ==

 * _count_ could be an expression which can be evaluated when *times*
instruction is encoutered, e.g. containing $, $$, or labels defined
before *times* instruction.
 * _instruction_ can be any other (pseudo) instructions.

