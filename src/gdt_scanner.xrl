Definitions.
D = [0-9]
UP = [A-Z]
LOW = [a-z]
L = [a-zA-Z]
WS = [\r\s\t\n]

Rules.

% comments
--[^\n]*\n         : skip_token.

% keywords
import        : {token,{import  , TokenLine}}.
module        : {token,{module  , TokenLine}}.
where         : {token,{where   , TokenLine}}.
data          : {token,{data    , TokenLine}}.
type          : {token,{type    , TokenLine}}.
required      : {token,{required, TokenLine}}.
optional      : {token,{optional, TokenLine}}.

% special
\:\:          : {token,{'::', TokenLine}}.
\-\>          : {token,{'->', TokenLine}}.
\<\-          : {token,{'<-', TokenLine}}.
\|            : {token,{'|', TokenLine}}.
\=            : {token,{'=', TokenLine}}.
\{            : {token,{'{', TokenLine}}.
\}            : {token,{'}', TokenLine}}.
\[            : {token,{'[', TokenLine}}.
\]            : {token,{']', TokenLine}}.
\<            : {token,{'<', TokenLine}}.
\>            : {token,{'>', TokenLine}}.
\:            : {token,{':', TokenLine}}.
\,            : {token,{',', TokenLine}}.
\(            : {token,{'(', TokenLine}}.
\)            : {token,{')', TokenLine}}.

% smth else =)
{UP}({L}|{D})*    : {token,{uname  , TokenLine, list_to_binary(TokenChars)}}.
{LOW}({L}|{D}|_)* : {token,{lname  , TokenLine, list_to_binary(TokenChars)}}.
{D}+              : {token,{integer, TokenLine, list_to_integer(TokenChars)}}.
{WS}+             : skip_token.

Erlang code.
