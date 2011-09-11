% Demo Parameterizaed Module
% It is similar to OOP

-module(oolite, [Name]).
-export([get_name/0]).

get_name() -> 
    Name.

% erlang compiler will automatically generate a 'new' method
% You can use oolite:new("xxx") to create an module instance. 
% > I = oolite.new("Dennis").
% > I:get_name()
% "Dennis"


