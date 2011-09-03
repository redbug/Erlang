-module(tree).
-export([empty/0, insert/3, lookup/2]).

% generator a root node
empty() -> {node, 'nil'}.

% Base Case: insert into a empty node(root node).
insert(Key, Val, {node, 'nil'}) ->
	{node, {Key, Val, {node, 'nil'}, {node, 'nil'}}};

% insert to the left node
insert(NewKey, NewVal, {node, {Key, Val, Smaller, Larger}}) when NewKey < Key ->
	{node, {Key, Val, insert(NewKey, NewVal, Smaller), Larger}};

% insert to the right node
insert(NewKey, NewVal, {node, {Key, Val, Smaller, Larger}}) when NewKey > Key ->
	{node, {Key, Val, Smaller, insert(NewKey, NewVal, Larger)}};

% if NewKey =:= Key, replace the Key with NewKey.
insert(Key, Val, {node, {Key, _, Smaller, Larger}}) ->
	{node, {Key, Val, Smaller, Larger}}.


% Base Case: The key isn't in the tree.
lookup(_, {node, 'nil'}) ->
	undefined;

% Match the key.
lookup(Key, {node, {Key, Val, _, _}}) ->
	{ok, Val};

% The search key is smaller than NodeKey.
lookup(Key, {node, {NodeKey, _, Smaller, _}}) when Key < NodeKey ->
	lookup(Key, Smaller);

lookup(Key, {node, {NodeKey, _, _, Larger}}) when Key > NodeKey ->
	lookup(Key, Larger).

