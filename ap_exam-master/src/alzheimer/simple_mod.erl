https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
-module(simple_mod).
-export([init/0, handle_read/2, handle_write/2]).

% should return {ok, State :: term()}.
init() ->
    {ok, []}.

% Should return {reply, Reply :: term()}
% or return stop if the server should be stopped.
handle_read(Request, State) ->
    {reply, lists:nth(Request, State)}.

% Should return one of the following values
% - {noupdate, Reply :: term()} if the operation does not result in a updated state,
% – {updated, Reply :: term(), NewState :: term()} if the operation results in the
%   updated state NewState,
% – stop if the server should be stopped.
handle_write(Request, State) ->
    {updated, inserted, [Request | State]}.
