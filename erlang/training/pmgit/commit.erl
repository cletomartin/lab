-module(commit).

-export([new/3, author/1, action/1, parents/1, affects_file/2]).

-type action() :: {add | delete, string()} | init | merge.
-opaque commit() :: {commit, atom(), action(), [commit()]}.
-export_type([commit/0]).

-spec new(atom(), action(), [commit()]) -> commit().
new(Author, Action, Parents) ->
  {commit, Author, Action, Parents}.

author({commit, Author, _, _}) -> Author.
action({commit, _, Action, _}) -> Action.
parents({commit, _, _, Parents}) -> Parents.

affects_file(File, {commit, _, {_, File}, _}) -> true;
affects_file(_, _) -> false.
