-module(commit).

-export([new/3, author/1, action/1, parents/1, affects_file/2]).

-type action() :: {add | delete, string()} | init | merge.
-opaque commit() ::
  #{ author => atom()
   , action => action()
   , parents => [commit()]
   }.
-export_type([commit/0]).

-spec new(atom(), action(), [commit()]) -> commit().
new(Author, Action, Parents) ->
  #{author => Author, action => Action, parents => Parents}.

author(#{author := Author}) -> Author.
action(#{action := Action}) -> Action.
parents(#{parents := Parents}) -> Parents.

affects_file(File, #{action := {_, File}}) -> true;
affects_file(_, _) -> false.
