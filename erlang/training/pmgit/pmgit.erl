-module(pmgit).

-export([test/0]).

-export([init/1, add/3, delete/3, merge/3, blame/2]).

test() ->
  % Mike initializes the repo
  Repo00 = pmgit:init(mike),
  % on brujo's branch, he adds files 1 and 2
  BruRepo01 = pmgit:add(brujo, "file1", Repo00),
  BruRepo02 = pmgit:add(brujo, "file2", BruRepo01),
  % on robert's branch, he adds files 3 and 4
  RobRepo01 = pmgit:add(robert, "file3", Repo00),
  RobRepo02 = pmgit:add(robert, "file4", RobRepo01),
  % Mike merges the branches
  MrgRepo01 = pmgit:merge(mike, BruRepo02, RobRepo02),
  % Now brujo removes and re-adds file4
  MrgRepo02 = pmgit:delete(brujo, "file4", MrgRepo01),
  MrgRepo03 = pmgit:add(brujo, "file4", MrgRepo02),
  % Who modified each file?
  noone = pmgit:blame("file3", Repo00),
  brujo = pmgit:blame("file1", BruRepo01),
  noone = pmgit:blame("file2", BruRepo01),
  brujo = pmgit:blame("file2", BruRepo02),
  brujo = pmgit:blame("file1", MrgRepo01),
  robert = pmgit:blame("file4", MrgRepo01),
  brujo = pmgit:blame("file4", MrgRepo02),
  brujo = pmgit:blame("file4", MrgRepo03),
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% V1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% init(Author) -> {commit, Author, init, []}.
% add(Author, File, Commit) -> {commit, Author, {add, File}, [Commit]}.
% delete(Author, File, Commit) -> {commit, Author, {delete, File}, [Commit]}.
% merge(Author, Commit1, Commit2) -> {commit, Author, merge, [Commit1, Commit2]}.

% blame(_File, {commit, _Author, init, _}) -> noone;
% blame(File, {commit, Author, {_, File}, _}) -> Author;
% blame(File, {commit, _Author, {_, _File}, [Commit]}) -> blame(File, Commit);
% blame(File, {commit, _Author, merge, [C1, C2]}) ->
%   case blame(File, C1) of
%     noone -> blame(File, C2);
%     Author -> Author
%   end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% V2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% init(Author) -> commit:new(Author, init, []).
% add(Author, File, Commit) ->
%   commit:new(Author, {add, File}, [Commit]).
% delete(Author, File, Commit) ->
%   commit:new(Author, {delete, File}, [Commit]).
% merge(Author, Commit1, Commit2) ->
%   commit:new(Author, merge, [Commit1, Commit2]).

% blame(File, Commit) ->
%   case commit:affects_file(File, Commit) of
%     true -> commit:author(Commit);
%     false ->
%       case commit:parents(Commit) of
%         [] -> noone;
%         [C1] -> blame(File, C1);
%         [C1, C2] ->
%           case blame(File, C1) of
%             noone -> blame(File, C2);
%             Author -> Author
%           end
%       end
%   end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% V3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% blame(File, Commit) -> do_blame(File, [Commit]).

% do_blame(_, []) -> noone;
% do_blame(File, [Commit | Commits]) ->
%   case commit:affects_file(File, Commit) of
%     true -> commit:author(Commit);
%     false ->
%       case do_blame(File, commit:parents(Commit)) of
%         noone -> do_blame(File, Commits);
%         Author -> Author
%       end
%   end.
