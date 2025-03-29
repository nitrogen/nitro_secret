%% nitro_secret for the nitrogen web framework
%% Copyright 2025 Jesse Gumm


-module(nitro_secret).
-export([get/1, get/2]).

-define(CACHE_NAME, ?MODULE).
-define(TTL, 1000).

secrets_filename() ->
    nitro_cache:get(?CACHE_NAME, ?TTL, secrets_filename, fun() ->
        case config(secrets_filename) of
            undefined -> default_secrets_file();
            File -> File
        end
    end).

default_secrets_file() ->
    NodeName = node_name(),
    {ok, [[Home]]} = init:get_argument(home),
    NitrogenDir = ".nitrogen",
    Filename = atom_to_list(NodeName) ++ ".config",
    filename:join([Home, NitrogenDir, Filename]).

node_name() ->
    Node1 = atom_to_list(node()),
    Node2 = hd(string:split(Node1, "@")),
    list_to_atom(Node2).

get(Key) ->
    get(Key, undefined).

get(Key, Default) ->
    Vals = get_all_values(),
    ds:get(Vals, Key, Default).

get_all_values() ->
    LookupFun = fun() ->
        File = secrets_filename(),
        case file:consult(File) of
            {ok, Body} ->
                Body;
            {error, enoent} ->
                logger:warning(not_found_msg(), [File, File]),
                []
        end
    end,
    nitro_cache:get(?CACHE_NAME, ?TTL, secrets_content, LookupFun).

config(Key) ->
    config(Key, undefined).

config(Key, Default) ->
    Apps = [nitro_secret, nitrogen_core, nitrogen],
    config(Apps, Key, Default).

config([], _Key, Default) ->
    Default;
config([App|Apps], Key, Default) ->
    case application:get_env(App, Key, undefined) of
        undefined -> config(Apps, Key, Default);
        Val -> Val
    end.

not_found_msg() ->
    "nitro_secret: Attempting to read secrets from non-existant file: ~ts.~n"
    "You have two options:~n"
    "  1: Add the secrets file at ~ts, or~n"
    "  2: Add a 'secrets_filename' to your app.config that points directly to the secrets file~n"
    "     Example:~n"
    "       [~n"
    "         {nitro_secret, [~n"
    "           {secrets_filename, \"/home/joe/.config/my_app_secrets.config\"}~n"
    "         ]}~n"
    "       ]~n"
    "~n"
    "Ensure this file exists, is in a format readable by file:consult/1, and the data~n"
    "is either a list of key-value tuples, or a map (or other format readable by~n"
    "ds_erlang [https://github.com/choptastic/erlang_ds]).~n"
    "  Example: ~n"
    "    {some_api_key, \"ABC123789XYZ\"}.~n"
    "    {another_api_key, \"QWERT-12345-ASDFG\"}.~n".
