%%%-------------------------------------------------------------------
%%% @author proctor
%%% @copyright (C) 2015, proctor
%%% @doc
%%%
%%% @end
%%% Created : 2015-02-06 12:15:05.506471
%%%-------------------------------------------------------------------
-module(markov_generator).

-behaviour(gen_server).

-export([start_link/0,
         generate_chain/2,
         parse_text_from_file/1,
         parse_text/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, pid()}
                      | ignore
                      | {error, {already_started, pid()}}
                      | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


-spec generate_chain(string(), non_neg_integer()) -> string().
generate_chain(FirstWord, Length) ->
    gen_server:call(?MODULE, {generate_chain, FirstWord, Length}).

-spec parse_text_from_file(string()) -> ok.
parse_text_from_file(FileName) ->
    gen_server:call(?MODULE, {parse_text_from_file, FileName}).

-spec parse_text(string()) -> ok.
parse_text(Text) ->
    gen_server:call(?MODULE, {parse_text, Text}).




%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
        {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec handle_call({generate, string(), non_neg_integer()}, _From, #state{}) -> {reply, ok, #state{}}
                ;({parse_text, string()}, _From, #state{}) -> {reply, string(), #state{}}.
handle_call({generate_chain, FirstWord, Length}, _From, State) ->
    NextWordFunction = fun markov_word:pick_next_word_after/1,
    Reply = generate(NextWordFunction, Length, [FirstWord]),
    {reply, Reply, State};
handle_call({parse_text_from_file, FileName}, _From, State) ->
    {ok, Content} = file:read_file(FileName),
    Text = binary_to_list(Content),
    [FirstWord | Words] = tokenize(Text),
    load_words(FirstWord, Words),
    {reply, ok, State};
handle_call({parse_text, Text}, _From, State) ->
    [FirstWord | Words] = tokenize(Text),
    load_words(FirstWord, Words),
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
        {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
        {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
        ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
        {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec tokenize(string()) -> [string(), ...].
tokenize(Text) ->
    string:tokens(Text, " \t\n\r").

-spec load_words(nonempty_string(), [nonempty_string(), ...]) -> ok.
load_words(_Word, []) ->
    ok;
load_words(Word, [Following | Words]) ->
    markov_word:add_following_word(Word, Following),
    load_words(Following, Words).

-spec generate(fun((string()) -> string()), non_neg_integer(), [string()]) -> string().
generate(_NextWord, 1, Words) ->
    WordListOrdered = lists:reverse(Words),
    string:join(WordListOrdered, " ");
generate(NextWord, Length, Words=[Word | _]) ->
    Next = NextWord(Word),
    case Next of
        undefined -> generate(NextWord, 1, Words);
        _ -> generate(NextWord, Length - 1, [Next | Words])
    end.



-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

generate_test() ->
    IncrementalGenerator = fun([Char]) -> [Char + 1] end,
    ?assertMatch("a b c",
                 generate(IncrementalGenerator, 3, ["a"])),

    ?assertMatch("a b c d e f g h i j k l m n o p q r s t u v w x y z",
                 generate(IncrementalGenerator, 26, ["a"])).

-endif.
