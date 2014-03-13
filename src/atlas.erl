%% @doc Common functions for atlas scientific circuits
-module(atlas).

-export([new/0,
         close/1,
         debug_on/1,
         debug_off/1,
         read/1,
         continous/1,
         cancel/1,
         version/1,
         reset/1,
         command/2,
         recv/0,
         recv/1]).

-include("atlas.hrl").

%% @doc Start a new serial port process at the default port.
new() -> 
  new(?DEFAULT_PORT).

%% @doc Start a new serial port.
new(SerialPort) ->
  serial:start([{speed, 38400}, {open, SerialPort}, {send, <<"\r">>}]).

%% @doc Close the serial port process.
close(Port) ->
  Port ! stop.

%% @doc Send debug on command
debug_on(Port) ->
  command(Port, ?DEBUG_ON).

%% @doc Send debug off command
debug_off(Port) ->
  command(Port, ?DEBUG_OFF).

%% @doc Send read command
read(Port) ->
  command(Port, ?READ).

%% @doc Send continous read command
continous(Port) ->
  command(Port, ?CONTINOUS).

%% @doc Send a cancel command
cancel(Port) ->
  command(Port, ?CANCEL).

%% @doc Send a reset command
reset(Port) ->
  command(Port, ?RESET).

%% @doc Return the version of the circuit.
version(Port) ->
  command(Port, ?VERSION),
  case recv() of
    {ok, VersionData} -> {ok, parse_versiondata(VersionData)};
    Err               -> Err
  end.

%% @doc Send a command with a line-feed appended
command(Port, Command) ->
  Port ! {send, [Command, <<"\r">>]}.

%% @doc Receive a response from a circuit, remove the line feed.
%%      Or if no response is received return a timeout error.
recv(Timeout) ->
  receive
    {data, Bytes} ->
      [Response, <<>>] = binary:split(Bytes, <<"\r">>),
      {ok, Response}
  after Timeout   ->
    {error, timeout}
  end.

%% @doc Same as recv/1 but use a default value for timeout.
recv() ->
  recv(?TIMEOUT).

%% @doc Parse version data.
%%      Returns a triple: {Circuit, Version, Date}
parse_versiondata(VersionData) ->
  [CData, VData, DData] = binary:split(VersionData, <<",">>, [global]),
  {parse_circuit(CData), parse_version(VData), parse_date(DData)}.

%% @doc Parse circuit identifier
parse_circuit(<<"P">>) -> ph;
parse_circuit(<<"E">>) -> ec;
parse_circuit(<<"D">>) -> do;
parse_circuit(<<"O">>) -> orp.

%% @doc Parse version
parse_version(<<"V", Version/binary>>) ->
  [Major, Minor] = binary:split(Version, <<".">>),
  {erlang:binary_to_integer(Major), erlang:binary_to_integer(Minor)}.

%% @doc Parse date
parse_date(Date) ->
  [Month, Year] = binary:split(Date, <<".">>),
  {Year + 2000, Month, 1}.
