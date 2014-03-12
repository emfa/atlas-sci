%% @doc Common functions for atlas scientific circuits
-module(atlas).

-export([new/0,
         close/1,
         debug_on/1,
         debug_off/1,
         version/1,
         read/1,
         continous/1,
         reset/1,
         command/2,
         recv/0,
         recv/1]).

-define(TIMEOUT, 10000).

%% @doc Start a new process that is connected to the serial port.
new() -> serial:start([{speed, 38400}, {open, "/dev/ttyAMA0"}]).

%% @doc Close the serial port process.
close(Port) -> Port ! stop.

%% @doc Send debug on command
debug_on(Port) -> command(Port, "L1").

%% @doc Send debug off command
debug_off(Port) -> command(Port, "L0").

%% @doc Send read command
read(Port) -> command(Port, "R").

%% @doc Send continous read command
continous(Port) -> command(Port, "C").

%% @doc Send reset command
reset(Port) -> command(Port, "X").

%% @doc Return the version of the circuit.
version(Port) ->
  command(Port, "I"),
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
      [Response, <<>>] = binary:part(Bytes, 0, byte_size(Bytes)-1),
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


