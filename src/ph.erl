-module(ph).

-export([read/1,
         read_temp_dependent/2,
         recv_measurement/0,
         reset/1]).

-define(TIMEOUT, 10000).

%% @doc Read a single measurement
read(Port) ->
  atlas:read(Port),
  recv_measurement().

%% @doc Receive a single measurement
recv_measurement() ->
  case atlas:recv() of
    {error, timeout}        -> {error, timeout};
    {ok, <<"check probe">>} -> {error, check_probe};
    {ok, Data}              -> {ok, erlang:binary_to_float(Data)}
  end.

%% @doc Take a single temperature dependent reading.
%%      The new temperature will be used in all consecutive calls to measure
%%      the pH. 
read_temp_dependent(Port, Temp) when Temp < 100 ->
  TempStr = erlang:float_to_list(Temp, [{decimals, 2}, compact]),
  atlas:command(Port, TempStr),
  recv_measurement().

%% @doc Return the circuit to factory settings.
reset(Port) ->
  atlas:reset(Port),  
  case atlas:recv() of
    {error, timeout}  -> {error, timeout};
    {ok, <<"reset">>} -> ok
  end. 

