-module(ph).

-export([read/1,
         read_temp_dependent/2,
         continous/1,
         cancel/1,
         recv_measurement/0,
         reset/1,
         calibrate_ph4/1,
         calibrate_ph7/1,
         calibrate_ph10/1]).

-include("ph.hrl").

%% @doc Read a single measurement
read(Port) ->
  atlas:read(Port),
  recv_measurement().

%% @doc Instruct circuit to send continous measurements.
%%      Use recv_measurement/0 to receive them.
continous(Port) ->
  atlas:continous(Port).

%% @doc Intruct circuit to cancel continous measurements.
cancel(Port) ->
  atlas:cancel(Port).

%% @doc Receive a single measurement
recv_measurement() ->
  case atlas:recv() of
    {error, timeout}   -> {error, timeout};
    {ok, ?CHECK_PROBE} -> {error, check_probe};
    {ok, MeasureData}  -> {ok, erlang:binary_to_float(MeasureData)}
  end.

%% @doc Take a single temperature dependent reading.
%%      The new temperature will be used in all consecutive calls to measure
%%      the pH. 
read_temp_dependent(Port, Temp) when Temp < 100 ->
  TempStr = erlang:float_to_binary(Temp, [{decimals, 2}, compact]),
  atlas:command(Port, TempStr),
  recv_measurement().

%% @doc Calibrate to ph4
calibrate_ph4(Port) ->
  atlas:command(Port, ?CALIBRATE_PH4).

%% @doc Calibrate to ph7
calibrate_ph7(Port) ->
  atlas:command(Port, ?CALIBRATE_PH7).

%% @doc Calibrate to ph10
calibrate_ph10(Port) ->
  atlas:command(Port, ?CALIBRATE_PH10).

%% @doc Return the circuit to factory settings.
reset(Port) ->
  atlas:reset(Port),  
  case atlas:recv() of
    {error, timeout} -> {error, timeout};
    {ok, ?RESET}     -> ok
  end. 

