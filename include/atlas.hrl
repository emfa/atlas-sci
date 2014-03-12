%% Default timeout to wait for a reply
-define(TIMEOUT, 10000).

%% Default port on a raspberry pi
-define(DEFAULT_PORT, "/dev/ttyAMA0").

%% Common commands between all circuits
-define(DEBUG_ON,  <<"L1">>).
-define(DEBUG_OFF, <<"L0">>).
-define(READ,      <<"R">>).
-define(CONTINOUS, <<"C">>).
-define(CANCEL,    <<"E">>).
-define(RESET,     <<"X">>).
-define(VERSION,   <<"I">>).
