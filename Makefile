PROJECT = atlas-sci

DEPS = erlang_ale erlang-serial
dep_erlang-serial = https://github.com/emfa/erlang-serial.git master
dep_erlang_ale = https://github.com/esl/erlang_ale.git master

include erlang.mk
