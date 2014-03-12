all: erl.mk

erl.mk:
	    wget -nv -O erl.mk 'https://raw.github.com/fenollp/erl-mk/master/erl.mk' || rm erl.mk

DEPS = erlang_ale erlang-serial
dep_erlang-serial = https://github.com/tonyg/erlang-serial.git master
dep_erlang_ale = https://github.com/esl/erlang_ale.git master

include erl.mk

# Your targets after this line.
#
# ## Redefine an erl.mk target here to implement special behavior!
