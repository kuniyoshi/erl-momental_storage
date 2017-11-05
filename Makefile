PROJECT = momental_storage

DEPS = cowboy
LOCAL_DEPS = ssl
dep_cowboy_commit = master

DEP_PLUGINS = cowboy

include ./erlang.mk
