PROJECT = basic_web
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

DEPS = cowboy
dep_cowboy_commit = 2.13.0

REL_DEPS += relx

DEP_PLUGINS = cowboy

LOCAL_DEPS = crypto
LOCAL_DEPS += mnesia

include erlang.mk
