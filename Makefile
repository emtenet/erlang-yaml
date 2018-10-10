# See LICENSE for licensing information.

PROJECT = yaml
PROJECT_DESCRIPTION = YAML decoder
PROJECT_VERSION = 0.1.0

TEST_DEPS = solarized
dep_solarized = git https://github.com/emtenet/erlang-solarized master

EUNIT_OPTS = no_tty, {report, {solarized_eunit, []}}
COVER = 1

# dialyzer is slower but can build the PLT on memory constrained machines
DIALYZER_PLT_OPTS = --no_native

include erlang.mk

