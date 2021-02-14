BASEDIR = $(shell pwd)
REBAR = rebar3

APPNAME = $(shell basename $(BASEDIR))
ifneq ($(APPNAME), rtrace)
  APPNAME = rtrace
endif

LINTERS = check-deps lint xref eunit

REGRESSION_TEST_DIR := test/
REGRESSION_TEST_FILES := $(shell find $(REGRESSION_TEST_DIR) -name '*_SUITE.erl' -exec basename '{}' \;)
REGRESSION_TEST_SUITES := $(shell echo $(REGRESSION_TEST_FILES) | sed -e 's/.erl//g' | sed -e 's/ /,/g')

compile: ## compile
	$(REBAR) compile

eunit: ## run eunit tests
	$(REBAR) eunit

xref: ## xref analysis
	$(REBAR) xref

dialyzer: ## dialyzer
	$(REBAR) dialyzer

check-deps: ## check dependencies
	$(REBAR) check-deps

lint: ## lint
	$(REBAR) lint

test: $(LINTERS) ## run ct test suites
	$(REBAR) ct -v \
	--dir $(REGRESSION_TEST_DIR) \
	--suite=$(REGRESSION_TEST_SUITES) \
	--include $(BASEDIR)/include \
	--sys_config $(BASEDIR)/test/stubs/sys.config

console: test ## launch a shell
	$(REBAR) shell

clean: ## clean
	$(REBAR) clean
	rm -rf _build

test-coverage-report: ## generate test coverage report
	$(REBAR) cover --verbose

help: ## Display help information
	@grep -E '^[a-zA-Z0-9_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
