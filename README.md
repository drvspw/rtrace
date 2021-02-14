### rtrace
A wrapper for [recon](Erlang call tracer) library. The application also exposes a REST api for call tracing. Although `recon` supports various trace patterns, this api allows on tracing function calls with just module and function name for a maximum of 100 calls.

### Usage
Add `rtrace` as a dependency in `rebar.config`
```erlang
{deps, [
	{rtrace, "<latest release>"}
	]}.
```

If not using `hex.pm`
```erlang
{deps, [
	{rtrace, {git, "https://github.com/drvspw/rtrace.git", {tag, "<latest release>"}}}
	]}.
```

### Contributing
