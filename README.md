### rtrace
A wrapper for [recon](https://github.com/ferd/recon/) library. This application also exposes a REST api for call tracing. Although `recon` supports various trace patterns, this api allows on tracing function calls with just module and function name for a maximum of 100 calls.

### Usage
Add `rtrace` as a dependency in `rebar.config`
```erlang
{deps, [
	{rtrace, "<latest release>"}
	]}.
```

By default, the application listens on port 15000 for incoming api requests. This port can be customized by setting `http_port` value in `sys.config`
```erlang
[
	{rtrace, [
		{http_port, 17000} %% another port
		]}
].
```

### Contributing
