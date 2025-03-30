# nitro_secret

A secrets management library for Erlang

(originally built for the [Nitrogen Web Framework](https://nitrogenproject.com))

## Build

```bash
make
```

## Add as dependency

Modify your `rebar.config` to add `nitro_secret` to your `deps` section:

```erlang
{deps, [
    nitro_secret
]}
```

And add `nitro_secret` to your application's `.app` or `.app.src` file in the
`applications` section:

```erlang
{application, your_app, [
    ...
    {applications,[
        kernel,
        stdlib,
        ...
        nitro_secret
    ]},
    ...
]}
```

## Build your secrets file

Place your secrets in an Erlang config file (a file that can be loaded with
[`file:consult/1`](https://www.erlang.org/doc/apps/kernel/file.html#consult/1)).

Here is a [sample secrets file](https://github.com/nitrogen/nitro_secret/blob/master/priv/sample_secrets.config).

## Specify the location of the secrets file

In your `app.config` (or `etc/nitro_secret.config`, if you're using a Nitrogen
release), add the following rule:

```erlang
[
    {nitro_secret, [
        {secrets_filename, "/path/to/your/secrets.config"}
    ]}
].
```

Here is a sample [nitro_secret.config](https://github.com/nitrogen/nitro_secret/blob/master/priv/nitro_secret.config)

Alternatively, you don't have to specify a `secrets_filename` if you want to
save your secrets to the default location.

The default location is: `$(HOME)/.nitrogen/SHORT_NODE_NAME.secrets.config`

`SHORT_NODE_NAME` is the portion of the return value of `node()` before the
`@`. For example is your node name is named `my_app@127.0.0.1`, then
`SHORT_NODE_NAME` will be `my_app`.

## Usage

With all this done, you can then lookup secrets with:

- `nitro_secret:get(Key, Default)`.
- `nitro_secret:get(Key)`. (a shortcut for `nitro_secret:get(Key, undefined)`).

## Author

Copyright 2025 Jesse Gumm
Apache 2.0 Liccense
