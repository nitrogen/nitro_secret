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

## Build your secrets file

Place your secrets in an Erlang config file (a file that can be loaded with
[`file:consult/1`](https://www.erlang.org/doc/apps/kernel/file.html#consult/1)).

Here is a [sample config file](sample_secrets.config).

## Specify the location of the secrets file

In your `app.config` (or `etc/nitro_secret.config`, if you're using a Nitrogen release), add the following rule:

```erlang
[
    {nitro_secret, [
        {secrets_filename, "/path/to/your/secrets.config"}
    ]}
].
```

* `$(HOME)/.nitrogen/APP_NAME.config` (the default behavior - app name is the short name of the node, `specifically, the return value of `application`), or


## Usage



