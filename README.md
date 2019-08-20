# WhateverTheWeather

A client for the DarkSky API, to be used to include local weather data in my terminal prompt.

## How to use this

- A clone and `stack build` should produce a binary
- Run that binary, it'll create a placeholder config file and error out
- Edit the config file at `~/.wtw/config` to include your API key and lat/lon
- Copy the binary to a sensible location (like `/usr/local/bin/`)
- Copy the scripts in `scripts/` to `~/.bin/` or similar
- Update your PS1 (in your bashrc or similar) to include something like the following

```
PS1="\$(~/.bin/ps1Weather)"
```

- Set up a cronjob to run the `updateWeather` script with whatever frequency you like

## TODO

- Implement the updateWeather script alongside the rest of the Haskell
- Implement some command to actually display the alert contents and/or URIs
- All the configurability
