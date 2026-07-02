# RLdrop 0.6.1

## Bugfixes

* `.win_long_path()` is nu gevectoriseerd, zodat `file.info()`-aanroepen met
  meerdere paden tegelijk (o.a. in `dropbox_upload_folder()`) niet meer falen op
  Windows. Eerder gaf dit de fout `'length = 3' in coercion to 'logical(1)'`.

# RLdrop 0.6.0

## Nieuwe functionaliteit

* `pattern`- en `welke`-argumenten voor selectief up-/downloaden en verwijderen.
* Long-path-ondersteuning op Windows: paden langer dan 250 tekens krijgen
  automatisch de `\\?\`-prefix zodat het 260-tekenlimiet vervalt.
* `overwrite`-logica (o.a. `"newer"`, `"missing"`, `"always"`) bij downloaden en
  uploaden.
