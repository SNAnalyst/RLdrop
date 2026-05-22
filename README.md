# RLdrop

Direct Dropbox API v2 access vanuit R. Bypassed de Dropbox desktop client sync queue voor on-demand up- en downloads.

## Installatie

```r
devtools::install_local("pad/naar/RLdrop.zip")
```

## Authenticatie

Dropbox gebruikt short-lived access tokens die na ~4 uur verlopen. RLdrop
gebruikt daarom een refresh token flow waarbij een permanent refresh token
automatisch nieuwe access tokens ophaalt.

### Eenmalige setup

**Stap 1: App aanmaken**

1. Ga naar https://www.dropbox.com/developers/apps
2. Klik **Create app** → **Scoped access** → **Full Dropbox**
3. Geef de app een naam
4. Tabblad **Permissions**: vink `files.content.read` en `files.content.write` aan
5. Noteer je **App key** en **App secret** (tabblad **Settings**)

**Stap 2: Refresh token ophalen**

Open in je browser (vervang `JOUW_APP_KEY`):

```
https://www.dropbox.com/oauth2/authorize?client_id=JOUW_APP_KEY&response_type=code&token_access_type=offline
```

Klik op toestaan en kopieer de authorization code. Voer dan in R uit:

```r
app_key    <- "jouw_app_key"
app_secret <- "jouw_app_secret"
auth_code  <- "de_code_uit_de_browser"

response <- httr::POST(
  "https://api.dropbox.com/oauth2/token",
  httr::authenticate(app_key, app_secret),
  body = list(code = auth_code, grant_type = "authorization_code"),
  encode = "form"
)

cat("Refresh token:", httr::content(response)$refresh_token)
```

**Stap 3: Credentials opslaan in `.Renviron`**

```r
usethis::edit_r_environ()
```

Voeg toe:

```
DROPBOX_APP_KEY=jouw_app_key
DROPBOX_APP_SECRET=jouw_app_secret
DROPBOX_REFRESH_TOKEN=jouw_refresh_token
```

Herstart R.

## Gebruik

```r
# Token ophalen (doet automatisch een API-aanroep)
token <- RLdrop::dropbox_token()

# Downloaden
RLdrop::dropbox_download_file("/data/bestand.parquet", "D:/lokaal/bestand.parquet", token)
RLdrop::dropbox_download_folder("/data/parquet", "D:/lokaal/parquet", token)
RLdrop::dropbox_download_folder("/data", "D:/lokaal/data", token, recursive = TRUE)

# Uploaden (automatische keuze gewone/chunked)
RLdrop::dropbox_upload("D:/data/bestand.parquet", "/data/bestand.parquet", token)
RLdrop::dropbox_upload_folder("D:/data/parquet", "/data/parquet", token)
RLdrop::dropbox_upload_folder("D:/data", "/data", token, recursive = TRUE)
```

## Functies

| Functie | Omschrijving |
|---|---|
| `dropbox_token()` | Haal een geldig access token op via refresh token |
| `dropbox_download_file()` | Download één bestand |
| `dropbox_download_folder()` | Download een map (optioneel recursief) |
| `dropbox_upload()` | Upload één bestand (automatische keuze gewone/chunked) |
| `dropbox_upload_file()` | Upload één bestand < 150MB |
| `dropbox_upload_large_file()` | Upload één bestand via chunked session |
| `dropbox_upload_folder()` | Upload een map (optioneel recursief) |

## Afhankelijkheden

- `httr`
- `jsonlite`
