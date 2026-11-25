# Biosecurity Dashboard

From the repository root:

```r
shiny::runApp("shiny_biosecurity", port = 3840)
```

Choose ASEAN (the default) or All countries, choose one to five consecutive days (including both dates), and click Fetch articles. The app requests each day sequentially for daily articles, then each day sequentially for clusters. A five-day window makes ten daily requests before any retries. Ranges longer than five days and future dates are rejected. All countries omits the API location filter. Country membership is also checked locally against returned location IDs and the workbook's country/province relationships, because the daily-articles endpoint can return foreign-only records despite `locationIds`. Records without a confirmed selected-country location are excluded from restricted country views. Original location tags remain intact.

**Exclude stories also tagged to other countries** is checked by default. For ASEAN, this keeps records with at least one ASEAN country and no identified country outside ASEAN. Clear it to include multi-country stories with any ASEAN location; the **matched_countries** column shows the selected countries responsible for each match. Selected countries uses the same rules. Changing the country selection filters loaded results immediately; when the fetched scope differs, the status asks you to fetch again for complete results. Dates require another fetch. The status distinguishes fetched scope from displayed scope.

Tags match any selected exact tag and filter the already-fetched daily articles and clusters without additional requests. An empty tag selection includes every returned record. Tag choices come from the BlueDot tag catalog and returned records; tags may also be typed manually. CSV downloads export the active tab with current country, disease, classification, and tag filters. Fetch status retains the API's original record counts before local filtering.

AMS location IDs and both biosecurity endpoints match `bluedot_biosecurity.qmd`, which uses the same helper file. The IDs now come from the bundled workbook catalog. Choose **Selected countries** to search 250 country names/IDs and request a custom selection; fetching requires at least one selected country. Cluster-source articles follow the filtered parent clusters and include **cluster_matched_countries**. The source-article payload has no individual location tags, so this column describes cluster context and does not independently verify the source article's country. Source-article dates can precede the cluster window. Cluster snapshots retain the requested date to preserve source links across days. Fetch status distinguishes empty results, failed requests, and possible truncation.

**Disease classification** offers human diseases, animal diseases, or both. Only the newest `disease-lookup_...` export is used; the old `DiseaseID` sheet is ignored. Its 309 records contain 258 human entries (`isAnimalDisease = FALSE`) and 51 animal entries (`TRUE`). Twelve IDs, including Anthrax, have both classifications and remain available in either scope.

**Diseases (any match)** lists the IDs in the selected classification, with classification labels beside their names. There are 297 distinct IDs across both scopes. Changing classification updates the choices and removes selections outside that scope. Classification, disease, and tag filters apply to already-fetched articles and clusters without another API request; cluster sources follow the matching clusters. An empty disease selection includes all diseases in the selected classification. Export names are preserved, including `Mpox`; older names or aliases are not automatically mapped.

Daily articles, clusters, and their CSV exports include `disease_classification` (`Human`, `Animal`, `Human; Animal`, or `Unclassified`). Labels reflect catalog membership based on returned disease names, not an article's affected host. Unknown or absent disease names remain visible in the default all-diseases scope but do not match a human-only or animal-only filter. The workbook is not needed at runtime. To refresh the bundled R lookups, run `Rscript scripts/build_bluedot_lookups.R` from the repository root; see the root README for reusable R functions.

Set `BLUEDOT_BIOSECURITY_API_KEY` (fallback: `BLUEDOT_API_KEY`) in the server environment or private app-local `secrets.Renviron`. The key is never exposed in the UI. See `secrets.Renviron.example`. The loader also checks the user, repository, and app `.Renviron` files; existing process environment variables take precedence. A key authorized for travel APIs might not authorize biosecurity.

Dashboard tables use `DT` with search, column filters, sorting, pagination, and horizontal/vertical scrolling. Table bodies are limited to 55% of the viewport height with headers kept above the scrolling rows. Select All in the page-size menu to scroll through every matching record. The CSV button exports all rows matching the tag selection in the active tab; DT search and column filters affect only the displayed table.

Dependencies: `shiny`, `bslib`, `tidyverse`, `DT`, `httr`, `jsonlite`; the Quarto report additionally uses `gt` and `here`.

## Upload to a Posit Cloud RStudio project

Prepare the upload ZIP from the repository root:

```sh
Rscript scripts/prepare_biosecurity_posit_cloud.R
```

This creates `output/biosecurity_posit_cloud/YYYYMMDD_biosecurity_posit_cloud.zip`. It includes the app, its helper, package setup, an RStudio project file, and the private `secrets.Renviron`. The archive is ignored by Git and contains your API key; keep the uploaded project private. The example file contains only a placeholder.

1. At <https://posit.cloud>, create a **New RStudio Project**.
2. In the RStudio **Files** pane, click **Upload**, select the ZIP, and upload it. RStudio extracts the files into the project folder. Confirm `app.R`, `setup.R`, and `secrets.Renviron` are beside the `R/` folder.
3. Run these commands in the RStudio console:

```r
source("setup.R")
shiny::runApp()
```

The setup script installs missing packages. The app reads the uploaded key on the server. Choose countries and dates, then click **Fetch articles**. If the files were extracted into a subfolder, open `shiny_biosecurity.Rproj` there before running the commands.

This runs the app inside your RStudio project. Posit Cloud retired standalone app publishing; use [Posit Connect Cloud](https://docs.posit.co/connect-cloud/how-to/r/shiny-r.html) for a hosted app URL. See [Posit Cloud's publishing retirement notice](https://docs.posit.co/cloud/whats_new/#sunsetting-publishing-feature) and [RStudio project documentation](https://docs.posit.co/cloud/guide/projects/).

## Deploy to shinyapps.io

Configure your shinyapps.io account locally with `rsconnect::setAccountInfo()`. Put the valid biosecurity key in the private `shiny_biosecurity/secrets.Renviron`, then deploy from the repository root:

```r
rsconnect::deployApp(
  appDir = "shiny_biosecurity",
  appFiles = c("app.R", "R/biosecurity.R", "R/bluedot_lookups.R",
               "R/bluedot_lookup_data.R", "secrets.Renviron"),
  appName = "bluedot-biosecurity"
)
```

This uploads the private credential file to the Shiny server, not to browser assets. Keep it out of `www/` and version control. Local environment variables alone are not copied to shinyapps.io. Deployment is a separate action; running locally does not publish the app.
