# Advanced R Programming Site

Course website source for `ETC4500/ETC5450 Advanced R Programming`, maintained in the fork [`adhni/arp`](https://github.com/adhni/arp).

## What Is In This Repo

- Quarto source for the public course site
- generated site output in [`docs/`](./docs/)
- weekly pages in [`week1/`](./week1/) to [`week12/`](./week12/)
- assignment pages in [`assignments/`](./assignments/)

## Local Workflow

Render the website without rebuilding slide PDFs:

```bash
R_PROFILE_USER=/dev/null RENV_CONFIG_AUTOLOADER_ENABLED=FALSE quarto render --profile noslides
```

Render a single page while iterating:

```bash
R_PROFILE_USER=/dev/null RENV_CONFIG_AUTOLOADER_ENABLED=FALSE quarto render index.qmd
```

Open the generated site locally:

```bash
open docs/index.html
```

## Notes

- The main site configuration lives in [`_quarto.yml`](./_quarto.yml).
- The `noslides` profile avoids LaTeX/PDF slide builds when only the HTML site needs to be updated.
- Week-specific homepage content is sourced from metadata in each `week*/index.qmd` file.

## Fork

This working copy is intended to stay on the GitHub fork [`@adhni`](https://github.com/adhni), not to open changes against the upstream course repository unless explicitly chosen later.
