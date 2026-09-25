# Editing and publishing the website

Open `Website.Rproj` in RStudio. This is a Quarto website; edit the source files, not the rendered files in `docs/`.

## Main files

- `index.qmd`: homepage text, social links, research interests, degrees, editorial roles, course highlights, and the two recent sections. The homepage uses an HTML block inside Quarto for precise layout; edit it in RStudio's Source mode.
- `home.css`: homepage layout, spacing, and responsive behavior.
- `styles.css`: shared colors, fonts, navigation, and link styles.
- `_quarto.yml`: site navigation and Quarto settings. `docs/` is the output directory.
- `courses.qmd`: course and workshop listings.
- `pubs/index.qmd`, `pubs/methods.qmd`, `pubs/applications.qmd`: publication lists.
- `images/`: course badges and other source images; `dc.jpg` is the avatar.

The homepage has two recent methods papers and two blog posts. Update the short entries in `index.qmd` as needed. Paper-title links should use `https://doi.org/...`. Keep editorial start dates and `present` current.

## Preview and build

Use Quarto in RStudio or the Terminal:

```sh
quarto preview
quarto render
```

To rebuild only the homepage:

```sh
quarto render index.qmd
```

The `resources` setting ensures that `home.css` is copied to `docs/`, including when its HTML reference contains a cache version such as `?v=5`.

## Publish

1. Before editing, pull the latest GitHub changes. If Git reports local changes, commit or stash those changes before pulling; do not discard them.
2. Edit the Quarto/CSS source and run `quarto render`.
3. Review `git status` and the changes. Commit both the source and generated `docs/` files.
4. Push to the existing `master` branch. GitHub Pages publishes the contents of `docs/` through the existing setup.

Use RStudio's Git pane or your usual Git client. No JavaScript framework or package installation is required for the homepage. The design uses system fonts and the existing badge files.

When changing `home.css`, increment its version in the `css:` field of `index.qmd` if visitors continue to see a cached version.
