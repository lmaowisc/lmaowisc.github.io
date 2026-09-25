# Editing and publishing the website

Open `Website.Rproj` in RStudio. This is a Quarto website; edit the source files, not the rendered files in `docs/`.

## Main files

- `index.qmd`: homepage text, social links, research interests, degrees, editorial roles, course highlights, and the two recent sections. The homepage uses an HTML block inside Quarto for precise layout; edit it in RStudio's Source mode.
- `home.css`: homepage layout, spacing, and responsive behavior.
- `people.qmd` and `people.css`: People page entries and styling. Current students use a three-column profile grid on desktop and stacked rows on smaller screens. Alumni use smaller names in the same dark color as current students, with muted details and semibold Dissertation and First placement labels. Preserve this visual distinction when adding entries. Student names link to a verified personal website, university profile, or LinkedIn profile, in that preference order; linked names turn red on hover. Alumni degree and first-placement rows use `alumni-meta` spans to display the same short muted red separators as the homepage. Keep first placements historical. Dissertation titles link to verified university records. Link alumni names directly to a verified personal website when available, otherwise to LinkedIn; do not add a separate LinkedIn label. These links also turn red on hover.
- `styles.css`: shared colors, fonts, navigation, and link styles.
- `research.qmd` and `research.css`: Research page content and styling. Use a full-width layout without a contents sidebar, aligned grant rows under Active Funding and Past Funding, and two columns for Collaborative Research. Match homepage typography and muted red metadata separators; keep modest extra space between sections and indent the collaboration topic lists with small disc bullets. CASCADE belongs under active funding with Lu Mao identified as UW sub-award PI; all PI labels use regular weight. Its description uses middle-dot separators without a forced line break. Agency images use `funding-agency-logo` to blend their white backgrounds into the page. Keep the vertical NHLBI and NCI variants in equal square frames, vertically centered beside the grant descriptions. NCI has a built-in frame; NHLBI uses a CSS border with inset padding. The user-supplied NCI square logo and source note are in `images/`.
- `_quarto.yml`: site navigation and Quarto settings. `docs/` is the output directory.
- `courses.qmd`: course and workshop listings.
- `packages/index.qmd` and `packages/packages.css`: compact badge-and-text entries for WR and rmt, with a muted introduction and Website / CRAN / GitHub links. Keep HTML inside the explicit raw-HTML fence. The editable SVG badges in `packages/images/` have comparable finish to the course badges, with their own identities: WR uses the package website's crimson wordmark and paired patient timelines; rmt uses a teal hexagon and shaded step curves. Use a consistent modest display width, preserve each badge's aspect ratio, and keep lowercase `rmt`. Increment the stylesheet version after CSS changes and the image version when changing a badge. The former `packages/packages.yml` table is no longer used by this page.
- `pubs/index.qmd`, `pubs/methods.qmd`, `pubs/applications.qmd`: three separate publication pages with a shared All / Methods / Applications navigation row. All and Methods use year groups; Applications uses subject groups, with newest years first within each subject. Each `pub-entry` contains `pub-title`, `pub-authors`, and `pub-citation` blocks, plus `pub-resources` or `pub-note` when applicable. Keep all author markers and links when editing. Titles link to the paper; the year appears in the journal citation. Use `pub-resource` spans for supporting links and muted red separators. Shared presentation is in `pubs/pubs.css`, and the symbol legend is in `pubs/_legend.qmd`. Increment the stylesheet version in all three pages after CSS changes. Keep existing section IDs stable and update the mobile jump links when adding sections. Hide empty manuscript sections.
- `images/`: course badges and other source images; `dc.jpg` is the avatar.

The homepage has one recent methods paper, one recent application paper, and two blog posts. Select papers assigned to a journal issue with final volume and page numbers or article numbers; do not feature online-first papers awaiting those details. Order the two papers newest first by journal issue date, regardless of category, keeping that date only in each `data-publication-date` attribute (use `YYYY-MM` when only the issue month is known). If both papers have the same issue month, list the methods paper first. Under each paper title, show the journal, year, volume(issue), and page range or article number; omit category labels and full publication dates. Update these entries in `index.qmd` as needed. Paper-title links should use `https://doi.org/...`. Keep editorial start dates and `present` current; keep each date's `–present` suffix in its own span so the dashes and end labels align vertically.

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

Shared desktop page widths and navigation typography are controlled in `styles.css`. External web links open in a new tab through the Quarto `link-external-newwindow` setting and `newpagelink.lua`; homepage HTML links also declare their targets explicitly. Social icons retain the original Bootstrap, Academicons (Google Scholar and ResearchGate), and Font Awesome (square X) outlines. All seven are inline SVGs in `index.qmd`, normalized to a shared 24-unit canvas and equal CSS dimensions; ResearchGate is raised slightly for visual balance. They use `currentColor` so their fill changes with the link text. Social links use the original gray (`#4e5862`) and outlined rectangles; icon, text, and border turn Wisconsin red together on hover or keyboard focus.
