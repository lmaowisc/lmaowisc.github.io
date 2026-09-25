# Editing and publishing the website

Open `Website.Rproj` in RStudio. This is a Quarto website; edit the source files, not the rendered files in `docs/`.

## Main files

- `index.qmd`: homepage text, social links, research interests, degrees, editorial roles, course highlights, and the two recent sections. The homepage uses an HTML block inside Quarto for precise layout; edit it in RStudio's Source mode.
- `home.css`: homepage layout, spacing, and responsive behavior.
- `people.qmd` and `people.css`: People page entries and styling. Current students use a three-column profile grid on desktop and stacked rows on smaller screens. Alumni use smaller names in the same dark color as current students, with muted details and semibold Dissertation and First placement labels. Preserve this visual distinction when adding entries. Student names link to a verified personal website, university profile, or LinkedIn profile, in that preference order; linked names turn red on hover. Alumni degree and first-placement rows use `alumni-meta` spans to display the same short muted red separators as the homepage. Keep first placements historical. Dissertation titles link to verified university records. Link alumni names directly to a verified personal website when available, otherwise to LinkedIn; do not add a separate LinkedIn label. These links also turn red on hover.
- `styles.css`: shared colors, fonts, navigation, and link styles.
- `research.qmd` and `research.css`: Research page content and styling. Use a full-width layout without a contents sidebar, aligned grant rows under Active Funding and Past Funding, and two columns for Collaborative Research. Match homepage typography and muted red metadata separators; keep modest extra space between sections and indent the collaboration topic lists with small disc bullets. CASCADE belongs under active funding with Lu Mao identified as UW sub-award PI; all PI labels use regular weight. Its description uses middle-dot separators without a forced line break. Agency images use `funding-agency-logo` to blend their white backgrounds into the page. Keep the vertical NHLBI and NCI variants in equal square frames, vertically centered beside the grant descriptions. NCI has a built-in frame; NHLBI uses a CSS border with inset padding. The user-supplied NCI square logo and source note are in `images/`.
- `_quarto.yml`: site navigation and Quarto settings. `docs/` is the output directory.
- `blog/index.qmd`, `blog/listing.ejs.md`, and `blog/blog.css`: the Blog index automatically lists published posts newest first, with figures on the left and titles, original publication dates, summaries, topic labels, and reading links on the right. The shared template and stylesheet handle the layout; do not add layout HTML to individual posts. White thumbnail backgrounds blend into the ivory page using CSS, preserving the original figure files. Increment the stylesheet version in the index after CSS changes.
- `courses.qmd` and `courses.css`: full-width University Teaching and Conference Workshops sections, with a short introduction, 170px badges, compact topic lists, dates beside titles, and course/workshop website links. Keep the content inside its explicit raw-HTML fence and retain the entry IDs and legacy section anchors. The approved Applied Survival Analysis badge is the navy-and-ivory `images/applied-survival-course.png`, also used in the homepage highlights; preserve the four other course/workshop badges. Increment the stylesheet version after CSS changes.
- `packages/index.qmd` and `packages/packages.css`: compact badge-and-text entries for WR and rmt, with a muted introduction and Website / CRAN / GitHub links. Keep HTML inside the explicit raw-HTML fence. The editable SVG badges in `packages/images/` have comparable finish to the course badges, with their own identities: WR remains a hexagonal patch using the package website's crimson palette and paired patient timelines, with the approved smaller DM Serif Display lettering stored as SVG outlines; rmt uses a teal hexagon and shaded step curves. Use a consistent modest display width, preserve each badge's aspect ratio, and keep lowercase `rmt`. Increment the stylesheet version after CSS changes and the image version when changing a badge. The former `packages/packages.yml` table is no longer used by this page.
- `pubs/index.qmd`, `pubs/methods.qmd`, `pubs/applications.qmd`: three separate publication pages with a shared All / Methods / Applications navigation row. All and Methods use year groups; Applications uses subject groups, with newest years first within each subject. Each `pub-entry` contains `pub-title`, `pub-authors`, and `pub-citation` blocks, plus `pub-resources` or `pub-note` when applicable. Keep all author markers and links when editing. Titles link to the paper; the year appears in the journal citation. Use `pub-resource` spans for supporting links and muted red separators. Shared presentation is in `pubs/pubs.css`, and the symbol legend is in `pubs/_legend.qmd`. Increment the stylesheet version in all three pages after CSS changes. Keep existing section IDs stable and update the mobile jump links when adding sections. Hide empty manuscript sections.
- `images/`: course badges and other source images; `dc.jpg` is the avatar.

The homepage has one recent methods paper, one recent application paper, and two blog posts. Select papers assigned to a journal issue with final volume and page numbers or article numbers; do not feature online-first papers awaiting those details. Order the two papers newest first by journal issue date, regardless of category, keeping that date only in each `data-publication-date` attribute (use `YYYY-MM` when only the issue month is known). If both papers have the same issue month, list the methods paper first. Under each paper title, show the author line from the Publications page in muted text, with Lu Mao’s name emphasized and corresponding-author markers preserved, then the journal, year, volume(issue), and page range or article number; omit category labels and full publication dates. Update these entries in `index.qmd` as needed. Paper-title links should use `https://doi.org/...`. Keep editorial start dates and `present` current; keep each date's `–present` suffix in its own span so the dashes and end labels align vertically.

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

### Add or update a blog post

1. Copy `blog/_post-template.qmd` to a new name such as `blog/regression-confidence-bands.qmd`. Keep the starter's leading underscore so Quarto excludes the starter itself from rendering and the listing.
2. Set the title, short description, categories, and optional image. Set `date` to the actual original publication date in `YYYY-MM-DD` form. Never use `today` or `last-modified` for this field; never change it just because you rebuild a post. An optional `date-modified` can record a substantive update separately.
3. Write normal Markdown and R chunks, then set `draft: false` when ready. The existing posts intentionally disable most code execution and include saved figures/output; keep their execution settings unless you mean to rerun the analysis.
4. Render the post and then rebuild the index:

```sh
quarto render blog/regression-confidence-bands.qmd
quarto render blog/index.qmd
```

There is no manual list of posts to update. Review and publish the source plus generated `docs/` files as below. Existing post filenames are their permanent URLs; retain them when updating a post.

`blog/_metadata.yml` applies `blog/post.css` to posts automatically. That shared stylesheet keeps article titles, native section headings, and subheadings compact and regular-weight in the site's serif font, with smaller muted subtitles. Keep presentation out of individual post files, and increment the stylesheet version in `_metadata.yml` after CSS changes. The Blog index uses its separate listing styles, loaded last, including its page-title size.

For new ggplot figures, a transparent background works naturally with the page color. After constructing your plot `p`, use:

```r
p <- p + theme(
  plot.background = element_rect(fill = "transparent", color = NA),
  panel.background = element_rect(fill = "transparent", color = NA)
)
ggsave("images/your-figure.png", plot = p, bg = "transparent")
```

This example assumes execution from the post's directory, Quarto's default. Keep an explicit seed for reproducible simulations.

`blog/regression-confidence.qmd` is a complete reproducible example: its R chunks generate the two illustrations in `blog/images/regression-confidence/`, and check the analytic quadratic limits against `predict.lm()`. It needs ggplot2 plus Quarto's usual knitr/rmarkdown dependencies. Its fixed publication date is September 25, 2026. The longer code remains folded using native Quarto chunk options.

### Publish the website

1. Before editing, pull the latest GitHub changes. If Git reports local changes, commit or stash those changes before pulling; do not discard them.
2. Edit the Quarto/CSS source and run `quarto render`.
3. Review `git status` and the changes. Commit both the source and generated `docs/` files.
4. Push to the existing `master` branch. GitHub Pages publishes the contents of `docs/` through the existing setup.

Use RStudio's Git pane or your usual Git client. No JavaScript framework or package installation is required for the homepage. The design uses system fonts and the existing badge files.

When changing `home.css`, increment its version in the `css:` field of `index.qmd` if visitors continue to see a cached version.

Shared desktop page widths and navigation typography are controlled in `styles.css`. External web links open in a new tab through the Quarto `link-external-newwindow` setting and `newpagelink.lua`; homepage HTML links also declare their targets explicitly. Social icons retain the original Bootstrap, Academicons (Google Scholar and ResearchGate), and Font Awesome (square X) outlines. All seven are inline SVGs in `index.qmd`, normalized to a shared 24-unit canvas and equal CSS dimensions; ResearchGate is raised slightly for visual balance. They use `currentColor` so their fill changes with the link text. Social links use the original gray (`#4e5862`) and outlined rectangles; icon, text, and border turn Wisconsin red together on hover or keyboard focus.
