# dnd_builder

A project scaffold for building **Scala.js + Tyrian** single-page web apps with an Elm-style architecture.

Clone, run the setup wizard, and start building.

## Quick Start

```sh
git clone https://github.com/YOUR_USER/tyrian-scaffold.git my-app
cd my-app
./setup.sh
```

The wizard will ask for:
1. **Project name** and **Scala package** -- renames everything automatically
2. **Optional modules** to include or exclude:
   - NES.css retro theme (vs. modern minimal CSS)
   - LocalStorage persistence (Circe JSON)
   - PDF generation (jsPDF)
   - Backend placeholder (http4s)

After setup, the wizard re-initialises git with a clean first commit.

## Run Locally

**Requirements:** [Node.js](https://nodejs.org/) (for yarn/npm), [sbt](https://www.scala-sbt.org/), JDK 21+.

Terminal 1 -- dev server:
```sh
yarn install
yarn start
```

Terminal 2 -- Scala.js compiler:
```sh
sbt
sbt:...> fastLinkJS
```

Open [http://localhost:1234](http://localhost:1234). After code changes, run `fastLinkJS` again and the browser will hot-reload.

## Project Structure

```
├── .cursor/rules/          Cursor AI rules (architecture, pitfalls, patterns)
├── .github/workflows/      GitHub Actions (deploy to Pages)
├── css/                    Stylesheets (NES.css or modern)
├── deploy/                 Production script block for index.html
├── project/                sbt configuration (plugins, version)
├── src/main/scala/
│   ├── App.scala           TyrianIOApp entry point
│   ├── Screen.scala        Screen trait, ScreenId, RootMsg, navigation
│   ├── ScreenRegistry.scala
│   ├── StoredEntities.scala    (if LocalStorage module)
│   ├── common/
│   │   ├── CmdUtils.scala      IO command helpers
│   │   ├── LocalStorageUtils.scala (if LocalStorage module)
│   │   ├── nescss/NesCss.scala (if NES.css module)
│   │   └── pdf/                (if PDF module)
│   └── screens/
│       ├── HomeScreen.scala    Hello World landing page
│       ├── AboutScreen.scala   Stack info + cache refresh
│       ├── NotesScreen.scala   (if LocalStorage: CRUD demo)
│       ├── ScreenHeader.scala
│       ├── GalleryLayout.scala
│       └── GalleryEmptyState.scala
├── build.sbt
├── index.html
├── package.json
├── sw.js                   Service worker (cache busting via __BUILD_TS__)
├── sw-register.js
├── export-wrapper.js       Dev entry: imports Scala.js output + launches app
├── setup.sh                Interactive setup wizard (self-deleting)
└── README.md
```

## Architecture

This scaffold uses the **Tyrian** framework (Elm architecture for Scala.js):

- **Model / Update / View** -- each screen has its own `Model`, `Msg`, `init`, `update`, `view`
- **Type-erased screen registry** -- screens are plugged in via `ScreenRegistry`; the root app dispatches messages
- **Navigation** -- screens emit `NavigateNext(screenId, output)` to switch; data flows via `ScreenOutput`
- **No URL routing** -- uses `Routing.none`; all state is in-memory (and optionally LocalStorage)

To add a new screen:
1. Add a `ScreenId` case object in `Screen.scala`
2. Create a `Screen` implementation (object with `init`, `update`, `view`)
3. Register it in the `ScreenRegistry` list in `App.scala`

## Optional Modules

### NES.css (retro theme)
Pixel-art CSS framework with `NesCss` constants for class names. If removed, a modern minimal CSS alternative is used. All components work with either theme.

### LocalStorage (persistence)
`LocalStorageUtils` provides `save`/`load`/`saveList`/`loadList` backed by Circe JSON. The `NotesScreen` is a working CRUD demo with pagination and delete confirmation.

### PDF (jsPDF)
Instruction-based PDF generation. `PdfUtils.generateSamplePdf(title)` creates a simple PDF. The `JsPDF` object handles all jsPDF API access.

### Backend (http4s)
Minimal http4s + Ember server with a `/api/health` endpoint. Added as a separate sbt subproject (`server/`). Includes a deploy workflow stub.

## Deploy

Push to GitHub and enable Pages:

1. Go to **Settings > Pages**
2. Set **Source** to **GitHub Actions**
3. Push to `main` -- the workflow builds, assembles `dist/`, and deploys

The deploy workflow:
- Runs `sbt fullLinkJS` (optimised Scala.js output)
- Replaces dev scripts with production scripts (`deploy/prod-scripts.html`)
- Cache-busts the service worker with a build timestamp

## Cursor Rules

The scaffold includes `.cursor/rules/` files that help AI assistants (Cursor, Copilot, etc.) understand the project:

| Rule | Scope | What it covers |
|------|-------|----------------|
| `project-architecture.mdc` | Always | Stack, navigation, persistence, file layout |
| `pitfalls.mdc` | Always | Scala.js, Tyrian, jsPDF, deployment gotchas |
| `scala-scalafix.mdc` | `*.scala` | Linting rules (no vars, no nulls, no returns) |
| `tyrian-ui.mdc` | `*.scala` | Tyrian patterns (disabled attrs, onLoad, canvas) |
| `deployment.mdc` | Workflows | GitHub Pages, service worker, build process |
| `nescss-styling.mdc` | CSS + Scala | NES.css classes, cursor, theming, galleries |

## Libraries

| Library | Purpose |
|---------|---------|
| [Tyrian](https://github.com/PurpleKingdomGames/tyrian) | Elm-style UI for Scala.js |
| [Circe](https://circe.github.io/circe/) | JSON encoding / decoding |
| [NES.css](https://nostalgic-css.github.io/NES.css/) | Retro pixel-art CSS (optional) |
| [jsPDF](https://github.com/parallax/jsPDF) | Client-side PDF generation (optional) |
| [http4s](https://http4s.org/) | HTTP server (optional backend) |
| [Scala.js](https://www.scala-js.org/) | Scala compiled to JavaScript |
| [Parcel](https://parceljs.org/) | Dev server and bundler |
