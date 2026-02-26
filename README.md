# D&D Character Builder

A type-safe D&D 5e (2024) character creator built with Scala.js and Tyrian. All game rules follow the [Systems Reference Document](https://5e24srd.com/) so character creation enforces valid choices at the type level — species, classes, backgrounds, ability scores, skills, and feats.

## Run Locally

**Requirements:** [Node.js](https://nodejs.org/), [sbt](https://www.scala-sbt.org/), JDK 21+.

Terminal 1 — dev server:
```sh
yarn install
yarn start
```

Terminal 2 — Scala.js compiler:
```sh
sbt fastLinkJS
```

Open [http://localhost:1234](http://localhost:1234). Re-run `fastLinkJS` after code changes; the browser will hot-reload.
