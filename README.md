# 2048
A clone of the game 2048 made with Elm.

### To run dev environment:
```
npm install
npm run watch
open build/index.html
```

### To deploy to Github Pages:
// TODO: update this
```shell
elm make src/Main.elm --output=main.js
git checkout gh-pages
git add index.html main.js
git commit -m "Deploy new version to Github Pages"
git push
```
I really should automate this.
