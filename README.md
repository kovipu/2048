# 2048
A clone of the game 2048 made with Elm.

### To run dev environment:
```
elm reactor
```

### To deploy to Github Pages:
```shell
elm make src/Main.elm --output=main.js
git checkout gh-pages
git add index.html main.js
git commit -m "Deploy new version to Github Pages"
git push
```
I really should automate this.
