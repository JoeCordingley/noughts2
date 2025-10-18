'use strict';
import Navigo from 'navigo'
import Mustache from 'mustache'

const router = new Navigo('/')
const app = document.getElementById('app')!
declare const htmx: any;

import gameTemplate from '../templates/game.mustache?raw'

async function loadPage(path: string) {
  const res = await fetch(path)
  if (!res.ok) {
    app.innerHTML = `<h1>404 Not Found</h1>`
    return
  }
  const html = await res.text()
  app.innerHTML = html
  router.updatePageLinks() // rebind <a> tags after load
  htmx.process(app)
}


router
  .on('/', () => loadPage('/src/tigris.html'))
  .on('/games', () => loadPage('/src/tigris.html'))
  .on('/games/tigris', () => loadPage('/src/tigris.html'))

if (import.meta.env.DEV) {
  const { default: devTemplate } = await import('@shared/templates/dynastyChoice.mustache?raw')
  const values = [ 
    {dynasty: "Archer", player: "Alice", taken : true, mine : false},
    {dynasty: "Bull", player: "Bob", taken : true, mine : false},
    {dynasty: "Pot", player: "Charlie", taken : true, mine : true},
    {dynasty: "Lion", taken : false, mine : false}
  ]
  router.on('/games/tigris/dev', () => {
    app.innerHTML = Mustache.render(devTemplate, {values})
  })
}

router.on('/games/tigris/:gameId', ({ data }) => {
  app.innerHTML = Mustache.render(gameTemplate, { gameId: data.gameId })
  router.updatePageLinks()
  htmx.process(app)
})

router.notFound(() => {
  app.innerHTML = `<h1>404 - Page not found</h1>`
})

router.resolve()
