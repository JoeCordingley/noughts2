// main.ts
import Navigo from 'navigo'

const router = new Navigo('/')

router
  .on('/', () => {
    document.body.innerHTML = `<h1>Home</h1><a href="/games">Go to games</a>`
  })
  .on('/games', () => {
    document.body.innerHTML = `
      <h1>Games</h1>
      <ul>
        <li><a href="/games/tigris">Tigris</a></li>
      </ul>`
  })
  .on('/games/tigris', () => {
    document.body.innerHTML = `
      <h1>Tigris</h1>
      <a href="/games/tigris/42">Open game 42</a>`
  })
  .on('/games/tigris/:gameId', ({ data }) => {
    const { gameId } = data
    document.body.innerHTML = `<h1>Game ${gameId}</h1>
      <a href="/games/tigris">Back</a>`
  })
  .resolve()
