export const flags = ({ env }) => {
  return {
    game: JSON.parse(localStorage.getItem('game') || null)
  }
}

export const onReady = ({ app }) => {

  // TOUCH CONTROLS
  if (app.ports && app.ports.touch) {
    const threshold = 50
  
    let start
    window.addEventListener('touchstart', (e) => {
      if (e.changedTouches && e.changedTouches[0]) {
        const touch = e.changedTouches[0]
        start = {
          x: touch.clientX,
          y: touch.clientY,
        }
      }
    })
    window.addEventListener('touchend', (e) => {
      if (start !== undefined) {
        if (e.changedTouches && e.changedTouches[0]) {
          const touch = e.changedTouches[0]
          const end = {
            x: touch.clientX,
            y: touch.clientY,
          }
          if (Math.abs(end.x - start.x) > threshold || Math.abs(end.y - start.y) > threshold) {
            app.ports.touch.send({
              dx: end.x - start.x,
              dy: end.y - start.y
            })
          }
          start = undefined
        }
      }
    })
  }

  // SAVE GAME TO LOCAL STORAGE FOR SAFE REFRESH
  if (app.ports && app.ports.saveGame) {
    app.ports.saveGame.subscribe(game => {
      localStorage.setItem('game', JSON.stringify(game))
    })
  }

}