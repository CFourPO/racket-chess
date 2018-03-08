#lang racket

(module chess racket

  (struct/lens game (white black turn))

  (struct/lens piece (loc type moves has-moved?))
  
  )