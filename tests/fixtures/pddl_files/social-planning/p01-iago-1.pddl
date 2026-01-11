
;; An instance of the social planning domain, based on Iago's problem:
;; How to make Othello kill Desdemona?

(define (problem Iago-1)
  (:domain social-planning)

  (:objects
   Iago Othello Emilia Desdemona Cassio - character
   handkerchief - item
   garden bedroom residence palace - place
   )

  (:init
   (man Iago)
   (man Othello)
   (woman Emilia)
   (woman Desdemona)
   (man Cassio)
   (married Iago Emilia)
   (married Emilia Iago)
   (married Othello Desdemona)
   (married Desdemona Othello)
   (friend-of Cassio Iago)
   (friend-of Othello Iago)
   (friend-of Desdemona Emilia)
   (precious handkerchief)
   (loves Emilia Iago)
   (loves Othello Desdemona)
   (loves Desdemona Othello)
   (is-curious Othello)
   (is-jealous Othello)
   (is-suspicious Othello)
   (is-wrathful Othello)
   (is-curious Cassio)
   (is-greedy Cassio)
   (is-lustful Cassio)
   (is-wrathful Cassio)
   (is-obedient Emilia)
   (is-vain Emilia)
   (is-curious Desdemona)
   (is-obedient Desdemona)
   (is-evil Iago) ;; MUHAHAHAHA!!
   (main-character Iago)
   (alive Iago)
   (alive Othello)
   (alive Emilia)
   (alive Desdemona)
   (alive Cassio)
   (at Iago garden)
   (at Othello palace)
   (at Desdemona bedroom)
   (at Emilia garden)
   (at Cassio residence)
   (at handkerchief bedroom)
   (gift Othello Desdemona handkerchief)
   )

  (:goal (killed Othello Desdemona))

  )
