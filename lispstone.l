#!/usr/local/bin/clisp
;;; Brandon Jank, LispStone

(load "game.l")

;; GLOBALS
(setq Turn 1)
(setq Energy 1)
(setq Minions '(
  ;(0 nil " " 0 1)
  ;(1 nil "Atom" 1 1)
  ;(2 nil "Cons Cell" 2 1)
))
(setq Monsters '(
  ;(0 nil " " 0 1)
  ;(1 nil "Atom2" 1 1)
  ;(2 nil "Cons Cell2" 2 1)
))
(setq tapped '())
(setq aitapped '())

(defun printState ()
  "Prints the state of the game."
  (progn
    (format t "--------------------------------------------~%")
    (format t "|                LISPSTONE                 |~%")
    (format t "--------------------------------------------~%")
    (format t "| Player Health: ~a~%" Health)
    (format t "|     AI Health: ~a~%" AIHealth)
    (format t "|          Turn: ~a~%" Turn)
    (format t "|        Energy: ~a~%" Energy)
    (format t "| Player Hand:~%|   (Cost, Spell?, Name, Attack, Health)") ; (LL2String Hand))
    (print Hand)
    (format t "~%| AI Hand:~%|   (Cost, Spell?, Name, Attack, Health)") ; (LL2String AIHand))
    (print AIHand)
    (format t "~%| Player Minions:~%|   (Cost, Spell?, Name, Attack, Health)") ; (LL2String Minions))
    (print Minions)
    (format t "~%| AI Monsters:~%|   (Cost, Spell?, Name, Attack, Health)") ; (LL2String Monsters))
    (print Monsters)
    (format t "~%--------------------------------------------~%")
    (format t "|    ENCASE ALL INPUT IN DOUBLE QUOTES!    |~%")
    (format t "--------------------------------------------~%")
  )
)

(defun longest-member (LL)
  "Finds the largest list in a list."
  (loop for x in LL
    maximizing (length x))
)

(defun LL2String (LL)
  "Prints a formatted list of lists."
  (if (or (null LL) (boundp 'LL)) (format nil "NONE~%")
    (format nil
      (format nil "~~{|~~{ ~~~ad~~}|~~%~~}" (longest-member LL))
      LL
    )
  )
)

(defun checkWin ()
  "Checks if player or AI is dead."
    (if (<= AIHealth 0)
      (progn
        (format t "~%~%The AI died, youre a winner!~%~%")
        (exit)
      )
      (if (<= Health 0)
        (progn
          (format t "~%~%You died, you lose!~%~%")
          (exit)
        )
      ))
)

(defun promptedread (s)
  "Displays a prompt and reads a string from the user."
  (format t "~a: " s) (read)
)

(defun quitGame ()
  "Exits the program."
  (progn
    (format t "~%~%Exiting the game...~%~%" nil)
    (exit)
  )
)

(defun playCard ()
  "Play a card from hand."
  (progn
    (if (or (null Hand) (< Energy 1))
      (progn
        (format t "~%~%ERROR: Can't play a card, not enough energy or hand is empty.~%~%")
        (return-from playCard nil)
      )
    )
    (loop named cardcheck
      do (progn
        (setq selection (promptedread "Type the name of the card to play > "))
        (if (cardExists selection t)
          (if (< Energy (cardEnergy selection t))
            (progn
              (format t "~%~%ERROR: Not enough energy to play that card.~%~%")
              (return-from playCard nil)
            )
            (return-from cardcheck t)
          )
          (progn
            (format t "~%~%ERROR: There is no card called '~a' in your hand.~%~%" selection)
            (return-from playCard nil)
          )
        )
      )
    )
    ; is spell?
    (if (equal (cardSpell selection t) t)
      (progn
        (loop named targetcheck
           do (progn
            (setq target (promptedread "Type the name of the target for the spell > "))
            (if (targetExists target)
              (return-from targetcheck t)
              (progn
                (format t "~%~%ERROR: Invalid target. Try ai, player, or a card name.~%~%" nil)
                (return-from playCard nil)
              )
            )
          )
        )
        ; is heal?
        (if (> (cardHealth selection t) 0)
          (progn
            (if (equalp target "player")
              (setq Health (+ Health (cardHealth selection t)))
              (if (equalp target "ai")
                (setq AIHealth (+ AIHealth (cardHealth selection t)))
                (healMinion selection target t)
              )
            )
            (format t "~%~%You healed '~a' with '~a' for ~a health.~%~%" target selection (cardHealth selection t))
          )
        )
        ; is dmg?
        (if (> (cardAttack selection t) 0)
          (progn
            (if (equalp target "player")
              (setq Health (- Health (cardAttack selection t)))
              (if (equalp target "ai")
                (setq AIHealth (- AIHealth (cardAttack selection t)))
                (damageMinion selection target t)
              )
            )
            (format t "~%~%You damaged '~a' with '~a' for ~a damage.~%~%" target selection (cardAttack selection t))
          )
        )
        ; cleanup
        (delEnergy selection t)
        (delCard selection t)
      )
    ; else is monster
      (progn
        (delEnergy selection t)
        (addMinion selection t)
        (delCard selection t)
        (addTapped selection t)
        (format t "~%~%You played a monster called ~a!~%~%" selection)
      )
    )
    ; check if monsters/minions are dead.
    (checkMinionDeath)
  )
)

(defun attackMonster ()
  "Attack with a monster."
  (progn
    (if (null Minions) (return-from attackMonster nil))
    (loop named minioncheck
       do (progn
        (setq selection (promptedread "Type the name of the minion to attack with > "))
        (if (targetExists selection)
          (if (not (isTapped selection t))
            (if (> (minionAttack selection t) 0)
              (return-from minioncheck t)
              (progn
                (format t "~%~%ERROR: The minion '~a' cannot attack, it's power is not greater than 0.~%~%" selection)
                (return-from attackMonster nil)
              )
            )
            (progn
              (format t "~%~%ERROR: The minion '~a' has already attacked or was summoned this turn.~%~%" selection)
              (return-from attackMonster nil)
            )
          )
          (progn
            (format t "~%~%ERROR: The minion '~a' is not in play.~%~%" selection)
            (return-from attackMonster nil)
          )
        )
      )
    )
    (loop named targetcheck
       do (progn
        (setq target (promptedread "Type the name of the target to attack > "))
        (if (targetExists target)
          (return-from targetcheck t)
          (progn
            (format t "~%~%ERROR: Invalid target '~a'. Try ai, player, or a monster name.~%~%" target)
            (return-from attackMonster nil)
          )
        )
      )
    )
    (if (returnCard Minions target)
      (progn
        (format t "~%~%ERROR: Attacking your own minon. Try ai, player, or a monster name.~%~%" target)
        (return-from attackMonster nil)
      )
    )

    (if (equalp target "player")
      (setq Health (- Health (minionAttack selection t)))
      (if (equalp target "ai")
        (setq AIHealth (- AIHealth (minionAttack selection t)))
        (damageMinion selection target t)
      )
    )
    (addTapped selection t)
    (format t "~%~%You attacked '~a' with '~a' for ~a damage.~%~%" target selection (minionAttack selection t))

    (checkMinionDeath)
  )
)

(defun checkMinionDeath ()
  "Checks if any minions died and removes them."
  (progn
    (loop for x in Minions
      when (< (nth 4 x) 1)
      do (progn
        (format t "~%~%The minion '~a' died!~%~%" (nth 2 x))
        (delMinion (nth 2 x) t)
      )
    )
    (loop for x in Monsters  ; x = (1 t "monster" 1 1)
      when (< (nth 4 x) 1)
      do (progn
        (format t "~%~%The monster '~a' died!~%~%" (nth 2 x))
        (delMinion (nth 2 x) nil)
      )
    )
  )
)

(defun delEnergy (str player)
  "Removes energy from player."
  (setq Energy (- Energy (cardEnergy str player)))
)

(defun healMinion (selection target player)
  (if (equalp player t)
    (progn
      (setq tempCard (returnCard Minions target))
      (setf (nth 4 tempCard) (+ (minionHealth target t) (cardHealth selection t) ))
      (push tempCard (cdr (nthcdr 0 Minions)))
      (setq Minions (remove (returnCard Minions target) Minions))
    )
    (progn
      (setq tempCard (returnCard Monsters target))
      (setf (nth 4 tempCard) (- (minionHealth target nil) (cardHealth selection nil) ))
      (push tempCard (cdr (nthcdr 0 Monsters)))
      (setq Monsters (remove (returnCard Monsters target) Monsters))
    )
  )
)

(defun damageMinion (selection target player)
  (if (equalp player t)
    (progn
      (setq tempCard (returnCard Monsters target))
      (setf (nth 4 tempCard) (- (minionHealth target nil) (minionAttack selection t) ))
      (setq tempCard2 (returnCard Minions selection))
      (setf (nth 4 tempCard2) (- (minionHealth selection t) (minionAttack target nil) ))
      (setq Monsters (remove (returnCard Monsters target) Monsters))
      (setq Minions (remove (returnCard Minions selection) Minions))
      ;(push tempCard (cdr (nthcdr 0 Monsters)))
      (if (null Monsters)
        (setf Monsters (cons tempCard nil))
        (push tempCard (cdr (nthcdr 0 Monsters)))
      )
      (if (null Minions)
        (setf Minions (cons tempCard2 nil))
        (push tempCard2 (cdr (nthcdr 0 Minions)))
      )
    )
    (progn
      (setf tempCard (returnCard Minions target))
      (setf (nth 4 tempCard) (- (minionHealth target t) (minionAttack selection nil) ))
      (setf tempCard2 (returnCard Monsters selection))
      (setf (nth 4 tempCard2) (- (minionHealth selection nil) (minionAttack target t) ))
      (setf Minions (remove (returnCard Minions target) Minions))
      (setf Monsters (remove (returnCard Monsters selection) Monsters))
      ;
      (if (null Minions)
        (setf Minions (cons tempCard nil))
        (push tempCard (cdr (nthcdr 0 Minions)))
      )
      (if (null Monsters)
        (setf Monsters (cons tempCard2 nil))
        (push tempCard2 (cdr (nthcdr 0 Monsters)))
      )
    )
  )
)

(defun returnCard (LL str)
  "Returns a card with the given name."
  (loop for x in LL
    when (equalp (nth 2 x) str)
    do (return x))
)

(defun cardEnergy (str player)
  "Returns the energy cost of a card."
  (if (equalp player t)
    (nth 0 (returnCard Hand str))
    (nth 0 (returnCard AIHand str))
  )
)

(defun cardSpell (str player)
  "Is the card a Spell?"
  (if (equalp player t)
    (if (equalp (nth 1 (returnCard Hand str)) t) t
      (if (equalp (nth 1 (returnCard Minions str)) t) t))
    (if (equalp (nth 1 (returnCard AIHand str)) t) t
      (if (equalp (nth 1 (returnCard Monsters str)) t) t))
  )
)

(defun cardAttack (str player)
  "Returns a card's attack value."
  (if (equalp player t)
    (nth 3 (returnCard Hand str))
    (nth 3 (returnCard AIHand str))
  )
)
(defun minionAttack (str player)
  "Returns a minion's attack value."
  (if (equalp player t)
    (nth 3 (returnCard Minions str))
    (nth 3 (returnCard Monsters str))
  )
)

(defun cardHealth (str player)
  "Returns a card's health value."
  (if (equalp player t)
    (nth 4 (returnCard Hand str))
    (nth 4 (returnCard AIHand str))
  )
)

(defun minionHealth (str player)
  "Returns a minions's health value."
  (if (equalp player t)
    (nth 4 (returnCard Minions str))
    (nth 4 (returnCard Monsters str))
  )
)

(defun delCard (str player)
  "Removes a card from the hand."
  (if (equalp player t)
    (setq Hand (remove (returnCard Hand str) Hand))
    (setq AIHand (remove (returnCard AIHand str) AIHand))
  )
)

(defun addMinion (str player)
  "Adds a minion into play."
  (if (equalp player t)
    (if (null Minions)
      (setq Minions (cons (returnCard Hand str) nil))
      (push (returnCard Hand str) (cdr (nthcdr 0 Minions)))
    )
    (if (null Monsters)
      (setq Monsters (cons (returnCard AIHand str) nil))
      (push (returnCard AIHand str) (cdr (nthcdr 0 Monsters)))
    )
  )
)

(defun delMinion (str player)
  "Removes a minion from play."
  (if (equalp player t)
    (setq Minions (remove (returnCard Minions str) Minions))
    (setq Monsters (remove (returnCard Monsters str) Monsters))
  )
)

(defun targetExists (str)
  "Does the target exist?"
  (if (returnCard Minions str) t
    (if (returnCard Monsters str) t
      (if (equalp "player" str) t
        (if (equalp "ai" str) t)
      )
    )
  )
)

(defun cardExists (str player)
  "Does the card exist in hand?"
  (if (equalp player t)
    (if (returnCard Hand str) t)
    (if (returnCard AIHand str) t)
  )
)

(defun unTap (player)
  "Untap all creatures for player."
  (if (equalp player t)
    (setq tapped '())
    (setq aitapped '())
  )
)

(defun isTapped (str player)
  "Has a creature attacked this turn?"
  (if (equalp player t)
    (if (find str tapped :test #'equalp) t)
    (if (find str tapped :test #'equalp) t)
  )
)

(defun addTapped (str player)
  "Tap a creature for a player."
  (if (isTapped str player) nil
    (if (equalp player t)
      (if (null tapped)
        (setq tapped (cons str nil))
        (push str (car tapped))
      )
      (if (null tapped)
        (setq aitapped (cons str nil))
        (push str (car aitapped))
      )
    )
  )
)

(defun playerTurn ()
  "Loop for the player's turn."
  (loop
    (progn
      (printState)
      (checkWin)
      (case (promptedread "Enter a number: 1) Quit, 2) End Turn, 3) Play, 4) Attack > ")
        (1 (quitGame))
        (2 (return-from playerTurn nil))
        (3 (playCard))
        (4 (attackMonster))
      )
    )
  )
)

(defun aiTurn ()
  "Loop for the AI's turn."
  ; execute one Attack for each monster on the board. each should aim at a random enemy (Player has same probability as each minion of being targeted).
  (aiAttack)
  ; play the highest-energy card <= available energy until energy is less than all available cards.
    ; Attack Spells target random enemies.
    ; Heal spells target random (wounded) minions (or the AI)
  (aiPlay)
  ; End Turn occurs when all monsters have attacked and all playable cards are played.
)

(defun aiAttack ()
  "AI attack phase."
  (progn
    ; if has monster
    (if (not (null Monsters))
      (loop for x in Monsters
        when (and (not (isTapped (nth 2 x) nil)) (> (minionAttack (nth 2 x) nil) 0))
        do (if (null Minions)
            (progn              ; attack player
              (setq Health (- Health (minionAttack (nth 2 x) nil)))
              (format t "~%~%AI attacked '~a' with '~a' for ~a damage.~%~%" "player" (nth 2 x) (minionAttack (nth 2 x) nil))
            )
            (case (random 2)    ; attack player or monster
              (0 (progn            ; player
                (setq Health (- Health (minionAttack (nth 2 x) nil)))
                (format t "~%~%AI attacked '~a' with '~a' for ~a damage.~%~%" "player" (nth 2 x) (minionAttack (nth 2 x) nil))
              ))
              (1 (progn            ; monster
                (damageMinion (nth 2 x) (nth 2 (car Minions)) nil) ; monster
                (addTapped (nth 2 x) nil)
                (format t "~%~%AI attacked '~a' with '~a' for ~a damage.~%~%" (nth 2 (car Minions)) (nth 2 x) (minionAttack (nth 2 x) nil))
              ))
            )
        )
      )
    )
    (checkMinionDeath)
  )
)

(defun aiPlay ()
  "AI play phase."
  (progn
    (if (or (null AIHand) (< Energy 1)) (return-from aiPlay nil))
    (if (>= Energy (nth 0 (car AIHand)))
      (progn
        (setq selection (nth 2 (car AIHand)))
        ; is spell?
        (if (equal (cardSpell selection nil) t)
          (progn
            ; is heal?
            (if (> (cardHealth selection nil) 0)
              (progn
                (setq AIHealth (+ AIHealth (cardHealth selection nil)))
                (format t "~%~%AI healed '~a' with '~a' for ~a health.~%~%" "ai" selection (cardHealth selection nil))
              )
            )
            ; is dmg?
            (if (> (cardAttack selection nil) 0)
              (progn
                (setq Health (- Health (cardAttack selection nil)))
                (format t "~%~%The AI damaged '~a' with '~a' for ~a damage.~%~%" "player" selection (cardAttack selection nil))
              )
            )
            ; cleanup
            (delEnergy selection nil)
            (delCard selection nil)
          )
        ; else is monster
          (progn
            (delEnergy selection nil)
            (addMinion selection nil)
            (delCard selection nil)
            (format t "~%~%The AI played a monster called ~a!~%~%" selection)
          )
        )
      )
    )
    ; check if monsters/minions are dead.
    (checkMinionDeath)
  )
)

;;;; START THE MAIN GAME LOOP
(loop
  (progn
    (setq Energy Turn)      ; set player energy to equal turn number
    (format t "(TURN #~a)~%" Turn)
    (unTap t)               ; untap player creatures
    (playerTurn)            ; PLAYER TURN
    (setq Energy Turn)      ; set AI energy to equal turn number
    (unTap nil)             ; untap ai creatures
    (aiTurn)                ; AI Turn
    (setq Turn (+ Turn 1))  ; Increment turn
  )
)
